{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall -Wno-redundant-constraints #-}

module Extra.Exceptionless
  ( Exceptionless(Exceptionless)
  , liftExceptionless, exceptionless
  , unExceptionless
  , runExceptionless
  , mapExceptionless
  , logExceptionless
  , tryExceptionless
  , catchExceptionless
  , handleExceptionless
  , catchExceptT
  , catchAllExceptT
  ) where

-- for reference:
--
-- try :: Exception e => IO a -> IO (Either e a)
-- try a = (Right <$> a) `catch` (pure . Left)
--
-- handle = flip catch

import Control.Exception (Exception(fromException, toException), SomeAsyncException(SomeAsyncException), SomeException)
import Control.Monad.Catch (MonadCatch(catch), MonadThrow(throwM), try)
import Control.Monad.Except (ExceptT, MonadError(catchError, throwError))
import Control.Monad.Reader (MonadReader(ask, local))
import Control.Monad.State as State (MonadState(get, put))
import Control.Monad.Trans (MonadIO(liftIO), MonadTrans(lift))
import GHC.Generics
import GHC.Stack (HasCallStack, callStack)
import SeeReason.Errors (isAsyncException)
import System.Log.Logger (logM, Priority(DEBUG))

-- | A monad transformer that catches all exceptions.
newtype Exceptionless m a = Exceptionless {unwrap :: m a} deriving Generic

instance Functor m => Functor (Exceptionless m) where
  fmap :: HasCallStack => (a -> b) -> Exceptionless m a -> Exceptionless m b
  fmap f (Exceptionless m) = Exceptionless $ fmap f m

instance (Monad m, Applicative m) => Applicative (Exceptionless m) where
  pure = Exceptionless . pure
  (<*>) :: HasCallStack => Exceptionless m (a -> b) -> Exceptionless m a -> Exceptionless m b
  (Exceptionless f) <*> (Exceptionless m) = Exceptionless $ f <*> m

instance (MonadCatch m, MonadIO m) => Monad (Exceptionless m) where
  (>>=) :: HasCallStack => Exceptionless m a -> (a -> Exceptionless m b) -> Exceptionless m b
  Exceptionless a >>= f = Exceptionless (a >>= unwrap . f)

instance (MonadCatch m, MonadIO m) => MonadIO (Exceptionless m) where
  liftIO io = liftExceptionless (liftIO io)

instance (MonadCatch m, MonadIO m) => MonadCatch (Exceptionless m) where
  catch (Exceptionless m) f = liftExceptionless (catch m (unwrap . f))

instance (MonadCatch m, MonadThrow m, MonadIO m) => MonadThrow (Exceptionless m) where
  throwM = liftExceptionless . throwM

instance MonadTrans Exceptionless where
  -- lift :: HasCallStack => m a -> Exceptionless m a
  lift m = liftExceptionless m

instance (MonadReader r m, MonadCatch m, MonadIO m) => MonadReader r (Exceptionless m) where
  ask = Exceptionless ask
  -- local :: MonadReader r m => (r -> r) -> Exceptionless m a -> Exceptionless m a
  local f m = mapExceptionless (local f) m

instance (MonadState s m, MonadCatch m, MonadIO m) => MonadState s (Exceptionless m) where
  get = Exceptionless State.get
  put = Exceptionless . State.put

instance (MonadError e m, MonadCatch m, MonadIO m) => MonadError e (Exceptionless m) where
  throwError e = Exceptionless $ throwError e
  catchError :: Exceptionless m a -> (e -> Exceptionless m a) -> Exceptionless m a
  catchError action handle =
    Exceptionless (catchError (unwrap action) (unwrap . handle))

-- | Apply the 'Exceptionless' newtype wrapper to a computation.
-- Exceptions are not caught here because this is a higher level call
-- and there will be less call stack information here than in
-- 'runExceptionless'.  This is equivalent to applying the
-- 'Exceptionless' constructor.
liftExceptionless :: HasCallStack => m a -> Exceptionless m a
liftExceptionless = Exceptionless

-- | Another name for 'liftExceptionless'.
exceptionless :: HasCallStack => m a -> Exceptionless m a
exceptionless = Exceptionless

-- | modify a Exceptionless computation without catching exceptions.
mapExceptionless :: (m a -> m b) -> Exceptionless m a -> Exceptionless m b
mapExceptionless f (Exceptionless m) = Exceptionless (f m)

-- | Use 'try' to bring a specific exception into the result value.
-- If @e ~ SomeException@ this should work on all exceptions.
tryExceptionless :: (MonadCatch m, Exception e) => Exceptionless m a -> Exceptionless m (Either e a)
tryExceptionless (Exceptionless m) = Exceptionless (try m)

-- | The 'catch' function for 'Exceptionless'.
catchExceptionless ::
  (MonadCatch m, MonadIO m, Exception e)
  => Exceptionless m a
  -> (e -> m a)
  -> Exceptionless m a
catchExceptionless m f = tryExceptionless m >>= lift . either f pure

-- | The 'handle' function for 'Exceptionless'.
handleExceptionless :: (MonadCatch m, MonadIO m, Exception e) => (e -> m a) -> Exceptionless m a -> Exceptionless m a
handleExceptionless = flip catchExceptionless

-- | Log and rethrow any exception.
logExceptionless ::
  (MonadCatch m, MonadIO m, HasCallStack)
  => Exceptionless m a
  -> Exceptionless m a
logExceptionless m =
  Exceptionless $ runExceptionless (\(e :: SomeException) -> liftIO (logM "Extra.Exceptionless" DEBUG (show e)) >> throwM e) m

-- | Exit the 'Exceptionless' monad, passing the 'SomeException'
-- containing any exception to @f@.
runExceptionless ::
  forall m a. (MonadCatch m, HasCallStack)
  => (SomeException -> m a)
  -> Exceptionless m a
  -> m a
runExceptionless f (Exceptionless m) =
  m `catch` handler
  where
    handler :: SomeException -> m a
    handler se
      | isAsyncException se = throwM se
      | otherwise = f se
    _ = callStack

-- | Catch some exception in the 'Exceptionless' monad.
{-
runExceptionless' ::
  forall e m a. (MonadCatch m, Exception e, HasCallStack)
  => (e -> m a)
  -> Exceptionless m a
  -> Exceptionless m a
runExceptionless' f (Exceptionless m) =
  Exceptionless (m `catch` f)
-}

unExceptionless ::
  forall m a. (MonadCatch m, HasCallStack)
  => (SomeException -> m a)
  -> Exceptionless m a
  -> m a
unExceptionless = runExceptionless

-- | Catch an exception thrown by @m@ in the ExceptT monad.
catchExceptT ::
  forall e m a. (MonadCatch m, Exception e, HasCallStack)
  => m a
  -> ExceptT e m a
catchExceptT m = (lift m) `catch` throwError

-- | Catch all exceptions thrown by @m@ in the ExceptT monad.
catchAllExceptT ::
  forall m a. (MonadCatch m, HasCallStack)
  => m a
  -> ExceptT SomeException m a
catchAllExceptT m = catchExceptT @SomeException m
