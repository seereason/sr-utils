{-# LANGUAGE CPP, DeriveGeneric, FlexibleInstances, InstanceSigs, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, StandaloneDeriving, UndecidableInstances #-}
{-# OPTIONS -Wall #-}

module Extra.Exceptionless
  ( Exceptionless, liftExceptionless
  , unExceptionless
  , runExceptionless
  , mapExceptionless
  , catchExceptionless
  ) where

import Control.Monad.Catch (Exception, MonadCatch(catch), MonadThrow(throwM), SomeException)
import Control.Monad.Except (MonadError(catchError, throwError))
import Control.Monad.Reader (MonadReader(ask, local))
import Control.Monad.State as State (MonadState(get, put))
import Control.Monad.Trans (MonadIO(liftIO), MonadTrans(lift))
import GHC.Generics
import GHC.Stack (HasCallStack)
import SeeReason.Log (alogDrop, Priority(DEBUG))

-- | A monad transformer that catches all exceptions.
newtype Exceptionless m a = Exceptionless {unwrap :: m a} deriving Generic

-- | Log and rethrow any exception.  If you have a function in the
-- Exceptionless monad you can be sure that all exceptions will be caught at
-- the point where either 'unTmpT' or 'runTmpT' is called, as these
-- are the only exits from the monad.
unExceptionless :: (MonadCatch m, MonadIO m, HasCallStack) => Exceptionless m a -> m a
unExceptionless m =
  runExceptionless (\(e :: SomeException) -> alogDrop id DEBUG (show e) >> throwM e) m

-- | Pass any exception to f.
runExceptionless ::
  forall m a. (MonadCatch m, HasCallStack)
  => (SomeException -> m a)
  -> Exceptionless m a
  -> m a
runExceptionless f (Exceptionless m) =
  m `catch` f

-- | Apply the 'Exceptionless' newtype wrapper to a computation.  Exceptions
-- are not caught here because this is a higher level call and there
-- will be less call stack information here than in 'runExceptionless'.
liftExceptionless :: HasCallStack => m a -> Exceptionless m a
liftExceptionless = Exceptionless

-- | modify a Exceptionless computation without catching exceptions.
mapExceptionless :: (m a -> m a) -> Exceptionless m a -> Exceptionless m a
mapExceptionless f (Exceptionless m) = Exceptionless (f m)

-- | Catch a specific exception in the Exceptionless monad.
catchExceptionless :: (MonadCatch m, Exception e) => Exceptionless m a -> (e -> m a) -> Exceptionless m a
catchExceptionless m f = Exceptionless (unwrap m `catch` f)

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
