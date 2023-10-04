{-# LANGUAGE CPP, DeriveGeneric, FlexibleInstances, InstanceSigs, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, StandaloneDeriving, UndecidableInstances #-}
{-# OPTIONS -Wall #-}

module Extra.Debug
  ( Temp(Tmp, unTmp)
  , TempT, liftTmpT
  , unTmpT, runTmpT
  , mapTempT
  , catchTempT
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
newtype TempT m a = TmpT {unwrap :: m a} deriving Generic

-- | Log and rethrow any exception.  If you have a function in the
-- TempT monad you can be sure that all exceptions will be caught at
-- the point where either 'unTmpT' or 'runTmpT' is called, as these
-- are the only exits from the monad.
unTmpT :: (MonadCatch m, MonadIO m, HasCallStack) => TempT m a -> m a
unTmpT m =
  runTmpT (\(e :: SomeException) -> alogDrop id DEBUG (show e) >> throwM e) m

-- | Pass any exception to f.
runTmpT ::
  forall m a. (MonadCatch m, HasCallStack)
  => (SomeException -> m a)
  -> TempT m a
  -> m a
runTmpT f (TmpT m) =
  m `catch` f

-- | Apply the 'TmpT' newtype wrapper to a computation.  Exceptions
-- are not caught here because this is a higher level call and there
-- will be less call stack information here than in 'runTmpT'.
liftTmpT :: HasCallStack => m a -> TempT m a
liftTmpT = TmpT

-- | modify a TempT computation without catching exceptions.
mapTempT :: (m a -> m a) -> TempT m a -> TempT m a
mapTempT f (TmpT m) = TmpT (f m)

-- | Catch a specific exception in the TempT monad.
catchTempT :: (MonadCatch m, Exception e) => TempT m a -> (e -> m a) -> TempT m a
catchTempT m f = TmpT (unwrap m `catch` f)

instance Functor m => Functor (TempT m) where
  fmap :: HasCallStack => (a -> b) -> TempT m a -> TempT m b
  fmap f (TmpT m) = TmpT $ fmap f m

instance (Monad m, Applicative m) => Applicative (TempT m) where
  pure = TmpT . pure
  (<*>) :: HasCallStack => TempT m (a -> b) -> TempT m a -> TempT m b
  (TmpT f) <*> (TmpT m) = TmpT $ f <*> m

instance (MonadCatch m, MonadIO m) => Monad (TempT m) where
  (>>=) :: HasCallStack => TempT m a -> (a -> TempT m b) -> TempT m b
  TmpT a >>= f = TmpT (a >>= unwrap . f)

instance (MonadCatch m, MonadIO m) => MonadIO (TempT m) where
  liftIO io = liftTmpT (liftIO io)

instance (MonadCatch m, MonadIO m) => MonadCatch (TempT m) where
  catch (TmpT m) f = liftTmpT (catch m (unwrap . f))

instance (MonadCatch m, MonadThrow m, MonadIO m) => MonadThrow (TempT m) where
  throwM = liftTmpT . throwM

instance MonadTrans TempT where
  lift :: Monad m => m a -> TempT m a
  lift m = liftTmpT m
instance (MonadReader r m, MonadCatch m, MonadIO m) => MonadReader r (TempT m) where
  ask = TmpT ask
  local :: MonadReader r m => (r -> r) -> TempT m a -> TempT m a
  local f m = mapTempT (local f) m
instance (MonadState s m, MonadCatch m, MonadIO m) => MonadState s (TempT m) where
  get = TmpT State.get
  put = TmpT . State.put
instance (MonadError e m, MonadCatch m, MonadIO m) => MonadError e (TempT m) where
  throwError e = TmpT $ throwError e
  catchError :: TempT m a -> (e -> TempT m a) -> TempT m a
  catchError action handle =
    TmpT (catchError (unwrap action) (unwrap . handle))

-- | A newtype wrapper for propagating changes through the codebase.
-- Add this to an argument that needs to change and the caller will
-- not compile until the changes have been made there.  Note that the
-- type constructor and data constructors have different names to make
-- it easier to grep for one or the other.  After the 'Temp' wrapper
-- is added, it is replaced by a more useful type.
newtype Temp a = Tmp {unTmp :: a}

{-
-- | Note that Temp has none of the obvious instances, such as
-- 'Functor', 'Applicative', 'Monad', or 'Generic'.  These can make
-- adding 'Temp' wrappers more difficult because the compiler can find
-- clever ways to do the wrong thing.  Some instances that might be of
-- temporary use are given below, they can be copied into your code.
-- DO NOT DELETE OR UNCOMMENT.
instance Monad Temp where
  (>>=) :: Temp a -> (a -> Temp b) -> Temp b
  a >>= f = f (unTmp a)

instance Applicative Temp where
  pure = Tmp
  (<*>) :: Temp (a -> b) -> Temp a -> Temp b
  f <*> a = Tmp $ (unTmp f) (unTmp a)

instance Functor Temp where
  fmap :: (a -> b) -> Temp a -> Temp b
  fmap f a = Tmp $ f $ unTmp a

deriving instance Generic (Temp a)
-}
