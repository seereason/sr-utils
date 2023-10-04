{-# LANGUAGE InstanceSigs #-}

module Extra.Debug
  ( Temp(Tmp, unTmp)
  , TempT, liftTmpT, unTmpT, runTmpT
  ) where

import Control.Monad.Catch
import Control.Monad.Trans
import GHC.Generics
import GHC.Stack (HasCallStack, callStack)
import SeeReason.Log (alogDrop, Priority(DEBUG))

-- | A newtype wrapper for propagating changes through the codebase.
-- Add this to an argument that needs to change and the caller will
-- not compile until the changes have been made there.  Note that the
-- type constructor and data constructors have different names to make
-- it easier to grep for one or the other.  After the 'Temp' wrapper
-- is added, it is replaced by a more useful type.
newtype Temp a = Tmp {unTmp :: a}

#if 0
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
#endif

-- Monad transformer version of Temp.  With instances.
-- newtype TempT m a = TmpT (RWST () [Text] (Map Text Text) m a)
newtype TempT m a = TmpT (m a)

deriving instance Generic (TempT m a)

-- | Log and rethrow any exception.
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

liftTmpT :: HasCallStack => m a -> TempT m a
liftTmpT = TmpT

instance Functor m => Functor (TempT m) where
  fmap :: HasCallStack => (a -> b) -> TempT m a -> TempT m b
  fmap f (TmpT m) = TmpT $ fmap f m

instance (Monad m, Applicative m) => Applicative (TempT m) where
  pure = TmpT . pure
  (<*>) :: HasCallStack => TempT m (a -> b) -> TempT m a -> TempT m b
  (TmpT f) <*> (TmpT m) = TmpT $ f <*> m

instance (MonadCatch m, MonadIO m) => Monad (TempT m) where
  (>>=) :: HasCallStack => TempT m a -> (a -> TempT m b) -> TempT m b
  TmpT a >>= f = TmpT (a >>= unTmpT . f)

instance (MonadCatch m, MonadIO m) => MonadIO (TempT m) where
  liftIO io = liftTmpT (liftIO io)

instance (MonadCatch m, MonadIO m) => MonadCatch (TempT m) where
  catch (TmpT m) f = liftTmpT (catch m (unTmpT . f))

instance (MonadCatch m, MonadThrow m, MonadIO m) => MonadThrow (TempT m) where
  throwM = liftTmpT . throwM

#if 0
instance MonadTrans TempT where
  lift :: Monad m => m a -> TempT m a
  lift m = TmpT (lift m)
#endif
