{-# LANGUAGE InstanceSigs #-}

module Extra.Debug
  ( Temp(Tmp, unTmp)
  , TempT, liftTmpT, unTmpT, runTmpT
  ) where

import Control.Monad.Trans
import Control.Monad.RWS
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics

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
#if 1
type TempT m = RWST () [Text] (Map Text Text) m

unTmpT :: Monad m => TempT m a -> m a
unTmpT m = fst <$> evalRWST m () mempty

runTmpT :: Monad m => TempT m a -> m (a, Map Text Text, [Text])
runTmpT m = runRWST m () mempty

liftTmpT :: Monad m => m a -> TempT m a
liftTmpT = lift

#else
newtype TempT m a = TmpT {unTmpT :: m a}

deriving instance Generic (TempT m a)

instance Functor m => Functor (TempT m) where
  fmap :: (a -> b) -> TempT m a -> TempT m b
  fmap f a = TmpT $ fmap f $ unTmpT a

instance Applicative m => Applicative (TempT m) where
  pure = TmpT . pure
  (<*>) :: TempT m (a -> b) -> TempT m a -> TempT m b
  f <*> a = TmpT $ (unTmpT f) <*> (unTmpT a)

instance Monad m => Monad (TempT m) where
  (>>=) :: TempT m a -> (a -> TempT m b) -> TempT m b
  a >>= f = TmpT ((unTmpT a) >>= (unTmpT . f))

instance MonadTrans TempT where
  lift :: Monad m => m a -> TempT m a
  lift m = TmpT m
#endif
