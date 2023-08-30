module Extra.Debug (Temp(Tmp, unTmp)) where

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
