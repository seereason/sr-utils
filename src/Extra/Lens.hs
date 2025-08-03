{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS -Wall #-}

module Extra.Lens
  ( HasReader(ask)
  , HasState(get, put)
  , HasReader1(ask1)
  , HasState1(get1, put1)
  , HasLens(hasLens)
  , nubBy
  -- * Re-exports
  , ReifiedLens'
  , ReifiedLens(Lens),
  ) where

import Control.Lens
import Data.Generics.Labels ()
import GHC.Stack (HasCallStack)

-- | This says we can obtain a value of type r from monad @m@.  It is
-- similar to doing a 'view' on a lens, but doesn't need to be.
class Monad m => HasReader r m where
  ask :: HasCallStack => m r

-- | Similar to HasReader, but 'ask1' takes an argument of type @a@.
class Monad m => HasReader1 r m a where
  ask1 :: HasCallStack => a -> m r

view1 :: (HasReader1 s m k, HasCallStack) => k -> Getter s a -> m a
view1 k lns = view lns <$> ask1 k

use1 :: (HasState1 s m k, HasCallStack) => k -> Getter s a -> m a
use1 k lns = view lns <$> get1 k

-- | Similar to HasReader, but also provides 'put' which sets the @r@ value.
class Monad m => HasState r m where
  get :: HasCallStack => m r
  put :: HasCallStack => r -> m ()

-- | Similar to HasState1, but the methods also take an argument of type a.
class Monad m => HasState1 r m a where
  get1 :: HasCallStack => a -> m r
  put1 :: HasCallStack => a -> r -> m ()

over1 :: forall s m k a. (HasState1 s m k, HasCallStack) => k -> Lens' s a -> (a -> a) -> m ()
over1 k lns f = put1 @s k =<< (over lns f <$> get1 k)

assign1 :: (HasState1 s m k, HasCallStack) => k -> Lens' s a -> a -> m ()
assign1 k lns a = over1 k lns (const a)

-- | If you don't want to use the 'Dyn' declare a 'HasLens'
-- instance.  This is necessary if you want a persistant value
-- (Dyn has no Serialize instance) or because you already
-- have a location (not in Dyn) where the value is stored.
class HasLens s a where
  hasLens :: HasCallStack => Lens' s a

-- | The 'nubBy' function generalized for any Cons instance.  Adapted
-- from the code in Data.List.
--
-- >>> nubBy (\x y -> mod x 3 == mod y 3) ([1,2,4,5,6] :: Vector Int)
-- [1,2,6]
-- stolen from HBC
nubBy :: forall s a. (Cons s s a a, Monoid s) => (a -> a -> Bool) -> s -> s
nubBy eq l = nubBy' l mempty
  where
    nubBy' :: s -> s -> s
    nubBy' t xs =
      case uncons t of
        Nothing -> mempty
        Just (y, ys)
          | elem_by eq y xs -> nubBy' ys xs
          | otherwise -> cons y (nubBy' ys (cons y xs :: s))

-- Not exported:
-- Note that we keep the call to `eq` with arguments in the
-- same order as in the reference (prelude) implementation,
-- and that this order is different from how `elem` calls (==).
-- See #2528, #3280 and #7913.
-- 'xs' is the list of things we've seen so far,
-- 'y' is the potential new element
elem_by :: forall s a. (Cons s s a a, Monoid s) => (a -> a -> Bool) -> a -> s -> Bool
elem_by eq y t =
  case uncons t of
    Nothing -> False
    Just (x, xs) -> x `eq` y || elem_by eq y xs
