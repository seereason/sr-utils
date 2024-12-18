{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS -Wall #-}

module Extra.Lens
  ( HasLens(hasLens)
  , nubBy
  -- * Re-exports
  , ReifiedLens'
  , ReifiedLens(Lens),
  ) where

import Control.Lens
import Data.Generics.Labels ()
import GHC.Stack (HasCallStack)

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
