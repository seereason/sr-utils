{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Extra.Lens
  ( HasLens(hasLens)
  , HasLens1(hasLens1)
  , HasM(hasM), viewM, previewM
  , HasM1(hasM1)
  , PutM(putM), assignM
  , PutM1(putM1)
  , nubBy
  -- * Re-exports
  , ReifiedLens'
  , ReifiedLens(Lens),
  ) where

import Control.Lens
import Data.Generics.Labels ()
import Data.Typeable (Typeable)
import GHC.Stack (HasCallStack)

-- | This says we can obtain a value of type r from monad @m@.  It is
-- similar to doing a 'view' on a lens, but doesn't need to be a
-- reader monad.  These classes really belong in a module called
-- Extra.Has, they are more general than HasLens.  (I'm not sure these
-- are a good idea, they confused me when I first tried them. -dsf)
class Monad m => HasM r m where hasM :: m r
class Monad m => HasM1 r m k where hasM1 :: k -> m r

viewM :: HasM r m => Getter r a -> m a
viewM lns = view lns <$> hasM

previewM :: HasM r m => Fold r a -> m (Maybe a)
previewM lns = preview lns <$> hasM

-- | These say we can both get a value from and put a value into a
-- monad m, like a state monad.
class HasM r m => PutM r m where putM :: r -> m ()
class HasM1 r m k => PutM1 r m k where putM1 :: k -> r -> m ()

assignM :: forall s m a b. PutM s m => ASetter s s a b -> b -> m ()
assignM lns b = putM @s =<< set lns b <$> hasM

class HasLens1 s r k where
  hasLens1 :: HasCallStack => k -> Lens' s r

-- | If you don't want to use the 'Dyn' declare a 'HasLens'
-- instance.  This is necessary if you want a persistant value
-- (Dyn has no Serialize instance) or because you already
-- have a location (not in Dyn) where the value is stored.
class Typeable s => HasLens s r where
  hasLens :: HasCallStack => Lens' s r

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
