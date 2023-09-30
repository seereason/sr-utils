-- | A @Lens' s a@ paired with an @a@.  The lens is guaranteed to be a
-- non-empty (in fact singular) traversal of s.

{-# LANGUAGE OverloadedLabels #-}

module Extra.Lens
  ( LensValue(.. {-_lens, _lval-})
  , idLens
  , idTraversal
  , dotLens
  , dotTraversal
  , predotLens
  , predotTraversal
  , readerLens
  , readerTraversal
  , stateLens
  , stateTraversal
  , theLens
  , theGetter
  , modifyLensValue
  , nubBy
  -- * Re-exports
  , ReifiedLens'
  , ReifiedLens(Lens),
  ) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Data.Generics.Labels ()
import GHC.Generics (Generic)

data LensValue s a =
  LensValue {
    _lens :: ReifiedLens' s a,
    _lval :: a
    } deriving Generic

idLens :: a -> LensValue a a
idLens a = LensValue (Lens id) a

idTraversal :: a -> LensValue a a
idTraversal a = LensValue (Lens id) a

dotLens :: LensValue s a -> Lens' a b -> LensValue s b
dotLens (LensValue (Lens sa) a) ab = LensValue (Lens (sa . ab)) (view ab a)

dotTraversal :: LensValue s a -> Traversal' a b -> Maybe (LensValue s b)
dotTraversal (LensValue (Lens sa) a) ab =
  fmap (LensValue (Lens (sa . unsafeSingular ab))) (preview ab a)

-- | Build a 'LensValue' for a type t which contains an s.
predotLens :: LensValue s a -> Lens' t s -> LensValue t a
predotLens (LensValue (Lens sa) a) ts = LensValue (Lens (ts . sa)) a

-- | Dicey.  We don't have a t, so we can't know whether it contains
-- the required a.  Thus Lens' t s rather than Traversal' t s.
predotTraversal :: LensValue s a -> Lens' t s -> LensValue t a
predotTraversal (LensValue (Lens sa) a) ts =
  LensValue (Lens (ts . sa)) a

readerLens :: MonadReader s m => Lens' s a -> m (LensValue s a)
readerLens lns = LensValue (Lens lns) <$> view lns

readerTraversal :: MonadReader s m => Traversal' s a -> m (Maybe (LensValue s a))
readerTraversal lns = fmap (LensValue (Lens (unsafeSingular lns))) <$> preview lns

-- | Build a 'LensValue' from a lens and the state monad
stateLens :: MonadState s m => Lens' s a -> m (LensValue s a)
stateLens lns = LensValue (Lens lns) <$> use lns

stateTraversal :: MonadState s m => Traversal' s a -> m (Maybe (LensValue s a))
stateTraversal lns = fmap (LensValue (Lens (unsafeSingular lns))) <$> preuse lns

-- When we modify the value this targets we also need to modify _lval.
-- So _lens should be private.  Can we make a lens that targets more
-- than one value?  I think so.
theLens :: LensValue s a -> Lens' s a
theLens = runLens . _lens
{-# WARNING theLens "Use theGetter or modifyLensValue" #-}

theGetter :: LensValue s a -> Getter s a
theGetter = theLens

modifyLensValue :: MonadState s m => (a -> a) -> LensValue s a -> m (LensValue s a)
modifyLensValue f v = do
  runLens (_lens v) %= f
  pure $ over (#_lval :: Lens' (LensValue s a) a) f v

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
