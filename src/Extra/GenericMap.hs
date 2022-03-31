{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall #-}

module Extra.GenericMap
  ( GmapM_(gmapM_)
  , gFind
  ) where

import Data.Typeable (cast, Typeable)
import Control.Monad.Writer -- (execWriter, tell)
import GHC.Generics
import Data.Ratio (Ratio)
import Data.Word (Word8)

-- λ> gFind ["abc", "def"] :: String
-- "abcdef"
gFind :: GmapM_ b a (Writer [b]) => a -> [b]
gFind b = execWriter (gmapM_ (\a -> tell [a]) b)

-- | The 'gmapM_' function will apply a monadic function to every
-- occurrence we can find of type @b@ within value @a@.  We use the
-- 'Typeable' instance to determine when @a ~ b@.
--
-- λ> execWriter (gFind @Char (\c -> tell [c]) ["abc", "def"])
-- "abcdef"
class (Typeable b, Typeable a, Applicative m) => GmapM_ b a m where
  gmapM_ :: (b -> m ()) -> a -> m a
  default gmapM_ :: (GGmapM_ m b (Rep a), Generic a) => (b -> m ()) -> a -> m a
  gmapM_ f a = do
    (to :: Rep a x -> a) <$> ggmapM_ f (from a :: Rep a x)
  -- ^ By default, the gmapM_ function calls the generic version
  -- 'ggmapM_', which works on any instance of 'Generic'.

-- | This class has instances that match the different structures of
-- type @a@'s representation @f p@.
class Applicative m => GGmapM_ m b f where
  ggmapM_ :: (b -> m ()) -> f p -> m (f p)

-- | Case for metadata (type constructor, data constructor, field selector)
instance GGmapM_ m b f => GGmapM_ m b (M1 i c f) where
  ggmapM_ f (M1 x) = M1 <$> ggmapM_ f x

-- | Case for product types
instance (Applicative m, GGmapM_ m b f, GGmapM_ m b g) => GGmapM_ m b (f :*: g) where
  ggmapM_ f (x :*: y) = (:*:) <$> ggmapM_ f x <*> ggmapM_ f y

-- | Case for sum types
instance (Monad m, GGmapM_ m b f, GGmapM_ m b g) => GGmapM_ m b (f :+: g) where
  ggmapM_ f (L1 x) = L1 <$> ggmapM_ f x
  ggmapM_ f (R1 y) = R1 <$> ggmapM_ f y

-- | Case for datatypes without any data constructors (why not?)
instance Monad m => GGmapM_ m b V1 where
  ggmapM_ _ v1 = v1 `seq` error "ggmapM_.V1"

-- | Case for datatypes without any fields
instance Applicative m => GGmapM_ m b U1 where
  ggmapM_ _ U1 = pure U1

-- | Case for data constructor fields.  Here we attempt to apply @f@,
-- and then recurse into the 'gmapM_' function for some new type.
instance (GmapM_ b a m) => GGmapM_ m b (K1 i a) where
  ggmapM_ f (K1 x) =
    case cast x :: Maybe b of
      Nothing -> K1 <$> gmapM_ f x
      Just x' -> f x' *> (K1 <$> gmapM_ f x)

-- The OVERLAPPABLE lets us override specific types with additional instances.
instance {-# OVERLAPPABLE #-} (Monad m, Typeable b, Typeable a, Generic a, GGmapM_ m b (Rep a)) => GmapM_ b a m

-- Instances for types with no Generic instance.
instance (Monad m, Typeable b, Typeable a) => GmapM_ b (Ratio a) m where gmapM_ _ r = pure r
instance (Monad m, Typeable b) => GmapM_ b Char m where gmapM_ _ r = pure r
instance (Monad m, Typeable b) => GmapM_ b Integer m where gmapM_ _ r = pure r
instance (Monad m, Typeable b) => GmapM_ b Word8 m where gmapM_ _ r = pure r
instance (Monad m, Typeable b) => GmapM_ b Int m where gmapM_ _ r = pure r
