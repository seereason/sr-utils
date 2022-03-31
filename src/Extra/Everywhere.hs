{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall #-}

module Extra.Everywhere
  ( Everywhere(everywhere)
  , tests
  ) where

import Data.ListLike as LL (length, ListLike, take)
import Data.Ratio (Ratio)
import Data.String (IsString)
import Data.Text as Text (Text)
import qualified Data.Text.Lazy as Lazy (Text)
import Data.Typeable (cast, Typeable)
import Data.Word (Word8)
import Debug.Trace
import GHC.Generics
import Test.HUnit

-- | The 'everywhere' function will apply a function to every
-- occurrence we can find of type @b@ within value @a@.  We use the
-- 'Typeable' instance to determine when @a ~ b@.  The 'Show' is for
-- debugging.
class (Typeable a, Typeable s) => Everywhere a s where
  everywhere :: (a -> a) -> s -> s
  default everywhere :: (GEverywhere a (Rep s), Generic s) => (a -> a) -> s -> s
  everywhere f = (to :: Rep s x -> s) . geverywhere f . (from :: s -> Rep s x)
  -- ^ By default, the everywhere function calls the generic version
  -- 'geverywhere', which works on any instance of 'Generic'.

-- | This class has instances that match the different structures of
-- type @a@'s representation @f p@.
class GEverywhere a f where
  geverywhere :: (a -> a) -> f p -> f p

-- | Case for metadata (type constructor, data constructor, field selector)
instance GEverywhere a f => GEverywhere a (M1 i c f) where
  geverywhere f (M1 x) = trace "M1" (M1 (geverywhere f x))

-- | Case for product types
instance (GEverywhere a f, GEverywhere a g) => GEverywhere a (f :*: g) where
  geverywhere f (x :*: y) = trace ":*:" (geverywhere f x :*: geverywhere f y)

-- | Case for sum types
instance (GEverywhere a f, GEverywhere a g) => GEverywhere a (f :+: g) where
  geverywhere f (L1 x) = trace "L1" (L1 (geverywhere f x))
  geverywhere f (R1 y) = trace "R1" (R1 (geverywhere f y))

-- | Case for datatypes without any data constructors (why not?)
instance GEverywhere a V1 where
  geverywhere _ v1 = trace "V1" (v1 `seq` error "geverywhere.V1")

-- | Case for datatypes without any fields
instance GEverywhere b U1 where
  geverywhere _ U1 = trace "U1" U1

-- | Case for data constructor fields.  Here we attempt to apply @f@,
-- and then recurse into the 'everywhere' function for some new type.
instance Everywhere a s => GEverywhere a (K1 i s) where
  geverywhere f (K1 x) =
    case cast x :: Maybe a of
      Nothing -> K1 (everywhere f x)
      Just x' -> case cast (f x') :: Maybe s of
                   Nothing -> K1 (everywhere f x) -- should never happen - if here we know s ~ a
                   Just x'' -> K1 x''

-- The OVERLAPPABLE lets us override specific types with additional instances.
instance {-# OVERLAPPABLE #-} (Typeable a, Typeable s, Generic s, GEverywhere a (Rep s)) => Everywhere a s

-- | Instances for types with no Generic instance.
instance Typeable r => Everywhere Char r where everywhere f r = mkT @Char f r
instance (Typeable r, Typeable s) => Everywhere (Ratio s) r where everywhere f r = mkT @(Ratio s) f r
instance Typeable r => Everywhere Integer r where everywhere f r = mkT @Integer f r
instance Typeable r => Everywhere Word8 r where everywhere f r = mkT @Word8 f r
instance Typeable r => Everywhere Int r where everywhere f r = mkT @Int f r
instance Typeable r => Everywhere String r where everywhere f r = mkT @String f r
instance Typeable r => Everywhere Text.Text r where everywhere f r = mkT @Text.Text f r
instance Typeable r => Everywhere Lazy.Text r where everywhere f r = mkT @Lazy.Text f r

-- | If a and b are the same type apply f.
mkT :: forall a b. (Typeable a, Typeable b) => (a -> a) -> b -> b
mkT f r = maybe r (\r' -> maybe r id (cast (f r'))) (cast @_ @a r)

-- | Haven't tried this.
extT :: (Typeable a, Typeable b, Typeable r) => (a -> a) -> (b -> b) -> r -> r
extT f g = mkT f . mkT g

shorten :: forall s. (Generic s, Typeable s, Show s) => Int -> s -> s
shorten n = f . g . h
  where
    f :: s -> s
    f = everywhere @String @s (shorten' n :: String -> String)
    g :: s -> s
    g = everywhere @Text.Text @s (shorten' n :: Text.Text -> Text.Text)
    h :: s -> s
    h = everywhere @Lazy.Text @s (shorten' n :: Lazy.Text -> Lazy.Text)

shorten' :: (LL.ListLike a Char, IsString a) => Int -> a -> a
shorten' n a =
      case LL.length a of
        i | i <= n -> a
        _i -> LL.take n a <> "..."

data T = T {str :: String, txt :: Text} deriving (Generic, Eq, Show)

-- > runTestTT tests
-- expected: T {str = "01234...", txt = "01234..."}
--  but got: T {str = "01234567890", txt = "01234567890"}
tests :: Test
tests =
  TestList
  [ TestCase (let s = "01234567890" :: String in
                 assertEqual "shorten 1"
                   "01234..."
                   (shorten 5 s))
  , TestCase (let s = T "01234567890" "01234567890" :: T in
                 assertEqual "shorten 2"
                   (T "01234..." "01234...")
                   (shorten 5 s))
  , TestCase (let s = ('a', 'b', 'c') in
                 assertEqual "shorten 2"
                   s
                   (shorten 5 s))
  ]
