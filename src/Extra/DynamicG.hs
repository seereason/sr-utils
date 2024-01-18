-- | Data.Dynamic with additional constraints on the type.  No
-- implementation for dynApply and dynApp.
--
-- Operations for injecting values of arbitrary type into a
-- dynamically typed value, Dynamic, are provided, together with
-- operations for converting dynamic values into a concrete
-- (monomorphic) type.

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Extra.DynamicG
  (
        -- * The @Dynamic@ type
        Dynamic(..),

        -- * Converting to and from @Dynamic@
        toDyn,
        fromDyn,
        fromDynamic,

        dynTypeRep,

        -- * Convenience re-exports
        Typeable

  ) where


import Data.Type.Equality
import Type.Reflection
import Data.Maybe

import GHC.Base
import GHC.Show
import GHC.Exception

import GHC.Generics
import Data.SafeCopy
import Data.Serialize

-------------------------------------------------------------
--
--              The type Dynamic
--
-------------------------------------------------------------

{-|
  A value of type 'Dynamic' is an object encapsulated together with its type.

  A 'Dynamic' may only represent a monomorphic value; an attempt to
  create a value of type 'Dynamic' from a polymorphically-typed
  expression will result in an ambiguity error (see 'toDyn').
-}
data Dynamic where
    Dynamic :: forall a. TypeRep a -> Dyn a -> Dynamic

type Constraints a = (Generic a, Typeable a, SafeCopy a, Serialize a, Show a)

-- | A wrapper GADT that imposes the extra constraints on a.
data Dyn a where
  Dyn :: Constraints a => a -> Dyn a

-- Adapted from the -ddump-derivs output for a plain newtype
instance Constraints a => Generic (Dyn a) where
  from (Dyn g1) = M1 (M1 (M1 (K1 g1)))
  to (M1 (M1 (M1 (K1 g1)))) = Dyn g1

  type Rep (Dyn a) =
    D1 ('MetaData "Dyn" "Extra.DynamicG" "sr-utils" 'True)
       (C1 ('MetaCons "Dyn" 'PrefixI 'False)
           (S1 ('MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
               (Rec0 a)))

deriving instance Constraints v => Serialize (Dyn v)
deriving instance Constraints v => SafeCopy (Dyn v)
deriving instance Constraints v => Typeable (Dyn v)

-- | @since 2.01
instance Show Dynamic where
   showsPrec p (Dynamic t (Dyn v)) =
          showsPrec p v
          -- showString " :: " .
          -- showsPrec 0 t  .

-- here so that it isn't an orphan:
-- | @since 4.0.0.0
instance Exception Dynamic

-- | Converts an arbitrary value into an object of type 'Dynamic'.
--
-- The type of the object must be an instance of 'Typeable', which
-- ensures that only monomorphically-typed objects may be converted to
-- 'Dynamic'.  To convert a polymorphic object into 'Dynamic', give it
-- a monomorphic type signature.  For example:
--
-- >    toDyn (id :: Int -> Int)
--
toDyn :: Constraints a => a -> Dynamic
toDyn v = Dynamic typeRep (Dyn v)

-- | Converts a 'Dynamic' object back into an ordinary Haskell value of
-- the correct type.  See also 'fromDynamic'.
fromDyn :: Typeable a
        => Dynamic     -- ^ the dynamically-typed object
        -> a            -- ^ a default value
        -> a            -- ^ returns: the value of the first argument, if
                        -- it has the correct type, otherwise the value of
                        -- the second argument.
fromDyn (Dynamic t (Dyn v)) def
  | Just HRefl <- t `eqTypeRep` typeOf def = v
  | otherwise                              = def

-- | Converts a 'Dynamic' object back into an ordinary Haskell value of
-- the correct type.  See also 'fromDyn'.
fromDynamic
        :: forall a. Typeable a
        => Dynamic      -- ^ the dynamically-typed object
        -> Maybe a      -- ^ returns: @'Just' a@, if the dynamically-typed
                        -- object has the correct type (and @a@ is its value),
                        -- or 'Nothing' otherwise.
fromDynamic (Dynamic t (Dyn v))
  | Just HRefl <- t `eqTypeRep` rep = Just v
  | otherwise                       = Nothing
  where rep = typeRep :: TypeRep a

dynTypeRep :: Dynamic -> SomeTypeRep
dynTypeRep (Dynamic tr _) = SomeTypeRep tr
