-- | This module exports a template haskell function to create
-- Serialize instances based on the SafeCopy instance, and an
-- alternative decode function that puts the decode type in the error
-- message.  It also re-exports all other Data.Serialize symbols

-- {-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Extra.Serialize
    ( DecodeError(..)
    , module Data.Serialize
    , deriveSerializeViaSafeCopy
    , decodeAll
    -- , decode'
    , FakeTypeRep(..)
    , fakeTypeRep
    , decodePrism
    , encodeGetter
    , HasDecodeError(fromDecodeError)
    ) where

--import Control.Exception (ErrorCall(..), evaluate, )
import Control.Lens (Getter, _Left, over, Prism', prism, re)
--import Control.Monad.Catch (catch, MonadCatch)
import Control.Monad.Except (liftEither, MonadError)
import Data.ByteString as B (ByteString, null)
#ifndef OMIT_DATA_INSTANCES
import Data.Data (Data)
#endif
import Data.Data (Proxy(Proxy))
import Data.SafeCopy (SafeCopy(..), safeGet, safePut)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup((<>)))
#endif
import Data.Serialize
import Data.Text as T hiding (concat, intercalate)
import Data.Text.Lazy as LT hiding (concat, intercalate)
import Data.Text.Encoding as TE
import Data.Text.Lazy.Encoding as TLE
import Data.Time (UTCTime(..), Day(ModifiedJulianDay), toModifiedJulianDay, DiffTime)
import Data.Typeable (Typeable, typeRep)
import Data.UUID.Orphans ()
import Data.UUID (UUID)
import Data.UUID.Orphans ()
import Extra.Orphans ()
import Extra.Time (Zulu(..))
import GHC.Generics (Generic)
import Language.Haskell.TH (Dec, Loc(..), TypeQ, Q)
import Network.URI (URI(..), URIAuth(..))
--import System.IO.Unsafe (unsafePerformIO)

#if 0
-- We can't make a Data instance for TypeRep because part of it is in
-- Data.Typeable.Internal, a hidden module in base.
instance SafeCopy TypeRep
deriving instance Data TypeRep
data DecodeError = DecodeError ByteString TypeRep String deriving (Generic, Eq, Ord, Typeable)
#else
newtype FakeTypeRep = FakeTypeRep String deriving (Generic, Eq, Ord, Serialize)
instance SafeCopy FakeTypeRep
fakeTypeRep :: forall a. Typeable a => Proxy a -> FakeTypeRep
fakeTypeRep a = FakeTypeRep (show (typeRep a))

data DecodeError = DecodeError ByteString FakeTypeRep String deriving (Generic, Eq, Ord, Typeable)
#endif

instance Serialize DecodeError where get = safeGet; put = safePut

-- | Decode a value from a strict ByteString, reconstructing the original
-- structure.  Unlike Data.Serialize.decode, this function only succeeds
-- if all the input is consumed.  Not sure if this is in use anywhere.
--
--   > Data.Serialize.decode (encode 'x' <> encode 'y') :: Either String Char
--   Right 'x'
--   > Extra.Serialize.decodeAll (encode 'x' <> encode 'y') :: Either String Char
--   Left "decode \"xy\" failed to consume \"y\""
decodeAll :: forall a. Serialize a => ByteString -> Either String a
decodeAll b =
  case runGetState get b 0 of
    Left s -> Left s
    Right (a, more) | B.null more -> Right a
    Right (_, more) -> Left ("decode " <> show b <> " failed to consume " <> show more)

-- | A Serialize instance based on safecopy.  This means that
-- migrations will be performed upon deserialization, which is handy
-- if the value is stored in the browser's local storage.  Thus, zero
-- downtime upgrades!
deriveSerializeViaSafeCopy :: TypeQ -> Q [Dec]
deriveSerializeViaSafeCopy typ =
    [d|instance Serialize $typ where
          get = safeGet
          put = safePut|]

-- | Lift 'decode' into a monad and improve its error type
decodeM :: forall a m. (MonadError DecodeError m, Serialize a, Typeable a) => ByteString -> m a
decodeM bs = liftEither $ over _Left (DecodeError bs (fakeTypeRep (Proxy @a))) $ decode bs

#if 0
-- | Version of decode that catches any thrown ErrorCall and modifies
-- its message.
decode' :: forall a. (Serialize a) => ByteString -> Either String a
decode' b =
  unsafePerformIO (evaluate (decode b :: Either String a) `catch` handle)
  where
    handle :: ErrorCall -> IO (Either String a)
    handle e = return $ Left (show e)
#endif

decodePrism :: forall a. (Serialize a) => Prism' ByteString a
decodePrism = prism encode (\s -> either (\_ -> Left s) Right (decode s :: Either String a))

encodeGetter :: forall a. (Serialize a) => Getter a ByteString
encodeGetter = re decodePrism

instance SafeCopy DecodeError where version = 1

#ifndef OMIT_DATA_INSTANCES
deriving instance Data FakeTypeRep
deriving instance Data DecodeError
#endif

#ifndef OMIT_SHOW_INSTANCES
deriving instance Show FakeTypeRep
deriving instance Show DecodeError
#endif

-- Required by appraisalscribe-migrate
class HasDecodeError e where fromDecodeError :: DecodeError -> e
{-# DEPRECATED HasDecodeError "use Member DecodeError" #-}
{-# DEPRECATED fromDecodeError "use throwMember or review oneOf" #-}
instance HasDecodeError DecodeError where fromDecodeError = id
