{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Extra.Actor
  ( -- * Types
    HasUserId(userId)
  , Actor(LoggedIn, Sudo, _effective, _real)
  , NoUser(NoUser)
  , logged
  , actorUIDs
  , WhoDis(..)
  ) where

import Control.Lens
import Data.Data
import Data.Generics.Labels ()
import Data.SafeCopy
import Data.Serialize (Serialize(get, put))
import Data.UserId
import Data.Text
import Extra.Except
import GHC.Generics (Generic)
import GHC.Stack (CallStack, getCallStack)
import SeeReason.SrcLoc (compactStack)

class HasUserId u where userId :: u -> UserId
instance HasUserId UserId where userId = id

-- | Error - a logged in user is required
data NoUser = NoUser Text CallStack
instance Show NoUser where
  show (NoUser t s) = "NoUser " <> show t <> " " <> compactStack (getCallStack s)

instance HasIOException e => HasIOException (Either NoUser e) where ioException = _Right . ioException

-- * Lenses and Paths

-- class HasCredentialName s where
--   credentialNameLens :: UserId -> Traversal' s Markup

-- * Getters

-- * Current User

-- | A user can be 'LoggedIn', or a 'LoggedIn' user can be 'Sudo'.
data Actor =
    LoggedIn {_effective :: !UserId}
  | Sudo {_effective :: !UserId, _real :: !Actor}
  deriving (Generic, Eq, Ord, Show, Typeable, Data)

-- | The actual authenticated user, disregarding any sudo-ing that may
-- have occurred.
logged :: Actor -> UserId
logged Sudo{..} = logged _real
logged LoggedIn{..} = _effective

instance SafeCopy Actor where version = 1
instance Serialize Actor where get = safeGet; put = safePut
-- instance Value Actor where hops _ = []
instance HasUserId Actor where userId = _effective

actorUIDs :: Actor -> [UserId]
actorUIDs (LoggedIn uid) = [uid]
actorUIDs (Sudo uid u') = [uid] ++ actorUIDs u'

newtype WhoDis = WhoDis UserId deriving (Generic, Eq, Ord, Show)
{-# DEPRECATED WhoDis "Use Actor" #-}
instance SafeCopy WhoDis where version = 3; kind = base
instance Serialize WhoDis where get = safeGet; put = safePut
