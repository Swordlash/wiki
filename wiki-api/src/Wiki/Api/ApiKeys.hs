{-# LANGUAGE DeriveAnyClass #-}

-- | The API keys (WP5.138 of docs/PLAN.md, issue #338): what a program on
-- a user's own machine signs in with to the API for programs
-- (@api\/markdown@, @api\/list@, @api\/search@), instead of the user's
-- login and password.
--
-- A key is made by its user in a signed-in session, under
-- @api\/v1\/account\/api-keys@: @POST@ makes one ('NewApiKey') and answers
-- it whole, once ('MadeApiKey'); @GET@ lists the user's keys
-- ('ApiKeyInfo'), without their secrets; @POST api-keys\/\<id>\/revoke@
-- revokes one for good. A program sends the whole key in each request, as
-- @Authorization: Bearer \<key>@ or @X-API-Key: \<key>@. The server keeps
-- only a hash of the key's secret ("Wiki.Server.ApiKeys").
--
-- A key has one 'KeyRight', which narrows what its user may do: a key
-- never does more than its user may do at the moment of the request.
module Wiki.Api.ApiKeys
  ( ApiKeyId (..)
  , KeyRight (..)
  , rightName
  , rightOf
  , ApiKeyInfo (..)
  , NewApiKey (..)
  , MadeApiKey (..)
  )
where

import Control.Monad.Fail (fail)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), withText)
import Data.Time (UTCTime)
import Protolude
import Text.Show (Show (showsPrec), showParen)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)

-- | 'ApiKeyId' is the public part of an API key: the part between the
-- prefix and the secret, which names the key in the list and in the
-- server's log. Knowing it gives nothing: the secret is what signs in.
newtype ApiKeyId = ApiKeyId {text :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

-- | 'KeyRight' is what a key may do with what its user may do. 'ReadRight'
-- allows the requests that only read (@GET@ and @HEAD@). 'WriteRight' also
-- allows the requests that change pages and spaces. 'AdminRight' is for
-- the administration through the API, of which there is none yet: today it
-- allows what 'WriteRight' allows. Only an administrator may make a key
-- with it, and it stops working when its user stops being one. In JSON:
-- @"read"@, @"write"@, @"admin"@.
data KeyRight = ReadRight | WriteRight | AdminRight
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | A right's name, in JSON and in the database: @read@, @write@ or
-- @admin@.
rightName :: KeyRight -> Text
rightName = \case
  ReadRight -> "read"
  WriteRight -> "write"
  AdminRight -> "admin"

-- | The right a name names, if any ('rightName').
rightOf :: Text -> Maybe KeyRight
rightOf name = find (\r -> rightName r == name) [minBound .. maxBound]

instance ToJSON KeyRight where
  toJSON = String . rightName

instance FromJSON KeyRight where
  parseJSON = withText "KeyRight" $ \t -> maybe (fail ("not a key's right (read, write or admin): " <> toS t)) pure (rightOf t)

-- | 'ApiKeyInfo' is one of a user's keys as their list shows it: never its
-- secret, nor the secret's hash.
data ApiKeyInfo = ApiKeyInfo
  { id :: ApiKeyId
  , name :: Text
  -- ^ The name its user gave it, or the day it was made.
  , right :: KeyRight
  , createdAt :: UTCTime
  , expiresAt :: Maybe UTCTime
  -- ^ When it stops working; 'Nothing' for never.
  , lastUsedAt :: Maybe UTCTime
  -- ^ When it last signed a request in, to within ten minutes: the
  -- server writes it at most once every ten minutes, not on every
  -- request.
  , revokedAt :: Maybe UTCTime
  -- ^ When its user revoked it. A revoked key never works again.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | 'NewApiKey' is a key to make: its name, its right, and when it
-- expires. The expiry is a moment, not a day: the browser knows the
-- user's time zone and turns the day they choose into the moment it ends.
-- It must be in the future; 'Nothing' makes a key that never expires.
data NewApiKey = NewApiKey
  { name :: Text
  , right :: KeyRight
  , expiresAt :: Maybe UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | 'MadeApiKey' is the answer that makes a key: the whole key, which the
-- server never shows again, and the key as the list shows it. Its 'Show'
-- hides the key.
data MadeApiKey = MadeApiKey
  { key :: Text
  , info :: ApiKeyInfo
  }
  deriving stock (Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Show MadeApiKey where
  showsPrec d m = showParen (d > 10) (("MadeApiKey <hidden> " <>) . showsPrec 11 m.info)
