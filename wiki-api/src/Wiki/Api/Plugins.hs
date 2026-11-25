{-# LANGUAGE DeriveAnyClass #-}

-- | Which plugins are on, as the server keeps it (WP5.84 of docs/PLAN.md,
-- decision 69, issue #186). To the server a plugin is an id
-- ('PluginId'): the application knows which plugins exist, what each
-- does, and whether each is on when nobody chose. The server keeps two
-- things.
--
-- * Each user's own choice for a plugin ('PluginChoice'): on, off, or
--   the default ('PluginState'), and the plugin's options, one JSON value
--   ('PluginOptions'). Options are kept in the clear. A plugin's secrets
--   (an assistant's API key) are not options: they belong in the user's
--   vault ("Wiki.Api.Vault"), and nothing here stores one.
-- * The plugins the wiki's administrator turned off for everyone
--   ('WikiPlugins'). A plugin off for the wiki is off whatever a user
--   chose. The user's choice is kept, and applies again once the
--   administrator turns the plugin back on.
--
-- @GET api\/v1\/plugins@ answers both at once ('Plugins'), to anyone who
-- may read the wiki, signed in or not: the application asks it as it
-- starts and after a sign-in. A user changes their state and their
-- options under @api\/v1\/account\/plugins\/\<id>\/@, by two operations
-- that each write one of the two ('PluginStateChange', 'PluginOptions'),
-- so that a request that changes one never writes the other back. The
-- administrator reads and sets the wiki's set at
-- @api\/v1\/admin\/plugins@.
module Wiki.Api.Plugins
  ( -- * A plugin's id
    PluginId (..)
  , parsePluginId
  , pluginIdRule
  , maxPluginIdLength

    -- * A user's choice
  , PluginState (..)
  , pluginStateName
  , PluginChoice (..)
  , PluginStateChange (..)
  , PluginOptions (..)
  , maxOptionsBytes
  , optionsTooLarge
  , optionsNull
  , maxPluginsPerUser

    -- * The wiki's set
  , WikiPlugins (..)
  , maxPluginsOff

    -- * What is on
  , Plugins (..)
  , noPlugins
  )
where

import Control.Monad.Fail (fail)
import Data.Aeson (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..), ToJSON (..), ToJSONKey (..), Value (..), eitherDecode, withText)
import Data.Aeson.Types (toJSONKeyText)
import Data.ByteString.Lazy qualified as BSL
import Data.Char (isAsciiLower)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Protolude
import Servant.API (JSON, MimeUnrender (..))
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

--------------------------------------------------------------------------------
-- A plugin's id

-- | 'PluginId' is the id a plugin's manifest gives it: @assistant@,
-- @errors@. It is 1 to 'maxPluginIdLength' characters of lowercase ASCII
-- letters, digits and @-@, and starts with a letter. Every way in (JSON,
-- a map's key, an address) checks the form ('parsePluginId'), so the
-- server keeps no other. The server knows no list of plugins: it takes
-- any id of this form, since plugins from elsewhere come later.
newtype PluginId = PluginId {text :: Text}
  deriving stock (Eq, Ord, Show)

-- | The longest id, in characters.
maxPluginIdLength :: Int
maxPluginIdLength = 64

-- | What an id is, in a sentence: the reason given for one refused.
pluginIdRule :: Text
pluginIdRule = "A plugin's id is 1 to " <> show maxPluginIdLength <> " characters of lowercase letters, digits and hyphens, and starts with a letter."

-- | The id a text is, if it has the form of one ('PluginId').
parsePluginId :: Text -> Either Text PluginId
parsePluginId t = case T.uncons t of
  Just (c, _)
    | isAsciiLower c
    , T.length t <= maxPluginIdLength
    , T.all (\x -> isAsciiLower x || isDigit x || x == '-') t ->
        Right (PluginId t)
  _ -> Left pluginIdRule

instance ToJSON PluginId where
  toJSON p = String p.text

instance FromJSON PluginId where
  parseJSON = withText "PluginId" (either (fail . toS) pure . parsePluginId)

instance ToJSONKey PluginId where
  toJSONKey = toJSONKeyText (\p -> p.text)

instance FromJSONKey PluginId where
  fromJSONKey = FromJSONKeyTextParser (either (fail . toS) pure . parsePluginId)

instance ToHttpApiData PluginId where
  toUrlPiece p = p.text

instance FromHttpApiData PluginId where
  parseUrlPiece = parsePluginId

--------------------------------------------------------------------------------
-- A user's choice

-- | 'PluginState' is what a user chose for a plugin: on, off, or nothing,
-- which leaves it to the plugin's default. In JSON: @"on"@, @"off"@,
-- @"default"@. Three names and not a boolean that may be @null@, so that
-- a request that leaves the field out is refused rather than read as a
-- return to the default.
data PluginState = PluginOn | PluginOff | PluginDefault
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | A state's name, in JSON and in the database: @on@, @off@, @default@.
pluginStateName :: PluginState -> Text
pluginStateName = \case
  PluginOn -> "on"
  PluginOff -> "off"
  PluginDefault -> "default"

instance ToJSON PluginState where
  toJSON = String . pluginStateName

instance FromJSON PluginState where
  parseJSON = withText "PluginState" $ \t ->
    maybe (fail ("not a plugin's state (on, off or default): " <> toS t)) pure (find (\s -> pluginStateName s == t) [minBound .. maxBound])

-- | 'PluginChoice' is one user's choice for one plugin, as the server
-- keeps it: the state, and the options, 'Nothing' when there are none. A
-- plugin with neither is not kept, and not listed.
data PluginChoice = PluginChoice
  { state :: PluginState
  , options :: Maybe Value
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | 'PluginStateChange' is the body that sets a user's state for a
-- plugin (@PUT api\/v1\/account\/plugins\/\<id>\/state@). It holds the
-- state only, so it cannot write the options back.
newtype PluginStateChange = PluginStateChange
  { state :: PluginState
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | 'PluginOptions' is a plugin's options for one user, the body of
-- @PUT api\/v1\/account\/plugins\/\<id>\/options@: one JSON value, which
-- replaces the old one whole. The body is the value itself, at most
-- 'maxOptionsBytes' bytes as sent. @null@ is refused: the options are
-- cleared by @DELETE@ on the same address, so that clearing them is
-- never an accident of a value that happened to be @null@. The server
-- reads nothing inside the value.
newtype PluginOptions = PluginOptions {value :: Value}
  deriving stock (Eq, Show)
  deriving newtype (ToJSON)

-- | The options are read from the body's bytes, which the size is
-- measured on, rather than from a parsed value, which no longer knows
-- how long its text was. The instance overlaps servant's for every type
-- with 'FromJSON', and is the one taken for 'PluginOptions'.
instance {-# OVERLAPPING #-} MimeUnrender JSON PluginOptions where
  mimeUnrender _ bytes
    | BSL.length bytes > fromIntegral maxOptionsBytes = Left (toS optionsTooLarge)
    | otherwise = case eitherDecode bytes of
        Left e -> Left e
        Right Null -> Left (toS optionsNull)
        Right v -> Right (PluginOptions v)

-- | The most bytes of JSON one plugin's options take: 16 kB (16 × 1024
-- bytes). Options are a plugin's settings; anything larger is not one.
maxOptionsBytes :: Int
maxOptionsBytes = 16 * 1024

-- | Why options were refused for their size.
optionsTooLarge :: Text
optionsTooLarge = "A plugin's options are at most 16 kB (" <> show maxOptionsBytes <> " bytes) of JSON."

-- | Why @null@ was refused as a plugin's options.
optionsNull :: Text
optionsNull = "A plugin's options are a JSON value other than null: to clear them, send DELETE to the same address."

-- | The most plugins one user has a choice kept for: a bound on what one
-- account stores, far above the plugins a wiki has.
maxPluginsPerUser :: Int
maxPluginsPerUser = 100

--------------------------------------------------------------------------------
-- The wiki's set

-- | 'WikiPlugins' is what the administrator decides for the whole wiki,
-- @GET@ and @PUT api\/v1\/admin\/plugins@: the plugins turned off for
-- everyone, at most 'maxPluginsOff' of them. A @PUT@ replaces the set.
newtype WikiPlugins = WikiPlugins
  { off :: Set PluginId
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | The most plugins the wiki turns off.
maxPluginsOff :: Int
maxPluginsOff = 100

--------------------------------------------------------------------------------
-- What is on

-- | 'Plugins' is what @GET api\/v1\/plugins@ answers: the plugins off for
-- the whole wiki, and the choices of whoever asks, by plugin. Someone not
-- signed in, and the server's operator, who is no user, have no choices.
-- Whether a plugin is on is then: off when the wiki turned it off;
-- otherwise the user's state, unless it is 'PluginDefault' or the plugin
-- is not listed; otherwise the plugin's default, which only the
-- application knows.
data Plugins = Plugins
  { offForWiki :: Set PluginId
  , mine :: Map PluginId PluginChoice
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Nothing off and nothing chosen: what a wiki that never stored
-- anything answers.
noPlugins :: Plugins
noPlugins = Plugins {offForWiki = Set.empty, mine = Map.empty}
