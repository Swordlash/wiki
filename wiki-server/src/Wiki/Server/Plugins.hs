-- | Which plugins are on, the server's half (WP5.84 of docs/PLAN.md,
-- decision 69, issue #186; the API's types are "Wiki.Api.Plugins"): what
-- @GET api\/v1\/plugins@ answers, and a user's changes to their own
-- choices. The administrator's set of plugins off for the whole wiki is
-- read and set through the storage ("Wiki.Server.Storage"'s
-- 'Wiki.Server.Storage.wikiPluginsOff'), which checks that the caller
-- administers the wiki and writes the change to the audit log. A user's
-- own change is not logged.
--
-- Options are kept in the clear, as the server writes the JSON value out
-- again: the order of an object's keys and the spelling of numbers are not
-- kept. A plugin's secrets are not options: they belong in the user's
-- vault ("Wiki.Server.Vault"). Nothing here stores a secret, and nothing
-- here looks inside options to tell.
--
-- Each operation is one transaction: a read sees the wiki's set and the
-- user's choices as one state of the database, and a change reads the
-- user's row and writes one of its columns on the writer, where no other
-- write runs between ("Wiki.Server.Storage.Plugins").
module Wiki.Server.Plugins
  ( pluginsFor
  , setPluginState
  , setPluginOptions
  , setPluginOptionsIfNone
  , optionsThere
  )
where

import Data.Aeson (Value, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Protolude
import Wiki.Api.Plugins (PluginChoice (..), PluginId, PluginState, Plugins (..), maxOptionsBytes, optionsTooLarge)
import Wiki.App.Backend (WikiError (..))
import Wiki.Core.Id (UserId)
import Wiki.Server.Storage (Storage, Who (..), reading, writingEither)
import Wiki.Server.Storage.Plugins (loadChoice, loadChoices, loadPluginsOff, writeOptions, writeState)

-- | What is on, for whoever asks: the plugins off for the wiki, and a
-- user's own choices. Someone not signed in and the server's operator,
-- who is no user, have no choices.
pluginsFor :: Storage -> Who -> IO Plugins
pluginsFor st who = reading st $ do
  offForWiki <- loadPluginsOff
  mine <- case who of
    AsUser u -> loadChoices u
    Anonymous -> pure Map.empty
    Administrator -> pure Map.empty
  pure Plugins {offForWiki, mine}

-- | Set a user's state for a plugin; its options stay as they are. The
-- result is the user's choice for the plugin after it.
setPluginState :: Storage -> UserId -> PluginId -> PluginState -> IO (Either WikiError PluginChoice)
setPluginState st u p s = writingEither st (writeState u p s)

-- | Replace a user's options for a plugin with a value, or clear them
-- ('Nothing'); its state stays as it is. The result is the user's choice
-- for the plugin after it.
--
-- The API bounds the options as sent ("Wiki.Api.Plugins"'s
-- 'Wiki.Api.Plugins.PluginOptions'). They are stored as the server
-- writes them out again, which can be longer than what was sent: a
-- number written @1e1000@ is written out with all its digits. So that
-- bound is checked again on what is stored.
setPluginOptions :: Storage -> UserId -> PluginId -> Maybe Value -> IO (Either WikiError PluginChoice)
setPluginOptions st u p options = case BSL.toStrict . encode <$> options of
  Just bytes | BS.length bytes > maxOptionsBytes -> pure (Left (Invalid optionsTooLarge))
  text -> writingEither st (writeOptions u p (decodeUtf8 <$> text))

-- | 'setPluginOptions' with a value, only if the user has no options for
-- the plugin: the request's @If-None-Match: *@. The options are read and
-- written in one transaction on the writer, where no other write runs
-- between, so two such writes for one plugin cannot both find none.
-- Options there already are 'Conflict' ('optionsThere'), and nothing is
-- written. A row with a state and no options has none.
--
-- The move of the writing assistant's settings from before plugins adopts
-- them this way ("Wiki.App.Assistant.Migrate"): options another browser
-- moved first, or the user chose since, are kept.
setPluginOptionsIfNone :: Storage -> UserId -> PluginId -> Value -> IO (Either WikiError PluginChoice)
setPluginOptionsIfNone st u p options = case BSL.toStrict (encode options) of
  bytes
    | BS.length bytes > maxOptionsBytes -> pure (Left (Invalid optionsTooLarge))
    | otherwise ->
        writingEither st $
          loadChoice u p >>= \before ->
            if isJust before.options
              then pure (Left (Conflict optionsThere))
              else writeOptions u p (Just (decodeUtf8 bytes))

-- | Why options written only where there are none were not written.
optionsThere :: Text
optionsThere = "You have options for this plugin already, and If-None-Match: * asks to write them only where there are none. Nothing was written."
