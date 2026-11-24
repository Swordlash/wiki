-- | The writing assistant, a built-in plugin ("Wiki.App.Plugin"; WP4.19
-- and WP5.84 of docs/PLAN.md): a chat with a model that edits the page
-- being written, through the user's own key for a provider. Its settings
-- are the settings' section "Writing assistant"
-- ("Wiki.App.Plugin.Assistant.Options").
module Wiki.App.Plugin.Assistant
  ( assistant
  , assistantId
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.UUID (MonadUUID)
import Protolude
import Wiki.App.Plugin (Manifest (..), Options (..), Plugin (..), PluginId, builtin)
import Wiki.App.Plugin.Assistant.Options qualified as Options

-- | The plugin.
assistant :: (Typeable m, MonadUUID m, MonadUnliftIO m) => Plugin m
assistant =
  Plugin
    { manifest
    , diagnostics = Nothing
    , agent = Nothing
    , options = Just (Options Options.component)
    }

-- | The plugin's id.
assistantId :: PluginId
assistantId = manifest.id

manifest :: Manifest
manifest = builtin "assistant" "Writing assistant" "A chat with a model of Anthropic's or OpenAI's, with your own key, that answers about the page you write and proposes changes for you to accept or reject."
