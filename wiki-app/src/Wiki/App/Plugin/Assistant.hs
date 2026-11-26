-- | The writing assistant, a built-in plugin ("Wiki.App.Plugin"; WP4.19
-- and WP5.84 of docs/PLAN.md): a chat with a model that edits the page
-- being written, through the user's own key for a provider. It is the
-- editor's agent ("Wiki.App.Plugin.Assistant.Agent"), with a panel of its
-- own ("Wiki.App.Plugin.Assistant.Panel"), and its settings are the
-- settings' section "Writing assistant"
-- ("Wiki.App.Plugin.Assistant.Options").
--
-- Its settings are kept as a plugin's ("Wiki.Core.Assistant.Split"): the
-- API keys, one per provider, are its secrets, sealed in the user's vault;
-- the provider, the mode, the page's cap, and each provider's model,
-- effort and service are its options, which the server keeps in the
-- clear; the lists of models are its cache in this browser.
module Wiki.App.Plugin.Assistant
  ( assistant
  , assistantId
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.UUID (MonadUUID)
import Protolude
import Wiki.App.Plugin (Agent (..), Manifest (..), Options (..), Plugin (..), PluginId, SecretSpec (..), builtin)
import Wiki.App.Plugin.Assistant.Agent qualified as Agent
import Wiki.App.Plugin.Assistant.Options qualified as Options
import Wiki.Core.Assistant (Provider (..))
import Wiki.Core.Assistant.Split (secretOf)

-- | The plugin.
assistant :: (Typeable m, MonadUUID m, MonadUnliftIO m) => Plugin m
assistant =
  Plugin
    { manifest
    , diagnostics = Nothing
    , agent = Just (Agent Agent.component)
    , options = Just (Options Options.component)
    }

-- | The plugin's id.
assistantId :: PluginId
assistantId = manifest.id

-- | The plugin's manifest: its name, and its secrets, an API key per
-- provider ('Wiki.Core.Assistant.Split.secretOf').
manifest :: Manifest
manifest =
  (builtin "assistant" "Writing assistant" "A chat with a model of Anthropic's or OpenAI's, with your own key, that answers about the page you write and proposes changes for you to accept or reject.")
    { secrets =
        [ SecretSpec {name = secretOf Anthropic, label = "API key", description = "Anthropic's, billed by Anthropic to your account there."}
        , SecretSpec {name = secretOf OpenAI, label = "API key", description = "OpenAI's, or the service's chosen above, billed to your account there."}
        , SecretSpec {name = secretOf Scripted, label = "Key", description = "The scripted provider needs none."}
        ]
    }
