-- | The capabilities only the writing assistant uses
-- ("Wiki.App.Plugin.Assistant"): its chats with the model providers
-- ('Models'), and the services that speak a provider's API which the
-- wiki's administrator lists.
--
-- They are today's shape of the assistant's needs, for one plugin, and
-- mirror the class the application had before the plugins
-- ("Wiki.App.Assistant"), which still does the work. A later package
-- replaces them by a network capability that reaches the hosts a plugin
-- names. The assistant's keys are its secrets and the rest of its
-- settings its options, as for any plugin ("Wiki.App.Plugin"). Nothing
-- outside the assistant's plugin should come to depend on this module.
module Wiki.App.Plugin.Capability.Assistant
  ( AssistantCapabilities (..)

    -- * The providers
  , Models (..)
  , Talk
  , Opening (..)
  , Heard (..)
  , AssistantService (..)
  , AssistantApi (..)
  )
where

import Data.Aeson (Value)
import Protolude
import Wiki.Api.ServerSettings (AssistantApi (..), AssistantService (..))
import Wiki.App.Assistant (Heard (..), Opening (..), Talk)
import Wiki.Core.Assistant (AssistantError, Listed, Ready)

-- | 'AssistantCapabilities' is what the writing assistant may ask of the
-- host beside what every plugin may: its chats ('models'), and the
-- services that speak a provider's API which the wiki's administrator
-- lists, as the session carries them ('services').
data AssistantCapabilities m = AssistantCapabilities
  { models :: Models m
  , services :: [AssistantService]
  }

-- | 'Models' is the chats with the model providers, through the host's
-- bridge to their SDKs ("Wiki.App.Assistant"'s 'Wiki.App.Assistant.MonadAssistant').
-- The host checks each address against the page's Content Security
-- Policy before a request is made. 'scripted' says a test installed the
-- scripted provider; 'prepare' starts fetching the providers' code; a
-- chat is opened ('open'), told a message ('say'), given a tool's result
-- ('answer'), stopped and closed; 'check' tests a key and 'list' fetches
-- a provider's models. 'check' and 'list' wait; the others return at
-- once.
data Models m = Models
  { scripted :: m Bool
  , prepare :: m ()
  , open :: Opening -> (Heard -> IO ()) -> m Talk
  , say :: Talk -> Value -> Text -> m ()
  , answer :: Talk -> Int -> Text -> Bool -> m ()
  , stop :: Talk -> m ()
  , close :: Talk -> m ()
  , check :: Ready -> m (Maybe AssistantError)
  , list :: Ready -> m (Either AssistantError [Listed])
  }
