-- | The capabilities only the writing assistant uses
-- ("Wiki.App.Plugin.Assistant"): its chats with the model providers
-- ('Models'), and its settings, kept sealed in the user's vault with the
-- vault's own controls ('Secret').
--
-- They are today's shape of the assistant's needs, for one plugin, and
-- mirror the classes the application had before the plugins
-- ("Wiki.App.Assistant", "Wiki.App.Assistant.Keys",
-- "Wiki.App.Assistant.Sync"), which still do the work. A later package
-- replaces them by capabilities for every plugin: a secret item per
-- plugin in the vault, and a network capability that reaches the hosts a
-- plugin names. Nothing outside the assistant's plugin should come to
-- depend on them.
module Wiki.App.Plugin.Capability.Assistant
  ( AssistantCapabilities (..)

    -- * The providers
  , Models (..)
  , Talk
  , Opening (..)
  , Heard (..)
  , AssistantService (..)
  , AssistantApi (..)

    -- * The settings in the vault
  , Secret (..)
  , Keys (..)
  , PasskeyInfo (..)
  , WikiError
  , Vaulted
  , KeyOwner
  , owning
  , releaseOwners
  )
where

import Data.Aeson (Value)
import Protolude
import Wiki.Api.ServerSettings (AssistantApi (..), AssistantService (..))
import Wiki.Api.Vault (PasskeyInfo (..))
import Wiki.App.Assistant (Heard (..), KeyOwner, Opening (..), Talk)
import Wiki.App.Assistant.Keys (Keys (..))
import Wiki.App.Assistant.Sync (Owned, owning, releaseOwners)
import Wiki.App.Backend (WikiError)
import Wiki.Core.Assistant (AssistantError, Listed, Ready, Settings)
import Wiki.Core.Assistant.Vault (CredentialId)
import Wiki.Core.Id (UserId)
import Wiki.Core.User (Password)

-- | 'AssistantCapabilities' is what the writing assistant may ask of the
-- host: its chats ('models'), its settings ('secret'), and the services
-- that speak a provider's API which the wiki's administrator lists, as
-- the session carries them ('services').
data AssistantCapabilities m = AssistantCapabilities
  { models :: Models m
  , secret :: Secret m
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

-- | 'Vaulted' is an operation of the user's vault, run with the keys it
-- registers in the page under an owner of its own ('owning'), which the
-- settings' section lets go when the operation ends or the section closes
-- ('releaseOwners').
type Vaulted = Owned

-- | 'Secret' is the assistant's settings, sealed in the user's vault
-- ("Wiki.App.Assistant.Keys"), with the vault's syncing across devices.
-- 'followed' and 'keep' read and change the settings as the editor does;
-- the others are the settings page's, each run under an owner of keys
-- ('Vaulted'). 'passkeys' lists the user's passkeys; 'syncing' says a
-- server keeps vaults; 'passkeysHere' and 'sealingHere' say what the
-- browser offers here.
data Secret m = Secret
  { followed :: UserId -> m (Either Text Keys)
  , keep :: UserId -> (Settings -> Settings) -> m (Either Text Keys)
  , load :: UserId -> Vaulted m (Either Text Keys)
  , read :: UserId -> Vaulted m (Either Text Keys)
  , change :: UserId -> (Settings -> Settings) -> Vaulted m (Either Text Keys)
  , turnOn :: UserId -> Maybe Password -> Vaulted m (Either Text Keys)
  , unlock :: UserId -> Vaulted m (Either Text Keys)
  , addPasskey :: UserId -> Maybe Password -> Vaulted m (Either Text Keys)
  , removePasskey :: UserId -> CredentialId -> Vaulted m (Either Text [PasskeyInfo])
  , turnOff :: UserId -> Vaulted m (Either Text Keys)
  , askEachTime :: UserId -> Bool -> Vaulted m (Either Text Keys)
  , passkeys :: m (Either WikiError [PasskeyInfo])
  , syncing :: m Bool
  , passkeysHere :: m Bool
  , sealingHere :: m Bool
  }
