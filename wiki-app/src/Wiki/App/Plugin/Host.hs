-- | The host's side of the plugins' capabilities ("Wiki.App.Plugin"): the
-- records of functions a plugin gets, made from the application's classes
-- and the views' context. The editor and the settings page make them as
-- they render; a plugin never imports this module.
module Wiki.App.Plugin.Host
  ( capabilities
  , documentOf
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Web.DOM.Internal.Types (Element)
import Wiki.App.Assistant (MonadAssistant (..))
import Wiki.App.Assistant.Keys (addPasskey, changeKeys, loadFollowedKeys, loadKeys, readKeys, removePasskey, setAskEachTime, turnOffSync, turnOnSync, unlockKeys)
import Wiki.App.Assistant.Sync (MonadVault (..))
import Wiki.App.Backend (Session (..))
import Wiki.App.Editor.Browser (currentText, focusText, proposeEdit, trackedChanges)
import Wiki.App.Plugin (Capabilities (..), Document (..), optionsLinkTo)
import Wiki.App.Plugin.Capability.Assistant (AssistantCapabilities (..), Models (..), Secret (..))
import Wiki.App.Route (Route (UserSettings), SettingsSection (AssistantSection))
import Wiki.App.View.Common (Context (..))

-- | The capabilities of a plugin in a view with this context. A plugin's
-- settings are the writing assistant's section: the assistant is the one
-- plugin with settings.
capabilities :: (MonadAssistant m, MonadVault m, MonadUnliftIO m) => Context -> Capabilities m
capabilities context =
  Capabilities
    { here = assistantHere
    , secure = secureContext
    , optionsLink = optionsLinkTo context.router (UserSettings AssistantSection)
    , assistant =
        AssistantCapabilities
          { models =
              Models
                { scripted = scriptedInstalled
                , prepare = prepareAssistant
                , open = openTalk
                , say = sayTo
                , answer = answerCall
                , stop = stopTalk
                , close = closeTalk
                , check = checkKey
                , list = listModels
                }
          , secret =
              Secret
                { followed = loadFollowedKeys
                , keep = changeKeys
                , load = loadKeys
                , read = readKeys
                , change = changeKeys
                , turnOn = turnOnSync
                , unlock = unlockKeys
                , addPasskey = addPasskey
                , removePasskey = removePasskey
                , turnOff = turnOffSync
                , askEachTime = setAskEachTime
                , passkeys = listPasskeys
                , syncing = vaultSync
                , passkeysHere = passkeysHere
                , sealingHere = sealingHere
                }
          , services = context.session.assistantServices
          }
    }

-- | The editor's text, reached through CodeMirror's host element
-- ("Wiki.App.Editor.Browser").
documentOf :: Element -> Document
documentOf host =
  Document
    { text = currentText host
    , focus = focusText host
    , propose = proposeEdit host
    , tracked = trackedChanges host
    }
