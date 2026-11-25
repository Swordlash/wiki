-- | The host's side of the plugins' capabilities ("Wiki.App.Plugin"): the
-- records of functions a plugin gets, made from the application's classes
-- and the views' context. The editor and the settings page make them as
-- they render, for one plugin; a plugin never imports this module.
module Wiki.App.Plugin.Host
  ( capabilities
  , documentOf
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Protolude
import Web.DOM.Internal.Types (Element)
import Wiki.App.Assistant (MonadAssistant (..), closeKeyOwner, openKeyOwner)
import Wiki.App.Assistant.Keys (addPasskey, changeKeys, loadFollowedKeys, loadKeys, readKeys, removePasskey, setAskEachTime, turnOffSync, turnOnSync, unlockKeys)
import Wiki.App.Assistant.Sync (MonadVault (..), Owned, owned)
import Wiki.App.Backend (Session (..))
import Wiki.App.Editor.Browser (currentText, focusText, proposeEdit, trackedChanges)
import Wiki.App.Plugin (Capabilities (..), Document (..), Local (..), Manifest (..), SecretField (..), Secrets (..), Unavailable (..), optionsLinkTo)
import Wiki.App.Plugin.Capability.Assistant (AssistantCapabilities (..), Models (..), Secret (..))
import Wiki.App.Plugin.Host.Secrets (loadLocal, putSecret, readSecrets, removeSecret, saveLocal)
import Wiki.App.Route (Route (UserSettings), SettingsSection (AssistantSection))
import Wiki.App.View.Common (Context (..))
import Wiki.App.View.SecretField qualified as SecretField
import Wiki.Core.Id (UserId)
import Wiki.Core.User (User (..))

-- | The capabilities of one plugin, by its manifest, in a view with this
-- context. A plugin's settings are the writing assistant's section: the
-- assistant is the one plugin with settings.
--
-- The plugin's secrets and its cache are the reader's: the user signed in
-- in the context, closed over with the plugin's manifest. The field of a
-- secret is made for the same reader, and its slot names the session's
-- number ('Wiki.App.Plugin.FieldSlot'): when the reader changes, a new
-- field is mounted.
capabilities :: (Typeable m, MonadAssistant m, MonadVault m, MonadUnliftIO m) => Context -> Manifest -> Capabilities m
capabilities context manifest =
  Capabilities
    { here = assistantHere
    , secure = secureContext
    , optionsLink = optionsLinkTo context.router (UserSettings AssistantSection)
    , secrets =
        Secrets
          { get = maybe (pure (Left NoUser)) (\u -> ownedFor u (readSecrets u manifest)) user
          , put = \name value -> maybe (pure (Left signedOut)) (\u -> ownedFor u (putSecret u manifest name value)) user
          , remove = \name -> maybe (pure (Left signedOut)) (\u -> ownedFor u (removeSecret u manifest name)) user
          , field = SecretField {component = SecretField.component user manifest, reader = (context.sessionNumber, user)}
          }
    , local =
        Local
          { load = maybe (pure Nothing) (\u -> loadLocal u manifest.id) user
          , save = \v -> maybe (pure (Left signedOut)) (\u -> saveLocal u manifest.id v) user
          }
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
  where
    user = (\u -> u.id) <$> context.session.user
    signedOut = "Nobody is signed in."

-- | Run an operation of the user's vault with the keys it registers in
-- the page under an owner of its own, let go when it ends, however it
-- ends: a plugin's handler cut off while it waits (its view closed) leaves
-- no key of it in the page.
ownedFor :: (MonadUnliftIO m) => UserId -> Owned m a -> m a
ownedFor u act = withRunInIO (\run -> bracket (openKeyOwner u) closeKeyOwner (\o -> run (owned o act)))

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
