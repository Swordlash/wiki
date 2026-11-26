{-# LANGUAGE TemplateHaskell #-}

-- | 'MonadAssistant': what the writing assistant (WP4.19 of docs/PLAN.md)
-- needs from the monad a component runs in. That is the user's settings,
-- stored in this browser, and chats with a provider.
--
-- The settings (a key per provider, the chosen provider and each
-- provider's model: "Wiki.Core.Assistant") are stored in this browser's
-- IndexedDB, in a separate database ('assistantDatabase'), under the
-- user's id, sealed in a vault (WP5.21, "Wiki.Core.Assistant.Vault"):
-- this class stores the device's record ('deviceRecord'), and seals and
-- opens it with a key the browser keeps and never exports ('sealWith',
-- 'deviceKey'). "Wiki.App.Assistant.Keys" reads and changes the settings
-- through it. So another user of the same browser has their own settings
-- and cannot see anyone else's key, and no key is stored as text. The
-- settings never reach the wiki's server in the clear and are never
-- logged. They are not deleted when the user signs out of the wiki: they
-- belong to this browser and this user, and the user finds them again on
-- signing in, as with a password manager. "Forget" is the way to delete a
-- key.
--
-- The vault can follow the user to their other devices, unlocked there by
-- a passkey: the class makes and uses passkeys ('createPasskey',
-- 'usePasskey') and wraps the vault's key with what a passkey's PRF
-- extension gives ('wrapWith', 'unwrapWith'), all in the browser
-- (@jsbits\/vault.js@). A key is held by the page and known here by a
-- number ('KeyRef'); its bytes never reach Haskell.
--
-- A chat is one conversation with a provider ('openTalk'), which the
-- browser-side code keeps for the editing session. Each message sent
-- ('sayTo') runs one turn of the model, whose events are reported as they
-- happen ('Heard'): its text as it streams, the summary of its thinking
-- as it streams, the start of each tool call, each whole tool call, which
-- the turn waits on until it is answered ('answerCall'), and how the turn
-- ended. A turn can be stopped ('stopTalk'). The providers are reached
-- through Anthropic's and OpenAI's SDKs, called from the browser with the
-- user's key (@web/src/assistant.js@, fetched the first time the
-- assistant is used), or through the scripted provider a test installs
-- ('scriptedInstalled'); see @jsbits/assistant.js@.
--
-- The provider's model list is fetched with the key ('listModels'), for
-- the model pickers ('Wiki.Core.Assistant.groupModels').
--
-- In a browser ('BrowserDOM') all of this is real. In the in-memory
-- document that the prerender and the native tests use ('MemDOM'), the
-- settings are stored in its in-memory IndexedDB, as the browser would
-- store them, and there is no provider: every turn of a chat fails with a
-- message saying so.
module Wiki.App.Assistant
  ( -- * The class
    MonadAssistant (..)
  , Talk
  , Opening (..)
  , Heard (..)

    -- * The vault in the browser
  , KeyRef
  , KeyOwner (..)
  , pageKeys
  , openKeyOwner
  , closeKeyOwner
  , closeKeyOwnersOf
  , Made (..)
  , NewIdentity (..)
  , keyRefOf
  , passwordKeyIO
  , wrapForPassword
  , forgetHeld
  , holdKey
  , deviceKeyIO
  , deviceSlotsIO
  , vaultGenerationIO
  , dropSlotsIfIO
  , listenToOtherTabs
  , announceSignIn
  , keysDroppedElsewhere
  , heldHandle
  , TabLock (..)
  , holdingAcrossTabs
  , PasskeyTicket
  , Used (..)
  , PasskeyRefusal (..)

    -- * Settings
  , assistantDatabase
  , readRecord
  , writeRecord
  , holdingDeviceRecords
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Control.Monad.Trans.Class (MonadTrans)
import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (Object), eitherDecodeStrict, encode, object, withObject, (.!=), (.:), (.:?), (.=))
import Data.Aeson.Types (Pair, Parser, parseEither)
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Type.Equality (type (~))
import Halogen.JSBits (Callback, Inert (..), JSText, JSVal, Safety (..), browserJS, freeCallback, fromJSText, inBrowser, jsValText, mkSyncCallback, toJSText)
import Halogen.JSBits.Fields (Member (..), jsFields)
import Halogen.JSBits.Promise (Promise (..), wait)
import Halogen.Query.Event.Window qualified as WindowEvent
import Halogen.Subscription (Emitter)
import Halogen.Subscription qualified as HS
import Halogen.VDom.DOM.Monad (BrowserDOM)
import Halogen.VDom.DOM.Monad.Class (MonadBrowserDOM)
import Halogen.VDom.DOM.Monad.Mem (MemDOM)
import Protolude hiding (wait)
import System.IO.Unsafe (unsafePerformIO)
import Web.DOM.Element qualified as Element
import Web.DOM.Internal.Types (Window (..))
import Web.Event.Event (EventType (..))
import Web.HTML.Window qualified as Window
import Web.Storage.IndexedDB (Database (..), IndexedDBError (..), MonadIndexedDB)
import Web.Storage.IndexedDB qualified as IDB
import Wiki.Api.ContentPolicy (allowsConnect, extraHosts, originOf)
import Wiki.Api.Vault (AssertionOptions, AssertionResponse, RegistrationOptions, RegistrationResponse)
import Wiki.Core.Assistant (AssistantError (..), ErrorKind (..), Listed, Ready (..), Usage, providerKey)
import Wiki.Core.Assistant.Vault (Ciphertext (..), CredentialId (..), KeyId (..), Nonce (..), PublicKey (..), Salt (..), Sealed (..), WrappedKey (..), identityData, identityDataPrefix, passwordWrappingInfo, wrappingData)
import Wiki.Core.Id (UserId (..))
import Wiki.Core.Url (Url (..))

-- The chat's bindings return immediately: a turn reports its events
-- through the callback. "Test the key" and the model list wait for the
-- provider's response: their functions return promises, which the
-- wrappers below the splice wait for (haskell-halogen's 'Promise'). The
-- vault's (jsbits/vault.js) answer with WebCrypto's and WebAuthn's
-- promises, so most are waited for the same way; their
-- results are JSON, or a key's handle (0 for none). One splice holds both
-- files: a module has room for one.
$( browserJS
     ["jsbits/assistant.js", "jsbits/vault.js", "jsbits/tabs.js"]
     [ ("js_open", "wiki_assistant_open", Safe, [t|JSText -> Callback -> IO Int|])
     , ("js_send", "wiki_assistant_send", Safe, [t|Int -> JSText -> JSText -> IO ()|])
     , ("js_answer", "wiki_assistant_answer", Safe, [t|Int -> Int -> JSText -> Bool -> IO ()|])
     , ("js_stop", "wiki_assistant_stop", Safe, [t|Int -> IO ()|])
     , ("js_close", "wiki_assistant_close", Safe, [t|Int -> IO ()|])
     , ("js_checkPromise", "wiki_assistant_check", Safe, [t|JSText -> IO (Promise JSText)|])
     , ("js_modelsPromise", "wiki_assistant_models", Safe, [t|JSText -> IO (Promise JSText)|])
     , ("js_prepare", "wiki_assistant_prepare", Safe, [t|IO ()|])
     , ("js_new_keyPromise", "wiki_vault_new_key", Safe, [t|Bool -> Int -> IO (Promise Int)|])
     , ("js_sealPromise", "wiki_vault_seal", Safe, [t|Int -> JSText -> JSText -> IO (Promise JSText)|])
     , ("js_open_sealedPromise", "wiki_vault_open", Safe, [t|Int -> JSText -> JSText -> JSText -> IO (Promise JSText)|])
     , ("js_forget_key", "wiki_vault_forget_key", Safe, [t|Int -> IO ()|])
     , ("js_hold_key", "wiki_vault_hold_key", Safe, [t|JSVal -> Int -> IO Int|])
     , ("js_owner_open", "wiki_vault_owner_open", Safe, [t|JSText -> IO Int|])
     , ("js_owner_close", "wiki_vault_owner_close", Safe, [t|Int -> IO ()|])
     , ("js_owners_close", "wiki_vault_owners_close", Safe, [t|JSText -> IO ()|])
     , ("js_salt", "wiki_vault_salt", Safe, [t|IO JSText|])
     , ("js_keep_keyPromise", "wiki_vault_keep_key", Safe, [t|JSText -> Int -> Bool -> Int -> IO (Promise JSText)|])
     , ("js_device_keyPromise", "wiki_vault_device_key", Safe, [t|JSText -> Int -> IO (Promise Int)|])
     , ("js_drop_keyPromise", "wiki_vault_drop_key", Safe, [t|JSText -> Int -> IO (Promise ())|])
     , ("js_listen", "wiki_vault_listen", Safe, [t|IO ()|])
     , ("js_signed_in", "wiki_vault_signed_in", Safe, [t|IO ()|])
     , ("js_slotsPromise", "wiki_vault_slots", Safe, [t|IO (Promise JSText)|])
     , ("js_generationPromise", "wiki_vault_generation", Safe, [t|IO (Promise Int)|])
     , ("js_drop_ifPromise", "wiki_vault_drop_if", Safe, [t|Int -> JSText -> IO (Promise Bool)|])
     , ("js_passkeys_here", "wiki_vault_passkeys_here", Safe, [t|IO Bool|])
     , ("js_sealing_here", "wiki_vault_sealing_here", Safe, [t|IO Bool|])
     , ("js_createPromise", "wiki_vault_create", Safe, [t|JSText -> JSText -> JSText -> Int -> IO (Promise JSText)|])
     , ("js_getPromise", "wiki_vault_get", Safe, [t|JSText -> JSText -> JSText -> Int -> IO (Promise JSText)|])
     , ("js_wrapPromise", "wiki_vault_wrap", Safe, [t|Int -> Int -> JSText -> IO (Promise JSText)|])
     , ("js_unwrapPromise", "wiki_vault_unwrap", Safe, [t|Int -> JSText -> JSText -> JSText -> Bool -> Int -> IO (Promise JSText)|])
     , ("js_discardPromise", "wiki_vault_discard", Safe, [t|JSText -> JSText -> IO (Promise ())|])
     , ("js_keys_generatePromise", "wiki_keys_generate", Safe, [t|Int -> JSText -> Int -> IO (Promise JSText)|])
     , ("js_keys_openPromise", "wiki_keys_open", Safe, [t|Int -> JSText -> JSText -> JSText -> Int -> IO (Promise JSText)|])
     , ("js_keys_fingerprintPromise", "wiki_keys_fingerprint", Safe, [t|JSText -> IO (Promise JSText)|])
     , ("js_password_keyPromise", "wiki_vault_password_key", Safe, [t|Int -> JSText -> JSText -> Int -> IO (Promise JSText)|])
     , ("js_conditional_herePromise", "wiki_passkey_conditional_here", Safe, [t|IO (Promise Bool)|])
     , ("js_passkey_sign_inPromise", "wiki_passkey_sign_in", Safe, [t|JSText -> Bool -> Int -> IO (Promise JSText)|])
     , ("js_passkey_offer_ticket", "wiki_passkey_offer_ticket", Safe, [t|IO Int|])
     , ("js_passkey_abort", "wiki_passkey_abort", Safe, [t|IO ()|])
     , ("js_secrets_changed", "wiki_vault_secrets_changed", Safe, [t|IO ()|])
     , ("js_on_secrets_changed", "wiki_vault_on_secrets_changed", Safe, [t|Callback -> IO ()|])
     , ("js_tab_lock_request", "wiki_tab_lock_request", Safe, [t|JSText -> IO Int|])
     , ("js_tab_lock_grantedPromise", "wiki_tab_lock_granted", Safe, [t|Int -> IO (Promise ())|])
     , ("js_tab_lock_release", "wiki_tab_lock_release", Safe, [t|Int -> IO ()|])
     , ("js_drop_record_ifPromise", "wiki_vault_drop_record_if", Safe, [t|JSText -> JSText -> Int -> IO (Promise Bool)|])
     , ("js_keep_recordPromise", "wiki_vault_keep_record", Safe, [t|JSText -> JSText -> Bool -> Int -> IO (Promise JSText)|])
     ]
 )

-- | The window as the scripted provider's page adds to it (a test's):
-- @wikiAssistantScript@, the script the provider answers with.
newtype Scripted = Scripted Window
  deriving newtype (Inert)

$( jsFields
     ''Scripted
     [ Live "wikiAssistantScript" [t|Maybe JSVal|]
     ]
 )

-- | Whether the page carries a scripted provider's script.
scripted :: IO Bool
scripted = Scripted <$> Window.window >>= fmap isJust . (.wikiAssistantScript)

-- | The page's Content Security Policy, from its @\<meta http-equiv>@, or
-- empty where the page has none.
pagePolicy :: IO Text
pagePolicy = do
  w <- Window.window
  content <- w.document.querySelector "meta[http-equiv=\"Content-Security-Policy\"]" >>= traverse (`Element.getAttribute` "content")
  pure (fromMaybe "" (join content))

js_check :: JSText -> IO JSText
js_check a1 = if inBrowser then wait =<< js_checkPromise a1 else pure inert

js_models :: JSText -> IO JSText
js_models a1 = if inBrowser then wait =<< js_modelsPromise a1 else pure inert

js_new_key :: Bool -> Int -> IO Int
js_new_key a1 a2 = if inBrowser then wait =<< js_new_keyPromise a1 a2 else pure inert

js_seal :: Int -> JSText -> JSText -> IO JSText
js_seal a1 a2 a3 = if inBrowser then wait =<< js_sealPromise a1 a2 a3 else pure inert

js_open_sealed :: Int -> JSText -> JSText -> JSText -> IO JSText
js_open_sealed a1 a2 a3 a4 = if inBrowser then wait =<< js_open_sealedPromise a1 a2 a3 a4 else pure inert

js_keep_key :: JSText -> Int -> Bool -> Int -> IO JSText
js_keep_key a1 a2 a3 a4 = if inBrowser then wait =<< js_keep_keyPromise a1 a2 a3 a4 else pure inert

js_device_key :: JSText -> Int -> IO Int
js_device_key a1 a2 = if inBrowser then wait =<< js_device_keyPromise a1 a2 else pure inert

js_drop_key :: JSText -> Int -> IO ()
js_drop_key a1 a2 = if inBrowser then wait =<< js_drop_keyPromise a1 a2 else pure inert

js_slots :: IO JSText
js_slots = if inBrowser then wait =<< js_slotsPromise else pure inert

js_generation :: IO Int
js_generation = if inBrowser then wait =<< js_generationPromise else pure inert

js_drop_record_if :: JSText -> JSText -> Int -> IO Bool
js_drop_record_if a1 a2 a3 = if inBrowser then wait =<< js_drop_record_ifPromise a1 a2 a3 else pure inert

js_keep_record :: JSText -> JSText -> Bool -> Int -> IO JSText
js_keep_record a1 a2 a3 a4 = if inBrowser then wait =<< js_keep_recordPromise a1 a2 a3 a4 else pure inert

js_tab_lock_granted :: Int -> IO ()
js_tab_lock_granted a1 = if inBrowser then wait =<< js_tab_lock_grantedPromise a1 else pure inert

js_drop_if :: Int -> JSText -> IO Bool
js_drop_if a1 a2 = if inBrowser then wait =<< js_drop_ifPromise a1 a2 else pure inert

js_create :: JSText -> JSText -> JSText -> Int -> IO JSText
js_create a1 a2 a3 a4 = if inBrowser then wait =<< js_createPromise a1 a2 a3 a4 else pure inert

js_get :: JSText -> JSText -> JSText -> Int -> IO JSText
js_get a1 a2 a3 a4 = if inBrowser then wait =<< js_getPromise a1 a2 a3 a4 else pure inert

js_keys_generate :: Int -> JSText -> Int -> IO JSText
js_keys_generate a1 a2 a3 = if inBrowser then wait =<< js_keys_generatePromise a1 a2 a3 else pure inert

js_keys_open :: Int -> JSText -> JSText -> JSText -> Int -> IO JSText
js_keys_open a1 a2 a3 a4 a5 = if inBrowser then wait =<< js_keys_openPromise a1 a2 a3 a4 a5 else pure inert

js_password_key :: Int -> JSText -> JSText -> Int -> IO JSText
js_password_key a1 a2 a3 a4 = if inBrowser then wait =<< js_password_keyPromise a1 a2 a3 a4 else pure inert

js_keys_fingerprint :: JSText -> IO JSText
js_keys_fingerprint a1 = if inBrowser then wait =<< js_keys_fingerprintPromise a1 else pure inert

js_wrap :: Int -> Int -> JSText -> IO JSText
js_wrap a1 a2 a3 = if inBrowser then wait =<< js_wrapPromise a1 a2 a3 else pure inert

js_unwrap :: Int -> JSText -> JSText -> JSText -> Bool -> Int -> IO JSText
js_unwrap a1 a2 a3 a4 a5 a6 = if inBrowser then wait =<< js_unwrapPromise a1 a2 a3 a4 a5 a6 else pure inert

js_discard :: JSText -> JSText -> IO ()
js_discard a1 a2 = if inBrowser then wait =<< js_discardPromise a1 a2 else pure inert

js_conditional_here :: IO Bool
js_conditional_here = if inBrowser then wait =<< js_conditional_herePromise else pure inert

js_passkey_sign_in :: JSText -> Bool -> Int -> IO JSText
js_passkey_sign_in a1 a2 a3 = if inBrowser then wait =<< js_passkey_sign_inPromise a1 a2 a3 else pure inert

-- | 'KeyRef': a key the page holds (a vault key, or the key that wraps it
-- for a passkey), by its number in @jsbits\/vault.js@. Its bytes stay in
-- the browser.
newtype KeyRef = KeyRef Int
  deriving stock (Eq, Show)

-- | 'KeyOwner' is who holds the keys an operation of the vault registers
-- in the page's registry of keys (@jsbits\/vault.js@), by its number. An
-- operation of a settings' section holds them under an owner of its own
-- ('openKeyOwner'), which the section lets go of when the operation ends
-- or the section closes ('closeKeyOwner'), and the page when the user
-- signs out ('closeKeyOwnersOf'). Letting an owner go deletes every key
-- registered under it, even those of an operation whose worker was
-- killed with the section, and keys made after it are not registered:
-- no key of a step cut off stays in the page. Nothing is written to this
-- browser under an owner let go either: no key kept, and no record of the
-- assistant's database kept or deleted ('keepDeviceRecordBy',
-- 'dropDeviceRecordIfBy', 'dropDeviceKeyBy'), each checked in the step
-- that writes. So an operation that outlives its reader (the move of the
-- assistant's legacy settings, a read of the vault that answers after a
-- sign-out) leaves nothing of its in this browser. 'pageKeys' is the
-- page's, which is never let go.
newtype KeyOwner = KeyOwner Int
  deriving stock (Eq, Ord, Show)

-- | The page's owner of keys: the keys of what lives as long as the page
-- (the sign-in, the shell), which forget their keys themselves.
pageKeys :: KeyOwner
pageKeys = KeyOwner 0

-- | A new owner of keys, for an operation of this user's ('KeyOwner'). It
-- returns at once. Natively there is no registry, and it is 'pageKeys'.
openKeyOwner :: UserId -> IO KeyOwner
openKeyOwner u
  | not inBrowser = pure pageKeys
  | otherwise = KeyOwner <$> js_owner_open (toJSText u.text)

-- | Let an owner of keys go: its keys leave the page, and none is
-- registered under it any more. It returns at once.
closeKeyOwner :: KeyOwner -> IO ()
closeKeyOwner (KeyOwner o) = when (inBrowser && o /= 0) (js_owner_close o)

-- | Let go of every owner of a user's keys, as their sign-out does, before
-- their keys leave this device. It returns at once.
closeKeyOwnersOf :: UserId -> IO ()
closeKeyOwnersOf u = when inBrowser (js_owners_close (toJSText u.text))

-- | 'Made': a passkey just made: the browser's answer for the server to
-- check, whether the passkey has the PRF extension, and the key that
-- wraps the vault key for it, when its authenticator gave the PRF output
-- at once (otherwise a use of the passkey gives it).
data Made = Made
  { response :: RegistrationResponse
  , prf :: Bool
  , wrapping :: Maybe KeyRef
  }
  deriving stock (Show)

-- | 'NewIdentity': a key pair just made for the user: its private key,
-- which the page holds and cannot export; its id; its public key; and its
-- private key sealed with the vault key, the vault's @identity@ item.
data NewIdentity = NewIdentity
  { key :: KeyRef
  , keyId :: KeyId
  , publicKey :: PublicKey
  , sealed :: Sealed
  }
  deriving stock (Show)

-- | 'Used': a passkey just used: the browser's answer for the server to
-- check, and the key that wraps the vault key for it, if the passkey
-- gave its PRF output.
data Used = Used
  { response :: AssertionResponse
  , wrapping :: Maybe KeyRef
  }
  deriving stock (Show)

-- | 'TabLock' is a request for a lock that the wiki's tabs in this browser
-- share ('requestTabLock'), by its number in @jsbits\/tabs.js@; 0 where
-- the browser has no such locks.
newtype TabLock = TabLock Int
  deriving stock (Eq, Show)

-- | 'PasskeyTicket': the number of a sign-in with a passkey, which starts
-- only while the number is current ('passkeyTicket').
newtype PasskeyTicket = PasskeyTicket Int
  deriving stock (Eq, Show)

-- | 'PasskeyRefusal': why a passkey was not made or used.
data PasskeyRefusal
  = -- | The person closed the browser's dialog, or let it time out.
    Cancelled
  | -- | Anything else, in the browser's words.
    PasskeyFailed Text
  deriving stock (Eq, Show)

-- | 'Talk': an open chat. It holds the chat's number in the browser-side
-- code, the callback its events come back through, which closing the chat
-- frees, and the handler the events go to.
data Talk = Talk
  { chat :: Int
  , callback :: Maybe Callback
  , heard :: Heard -> IO ()
  , blocked :: Maybe AssistantError
  -- ^ Why no message can be sent, when the page's policy refuses the
  -- chat's address ('blockedBy'): each message then fails with it.
  }

-- | 'Opening': what a chat is opened with. The provider, the model and the
-- key to use, and the request without its conversation
-- ("Wiki.Core.Assistant.Prompt").
data Opening = Opening
  { ready :: Ready
  , request :: Value
  }

-- | 'Heard': an event of one turn of the model.
data Heard
  = -- | Text the model wrote, as it streams.
    HeardText Text
  | -- | A piece of the summary of its thinking, as it streams. Anthropic
    -- and OpenAI send one when the request asks for it
    -- ("Wiki.Core.Assistant.Prompt") and the model thought; a model that
    -- did not think sends none.
    HeardThinking Text
  | -- | The model began writing a call of the tool named. The whole call
    -- follows as 'HeardCall' once its input is written, which for a long
    -- edit takes a while.
    HeardToolStart Text
  | -- | A tool call, by its number in the chat: the tool's name and its
    -- input. The turn waits for 'answerCall'.
    HeardCall Int Text Value
  | -- | The tokens of one request of the turn, once it finished: one per
    -- round of its tool loop, when the service reports them.
    HeardUsage Usage
  | -- | No conversation took the message: the provider's chat could not be
    -- made (its script did not load), and the next message starts a new
    -- one, empty. 'HeardFailed' or 'HeardStopped' follows.
    HeardUnsent
  | -- | The turn ended.
    HeardDone
  | -- | The turn was stopped.
    HeardStopped
  | -- | The turn failed.
    HeardFailed AssistantError
  deriving stock (Eq, Show)

instance FromJSON Heard where
  parseJSON = withObject "Heard" $ \o ->
    o .: "type" >>= \case
      "text" -> HeardText <$> o .: "text"
      "thinking" -> HeardThinking <$> o .: "text"
      "tool-start" -> HeardToolStart <$> o .: "name"
      "tool" -> HeardCall <$> o .: "call" <*> o .: "name" <*> o .: "input"
      "usage" -> HeardUsage <$> parseJSON (Object o)
      "unsent" -> pure HeardUnsent
      "done" -> pure HeardDone
      "stopped" -> pure HeardStopped
      "error" -> HeardFailed <$> parseJSON (Object o)
      other -> pure (HeardFailed (AssistantError Failed Nothing ("An event of a kind not known: " <> other)))

-- | The writing assistant, in the monad a component runs in.
--
-- The settings' methods and 'checkKey' wait (on IndexedDB, on the
-- provider), so a component calls them in @H.async@. The others return
-- immediately, and are called with 'Halogen.liftEffect'.
--
-- A monad transformer over a monad with an instance gets the methods
-- lifted, so @instance MonadAssistant App@ is enough for an application's
-- transformer stack.
class (Monad m) => MonadAssistant m where
  -- | Whether a browser is present to store the settings in and reach the
  -- providers from. A page rendered ahead of the program (by the prerender
  -- or the server) has none, and leaves the assistant's views as first
  -- rendered ("Loading…"), which the program then adopts unchanged.
  assistantHere :: m Bool
  default assistantHere :: (MonadTrans t, MonadAssistant n, m ~ t n) => m Bool
  assistantHere = lift assistantHere

  -- | What this device stores for a user ("Wiki.Core.Assistant.Vault"'s
  -- 'Wiki.Core.Assistant.Vault.DeviceRecord', as JSON), if anything, or
  -- why it cannot be read (a browser that stores nothing, as in some
  -- private windows).
  deviceRecord :: UserId -> m (Either Text (Maybe ByteString))
  default deviceRecord :: (MonadTrans t, MonadAssistant n, m ~ t n) => UserId -> m (Either Text (Maybe ByteString))
  deviceRecord = lift . deviceRecord

  -- | Store what this device keeps for a user, replacing what it kept; or,
  -- with nothing, delete it.
  keepDeviceRecord :: UserId -> Maybe ByteString -> m (Either Text ())
  keepDeviceRecord = keepDeviceRecordBy pageKeys

  -- | 'keepDeviceRecord', for an operation under this owner ('KeyOwner'):
  -- refused once the owner is let go, which the browser checks in the step
  -- that starts the write. A record is JSON, and crosses to the browser as
  -- text.
  keepDeviceRecordBy :: KeyOwner -> UserId -> Maybe ByteString -> m (Either Text ())
  default keepDeviceRecordBy :: (MonadTrans t, MonadAssistant n, m ~ t n) => KeyOwner -> UserId -> Maybe ByteString -> m (Either Text ())
  keepDeviceRecordBy o user = lift . keepDeviceRecordBy o user

  -- | Delete what this device keeps for a user only if it is still these
  -- bytes: decided and done in one transaction of the store, so that a tab
  -- of an earlier version that writes the record in between keeps its
  -- write. Whether it was deleted.
  dropDeviceRecordIf :: UserId -> ByteString -> m (Either Text Bool)
  dropDeviceRecordIf = dropDeviceRecordIfBy pageKeys

  -- | 'dropDeviceRecordIf', for an operation under this owner
  -- ('KeyOwner'): nothing is deleted once the owner is let go, checked in
  -- the transaction that deletes.
  dropDeviceRecordIfBy :: KeyOwner -> UserId -> ByteString -> m (Either Text Bool)
  default dropDeviceRecordIfBy :: (MonadTrans t, MonadAssistant n, m ~ t n) => KeyOwner -> UserId -> ByteString -> m (Either Text Bool)
  dropDeviceRecordIfBy o user = lift . dropDeviceRecordIfBy o user

  -- | A new vault key (AES-GCM), which can be exported only if asked: to
  -- be wrapped for a passkey.
  newVaultKey :: Bool -> m KeyRef
  newVaultKey = newVaultKeyBy pageKeys

  -- | 'newVaultKey', with the keys it registers under this owner ('KeyOwner').
  newVaultKeyBy :: KeyOwner -> Bool -> m KeyRef
  default newVaultKeyBy :: (MonadTrans t, MonadAssistant n, m ~ t n) => KeyOwner -> Bool -> m KeyRef
  newVaultKeyBy o = lift . newVaultKeyBy o

  -- | Seal text with a vault key, authenticating the given data with it.
  sealWith :: KeyRef -> Text -> Text -> m (Either Text Sealed)
  default sealWith :: (MonadTrans t, MonadAssistant n, m ~ t n) => KeyRef -> Text -> Text -> m (Either Text Sealed)
  sealWith key text = lift . sealWith key text

  -- | Open what 'sealWith' sealed, with the same key and data; or why not.
  openWith :: KeyRef -> Sealed -> Text -> m (Either Text Text)
  default openWith :: (MonadTrans t, MonadAssistant n, m ~ t n) => KeyRef -> Sealed -> Text -> m (Either Text Text)
  openWith key sealed = lift . openWith key sealed

  -- | Let the page forget a key it holds.
  forgetKey :: KeyRef -> m ()
  default forgetKey :: (MonadTrans t, MonadAssistant n, m ~ t n) => KeyRef -> m ()
  forgetKey = lift . forgetKey

  -- | 32 random bytes, in base64url: a new vault's salt.
  newSalt :: m Text
  default newSalt :: (MonadTrans t, MonadAssistant n, m ~ t n) => m Text
  newSalt = lift newSalt

  -- | Keep a vault key as this device's for a user: for the page's life,
  -- and in IndexedDB too unless told not to ("ask for the passkey each
  -- time"), as a key that cannot be exported.
  keepDeviceKey :: UserId -> KeyRef -> Bool -> m (Either Text ())
  keepDeviceKey = keepDeviceKeyBy pageKeys

  -- | 'keepDeviceKey', with the keys it registers under this owner ('KeyOwner').
  keepDeviceKeyBy :: KeyOwner -> UserId -> KeyRef -> Bool -> m (Either Text ())
  default keepDeviceKeyBy :: (MonadTrans t, MonadAssistant n, m ~ t n) => KeyOwner -> UserId -> KeyRef -> Bool -> m (Either Text ())
  keepDeviceKeyBy o user key = lift . keepDeviceKeyBy o user key

  -- | The vault key this device holds for a user, if any.
  deviceKey :: UserId -> m (Maybe KeyRef)
  deviceKey = deviceKeyBy pageKeys

  -- | 'deviceKey', with the keys it registers under this owner ('KeyOwner').
  deviceKeyBy :: KeyOwner -> UserId -> m (Maybe KeyRef)
  default deviceKeyBy :: (MonadTrans t, MonadAssistant n, m ~ t n) => KeyOwner -> UserId -> m (Maybe KeyRef)
  deviceKeyBy o = lift . deviceKeyBy o

  -- | Let go of this device's vault key for a user.
  dropDeviceKey :: UserId -> m ()
  dropDeviceKey = dropDeviceKeyBy pageKeys

  -- | 'dropDeviceKey', for an operation under this owner ('KeyOwner'):
  -- nothing is let go once the owner is let go, checked as the drop starts
  -- and again in the transaction that deletes the stored key.
  dropDeviceKeyBy :: KeyOwner -> UserId -> m ()
  default dropDeviceKeyBy :: (MonadTrans t, MonadAssistant n, m ~ t n) => KeyOwner -> UserId -> m ()
  dropDeviceKeyBy o = lift . dropDeviceKeyBy o

  -- | The changes of what the users' secrets read as in this page: a key
  -- kept or let go on this device (a vault unlocked or locked, by this
  -- tab or another), and a secret of a plugin kept or forgotten, by this
  -- tab or another ('announceSecretsChanged'). A view that shows whether
  -- a secret is set, or whether the vault is locked, reads again on each.
  -- Made in place: call it with 'Halogen.liftEffect'.
  secretsChanges :: m (Emitter IO ())
  default secretsChanges :: (MonadTrans t, MonadAssistant n, m ~ t n) => m (Emitter IO ())
  secretsChanges = lift secretsChanges

  -- | Tell this page and the wiki's other tabs that a secret of a plugin
  -- was kept or forgotten ('secretsChanges').
  announceSecretsChanged :: m ()
  default announceSecretsChanged :: (MonadTrans t, MonadAssistant n, m ~ t n) => m ()
  announceSecretsChanged = lift announceSecretsChanged

  -- | Ask for the lock of this name that every tab of the wiki in this
  -- browser shares (the browser's Web Locks). It returns at once, with the
  -- request: 'tabLockGranted' waits until the lock is held, and
  -- 'releaseTabLock' lets it go, or gives the request up if it is not
  -- held yet. Use 'holdingAcrossTabs', which always lets it go.
  requestTabLock :: Text -> m TabLock
  default requestTabLock :: (MonadTrans t, MonadAssistant n, m ~ t n) => Text -> m TabLock
  requestTabLock = lift . requestTabLock

  -- | Wait until the lock requested is held.
  tabLockGranted :: TabLock -> m ()
  default tabLockGranted :: (MonadTrans t, MonadAssistant n, m ~ t n) => TabLock -> m ()
  tabLockGranted = lift . tabLockGranted

  -- | Let go of the lock requested, held or not yet. It returns at once.
  releaseTabLock :: TabLock -> m ()
  default releaseTabLock :: (MonadTrans t, MonadAssistant n, m ~ t n) => TabLock -> m ()
  releaseTabLock = lift . releaseTabLock

  -- | Whether passkeys can be used here: a browser with WebAuthn, on a
  -- secure page.
  passkeysHere :: m Bool
  default passkeysHere :: (MonadTrans t, MonadAssistant n, m ~ t n) => m Bool
  passkeysHere = lift passkeysHere

  -- | Whether settings can be sealed here: the browser offers WebCrypto,
  -- which it does only to a secure page (HTTPS, or the machine's own
  -- address). Where it does not, the settings are stored as text, as
  -- before the vault ("Wiki.App.Assistant.Keys").
  sealingHere :: m Bool
  default sealingHere :: (MonadTrans t, MonadAssistant n, m ~ t n) => m Bool
  sealingHere = lift sealingHere

  -- | Make a passkey with the server's options, asking for its PRF on the
  -- salt, whose output gives the wrapping key with this HKDF info.
  createPasskey :: RegistrationOptions -> Text -> Text -> m (Either PasskeyRefusal Made)
  createPasskey = createPasskeyBy pageKeys

  -- | 'createPasskey', with the keys it registers under this owner ('KeyOwner').
  createPasskeyBy :: KeyOwner -> RegistrationOptions -> Text -> Text -> m (Either PasskeyRefusal Made)
  default createPasskeyBy :: (MonadTrans t, MonadAssistant n, m ~ t n) => KeyOwner -> RegistrationOptions -> Text -> Text -> m (Either PasskeyRefusal Made)
  createPasskeyBy o options salt = lift . createPasskeyBy o options salt

  -- | Use one of the user's passkeys, with the server's options, asking
  -- for its PRF on the salt, as 'createPasskey'.
  usePasskey :: AssertionOptions -> Text -> Text -> m (Either PasskeyRefusal Used)
  usePasskey = usePasskeyBy pageKeys

  -- | 'usePasskey', with the keys it registers under this owner ('KeyOwner').
  usePasskeyBy :: KeyOwner -> AssertionOptions -> Text -> Text -> m (Either PasskeyRefusal Used)
  default usePasskeyBy :: (MonadTrans t, MonadAssistant n, m ~ t n) => KeyOwner -> AssertionOptions -> Text -> Text -> m (Either PasskeyRefusal Used)
  usePasskeyBy o options salt = lift . usePasskeyBy o options salt

  -- | Whether the browser offers saved passkeys in a login field
  -- (conditional mediation), for 'signInPasskey'.
  conditionalHere :: m Bool
  default conditionalHere :: (MonadTrans t, MonadAssistant n, m ~ t n) => m Bool
  conditionalHere = lift conditionalHere

  -- | The ticket of a sign-in with a passkey about to ask the server for
  -- its challenge: it starts only if no abort nor other dialog came since
  -- ('signInPasskey'). It returns at once.
  passkeyTicket :: m PasskeyTicket
  default passkeyTicket :: (MonadTrans t, MonadAssistant n, m ~ t n) => m PasskeyTicket
  passkeyTicket = lift passkeyTicket

  -- | Use a passkey to sign in with (WP5.30 of docs/PLAN.md), with the
  -- server's options, asking for no PRF: in the browser's dialog, which
  -- ends any offer, or, when conditional, from the login field's offer,
  -- which waits until a passkey is picked there or 'abortSignInPasskey' is
  -- called. Neither starts ('Cancelled') if an abort or another dialog
  -- came since the ticket was taken ('passkeyTicket'), as when the form
  -- closed while the challenge was being fetched. The result is the
  -- browser's answer for the server to check.
  signInPasskey :: AssertionOptions -> Bool -> PasskeyTicket -> m (Either PasskeyRefusal AssertionResponse)
  default signInPasskey :: (MonadTrans t, MonadAssistant n, m ~ t n) => AssertionOptions -> Bool -> PasskeyTicket -> m (Either PasskeyRefusal AssertionResponse)
  signInPasskey options conditional = lift . signInPasskey options conditional

  -- | End the sign-in with a passkey under way, if any; it comes back
  -- 'Cancelled'.
  abortSignInPasskey :: m ()
  default abortSignInPasskey :: (MonadTrans t, MonadAssistant n, m ~ t n) => m ()
  abortSignInPasskey = lift abortSignInPasskey

  -- | Wrap a vault key (one that can be exported) with a passkey's
  -- wrapping key, authenticating the given data with it.
  wrapWith :: KeyRef -> KeyRef -> Text -> m (Either Text WrappedKey)
  default wrapWith :: (MonadTrans t, MonadAssistant n, m ~ t n) => KeyRef -> KeyRef -> Text -> m (Either Text WrappedKey)
  wrapWith wrapping key = lift . wrapWith wrapping key

  -- | Unwrap a vault key with a passkey's wrapping key, as one that can be
  -- exported only if asked.
  unwrapWith :: KeyRef -> WrappedKey -> Text -> Bool -> m (Either Text KeyRef)
  unwrapWith = unwrapWithBy pageKeys

  -- | 'unwrapWith', with the keys it registers under this owner ('KeyOwner').
  unwrapWithBy :: KeyOwner -> KeyRef -> WrappedKey -> Text -> Bool -> m (Either Text KeyRef)
  default unwrapWithBy :: (MonadTrans t, MonadAssistant n, m ~ t n) => KeyOwner -> KeyRef -> WrappedKey -> Text -> Bool -> m (Either Text KeyRef)
  unwrapWithBy o wrapping wrapped info = lift . unwrapWithBy o wrapping wrapped info

  -- | A new key pair for the user (X25519), its private key sealed with
  -- the vault key under 'Wiki.Core.Vault.identityData' of its id. The
  -- private key the result holds cannot be exported; the PKCS #8 bytes
  -- exist in the page only while they are sealed.
  newIdentity :: KeyRef -> m (Either Text NewIdentity)
  newIdentity = newIdentityBy pageKeys

  -- | 'newIdentity', with the keys it registers under this owner ('KeyOwner').
  newIdentityBy :: KeyOwner -> KeyRef -> m (Either Text NewIdentity)
  default newIdentityBy :: (MonadTrans t, MonadAssistant n, m ~ t n) => KeyOwner -> KeyRef -> m (Either Text NewIdentity)
  newIdentityBy o = lift . newIdentityBy o

  -- | Open the user's sealed private key with the vault key: a key that
  -- cannot be exported.
  openIdentity :: KeyRef -> KeyId -> Sealed -> m (Either Text KeyRef)
  openIdentity = openIdentityBy pageKeys

  -- | 'openIdentity', with the keys it registers under this owner ('KeyOwner').
  openIdentityBy :: KeyOwner -> KeyRef -> KeyId -> Sealed -> m (Either Text KeyRef)
  default openIdentityBy :: (MonadTrans t, MonadAssistant n, m ~ t n) => KeyOwner -> KeyRef -> KeyId -> Sealed -> m (Either Text KeyRef)
  openIdentityBy o k i = lift . openIdentityBy o k i

  -- | The key that wraps the vault key for the password, from the key
  -- OPAQUE's export key gave the page ("Wiki.App.Backend.Opaque") and the
  -- vault's salt: one that only wraps and unwraps.
  passwordKey :: KeyRef -> Salt -> m (Either Text KeyRef)
  passwordKey = passwordKeyBy pageKeys

  -- | 'passwordKey', with the keys it registers under this owner ('KeyOwner').
  passwordKeyBy :: KeyOwner -> KeyRef -> Salt -> m (Either Text KeyRef)
  default passwordKeyBy :: (MonadTrans t, MonadAssistant n, m ~ t n) => KeyOwner -> KeyRef -> Salt -> m (Either Text KeyRef)
  passwordKeyBy o k = lift . passwordKeyBy o k

  -- | A public key's fingerprint: SHA-256 of its bytes, in hexadecimal;
  -- empty where the browser cannot compute it.
  fingerprintOf :: PublicKey -> m Text
  default fingerprintOf :: (MonadTrans t, MonadAssistant n, m ~ t n) => PublicKey -> m Text
  fingerprintOf = lift . fingerprintOf

  -- | Tell the person's password manager that a passkey of this relying
  -- party is not the wiki's any more, where the browser can.
  discardPasskey :: Text -> CredentialId -> m ()
  default discardPasskey :: (MonadTrans t, MonadAssistant n, m ~ t n) => Text -> CredentialId -> m ()
  discardPasskey rpId = lift . discardPasskey rpId

  -- | Whether a test installed the scripted provider.
  scriptedInstalled :: m Bool
  default scriptedInstalled :: (MonadTrans t, MonadAssistant n, m ~ t n) => m Bool
  scriptedInstalled = lift scriptedInstalled

  -- | Whether the page is a secure context: served over HTTPS, or from the
  -- local machine's address.
  secureContext :: m Bool
  default secureContext :: (MonadTrans t, MonadAssistant n, m ~ t n) => m Bool
  secureContext = lift secureContext

  -- | Start fetching the code the providers need, so that it is loaded by
  -- the first message.
  prepareAssistant :: m ()
  default prepareAssistant :: (MonadTrans t, MonadAssistant n, m ~ t n) => m ()
  prepareAssistant = lift prepareAssistant

  -- | Open a chat, whose turn events are passed to the given function.
  openTalk :: Opening -> (Heard -> IO ()) -> m Talk
  default openTalk :: (MonadTrans t, MonadAssistant n, m ~ t n) => Opening -> (Heard -> IO ()) -> m Talk
  openTalk opening = lift . openTalk opening

  -- | Send a user message (as 'Wiki.Core.Assistant.Prompt.userTurn' builds
  -- it) with the current request (the chosen model and effort may have
  -- changed since the chat was opened). This runs one turn of the model,
  -- unless a turn is already running.
  sayTo :: Talk -> Value -> Text -> m ()
  default sayTo :: (MonadTrans t, MonadAssistant n, m ~ t n) => Talk -> Value -> Text -> m ()
  sayTo talk request = lift . sayTo talk request

  -- | Answer a tool call: the result the model receives, and whether the
  -- edit was made.
  answerCall :: Talk -> Int -> Text -> Bool -> m ()
  default answerCall :: (MonadTrans t, MonadAssistant n, m ~ t n) => Talk -> Int -> Text -> Bool -> m ()
  answerCall talk call text ok = lift (answerCall talk call text ok)

  -- | Stop the running turn, if any.
  stopTalk :: Talk -> m ()
  default stopTalk :: (MonadTrans t, MonadAssistant n, m ~ t n) => Talk -> m ()
  stopTalk = lift . stopTalk

  -- | Close a chat: its turn is stopped, and no more events arrive.
  closeTalk :: Talk -> m ()
  default closeTalk :: (MonadTrans t, MonadAssistant n, m ~ t n) => Talk -> m ()
  closeTalk = lift . closeTalk

  -- | "Test the key": the smallest possible request, looking up the model
  -- with the key, which is free. The result is the error, if any.
  checkKey :: Ready -> m (Maybe AssistantError)
  default checkKey :: (MonadTrans t, MonadAssistant n, m ~ t n) => Ready -> m (Maybe AssistantError)
  checkKey = lift . checkKey

  -- | The provider's models, every one in its list, fetched with the key
  -- (the scripted provider's come from its script); or the error. Any
  -- response, including a refusal, shows that the provider can be reached;
  -- an error of kind 'Unreachable' shows that it cannot.
  listModels :: Ready -> m (Either AssistantError [Listed])
  default listModels :: (MonadTrans t, MonadAssistant n, m ~ t n) => Ready -> m (Either AssistantError [Listed])
  listModels = lift . listModels

instance (MonadAssistant m) => MonadAssistant (ReaderT r m)

-- | The browser's instance. Natively the bindings are inert stubs: nothing
-- runs 'BrowserDOM' there, but code written against it still type-checks.
instance MonadAssistant BrowserDOM where
  assistantHere = pure inBrowser
  deviceRecord = recordIn

  -- The page's own writes go through the store's library; an operation's
  -- under an owner through one call of @jsbits\/vault.js@ that checks the
  -- owner as it starts the transaction ('KeyOwner').
  keepDeviceRecordBy (KeyOwner 0) user record = keepRecordIn user record
  keepDeviceRecordBy (KeyOwner owner) user record = liftIO $ do
    failure <- evaluate . fromJSText =<< js_keep_record (toJSText user.text) (toJSText (maybe "" decodeUtf8 record)) (isJust record) owner
    pure (if T.null failure then Right () else Left ("This browser could not keep the assistant's settings (" <> failure <> ")."))
  dropDeviceRecordIfBy (KeyOwner owner) user bytes = liftIO (Right <$> (evaluate =<< js_drop_record_if (toJSText user.text) (toJSText (decodeUtf8 bytes)) owner))
  newVaultKeyBy (KeyOwner owner) exportable = liftIO (KeyRef <$> (evaluate =<< js_new_key exportable owner))
  sealWith (KeyRef k) text info = liftIO $ do
    answer <- evaluate . fromJSText =<< js_seal k (toJSText text) (toJSText info)
    pure (answered (\o -> Sealed <$> o .: "nonce" <*> o .: "ciphertext") answer)
  openWith (KeyRef k) sealed info = liftIO $ do
    answer <- evaluate . fromJSText =<< js_open_sealed k (toJSText sealed.nonce.text) (toJSText sealed.ciphertext.text) (toJSText info)
    pure (answered (.: "text") answer)
  forgetKey (KeyRef k) = liftIO (js_forget_key k)
  newSalt = liftIO (fromJSText <$> js_salt)
  keepDeviceKeyBy (KeyOwner owner) user (KeyRef k) onDisk = liftIO $ do
    failure <- evaluate . fromJSText =<< js_keep_key (toJSText user.text) k onDisk owner
    pure (if T.null failure then Right () else Left ("This browser could not keep the vault's key: " <> failure))
  deviceKeyBy (KeyOwner owner) user = liftIO $ do
    k <- evaluate =<< js_device_key (toJSText user.text) owner
    pure (if k == 0 then Nothing else Just (KeyRef k))
  dropDeviceKeyBy (KeyOwner owner) user = liftIO (evaluate =<< js_drop_key (toJSText user.text) owner)
  secretsChanges = liftIO secretsHeard
  announceSecretsChanged = liftIO js_secrets_changed
  requestTabLock name = liftIO (TabLock <$> js_tab_lock_request (toJSText name))
  tabLockGranted (TabLock n) = liftIO (when (n /= 0) (evaluate =<< js_tab_lock_granted n))
  releaseTabLock (TabLock n) = liftIO (when (n /= 0) (js_tab_lock_release n))
  passkeysHere = liftIO js_passkeys_here
  sealingHere = liftIO js_sealing_here
  createPasskeyBy (KeyOwner owner) options salt info = liftIO $ do
    answer <- evaluate . fromJSText =<< js_create (toJSText (json (toJSON options))) (toJSText salt) (toJSText info) owner
    pure . passkeyAnswer answer $ \o -> Made <$> o .: "response" <*> o .: "prf" <*> (keyRefOf <$> o .: "wrapping")
  usePasskeyBy (KeyOwner owner) options salt info = liftIO $ do
    answer <- evaluate . fromJSText =<< js_get (toJSText (json (toJSON options))) (toJSText salt) (toJSText info) owner
    pure . passkeyAnswer answer $ \o -> Used <$> o .: "response" <*> (keyRefOf <$> o .: "wrapping")
  wrapWith (KeyRef wrapping) (KeyRef k) info = liftIO $ do
    answer <- evaluate . fromJSText =<< js_wrap wrapping k (toJSText info)
    pure (answered (\o -> WrappedKey <$> o .: "nonce" <*> o .: "key") answer)
  unwrapWithBy (KeyOwner owner) (KeyRef wrapping) wrapped info exportable = liftIO $ do
    answer <- evaluate . fromJSText =<< js_unwrap wrapping (toJSText wrapped.nonce.text) (toJSText wrapped.key.text) (toJSText info) exportable owner
    pure (answered (\o -> KeyRef <$> o .: "handle") answer)
  discardPasskey rpId credential = liftIO (evaluate =<< js_discard (toJSText rpId) (toJSText credential.text))
  newIdentityBy (KeyOwner owner) (KeyRef v) = liftIO $ do
    answer <- evaluate . fromJSText =<< js_keys_generate v (toJSText identityDataPrefix) owner
    pure . flip answered answer $ \o ->
      NewIdentity
        <$> (KeyRef <$> o .: "handle")
        <*> o .: "keyId"
        <*> o .: "publicKey"
        <*> (Sealed <$> o .: "nonce" <*> o .: "ciphertext")
  openIdentityBy (KeyOwner owner) (KeyRef v) keyId sealed = liftIO $ do
    answer <- evaluate . fromJSText =<< js_keys_open v (toJSText sealed.nonce.text) (toJSText sealed.ciphertext.text) (toJSText (identityData keyId)) owner
    pure (answered (\o -> KeyRef <$> o .: "handle") answer)
  fingerprintOf k = liftIO (fromJSText <$> (evaluate =<< js_keys_fingerprint (toJSText k.text)))
  passwordKeyBy owner base salt = liftIO (passwordKeyIn owner base salt)
  conditionalHere = liftIO (evaluate =<< js_conditional_here)
  passkeyTicket = liftIO (PasskeyTicket <$> js_passkey_offer_ticket)
  signInPasskey options conditional (PasskeyTicket ticket) = liftIO $ do
    answer <- evaluate . fromJSText =<< js_passkey_sign_in (toJSText (json (toJSON options))) conditional ticket
    pure . passkeyAnswer answer $ \o -> o .: "response"
  abortSignInPasskey = liftIO js_passkey_abort
  scriptedInstalled = liftIO scripted
  secureContext = liftIO ((.isSecureContext) <$> Window.window)
  prepareAssistant = liftIO js_prepare
  openTalk opening heard = liftIO $ do
    callback <- mkSyncCallback (heard . heardOf . jsValText)
    chat <- js_open (toJSText (json (openingJSON opening))) callback
    blocked <- blockedBy opening.ready
    pure Talk {chat, callback = Just callback, heard, blocked}
  sayTo talk request user = liftIO $ case talk.blocked of
    Just e -> talk.heard (HeardFailed e)
    Nothing -> js_send talk.chat (toJSText user) (toJSText (json request))
  answerCall talk call text ok = liftIO (js_answer talk.chat call (toJSText text) ok)
  stopTalk talk = liftIO (js_stop talk.chat)
  closeTalk talk = liftIO $ do
    js_close talk.chat
    traverse_ freeCallback talk.callback
  checkKey r =
    liftIO $
      blockedBy r >>= \case
        Just e -> pure (Just e)
        Nothing -> do
          -- The binding waits for the promise ('wait'), and 'evaluate' forces the answer.
          answer <- evaluate . fromJSText =<< js_check (toJSText (json (endpointJSON r ["model" .= r.model])))
          pure $
            if T.null answer
              then Nothing
              else Just (either (\why -> AssistantError Failed Nothing (toS why)) identity (eitherDecodeStrict (encodeUtf8 answer)))
  listModels r =
    liftIO $
      blockedBy r >>= \case
        Just e -> pure (Left e)
        Nothing -> do
          answer <- evaluate . fromJSText =<< js_models (toJSText (json (endpointJSON r [])))
          pure (listedOf answer)

-- | The in-memory document's instance: records stored as in the browser,
-- in its in-memory IndexedDB, but no WebCrypto to seal them with, no
-- passkeys and no provider. Since it is not a browser, no assistant is
-- shown ('assistantHere'), and nothing reads its settings.
instance MonadAssistant MemDOM where
  assistantHere = pure False
  deviceRecord = recordIn

  -- The in-memory document has no owners of keys ('openKeyOwner' gives the
  -- page's): every write is the page's.
  keepDeviceRecordBy _ = keepRecordIn

  -- The in-memory document runs one program at a time: nothing writes
  -- between the read and the delete.
  dropDeviceRecordIfBy _ user bytes =
    recordIn user >>= \case
      Right (Just now) | now == bytes -> fmap (const True) <$> keepRecordIn user Nothing
      Right _ -> pure (Right False)
      Left why -> pure (Left why)
  newVaultKeyBy _ _ = pure (KeyRef 0)
  sealWith _ _ _ = pure (Left noBrowser)
  openWith _ _ _ = pure (Left noBrowser)
  forgetKey _ = pass
  newSalt = pure ""
  keepDeviceKeyBy _ _ _ _ = pure (Left noBrowser)
  deviceKeyBy _ _ = pure Nothing
  dropDeviceKeyBy _ _ = pass

  -- No key is kept here, and the page is the only one.
  secretsChanges = liftIO ((\s -> s.emitter) <$> HS.create)
  announceSecretsChanged = pass
  requestTabLock _ = pure (TabLock 0)
  tabLockGranted _ = pass
  releaseTabLock _ = pass
  passkeysHere = pure False
  sealingHere = pure False
  createPasskeyBy _ _ _ _ = pure (Left (PasskeyFailed noBrowser))
  usePasskeyBy _ _ _ _ = pure (Left (PasskeyFailed noBrowser))
  wrapWith _ _ _ = pure (Left noBrowser)
  unwrapWithBy _ _ _ _ _ = pure (Left noBrowser)
  discardPasskey _ _ = pass
  newIdentityBy _ _ = pure (Left noBrowser)
  openIdentityBy _ _ _ _ = pure (Left noBrowser)
  fingerprintOf _ = pure ""
  passwordKeyBy _ _ _ = pure (Left noBrowser)
  conditionalHere = pure False
  passkeyTicket = pure (PasskeyTicket 0)
  signInPasskey _ _ _ = pure (Left (PasskeyFailed noBrowser))
  abortSignInPasskey = pass
  scriptedInstalled = pure False
  secureContext = pure True
  prepareAssistant = pass
  openTalk _ heard = pure Talk {chat = 0, callback = Nothing, heard, blocked = Nothing}
  sayTo talk _ _ = liftIO (talk.heard (HeardFailed (AssistantError Unreachable Nothing "There is no browser here to reach a provider from.")))
  answerCall _ _ _ _ = pass
  stopTalk _ = pass
  closeTalk _ = pass
  checkKey _ = pure (Just (AssistantError Unreachable Nothing "There is no browser here to reach a provider from."))
  listModels _ = pure (Left (AssistantError Unreachable Nothing "There is no browser here to reach a provider from."))

-- | 'passwordKey' in IO, for the HTTP backend, which holds no
-- 'MonadAssistant' ("Wiki.App.Backend.Http"). Natively there is no key.
passwordKeyIO :: KeyRef -> Salt -> IO (Either Text KeyRef)
passwordKeyIO = passwordKeyIn pageKeys

-- | 'passwordKeyIO', with the key registered under this owner.
passwordKeyIn :: KeyOwner -> KeyRef -> Salt -> IO (Either Text KeyRef)
passwordKeyIn (KeyOwner owner) (KeyRef base) salt
  | not inBrowser = pure (Left noBrowser)
  | otherwise = do
      answer <- evaluate . fromJSText =<< js_password_key base (toJSText salt.text) (toJSText passwordWrappingInfo) owner
      pure (answered (\o -> KeyRef <$> o .: "handle") answer)

-- | Wrap a vault key (one that can be exported) for a new password: the
-- key OPAQUE's export key gave for it, and the vault's salt, give the
-- wrapping key, which the page forgets at once. For the HTTP backend's
-- change of password, which sends the new wrap with the new password.
wrapForPassword :: KeyRef -> Salt -> KeyRef -> IO (Either Text WrappedKey)
wrapForPassword base salt (KeyRef vault)
  | not inBrowser = pure (Left noBrowser)
  | otherwise =
      passwordKeyIO base salt >>= \case
        Left e -> pure (Left e)
        Right (KeyRef wrapping) -> do
          answer <- evaluate . fromJSText =<< js_wrap wrapping vault (toJSText wrappingData)
          js_forget_key wrapping
          pure (answered (\o -> WrappedKey <$> o .: "nonce" <*> o .: "key") answer)

-- | Let the page forget a key, in IO, for the HTTP backend.
forgetHeld :: KeyRef -> IO ()
forgetHeld (KeyRef k) = when inBrowser (js_forget_key k)

-- | 'holdKey' puts a WebCrypto key made outside @jsbits\/vault.js@ into the
-- page's registry of keys, and gives its 'KeyRef'. It is for the key
-- OPAQUE's export key gives, which opaque-hs's client imports into
-- WebCrypto ("Wiki.App.Backend.Opaque"), under the owner given
-- ('KeyOwner'). 'Nothing' for a value that is not a @CryptoKey@, under an
-- owner let go, and natively.
holdKey :: KeyOwner -> JSVal -> IO (Maybe KeyRef)
holdKey (KeyOwner owner) key
  | not inBrowser = pure Nothing
  | otherwise = keyRefOf <$> (evaluate =<< js_hold_key key owner)

-- | A key's handle in the page's registry of keys, which every module of
-- JavaScript that holds keys shares (@globalThis.wikiKeyRegistry@): for
-- "Wiki.App.Encrypted", which opens a space's key with the private key
-- this module found.
heldHandle :: KeyRef -> Int
heldHandle (KeyRef k) = k

-- | The key this device keeps in a slot ('deviceKey'), in IO, for the HTTP
-- backend's encrypted spaces ("Wiki.App.Encrypted"), which open a space's
-- key with the user's private key. Natively there is none.
deviceKeyIO :: UserId -> IO (Maybe KeyRef)
deviceKeyIO user
  | not inBrowser = pure Nothing
  | otherwise = keyRefOf <$> (evaluate =<< js_device_key (toJSText user.text) 0)

-- | Listen to the wiki's other tabs from now on: a key let go in one of them
-- is let go here ('dropDeviceKey'), and a vault key or private key let go
-- there (a sign-out) is announced in this window ('keysDroppedElsewhere').
listenToOtherTabs :: IO ()
listenToOtherTabs = when inBrowser js_listen

-- | Tell the wiki's tabs that a sign-in was completed: a tab that found
-- nobody signed in, and has not yet let go of every user's keys
-- ("Wiki.App.Vault"'s 'Wiki.App.Vault.forgetEveryUsersKeys'), lets go of
-- none.
announceSignIn :: IO ()
announceSignIn = when inBrowser js_signed_in

-- | The announcement that another tab let go of a user's vault key or
-- private key, as its sign-out does.
keysDroppedElsewhere :: (MonadBrowserDOM m, MonadUnliftIO m) => m (Emitter IO ())
keysDroppedElsewhere = WindowEvent.windowEvent (EventType "wiki-keys-dropped") (const (Just ()))

-- | How many times a key was kept on this device: what a page that lets
-- go of every user's keys reads first, and compares when it deletes them
-- ('dropSlotsIfIO').
vaultGenerationIO :: IO Int
vaultGenerationIO = if inBrowser then evaluate =<< js_generation else pure 0

-- | Let go of these slots' keys on this device, in every tab, only if no
-- key was kept since the generation given ('vaultGenerationIO') and no
-- sign-in was heard of: decided and done in one transaction of the
-- device's store. Whether they were let go.
dropSlotsIfIO :: Int -> [UserId] -> IO Bool
dropSlotsIfIO generation slots
  | not inBrowser = pure False
  | otherwise = evaluate =<< js_drop_if generation (toJSText (decodeUtf8 (BSL.toStrict (encode (map (\u -> u.text) slots)))))

-- | The slots this device keeps a key in (a user's id, or "vault:" or
-- "identity:" before it), in IndexedDB and in the page. Natively none.
deviceSlotsIO :: IO [Text]
deviceSlotsIO
  | not inBrowser = pure []
  | otherwise = fromMaybe [] . rightToMaybe . eitherDecodeStrict . encodeUtf8 . fromJSText <$> (evaluate =<< js_slots)

-- | Why the browser would refuse a request to another service's base URL,
-- if it would: the page's Content Security Policy ("Wiki.Api.ContentPolicy")
-- does not allow its address. Checked before a request is made, so that
-- the settings and the chat say why at once, instead of the SDK retrying
-- a refused connection and reporting the network as down. A page without
-- a policy, and the scripted provider of the tests (which sends nothing),
-- refuse nothing.
blockedBy :: Ready -> IO (Maybe AssistantError)
blockedBy r = case r.baseUrl of
  Nothing -> pure Nothing
  Just url ->
    scripted >>= \case
      True -> pure Nothing
      False -> do
        policy <- pagePolicy
        origin <- Window.window >>= \w -> w.location.origin
        pure $
          if T.null policy || allowsConnect origin policy url.text
            then Nothing
            else Just (AssistantError Blocked Nothing (blockedMessage policy url))

-- | What the assistant says of an address the page does not allow: the
-- addresses it does allow, and who can add one.
blockedMessage :: Text -> Url -> Text
blockedMessage policy url =
  maybe url.text identity (originOf url.text)
    <> " is not among the addresses they allow: "
    <> T.intercalate ", " (extraHosts policy)
    <> ". On a wiki with a server, an administrator can add it to the assistant's hosts, in the administration's server settings."

-- | The database the assistant's settings are stored in, separate from the
-- demo's wiki: one store, @settings@, keyed by user id, each value what this
-- device keeps for a user ("Wiki.Core.Assistant.Vault"'s
-- 'Wiki.Core.Assistant.Vault.DeviceRecord', or, written before the vault,
-- the settings as text, which are sealed and replaced the first time they
-- are read). The vault's key is kept apart, by @jsbits\/vault.js@, in a
-- database of its own (@wiki-assistant-keys@), since IndexedDB stores a
-- WebCrypto key only as the key itself.
assistantDatabase :: Database
assistantDatabase = Database {name = "wiki-assistant", version = 1, stores = ["settings"]}

-- | What a database keeps for a user, if anything.
readRecord :: (MonadIndexedDB m) => Database -> UserId -> m (Maybe ByteString)
readRecord db user = IDB.get db "settings" user.text

-- | Store what is kept for a user in a database, or delete it.
writeRecord :: (MonadIndexedDB m) => Database -> UserId -> Maybe ByteString -> m ()
writeRecord db user = \case
  Nothing -> IDB.delete db "settings" user.text
  Just bytes -> IDB.put db "settings" user.text bytes

-- | 'deviceRecordsLock' is the lock a function holds, for the whole of
-- it, when it reads what this device records for a user and may write it
-- back ('holdingDeviceRecords'): the assistant's settings
-- ("Wiki.App.Assistant.Keys") and the record of the user's vault
-- ("Wiki.App.Vault"'s 'Wiki.App.Vault.VaultDevice'). Each such function
-- reads a record, waits (on IndexedDB, on WebCrypto, on the server, on a
-- passkey) and writes back the record it read with its own change, so two
-- at once wrote over each other: two checks of providers, the second
-- putting back a provider's reachability the first had changed; a
-- sign-in's settling of the keys putting back the "Ask each time" the
-- security panel had turned on. With the lock the second reads what the
-- first wrote.
--
-- One lock covers both records, because they are read together: whether
-- the vault key is kept on disk follows the vault's "Ask each time" and
-- the assistant's, read from both records ('Wiki.App.Vault.keepSharedKey',
-- 'Wiki.App.Vault.vaultKeyOnDisk'), and a function of either module may
-- read the other's record. Two locks would have to be taken in one order
-- everywhere. Only the two modules' exported functions take it, never a
-- helper, which they call while they hold it; neither module's exported
-- functions call the other's. It is one lock for every user, in this page
-- only: another tab of the wiki has its own, and can still write over a
-- change of this one.
deviceRecordsLock :: MVar ()
deviceRecordsLock = unsafePerformIO (newMVar ())
{-# NOINLINE deviceRecordsLock #-}

-- | Run a reading and writing of this device's records holding
-- 'deviceRecordsLock'. The lock is given back however the run ends, a
-- cancelled fork's included.
holdingDeviceRecords :: (MonadUnliftIO m) => m a -> m a
holdingDeviceRecords action = withRunInIO (\run -> withMVar deviceRecordsLock (const (run action)))

-- | 'secretsHearing' is the page's one emitter of 'secretsChanges', once
-- made: the window's @wiki-secrets-changed@ event, which
-- @jsbits\/vault.js@ dispatches, listened for from the first call on.
secretsHearing :: MVar (Maybe (Emitter IO ()))
secretsHearing = unsafePerformIO (newMVar Nothing)
{-# NOINLINE secretsHearing #-}

-- | 'secretsChanges' in the browser: the page's emitter, made and
-- listening at the first call. Natively nothing is ever heard.
secretsHeard :: IO (Emitter IO ())
secretsHeard = modifyMVar secretsHearing $ \case
  Just e -> pure (Just e, e)
  Nothing -> do
    HS.Subscribe {listener, emitter} <- HS.create
    when inBrowser (js_on_secrets_changed =<< mkSyncCallback (const (HS.notify listener ())))
    pure (Just emitter, emitter)

-- | Run this holding the lock of this name that the wiki's tabs in this
-- browser share ('requestTabLock'): a tab that asks for it meanwhile waits
-- until this ends. The lock is let go however the run ends, a cancelled
-- fork's included; the request is made in place, so a fork cancelled
-- while it waits gives up its request. Where the browser has no such
-- locks (a page over plain HTTP), it runs without one.
holdingAcrossTabs :: (MonadAssistant m, MonadUnliftIO m) => Text -> m a -> m a
holdingAcrossTabs name action = withRunInIO (\run -> bracket (run (requestTabLock name)) (run . releaseTabLock) (\l -> run (tabLockGranted l >> action)))

recordIn :: (MonadIndexedDB m, MonadUnliftIO m) => UserId -> m (Either Text (Maybe ByteString))
recordIn user = failingAs (readRecord assistantDatabase user)

keepRecordIn :: (MonadIndexedDB m, MonadUnliftIO m) => UserId -> Maybe ByteString -> m (Either Text ())
keepRecordIn user record = failingAs (writeRecord assistantDatabase user record)

-- | What the in-memory document says of what only a browser does.
noBrowser :: Text
noBrowser = "There is no browser here to keep the assistant's keys in."

-- | A key's handle from the browser: 0 for none.
keyRefOf :: Int -> Maybe KeyRef
keyRefOf k = if k == 0 then Nothing else Just (KeyRef k)

-- | An answer of @jsbits\/vault.js@: JSON with what was asked for, or
-- @{error}@.
answered :: (Object -> Parser a) -> Text -> Either Text a
answered parse answer =
  case eitherDecodeStrict (encodeUtf8 answer) >>= parseEither (withObject "Answer" (\o -> (Left <$> o .: "error") <|> (Right <$> parse o))) of
    Left why -> Left ("The browser's answer could not be read: " <> toS why)
    Right result -> result

-- | A passkey's answer: what was asked for, or why not, with a dialog the
-- person closed told apart.
passkeyAnswer :: Text -> (Object -> Parser a) -> Either PasskeyRefusal a
passkeyAnswer answer parse =
  case eitherDecodeStrict (encodeUtf8 answer) >>= parseEither (withObject "Answer" (\o -> (Left <$> refusal o) <|> (Right <$> parse o))) of
    Left why -> Left (PasskeyFailed ("The browser's answer could not be read: " <> toS why))
    Right result -> result
  where
    refusal o = do
      why <- o .: "error"
      cancelled <- o .:? "cancelled" .!= False
      pure (if cancelled then Cancelled else PasskeyFailed why)

-- | An IndexedDB failure, as a message to the user.
failingAs :: (MonadUnliftIO m) => m a -> m (Either Text a)
failingAs action =
  withRunInIO (\run -> try (run action)) <&> first (\(IndexedDBError _ failure) -> "This browser could not keep the assistant's settings (" <> show failure <> ").")

-- | Decode the browser's response to a model list request: @{models:
-- [...]}@, or @{error: {kind, status, message}}@.
listedOf :: Text -> Either AssistantError [Listed]
listedOf answer =
  case eitherDecodeStrict (encodeUtf8 answer) >>= parseEither (withObject "Models" (\o -> (Right <$> o .: "models") <|> (Left <$> o .: "error"))) of
    Left why -> Left (AssistantError Failed Nothing ("A list of models that could not be read: " <> toS why))
    Right listed -> listed

heardOf :: Text -> Heard
heardOf text = either (\why -> HeardFailed (AssistantError Failed Nothing ("An event that could not be read: " <> toS why))) identity (eitherDecodeStrict (encodeUtf8 text))

openingJSON :: Opening -> Value
openingJSON o = endpointJSON o.ready ["model" .= o.ready.model, "request" .= o.request]

-- | What the browser-side code needs to reach a provider: the provider,
-- the key (empty for a server that needs none) and the base URL of another
-- service, if any; with the given extra fields.
endpointJSON :: Ready -> [Pair] -> Value
endpointJSON r more = object (["provider" .= providerKey r.provider, "key" .= r.key, "baseUrl" .= r.baseUrl] <> more)

json :: Value -> Text
json = decodeUtf8 . BSL.toStrict . encode
