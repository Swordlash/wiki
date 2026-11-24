-- | 'MonadVault': the server's half of the assistant's keys across
-- devices (WP5.21 of docs/PLAN.md, "Wiki.Api.Vault"): the vault the server
-- keeps for the signed-in user, and the passkeys that unlock it. The
-- browser's half (WebCrypto, WebAuthn, this device's key) is
-- "Wiki.App.Assistant"'s, and "Wiki.App.Assistant.Keys" puts the two
-- together.
--
-- The HTTP backend asks the server ("Wiki.App.Backend.Http"). The demo has
-- no server, so nothing syncs there ('vaultSync' is 'False'): its keys
-- stay in the browser, in a vault of the same format.
module Wiki.App.Assistant.Sync
  ( MonadVault (..)
  , Fetched (..)
  , Rewrap (..)
  , Owned
  , owned
  , owning
  , releaseOwners
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Set qualified as Set
import Data.Type.Equality (type (~))
import Halogen.Query.HalogenM (HalogenM)
import Halogen.Query.HalogenM qualified as H
import Halogen.VDom.DOM.Monad (BrowserDOM)
import Halogen.VDom.DOM.Monad.Mem (MemDOM)
import Protolude
import Wiki.Api.Account (PasswordChange, SignInMethods)
import Wiki.Api.Vault (AssertionOptions, AssertionResponse, PasskeyInfo, PublicKeyEntry, RegistrationOptions, RegistrationResponse, StoredVault, VaultChange)
import Wiki.App.Account (SignInError (..))
import Wiki.App.Assistant (KeyOwner, KeyRef, MonadAssistant (..), closeKeyOwner, openKeyOwner, pageKeys)
import Wiki.App.Backend (WikiError (..))
import Wiki.App.Backend.Mem (MemWiki)
import Wiki.Core.Assistant.Vault (CredentialId, Salt, Vault, VaultTag)
import Wiki.Core.Id (UserId)
import Wiki.Core.User (Password)

-- | 'Fetched': the server's answer to a device that asks for the vault.
data Fetched
  = -- | The user has no vault: synchronisation is off.
    NoVault
  | -- | The vault is the version the device has.
    Unchanged
  | -- | The vault, at another version than the device's.
    Fetched StoredVault
  deriving stock (Eq, Show)

-- | 'MonadVault': the vault and the passkeys, as the server keeps them for
-- the signed-in user.
class (Monad m) => MonadVault m where
  -- | Whether a server keeps a vault at all: over HTTP, not in the demo.
  vaultSync :: m Bool
  default vaultSync :: (MonadTrans t, m ~ t n, MonadVault n) => m Bool
  vaultSync = lift vaultSync

  -- | The vault, unless it is the version the given tag names.
  fetchVault :: Maybe VaultTag -> m (Either WikiError Fetched)
  default fetchVault :: (MonadTrans t, m ~ t n, MonadVault n) => Maybe VaultTag -> m (Either WikiError Fetched)
  fetchVault = lift . fetchVault

  -- | Store a new version of the vault over the one the tag names, or,
  -- without one, a first vault. 'Conflict' when the server has another.
  storeVault :: Maybe VaultTag -> Vault -> m (Either WikiError StoredVault)
  default storeVault :: (MonadTrans t, m ~ t n, MonadVault n) => Maybe VaultTag -> Vault -> m (Either WikiError StoredVault)
  storeVault tag = lift . storeVault tag

  -- | Store a new version of the vault as 'storeVault' does, with the
  -- public half of the key pair its @identity@ item holds when that item
  -- is new.
  storeVaultChange :: Maybe VaultTag -> VaultChange -> m (Either WikiError StoredVault)
  default storeVaultChange :: (MonadTrans t, m ~ t n, MonadVault n) => Maybe VaultTag -> VaultChange -> m (Either WikiError StoredVault)
  storeVaultChange tag = lift . storeVaultChange tag

  -- | A user's current public key in the server's directory, if they have
  -- one.
  publicKeyOf :: UserId -> m (Either WikiError (Maybe PublicKeyEntry))
  default publicKeyOf :: (MonadTrans t, m ~ t n, MonadVault n) => UserId -> m (Either WikiError (Maybe PublicKeyEntry))
  publicKeyOf = lift . publicKeyOf

  -- | Turn the assistant's synchronisation off: the vault's settings go,
  -- and the vault with them when it holds nothing else.
  deleteVault :: m (Either WikiError ())
  default deleteVault :: (MonadTrans t, m ~ t n, MonadVault n) => m (Either WikiError ())
  deleteVault = lift deleteVault

  -- | The user's passkeys.
  listPasskeys :: m (Either WikiError [PasskeyInfo])
  default listPasskeys :: (MonadTrans t, m ~ t n, MonadVault n) => m (Either WikiError [PasskeyInfo])
  listPasskeys = lift listPasskeys

  -- | The options of a new passkey, with the server's challenge.
  beginRegistration :: Maybe Password -> m (Either SignInError RegistrationOptions)
  default beginRegistration :: (MonadTrans t, m ~ t n, MonadVault n) => Maybe Password -> m (Either SignInError RegistrationOptions)
  beginRegistration = lift . beginRegistration

  -- | The browser's answer to a registration, which the server checks and
  -- stores.
  finishRegistration :: RegistrationResponse -> m (Either WikiError PasskeyInfo)
  default finishRegistration :: (MonadTrans t, m ~ t n, MonadVault n) => RegistrationResponse -> m (Either WikiError PasskeyInfo)
  finishRegistration = lift . finishRegistration

  -- | The options of a use of one of the user's passkeys, with the
  -- server's challenge.
  beginAssertion :: m (Either WikiError AssertionOptions)
  default beginAssertion :: (MonadTrans t, m ~ t n, MonadVault n) => m (Either WikiError AssertionOptions)
  beginAssertion = lift beginAssertion

  -- | The browser's answer to an assertion, which the server checks.
  finishAssertion :: AssertionResponse -> m (Either WikiError PasskeyInfo)
  default finishAssertion :: (MonadTrans t, m ~ t n, MonadVault n) => AssertionResponse -> m (Either WikiError PasskeyInfo)
  finishAssertion = lift . finishAssertion

  -- | Remove one of the user's passkeys, with its copy of the vault key.
  -- The result is the passkeys left.
  dropPasskey :: CredentialId -> m (Either WikiError [PasskeyInfo])
  default dropPasskey :: (MonadTrans t, m ~ t n, MonadVault n) => CredentialId -> m (Either WikiError [PasskeyInfo])
  dropPasskey = lift . dropPasskey

  -- | The key OPAQUE's export key gave at the last sign-in with a password
  -- (or a registration that signed in), if any: the page's, taken once,
  -- for the user's vault ("Wiki.App.Vault"), which forgets it.
  takePasswordKey :: m (Maybe KeyRef)
  default takePasswordKey :: (MonadTrans t, m ~ t n, MonadVault n) => m (Maybe KeyRef)
  takePasswordKey = lift takePasswordKey

  -- | Prove the signed-in user's password again (a re-authentication: no
  -- new session), for a change that needs it: the key the export key gives
  -- the page, when the proof went through OPAQUE in a browser. The proof
  -- left on the session lets one change go without the password.
  provePassword :: Password -> m (Either SignInError (Maybe KeyRef))
  provePassword = provePasswordBy pageKeys

  -- | 'provePassword', with the key held under this owner ('KeyOwner').
  provePasswordBy :: KeyOwner -> Password -> m (Either SignInError (Maybe KeyRef))
  default provePasswordBy :: (MonadTrans t, m ~ t n, MonadVault n) => KeyOwner -> Password -> m (Either SignInError (Maybe KeyRef))
  provePasswordBy o = lift . provePasswordBy o

  -- | Change the signed-in user's password and carry their vault along:
  -- the vault key, which the page holds and can export, is wrapped with
  -- the new password's key and stored with the new password, both or
  -- neither.
  changePasswordCarrying :: UserId -> PasswordChange -> Rewrap -> m (Either SignInError SignInMethods)
  default changePasswordCarrying :: (MonadTrans t, m ~ t n, MonadVault n) => UserId -> PasswordChange -> Rewrap -> m (Either SignInError SignInMethods)
  changePasswordCarrying u c = lift . changePasswordCarrying u c

-- | 'Rewrap': what carrying the vault over to a new password needs: the
-- version of the vault the change is made to, its salt, and its key, held
-- by the page, which can export it for this.
data Rewrap = Rewrap
  { tag :: VaultTag
  , salt :: Salt
  , vaultKey :: KeyRef
  }

-- | A component's handlers ask through its monad.
-- Each method runs in 'H.async': the handler waits for it, and the rest
-- of the tree goes on meanwhile.
instance (MonadVault m) => MonadVault (HalogenM st act slots output m) where
  vaultSync = H.async vaultSync
  fetchVault = H.async . fetchVault
  storeVault tag = H.async . storeVault tag
  storeVaultChange tag = H.async . storeVaultChange tag
  publicKeyOf = H.async . publicKeyOf
  deleteVault = H.async deleteVault
  listPasskeys = H.async listPasskeys
  beginRegistration = H.async . beginRegistration
  finishRegistration = H.async . finishRegistration
  beginAssertion = H.async beginAssertion
  finishAssertion = H.async . finishAssertion
  dropPasskey = H.async . dropPasskey
  takePasswordKey = H.async takePasswordKey
  provePasswordBy o = H.async . provePasswordBy o
  changePasswordCarrying u c = H.async . changePasswordCarrying u c

instance (MonadVault m) => MonadVault (ReaderT r m)

-- | The demo's instance: no server, so no vault to sync, and every request
-- of one refused, saying why.
instance (Monad m) => MonadVault (MemWiki m) where
  vaultSync = pure False
  fetchVault _ = pure (Right NoVault)
  storeVault _ _ = pure (Left unavailable)
  storeVaultChange _ _ = pure (Left unavailable)
  publicKeyOf _ = pure (Right Nothing)
  deleteVault = pure (Right ())
  listPasskeys = pure (Right [])
  beginRegistration _ = pure (Left (SignInFailed unavailable))
  finishRegistration _ = pure (Left unavailable)
  beginAssertion = pure (Left unavailable)
  finishAssertion _ = pure (Left unavailable)
  dropPasskey _ = pure (Left unavailable)
  takePasswordKey = pure Nothing
  provePasswordBy _ _ = pure (Left (SignInFailed unavailable))
  changePasswordCarrying _ _ _ = pure (Left (SignInFailed unavailable))

-- | The browser's DOM alone has no server either: what runs in it
-- directly (the browser tests' own reads of the settings) keeps the keys
-- in this browser, as the demo does.
instance MonadVault BrowserDOM where
  vaultSync = pure False
  fetchVault _ = pure (Right NoVault)
  storeVault _ _ = pure (Left unavailable)
  storeVaultChange _ _ = pure (Left unavailable)
  publicKeyOf _ = pure (Right Nothing)
  deleteVault = pure (Right ())
  listPasskeys = pure (Right [])
  beginRegistration _ = pure (Left (SignInFailed unavailable))
  finishRegistration _ = pure (Left unavailable)
  beginAssertion = pure (Left unavailable)
  finishAssertion _ = pure (Left unavailable)
  dropPasskey _ = pure (Left unavailable)
  takePasswordKey = pure Nothing
  provePasswordBy _ _ = pure (Left (SignInFailed unavailable))
  changePasswordCarrying _ _ _ = pure (Left (SignInFailed unavailable))

-- | Nor has the in-memory document of the prerender and the native tests.
instance MonadVault MemDOM where
  vaultSync = pure False
  fetchVault _ = pure (Right NoVault)
  storeVault _ _ = pure (Left unavailable)
  storeVaultChange _ _ = pure (Left unavailable)
  publicKeyOf _ = pure (Right Nothing)
  deleteVault = pure (Right ())
  listPasskeys = pure (Right [])
  beginRegistration _ = pure (Left (SignInFailed unavailable))
  finishRegistration _ = pure (Left unavailable)
  beginAssertion = pure (Left unavailable)
  finishAssertion _ = pure (Left unavailable)
  dropPasskey _ = pure (Left unavailable)
  takePasswordKey = pure Nothing
  provePasswordBy _ _ = pure (Left (SignInFailed unavailable))
  changePasswordCarrying _ _ _ = pure (Left (SignInFailed unavailable))

-- | The demo's refusal: syncing the keys is a server's.
unavailable :: WikiError
unavailable = Invalid "This demo has no server: the assistant's keys stay in this browser."

-- | 'Owned' runs an operation of the vault with every key it registers in
-- the page under one owner ('Wiki.App.Assistant.KeyOwner'): the methods
-- that make a key, keep one on this device or prove the password, whatever
-- owner they are given, are given this one. A settings' section runs each
-- operation of its under an owner of its own ('owned'), and lets the owner
-- go when the operation ends or the section closes: what the operation
-- left in the page then goes, even when the close killed its worker.
newtype Owned m a = Owned (ReaderT KeyOwner m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

instance MonadTrans Owned where
  lift = Owned . lift

-- | Run an operation with its keys under this owner.
owned :: KeyOwner -> Owned m a -> m a
owned o (Owned r) = runReaderT r o

-- | The owner the operation runs under.
owner :: (Monad m) => Owned m KeyOwner
owner = Owned ask

instance (MonadAssistant m) => MonadAssistant (Owned m) where
  newVaultKeyBy _ exportable = owner >>= \o -> lift (newVaultKeyBy o exportable)
  keepDeviceKeyBy _ user key onDisk = owner >>= \o -> lift (keepDeviceKeyBy o user key onDisk)
  deviceKeyBy _ user = owner >>= \o -> lift (deviceKeyBy o user)
  createPasskeyBy _ options salt info = owner >>= \o -> lift (createPasskeyBy o options salt info)
  usePasskeyBy _ options salt info = owner >>= \o -> lift (usePasskeyBy o options salt info)
  unwrapWithBy _ wrapping wrapped info exportable = owner >>= \o -> lift (unwrapWithBy o wrapping wrapped info exportable)
  newIdentityBy _ key = owner >>= \o -> lift (newIdentityBy o key)
  openIdentityBy _ key keyId sealed = owner >>= \o -> lift (openIdentityBy o key keyId sealed)
  passwordKeyBy _ key salt = owner >>= \o -> lift (passwordKeyBy o key salt)

instance (MonadVault m) => MonadVault (Owned m) where
  provePasswordBy _ password = owner >>= \o -> lift (provePasswordBy o password)

-- | An operation of a user's vault, waited for in a component's program,
-- with the keys it registers in the page under an owner of its own
-- ('Wiki.App.Assistant.KeyOwner'). The owner is made, and kept in the
-- component's state (the set the two functions read and write), in one
-- step before the wait, and let go after it. The component lets go of
-- the owners in its state when it closes ('releaseOwners'), which kills
-- the operation's worker: what the operation left in the page goes then.
owning :: (MonadIO m) => (st -> Set KeyOwner) -> (Set KeyOwner -> st -> st) -> UserId -> Owned m a -> HalogenM st act slots output m a
owning owners setOwners u act = do
  o <- H.sync (liftIO (openKeyOwner u))
  modify (\st -> setOwners (Set.insert o (owners st)) st)
  -- Let go however the operation ends, a failure included; a close
  -- kills the program instead, and 'releaseOwners' lets go then.
  H.async (owned o act)
    `H.finally` (H.sync (liftIO (closeKeyOwner o)) >> modify (\st -> setOwners (Set.delete o (owners st)) st))

-- | Let go of the owners of keys in a component's state, as it closes:
-- in this step, with the keys of the operations the close cuts off.
releaseOwners :: (MonadIO m) => (st -> Set KeyOwner) -> (Set KeyOwner -> st -> st) -> HalogenM st act slots output m ()
releaseOwners owners setOwners = do
  gets owners >>= traverse_ (H.sync . liftIO . closeKeyOwner)
  modify (setOwners Set.empty)
