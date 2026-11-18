-- | The writing assistant's settings as a device keeps them
-- ("Wiki.App.Assistant", "Wiki.App.Assistant.Keys"), over the in-memory
-- document the native tests have. Its IndexedDB behaves as the browser's:
-- each user's record apart, under their id, in a database of the
-- assistant's own, deleted on request. It has no WebCrypto, as a page
-- served over plain HTTP has none: there the settings are stored as text,
-- as before the vault, and sealed ones cannot be opened. The settings
-- sealed, per user, forgotten, synced and unlocked are the browser tests'
-- (Test.Browser.Assistant, Test.Browser.Vault).
module Test.Assistant (spec) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as BSL
import Data.IORef (IORef, atomicModifyIORef', newIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime (..), fromGregorian)
import GHC.Conc (BlockReason (..), ThreadStatus (..), threadStatus)
import Halogen.VDom.DOM.Monad (MemDOM, runMemDOM)
import Protolude
import Test.Hspec
import Web.Storage.IndexedDB (Database (..))
import Web.Storage.IndexedDB qualified as IDB
import Wiki.Api.Vault (StoredVault (..))
import Wiki.App.Assistant (MonadAssistant (..), assistantDatabase, readRecord, writeRecord)
import Wiki.App.Assistant.Keys (Keys (..), changeKeys, readKeys)
import Wiki.App.Assistant.Sync (Fetched (..), MonadVault (..))
import Wiki.App.Vault (VaultDevice (..), readDevice, setVaultAskEachTime, settleKeys)
import Wiki.Core.Assistant
  ( AssistantError (..)
  , ErrorKind (..)
  , Listed (..)
  , ModelList (..)
  , ModelName (..)
  , Provider (..)
  , ProviderSettings (..)
  , defaultSettings
  , readyFor
  , settingsOf
  , withKey
  , withModels
  , withReached
  )
import Wiki.Core.Assistant.Vault (emptyRecord)
import Wiki.Core.Id (UserId (..))
import Wiki.Core.Vault (Item (..), KeyId (..), Sealed (..), Vault (..), VaultTag (..), identityItem, vaultFormat)

spec :: Spec
spec = do
  settings
  device

settings :: Spec
settings = describe "Wiki.App.Assistant's settings" $ do
  let db = Database {name = "wiki-assistant-test", version = 1, stores = ["settings"]}
      fresh = run (IDB.deleteDatabase db.name)
      alice = UserId "user-alice"
      bob = UserId "user-bob"
      withKeys = withKey OpenAI (Just "test-key-open") (withKey Anthropic (Just "test-key-ant") defaultSettings)

  it "keeps each user's record apart, under their id, and deletes it on request" $ do
    fresh
    run (readRecord db alice) `shouldReturn` Nothing
    run (writeRecord db alice (Just "alice's"))
    run (writeRecord db bob (Just "bob's"))
    run (readRecord db alice) `shouldReturn` Just "alice's"
    sort <$> run (IDB.keys db "settings") `shouldReturn` ["user-alice", "user-bob"]
    run (writeRecord db alice Nothing)
    run (IDB.keys db "settings") `shouldReturn` ["user-bob"]

  it "reads the defaults where nothing is kept, in a database of the assistant's own" $ do
    run (IDB.deleteDatabase assistantDatabase.name)
    assistantDatabase.name `shouldBe` "wiki-assistant"
    fmap (\k -> (k.settings, k.locked, k.synced)) <$> run (readKeys alice) `shouldReturn` Right (defaultSettings, False, False)

  it "keeps the settings as text where it cannot seal them, as before the vault" $ do
    run (IDB.deleteDatabase assistantDatabase.name)
    run (sealingHere) `shouldReturn` False
    fmap (\k -> (k.settings, k.locked, k.synced)) <$> run (changeKeys alice (const withKeys)) `shouldReturn` Right (withKeys, False, False)
    run (deviceRecord alice) `shouldReturn` Right (Just (BSL.toStrict (encode withKeys)))
    fmap (\k -> k.settings) <$> run (readKeys alice) `shouldReturn` Right withKeys
    -- Nothing set: nothing kept.
    _ <- run (changeKeys alice (const defaultSettings))
    run (deviceRecord alice) `shouldReturn` Right Nothing

  it "cannot open settings a browser sealed, and leaves them as they are" $ do
    run (IDB.deleteDatabase assistantDatabase.name)
    let sealed = BSL.toStrict (encode emptyRecord)
    run (keepDeviceRecord alice (Just sealed)) `shouldReturn` Right ()
    run (readKeys alice) >>= (`shouldSatisfy` isLeft)
    run (changeKeys alice (const withKeys)) >>= (`shouldSatisfy` isLeft)
    run (deviceRecord alice) `shouldReturn` Right (Just sealed)

  -- The editor checks each provider with a key in a fork of its own, and
  -- each check keeps what it learnt with changeKeys: whether the provider
  -- could be reached, its list of models. Two checks ran at once. Each
  -- read the record, waited, and wrote the record it had read with its
  -- own change, so the second write put back what the first had changed:
  -- a provider shown dimmed was kept as reachable. Here the second change
  -- starts between the first one's read and its write, in that order
  -- every time: it either runs to its end there (and the first change's
  -- write then takes its change away) or waits for the first change to
  -- end.
  it "keeps both of two changes, the second started between the first one's read and its write" $ do
    run (IDB.deleteDatabase assistantDatabase.name)
    beforeWrite <- newIORef Nothing
    secondDone <- newEmptyMVar
    let hooked :: Hooked MemDOM a -> IO a
        hooked (Hooked m) = runMemDOM (runReaderT m beforeWrite)
        listed = [Listed {id = ModelName "scripted", label = Nothing, created = Nothing}]
        at = UTCTime (fromGregorian 2026 11 18) 0
    _ <- run (changeKeys alice (const withKeys))
    writeIORef beforeWrite . Just $ do
      thread <- forkIO (hooked (changeKeys alice (withModels Scripted listed at)) >>= putMVar secondDone)
      untilFinishedOrWaiting thread
    unreached <- hooked (changeKeys alice (withReached Anthropic False))
    learnt <- takeMVar secondDone
    void unreached `shouldBe` Right ()
    void learnt `shouldBe` Right ()
    Right kept <- fmap (\k -> k.settings) <$> run (readKeys alice)
    (settingsOf Anthropic kept).unreachable `shouldBe` True
    (\l -> l.listed) <$> (settingsOf Scripted kept).fetched `shouldBe` Just listed

  it "reaches no provider in the in-memory document" $
    case readyFor Anthropic withKeys of
      Left why -> expectationFailure (toS why)
      Right r -> (either (\e -> Just e.kind) (const Nothing) <$> run (listModels r)) `shouldReturn` Just Unreachable
  where
    run :: MemDOM a -> IO a
    run = runMemDOM

-- | What this device records of the user's vault ("Wiki.App.Vault"'s
-- 'VaultDevice'): which key pair it holds, whether it asks for an unlock
-- each time, whether it offered the password. Each function that changes
-- it reads it, waits, and writes back what it read with its own change.
device :: Spec
device = describe "Wiki.App.Vault's record of this device" $
  -- A sign-in settles the keys (settleKeys, as afterSignIn does): it reads
  -- the record, fetches the vault, unseals the private key, and records
  -- the key pair it holds. The security panel's "Ask each time" changes
  -- the record meanwhile. The settling then wrote the record it had read,
  -- with the key pair, and "Ask each time" was off again: the vault key
  -- stayed on disk. Here "Ask each time" starts between the settling's
  -- read and its write, every time, and either runs to its end there or
  -- waits for the settling to end.
  it "keeps Ask each time set while a sign-in's settling of the keys is under way" $ do
    let carol = UserId "user-carol-device"
        keyId = KeyId "test-key-pair"
        stored =
          StoredVault
            { tag = VaultTag "1"
            , vault =
                Vault
                  { format = vaultFormat
                  , salt = "salt"
                  , items = Map.singleton identityItem Item {keyId = Just keyId, sealed = Sealed {nonce = "nonce", ciphertext = "sealed"}}
                  , wraps = mempty
                  }
            }
    beforeWrite <- newIORef Nothing
    askedDone <- newEmptyMVar
    let unlocked :: Unlocked (Hooked MemDOM) a -> IO a
        unlocked (Unlocked m) = let Hooked h = runReaderT m stored in runMemDOM (runReaderT h beforeWrite)
    writeIORef beforeWrite . Just $ do
      thread <- forkIO (unlocked (setVaultAskEachTime carol True) >>= putMVar askedDone)
      untilFinishedOrWaiting thread
    settled <- unlocked (settleKeys carol)
    asked <- takeMVar askedDone
    void settled `shouldBe` Right ()
    void asked `shouldBe` Right ()
    recorded <- unlocked (readDevice carol)
    (recorded.held, recorded.askEachTime) `shouldBe` (Just keyId, True)

-- | A monad with an action run before its next write of a device record
-- ('keepDeviceRecord'), once: where a test puts another change between a
-- change's read and its write. Everything else is the monad's below.
newtype Hooked m a = Hooked (ReaderT (IORef (Maybe (IO ()))) m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

instance MonadTrans Hooked where
  lift = Hooked . lift

instance (MonadAssistant m, MonadIO m) => MonadAssistant (Hooked m) where
  keepDeviceRecord user record = do
    action <- Hooked ask >>= \ref -> liftIO (atomicModifyIORef' ref (Nothing,))
    liftIO (sequence_ action)
    lift (keepDeviceRecord user record)

instance (MonadVault m) => MonadVault (Hooked m)

-- | A browser's unlocked vault, as far as the record of the device needs
-- one: the server's vault is the one given, every slot holds a key, and
-- every key opens the user's key pair and is kept. The in-memory document
-- has no WebCrypto to do these.
newtype Unlocked m a = Unlocked (ReaderT StoredVault m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

instance MonadTrans Unlocked where
  lift = Unlocked . lift

instance (MonadAssistant m) => MonadAssistant (Unlocked m) where
  deviceKey _ = Just <$> lift (newVaultKey False)
  openIdentity _ _ _ = Right <$> lift (newVaultKey False)
  keepDeviceKey _ _ _ = pure (Right ())

instance (MonadVault m) => MonadVault (Unlocked m) where
  fetchVault _ = Unlocked (asks (Right . Fetched))

-- | Wait until a thread has finished, or waits for an MVar: a change of
-- the keys that waits for another to end. Nothing else in the in-memory
-- document's IndexedDB waits for an MVar.
untilFinishedOrWaiting :: ThreadId -> IO ()
untilFinishedOrWaiting thread =
  threadStatus thread >>= \case
    ThreadFinished -> pass
    ThreadDied -> pass
    ThreadBlocked BlockedOnMVar -> pass
    _ -> yield >> untilFinishedOrWaiting thread
