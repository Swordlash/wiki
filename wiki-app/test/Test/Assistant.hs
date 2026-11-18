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
import Data.Time (UTCTime (..), fromGregorian)
import GHC.Conc (BlockReason (..), ThreadStatus (..), threadStatus)
import Halogen.VDom.DOM.Monad (MemDOM, runMemDOM)
import Protolude
import Test.Hspec
import Web.Storage.IndexedDB (Database (..))
import Web.Storage.IndexedDB qualified as IDB
import Wiki.App.Assistant (MonadAssistant (..), assistantDatabase, readRecord, writeRecord)
import Wiki.App.Assistant.Keys (Keys (..), changeKeys, readKeys)
import Wiki.App.Assistant.Sync (MonadVault)
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

spec :: Spec
spec = describe "Wiki.App.Assistant's settings" $ do
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
