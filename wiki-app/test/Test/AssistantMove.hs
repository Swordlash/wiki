-- | The move of the writing assistant's legacy settings to the plugin's
-- places ("Wiki.App.Assistant.Migrate"), run against a model of the
-- server's vault (@Test.VaultServer@) on browsers whose cryptography is
-- faked (@Test.FakeVault@). Each legacy state a browser can be in is
-- moved: L1 (it follows the vault, whose key may be in the assistant's
-- slot only), L2 (sealed with the assistant's own key), L3 (as text, where
-- the page cannot seal) and L4 (it follows a vault locked here, with what
-- was typed meanwhile). A run stopped at any write and run again ends as
-- one never stopped; two browsers moving at once keep every key; an
-- earlier tab that adds the vault's legacy item again has it moved again,
-- the plugin's keys winning. A run that the reader's sign-out overtakes
-- writes nothing more in this browser.
module Test.AssistantMove (spec) where

import Data.Aeson (Value, decodeStrict, encode)
import Data.ByteString.Lazy qualified as BSL
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Halogen.VDom.DOM.Monad (MemDOM, runMemDOM)
import Protolude hiding (on)
import Test.FakeVault
import Test.Hspec hiding (after, before)
import Test.VaultServer
import Wiki.Api.Plugins (PluginId (..))
import Wiki.App.Assistant (KeyOwner (..), pageKeys)
import Wiki.App.Assistant.Keys (recordSource)
import Wiki.App.Assistant.Migrate (Outcome (..), migrateAssistant, migrateAssistantHeld)
import Wiki.App.Assistant.Sync (owned)
import Wiki.App.Plugin (Manifest (..), Unavailable (..))
import Wiki.App.Plugin.Host.Secrets (DeviceEntry (..), deviceSecretsSlot, heldSlot, localSlot, putSecret, readSecrets, removeSecret)
import Wiki.App.Plugin.Registry (assistantId, manifests)
import Wiki.App.Vault (VaultDevice (..), vaultSlot)
import Wiki.Core.Assistant (Mode (..), Provider (..), Settings (..), defaultSettings, withKey, withMode, withModel, withReached)
import Wiki.Core.Assistant.Split (keysOf, learntValue, optionsValue)
import Wiki.Core.Assistant.Vault (DeviceRecord (..), ServerCopy (..), assistantItem, learntOf, sealingData, secretsOf)
import Wiki.Core.Id (UserId (..))
import Wiki.Core.Vault (Item (..), ItemName (..), Sealed, Vault (..), VaultTag (..), itemOf, withItem)
import Wiki.Core.Vault.Plugin (Contents (..), SecretName (..), decodeSecrets, pluginItem, pluginSealingData)

spec :: Spec
spec = describe "Wiki.App.Assistant.Migrate" $ do
  states
  stopped
  signedOut
  together

alice :: UserId
alice = UserId "user-alice"

-- | The writing assistant's manifest, as the registry has it.
assistant :: Manifest
assistant = fromMaybe (panic "no assistant") (head (filter (\x -> x.id == assistantId) manifests))

-- | Settings with a key for each of two providers, as a device had them.
synced, ownHere, typedHere :: Settings
synced = withMode Ask (withModel OpenAI "gpt-from-the-vault" (withKey OpenAI (Just "sk-open-vault") (withKey Anthropic (Just "sk-ant-vault") defaultSettings)))
ownHere = withReached Anthropic False (withModel Anthropic "claude-own" (withKey Anthropic (Just "sk-ant-own") defaultSettings))
typedHere = withKey OpenAI (Just "sk-open-typed") (withKey Scripted (Just "typed-scripted") defaultSettings)

-- | 'World' is one test's server, keys' counter and vault key.
data World = World
  { server :: IORef ServerState
  , counter :: IORef Int
  , vaultKey :: Int
  }

newWorld :: IO World
newWorld = do
  counter <- newIORef 0
  k <- atomicModifyIORef' counter (\n -> (n + 1, n + 1))
  server <- newServer (Just (userVault k))
  pure World {server, counter, vaultKey = k}

newKey :: World -> IO Int
newKey w = atomicModifyIORef' w.counter (\n -> (n + 1, n + 1))

on :: World -> Device -> Served (Fake MemDOM) a -> IO a
on w d act = runMemDOM (onDevice d (served w.server act))

vaultNow :: World -> IO Vault
vaultNow w = serverState w.server >>= maybe (panic "no vault") (pure . snd) . (\s -> s.kept)

-- | The vault's legacy item, sealed with the vault key, holding these
-- settings: what a browser that synced stored.
withLegacyItem :: World -> Settings -> IO ()
withLegacyItem w s = vaultNow w >>= setVault w.server . withItem assistantItem Item {keyId = Nothing, sealed = sealedSettings w.vaultKey s}

sealedSettings :: Int -> Settings -> Sealed
sealedSettings k s = fakeSeal k sealingData (decodeUtf8 (BSL.toStrict (encode (secretsOf s))))

-- | A record that follows the vault as it is now, its copy sealed with
-- the vault key; with what was typed here, sealed with another key, for
-- an L4 record.
followingRecord :: World -> Maybe (Int, Settings) -> Bool -> Settings -> IO ByteString
followingRecord w typed asking learnt = do
  stored <- serverState w.server
  let (r, v) = fromMaybe (panic "no vault") stored.kept
      copy = ServerCopy {tag = VaultTag (show r), vault = v}
      sealed = case typed of
        Just (k, s) -> Just (fakeSeal k sealingData (decodeUtf8 (BSL.toStrict (encode (secretsOf s)))))
        Nothing -> (\i -> i.sealed) <$> itemOf assistantItem v
  pure (BSL.toStrict (encode DeviceRecord {sealed, server = Just copy, learnt = learntOf learnt, askEachTime = asking}))

-- | A record of a browser that did not sync: sealed with its own key.
ownRecord :: Int -> Settings -> ByteString
ownRecord k s = BSL.toStrict (encode DeviceRecord {sealed = Just (fakeSeal k sealingData (decodeUtf8 (BSL.toStrict (encode (secretsOf s))))), server = Nothing, learnt = learntOf s, askEachTime = False})

putRecord :: Device -> UserId -> ByteString -> IO ()
putRecord d u bytes = modifyIORef' d.records (Map.insert u.text bytes)

putKey :: Device -> UserId -> Int -> IO ()
putKey d u k = modifyIORef' d.keys (Map.insert u.text k)

-- | The assistant's secrets in the vault's plugin item.
pluginKeys :: World -> IO (Map SecretName Text)
pluginKeys w = do
  v <- vaultNow w
  pure . fromMaybe mempty $ do
    name <- pluginItem "assistant"
    i <- itemOf name v
    rightToMaybe (fakeOpen w.vaultKey (pluginSealingData "assistant") i.sealed >>= decodeSecrets)

hasPluginItem :: World -> IO Bool
hasPluginItem w = (\v -> isJust (pluginItem "assistant" >>= (`itemOf` v))) <$> vaultNow w

hasLegacyItem :: World -> IO Bool
hasLegacyItem w = isJust . itemOf assistantItem <$> vaultNow w

-- | What is left on a browser: its record, its assistant's key, its
-- vault's key, its cache, and its vault's "ask each time".
data Left' = Left'
  { record :: Bool
  , assistantKey :: Bool
  , vaultKeyHere :: Maybe Int
  , cache :: Maybe Value
  , askEachTime :: Bool
  }
  deriving stock (Eq, Show)

leftOn :: Device -> IO Left'
leftOn d = do
  records <- readIORef d.records
  keys <- readIORef d.keys
  pure
    Left'
      { record = Map.member alice.text records
      , assistantKey = Map.member alice.text keys
      , vaultKeyHere = Map.lookup (vaultSlot alice).text keys
      , cache = decodeStrict =<< Map.lookup (localSlot alice (PluginId "assistant")).text records
      , askEachTime = maybe False (\v -> v.askEachTime) (decodeStrict @VaultDevice =<< Map.lookup (vaultSlot alice).text records)
      }

-- | Run the move to its end on a browser: options adopted as the plugins'
-- state does, when the server holds none, and the move run again.
moveAll :: World -> Device -> IORef (Maybe Value) -> IO (Either Text Outcome)
moveAll w d options = go (4 :: Int)
  where
    go 0 = pure (Left "the move did not end")
    go n = do
      known <- readIORef options
      on w d (migrateAssistant pageKeys alice True (Just known)) >>= \case
        Right (Adopt v) -> modifyIORef' options (<|> Just v) >> go (n - 1)
        other -> pure other

states :: Spec
states = describe "moves each legacy state" $ do
  it "L2: the keys into the vault, the options to adopt, then the legacy places emptied once the server holds the options" $ do
    w <- newWorld
    d <- newDevice w.counter True
    withVaultKey d alice w.vaultKey
    own <- newKey w
    putKey d alice own
    putRecord d alice (ownRecord own ownHere)
    first' <- on w d (migrateAssistant pageKeys alice True (Just Nothing))
    first' `shouldBe` Right (Adopt (optionsValue ownHere))
    -- Nothing is deleted before the options are the server's.
    pluginKeys w `shouldReturn` keysOf ownHere
    (\l -> l.record) <$> leftOn d `shouldReturn` True
    on w d (migrateAssistant pageKeys alice True (Just (Just (optionsValue ownHere)))) `shouldReturn` Right Moved
    after <- leftOn d
    after `shouldBe` Left' {record = False, assistantKey = False, vaultKeyHere = Just w.vaultKey, cache = Just (learntValue ownHere), askEachTime = False}
    -- Run again, nothing is left to do.
    on w d (migrateAssistant pageKeys alice True (Just (Just (optionsValue ownHere)))) `shouldReturn` Right Moved

  it "L1: the vault's item, from a browser whose vault key is in the assistant's slot only, which asked each time" $ do
    w <- newWorld
    withLegacyItem w synced
    d <- newDevice w.counter True
    putKey d alice w.vaultKey
    followingRecord w Nothing True synced >>= putRecord d alice
    options <- newIORef Nothing
    moveAll w d options `shouldReturn` Right Moved
    pluginKeys w `shouldReturn` keysOf synced
    hasLegacyItem w `shouldReturn` False
    readIORef options `shouldReturn` Just (optionsValue synced)
    after <- leftOn d
    after `shouldBe` Left' {record = False, assistantKey = False, vaultKeyHere = Just w.vaultKey, cache = Nothing, askEachTime = True}

  it "L1 after a reload on \"ask each time\", unlocked through the vault's slot: the record is the vault's, and goes" $ do
    w <- newWorld
    withLegacyItem w synced
    d <- newDevice w.counter True
    -- The assistant's slot emptied by the reload; the vault's slot holds
    -- the key once unlocked again.
    withVaultKey d alice w.vaultKey
    followingRecord w Nothing True synced >>= putRecord d alice
    options <- newIORef Nothing
    moveAll w d options `shouldReturn` Right Moved
    pluginKeys w `shouldReturn` keysOf synced
    hasLegacyItem w `shouldReturn` False
    after <- leftOn d
    (after.record, after.askEachTime) `shouldBe` (False, True)

  it "L4: waits while the vault is locked, then takes the vault's keys and those typed here for the names the vault lacks" $ do
    w <- newWorld
    withLegacyItem w synced
    d <- newDevice w.counter True
    own <- newKey w
    putKey d alice own
    followingRecord w (Just (own, typedHere)) False defaultSettings >>= putRecord d alice
    on w d (migrateAssistant pageKeys alice True (Just Nothing)) `shouldReturn` Right Waiting
    pluginKeys w `shouldReturn` mempty
    (\l -> l.record) <$> leftOn d `shouldReturn` True
    hasLegacyItem w `shouldReturn` True
    -- Unlocked (by a passkey, in the Security section).
    withVaultKey d alice w.vaultKey
    options <- newIORef Nothing
    moveAll w d options `shouldReturn` Right Moved
    pluginKeys w `shouldReturn` Map.union (keysOf synced) (keysOf typedHere)
    pluginKeys w >>= (`shouldBe` Map.lookup (SecretName "openai") (keysOf synced)) . Map.lookup (SecretName "openai")
    readIORef options `shouldReturn` Just (optionsValue synced)
    hasLegacyItem w `shouldReturn` False
    (\l -> (l.record, l.assistantKey)) <$> leftOn d `shouldReturn` (False, False)

  it "L3: settings kept as text, where the page cannot seal, stay in this browser as text" $ do
    w <- newWorld
    d <- newDevice w.counter False
    let record = BSL.toStrict (encode ownHere)
    putRecord d alice record
    options <- newIORef Nothing
    moveAll w d options `shouldReturn` Right Moved
    store <- readIORef d.records
    -- The record's keys, and the record as the place they were taken from.
    (decodeStrict =<< Map.lookup (deviceSecretsSlot alice).text store) `shouldBe` Just (Map.singleton (ItemName "plugin-assistant") (PlainEntry Contents {secrets = keysOf ownHere, taken = Set.singleton (recordSource record)}))
    (\l -> l.record) <$> leftOn d `shouldReturn` False
    readIORef options `shouldReturn` Just (optionsValue ownHere)

  it "L3 with a legacy item in the vault: the options adopted without waiting for the item, which a browser that can open it moves later, keeping them" $ do
    w <- newWorld
    withLegacyItem w synced
    d <- newDevice w.counter False
    putRecord d alice (BSL.toStrict (encode ownHere))
    options <- newIORef Nothing
    -- The item is left: this page can never open it.
    moveAll w d options `shouldReturn` Right Waiting
    readIORef options `shouldReturn` Just (optionsValue ownHere)
    (\l -> l.record) <$> leftOn d `shouldReturn` False
    hasLegacyItem w `shouldReturn` True
    e <- newDevice w.counter True
    withVaultKey e alice w.vaultKey
    moveAll w e options `shouldReturn` Right Moved
    readIORef options `shouldReturn` Just (optionsValue ownHere)
    hasLegacyItem w `shouldReturn` False
    pluginKeys w `shouldReturn` keysOf synced

  it "asks the server nothing where this browser keeps nothing legacy, unless told to look at the vault" $ do
    w <- newWorld
    withLegacyItem w synced
    d <- newDevice w.counter True
    withVaultKey d alice w.vaultKey
    before <- (\s -> s.fetches) <$> serverState w.server
    on w d (migrateAssistant pageKeys alice False (Just Nothing)) `shouldReturn` Right NothingHere
    (\s -> s.fetches) <$> serverState w.server `shouldReturn` before
    -- Shown able to fail: told to look, the run fetches the vault, and
    -- moves the legacy item another browser left.
    on w d (migrateAssistant pageKeys alice True (Just Nothing)) `shouldReturn` Right (Adopt (optionsValue synced))
    (\s -> s.fetches) <$> serverState w.server >>= (`shouldSatisfy` (> before))
    pluginKeys w `shouldReturn` keysOf synced

  it "leaves a record it cannot open where it is" $ do
    w <- newWorld
    d <- newDevice w.counter True
    withVaultKey d alice w.vaultKey
    -- Sealed with a key this browser no longer holds.
    gone <- newKey w
    putRecord d alice (ownRecord gone ownHere)
    on w d (migrateAssistant pageKeys alice True (Just Nothing)) `shouldReturn` Right Waiting
    (\l -> l.record) <$> leftOn d `shouldReturn` True

stopped :: Spec
stopped = describe "a run stopped" $ do
  -- Each write the run makes (a key or a record kept on the browser, a
  -- version of the vault stored) is a place it can be stopped: the k-th
  -- write throws, and the move is run again to its end.
  it "at any write and run again ends as a run never stopped" $ do
    reference <- moved Nothing
    for_ [0 .. 12] $ \k -> do
      end <- moved (Just k)
      unless (end == reference) (expectationFailure ("stopped at write " <> show k <> ": " <> show end <> " /= " <> show reference))
  where
    moved :: Maybe Int -> IO (Map SecretName Text, Bool, Left', Maybe Value)
    moved stopAt = do
      w <- newWorld
      withLegacyItem w synced
      d <- newDevice w.counter True
      -- L1, whose browser also kept a key of its own for what it typed
      -- before it synced: the vault key in the assistant's slot only.
      putKey d alice w.vaultKey
      followingRecord w Nothing True ownHere >>= putRecord d alice
      writes <- newIORef (0 :: Int)
      let hook = do
            n <- atomicModifyIORef' writes (\x -> (x + 1, x))
            when (Just n == stopAt) (throwIO (ErrorCall "stopped"))
      replicateM_ 40 (beforeNextWrite d hook >> beforeNextStore w.server hook)
      options <- newIORef Nothing
      _ <- try @SomeException (moveAll w d options)
      _ <- moveAll w d options
      (,,,) <$> pluginKeys w <*> hasLegacyItem w <*> leftOn d <*> readIORef options

signedOut :: Spec
signedOut = describe "a run the reader's sign-out overtakes" $ do
  -- The run goes under an owner of the reader's keys, as the shell runs
  -- it. At each write the run makes in this browser, the write is held,
  -- the reader signs out as the shell's sign-out does (the owner let go,
  -- then the reader's keys deleted), and the write is let go. What the
  -- browser holds right after the sign-out is what it holds once the run
  -- ends: nothing written after the sign-out stays, the vault key the run
  -- copies into the vault's slot included.
  it "writes nothing in this browser after the sign-out, at whichever of its writes the sign-out comes" $
    for_ [False, True] $ \asking -> do
      (total, _, reference) <- run asking Nothing
      total `shouldSatisfy` (> 3)
      -- The run keeps the vault key in the vault's slot when nobody signs
      -- out: a key the sign-out deletes is one the run would write.
      Map.member (vaultSlot alice).text (snd reference) `shouldBe` True
      for_ [0 .. total - 1] $ \k -> do
        (_, atSignOut, end) <- run asking (Just k)
        case atSignOut of
          Nothing -> expectationFailure ("no sign-out at write " <> show k)
          Just held -> unless (end == held) (expectationFailure ("asking each time " <> show asking <> ", signed out at write " <> show k <> ": " <> show end <> " /= " <> show held))

  -- The sign-out comes while the run keeps the vault key in the vault's
  -- slot, and deletes the user's keys; the key is not put back. On a
  -- browser that does not ask each time, that is the run's second write:
  -- its first is the copy of the vault's item its first read keeps.
  it "puts back no vault key that the sign-out deleted while the run was keeping it" $ do
    (_, _, reference) <- run False Nothing
    Map.member (vaultSlot alice).text (snd reference) `shouldBe` True
    (_, atSignOut, end) <- run False (Just 1)
    (snd <$> atSignOut) `shouldBe` Just mempty
    snd end `shouldBe` mempty

  -- A read of the vault keeps a copy of the plugin's item in this browser
  -- ('heldSlot'). The read of a field, or of the editor's assistant, goes
  -- under an owner of the reader's keys too: its answer, come after the
  -- sign-out, leaves no copy.
  it "leaves no copy of the vault's item when a read of the vault answers after the sign-out" $
    for_ [False, True] $ \signingOut -> do
      w <- newWorld
      d <- newDevice w.counter True
      withVaultKey d alice w.vaultKey
      let o = KeyOwner 1
      beforeNextWrite d (when signingOut (closeOwner d o))
      _ <- on w d (owned o (readSecrets alice assistant))
      held <- Map.member (heldSlot alice (ItemName "plugin-assistant")).text <$> readIORef d.records
      -- Shown able to fail: with no sign-out, the read keeps the copy.
      held `shouldBe` not signingOut
  where
    run :: Bool -> Maybe Int -> IO (Int, Maybe (Map Text ByteString, Map Text Int), (Map Text ByteString, Map Text Int))
    run asking signOutAt = do
      w <- newWorld
      withLegacyItem w synced
      d <- newDevice w.counter True
      -- L1, whose browser kept a key of its own for what it typed before
      -- it synced: the vault key in the assistant's slot only, which the
      -- run copies into the vault's slot; asking for an unlock each time
      -- or not.
      putKey d alice w.vaultKey
      followingRecord w Nothing asking ownHere >>= putRecord d alice
      writes <- newIORef (0 :: Int)
      atSignOut <- newIORef Nothing
      let o = KeyOwner 1
          held = (,) <$> readIORef d.records <*> readIORef d.keys
          hook = do
            n <- atomicModifyIORef' writes (\x -> (x + 1, x))
            when (Just n == signOutAt) $ do
              closeOwner d o
              modifyIORef' d.keys (Map.delete alice.text . Map.delete (vaultSlot alice).text)
              held >>= writeIORef atSignOut . Just
      replicateM_ 40 (beforeNextWrite d hook)
      _ <- on w d (migrateAssistant o alice True (Just (Just (optionsValue synced))))
      (,,) <$> readIORef writes <*> readIORef atSignOut <*> held

together :: Spec
together = describe "with other browsers and tabs" $ do
  it "two browsers moving at once keep every key: each name the vault's, or else the first browser's to keep it" $ do
    w <- newWorld
    withLegacyItem w synced
    a <- newDevice w.counter True
    b <- newDevice w.counter True
    putKey a alice w.vaultKey
    followingRecord w Nothing False synced >>= putRecord a alice
    withVaultKey b alice w.vaultKey
    own <- newKey w
    putKey b alice own
    putRecord b alice (ownRecord own (withKey Scripted (Just "b-scripted") ownHere))
    -- B's whole run happens between A's read and A's first store.
    bDone <- newIORef Nothing
    beforeNextStore w.server $ do
      r <- on w b (migrateAssistantHeld alice True (Just Nothing))
      modifyIORef' bDone (const (Just r))
    options <- newIORef Nothing
    moveAll w a options `shouldReturn` Right Moved
    readIORef bDone >>= (`shouldSatisfy` maybe False isRight)
    _ <- moveAll w b options
    keys <- pluginKeys w
    keys `shouldBe` Map.unions [keysOf synced, keysOf (withKey Scripted (Just "b-scripted") defaultSettings)]
    hasLegacyItem w `shouldReturn` False
    (\l -> l.record) <$> leftOn a `shouldReturn` False
    (\l -> l.record) <$> leftOn b `shouldReturn` False

  it "keeps a record an earlier tab writes between the move's read and its delete, and moves it next" $ do
    w <- newWorld
    d <- newDevice w.counter True
    withVaultKey d alice w.vaultKey
    own <- newKey w
    putKey d alice own
    putRecord d alice (ownRecord own ownHere)
    let later = withKey OpenAI (Just "sk-open-later") ownHere
    -- The first write on the browser is the cache's; the second, the
    -- record's delete, finds the record the earlier tab wrote just before.
    beforeNextWrite d pass
    beforeNextWrite d (putRecord d alice (ownRecord own later))
    on w d (migrateAssistant pageKeys alice True (Just (Just (optionsValue ownHere)))) `shouldReturn` Right Moved
    pluginKeys w `shouldReturn` keysOf later
    (\l -> l.record) <$> leftOn d `shouldReturn` False

  it "a key kept on one browser is another's once its vault is unlocked there, and a key changed on one is the other's at its next read" $ do
    w <- newWorld
    a <- newDevice w.counter True
    b <- newDevice w.counter True
    withVaultKey a alice w.vaultKey
    on w a (putSecret alice assistant (SecretName "anthropic") "sk-ant-on-a") `shouldReturn` Right ()
    -- Shown able to fail: locked, the second browser reads nothing.
    on w b (readSecrets alice assistant) `shouldReturn` Left Locked
    withVaultKey b alice w.vaultKey
    on w b (readSecrets alice assistant) `shouldReturn` Right (Map.singleton (SecretName "anthropic") "sk-ant-on-a")
    on w a (putSecret alice assistant (SecretName "anthropic") "sk-ant-changed-on-a") `shouldReturn` Right ()
    on w b (readSecrets alice assistant) `shouldReturn` Right (Map.singleton (SecretName "anthropic") "sk-ant-changed-on-a")

  it "does not bring back from another browser's legacy copy a key forgotten after the move" $ do
    w <- newWorld
    withLegacyItem w synced
    a <- newDevice w.counter True
    b <- newDevice w.counter True
    for_ [a, b] $ \d -> do
      putKey d alice w.vaultKey
      followingRecord w Nothing False synced >>= putRecord d alice
    options <- newIORef Nothing
    moveAll w a options `shouldReturn` Right Moved
    on w a (removeSecret alice assistant (SecretName "anthropic")) `shouldReturn` Right ()
    -- The second browser still holds its copy of the vault's settings,
    -- with the key forgotten on the first.
    (\l -> l.record) <$> leftOn b `shouldReturn` True
    moveAll w b options `shouldReturn` Right Moved
    Map.member (SecretName "anthropic") <$> pluginKeys w `shouldReturn` False
    Map.member (SecretName "openai") <$> pluginKeys w `shouldReturn` True
    (\l -> l.record) <$> leftOn b `shouldReturn` False

  it "does not bring back from another browser's legacy copy every key forgotten after the move: the emptied item stays" $ do
    w <- newWorld
    withLegacyItem w synced
    a <- newDevice w.counter True
    b <- newDevice w.counter True
    for_ [a, b] $ \d -> do
      putKey d alice w.vaultKey
      followingRecord w Nothing False synced >>= putRecord d alice
    options <- newIORef Nothing
    moveAll w a options `shouldReturn` Right Moved
    for_ (Map.keys (keysOf synced)) $ \k -> on w a (removeSecret alice assistant k) `shouldReturn` Right ()
    pluginKeys w `shouldReturn` mempty
    -- The plugin's item is still there, with nothing in it, and says the
    -- vault's item was moved.
    hasPluginItem w `shouldReturn` True
    (\l -> l.record) <$> leftOn b `shouldReturn` True
    moveAll w b options `shouldReturn` Right Moved
    pluginKeys w `shouldReturn` mempty
    (\l -> l.record) <$> leftOn b `shouldReturn` False

  it "takes the copies two browsers keep of the vault's last item once, when syncing was turned off elsewhere: a key forgotten after the first moved does not come back" $ do
    w <- newWorld
    withLegacyItem w synced
    a <- newDevice w.counter True
    b <- newDevice w.counter True
    -- Two records of their own (one asks each time), with copies of one
    -- item.
    for_ [(a, False), (b, True)] $ \(d, asking) -> do
      putKey d alice w.vaultKey
      followingRecord w Nothing asking synced >>= putRecord d alice
    -- An earlier version turned syncing off on a third browser: the item
    -- went, and the two copies are the settings' last.
    vaultNow w >>= setVault w.server . (\v -> v {items = Map.delete assistantItem v.items})
    hasLegacyItem w `shouldReturn` False
    options <- newIORef Nothing
    moveAll w a options `shouldReturn` Right Moved
    pluginKeys w `shouldReturn` keysOf synced
    for_ (Map.keys (keysOf synced)) $ \k -> on w a (removeSecret alice assistant k) `shouldReturn` Right ()
    (\l -> l.record) <$> leftOn b `shouldReturn` True
    moveAll w b options `shouldReturn` Right Moved
    pluginKeys w `shouldReturn` mempty
    (\l -> l.record) <$> leftOn b `shouldReturn` False

  it "takes a browser's own keys once: a key forgotten while the move waits for the options is not taken again" $ do
    w <- newWorld
    d <- newDevice w.counter True
    withVaultKey d alice w.vaultKey
    k <- newKey w
    putKey d alice k
    putRecord d alice (ownRecord k ownHere)
    -- The options are not known yet: the run keeps the keys and waits.
    on w d (migrateAssistant pageKeys alice True Nothing) `shouldReturn` Right (Adopt (optionsValue ownHere))
    pluginKeys w `shouldReturn` keysOf ownHere
    on w d (removeSecret alice assistant (SecretName "anthropic")) `shouldReturn` Right ()
    -- Shown able to fail: the record still holds the key.
    (\l -> l.record) <$> leftOn d `shouldReturn` True
    on w d (migrateAssistant pageKeys alice True (Just (Just (optionsValue ownHere)))) `shouldReturn` Right Moved
    pluginKeys w `shouldReturn` mempty
    (\l -> l.record) <$> leftOn d `shouldReturn` False

  it "moves again the vault's item an earlier tab adds back, the plugin's keys winning, and the names it lacks taken" $ do
    w <- newWorld
    withLegacyItem w synced
    d <- newDevice w.counter True
    putKey d alice w.vaultKey
    followingRecord w Nothing False synced >>= putRecord d alice
    options <- newIORef Nothing
    moveAll w d options `shouldReturn` Right Moved
    -- An earlier tab turns syncing on again, with a key changed there and
    -- a provider's key the plugin does not hold.
    let older = withKey Scripted (Just "scripted-older-tab") (withKey Anthropic (Just "sk-ant-older-tab") synced)
    withLegacyItem w older
    moveAll w d options `shouldReturn` Right Moved
    keys <- pluginKeys w
    Map.lookup (SecretName "anthropic") keys `shouldBe` Map.lookup (SecretName "anthropic") (keysOf synced)
    Map.lookup (SecretName "scripted") keys `shouldBe` Map.lookup (SecretName "scripted") (keysOf older)
    hasLegacyItem w `shouldReturn` False
