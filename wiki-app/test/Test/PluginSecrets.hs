-- | A plugin's secrets as the host keeps them
-- ("Wiki.App.Plugin.Host.Secrets"): in the user's vault, against a model
-- of the server's (@Test.VaultServer@), with a browser whose cryptography
-- is faked (@Test.FakeVault@); and in the browser where the user has no
-- vault. One plugin's change leaves every other item of the vault as it
-- was; a name the manifest does not declare is refused before anything is
-- read; an item moved to another plugin's name is not opened, nor written
-- over; two devices that change the vault at once both keep their change;
-- two tabs of one browser that change what it keeps at once both keep
-- their change, one after the other.
module Test.PluginSecrets (spec, storageSpec) where

import Control.Concurrent.STM (isEmptyTMVar, newEmptyTMVarIO, putTMVar, takeTMVar)
import Data.Aeson (decodeStrict, object, (.=))
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Halogen.VDom.DOM.Monad (MemDOM, runMemDOM)
import Protolude hiding (on)
import Test.FakeVault
import Test.Hspec hiding (after, before)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
import Test.VaultServer
import Text.Show (Show (showsPrec))
import Wiki.Api.Plugins (PluginId (..))
import Wiki.App.Plugin (Manifest (..), SecretName (..), SecretSpec (..), Unavailable (..), builtin)
import Wiki.App.Plugin.Host.Secrets
import Wiki.App.Plugin.Registry (manifests)
import Wiki.App.Vault (vaultSlot)
import Wiki.Core.Id (UserId (..))
import Wiki.Core.Vault (Item (..), ItemName (..), Vault (..), itemOf, withItem)
import Wiki.Core.Vault.Plugin (decodeSecrets, encodeSecrets, maxPluginIdForSecrets, pluginItem, pluginSealingData, secretNameOk)

spec :: Spec
spec = describe "Wiki.App.Plugin.Host.Secrets" $ do
  inTheVault
  isolation
  twoDevices
  twoTabs
  inThisBrowser
  registry

-- | What the in-memory document's IndexedDB keeps, which is the process's:
-- run apart from every other example that keeps something there.
storageSpec :: Spec
storageSpec = describe "Wiki.App.Plugin.Host.Secrets, in the in-memory document" $ do
  it "keeps a plugin's secrets in the clear where the page cannot seal (plain HTTP), as the assistant's keys are kept there" $ do
    let carol = UserId "user-carol-plugin-secrets"
    runMemDOM (putSecret carol notes token "sk-1") `shouldReturn` Right ()
    runMemDOM (readSecrets carol notes) `shouldReturn` Right (Map.fromList [(token, "sk-1")])
    runMemDOM (removeSecret carol notes token) `shouldReturn` Right ()
    runMemDOM (readSecrets carol notes) `shouldReturn` Right mempty

  it "keeps a plugin's cache for each user and plugin apart" $ do
    let dave = UserId "user-dave-plugin-cache"
        erin = UserId "user-erin-plugin-cache"
    runMemDOM (saveLocal dave (PluginId "notes") (object ["models" .= ["a" :: Text]])) `shouldReturn` Right ()
    runMemDOM (loadLocal dave (PluginId "notes")) `shouldReturn` Just (object ["models" .= ["a" :: Text]])
    runMemDOM (loadLocal dave (PluginId "mail")) `shouldReturn` Nothing
    runMemDOM (loadLocal erin (PluginId "notes")) `shouldReturn` Nothing

alice, bob :: UserId
alice = UserId "user-alice"
bob = UserId "user-bob"

-- | A plugin that declares these secrets.
plugin :: Text -> [Text] -> Manifest
plugin pid names = (builtin pid pid "") {secrets = [SecretSpec {name = SecretName n, label = n, description = ""} | n <- names]}

notes, mail, calendar :: Manifest
notes = plugin "notes" ["token", "other"]
mail = plugin "mail" ["token", "password"]
calendar = plugin "calendar" ["key"]

token, other, password :: SecretName
token = SecretName "token"
other = SecretName "other"
password = SecretName "password"

-- | 'World' is one test's server and the number of the user's vault key.
data World = World
  { server :: IORef ServerState
  , counter :: IORef Int
  , vaultKey :: Int
  }

-- | A server with Alice's vault, and a device where it is unlocked.
unlocked :: IO (World, Device)
unlocked = do
  counter <- newIORef 0
  k <- atomicModifyIORef' counter (\n -> (n + 1, n + 1))
  server <- newServer (Just (userVault k))
  d <- newDevice counter True
  withVaultKey d alice k
  pure (World {server, counter, vaultKey = k}, d)

-- | Run on a device, with the world's server.
on :: World -> Device -> Served (Fake MemDOM) a -> IO a
on w d act = runMemDOM (onDevice d (served w.server act))

-- | The vault as the server holds it.
vaultNow :: World -> IO Vault
vaultNow w = serverState w.server >>= maybe (panic "no vault") (pure . snd) . (\s -> s.kept)

-- | The secrets of a plugin's item in the server's vault, opened with the
-- vault key: 'Nothing' without an item.
itemSecrets :: World -> Manifest -> IO (Maybe (Map SecretName Text))
itemSecrets w manifest = do
  v <- vaultNow w
  pure $ do
    name <- pluginItem manifest.id.text
    i <- itemOf name v
    either (panic . ("an item that does not open: " <>)) Just (fakeOpen w.vaultKey (pluginSealingData manifest.id.text) i.sealed >>= decodeSecrets)

inTheVault :: Spec
inTheVault = describe "in the user's vault" $ do
  it "keeps, reads and forgets a plugin's secrets, in its item" $ do
    (w, d) <- unlocked
    on w d (putSecret alice notes token "sk-1") `shouldReturn` Right ()
    on w d (putSecret alice notes other "two") `shouldReturn` Right ()
    on w d (readSecrets alice notes) `shouldReturn` Right (Map.fromList [(token, "sk-1"), (other, "two")])
    itemSecrets w notes `shouldReturn` Just (Map.fromList [(token, "sk-1"), (other, "two")])
    on w d (removeSecret alice notes token) `shouldReturn` Right ()
    on w d (readSecrets alice notes) `shouldReturn` Right (Map.fromList [(other, "two")])
    -- The last secret forgotten, the item stays, with no secret.
    on w d (removeSecret alice notes other) `shouldReturn` Right ()
    itemSecrets w notes `shouldReturn` Just mempty
    on w d (readSecrets alice notes) `shouldReturn` Right mempty

  it "refuses a name the manifest does not declare, before anything is read" $ do
    (w, d) <- unlocked
    before <- serverState w.server
    -- "password" is the mail plugin's, not the notes plugin's.
    on w d (putSecret alice notes password "hunter2") >>= (`shouldSatisfy` isLeft)
    on w d (removeSecret alice notes password) >>= (`shouldSatisfy` isLeft)
    after <- serverState w.server
    (after.fetches, after.stores) `shouldBe` (before.fetches, before.stores)
    deviceRecords d `shouldReturn` mempty

  it "reads only the names the manifest declares" $ do
    (w, d) <- unlocked
    v <- vaultNow w
    let planted = Map.fromList [(token, "kept"), (password, "not the notes plugin's")]
        sealed = fakeSeal w.vaultKey (pluginSealingData "notes") (encodeOf planted)
    setVault w.server (withItem (ItemName "plugin-notes") Item {keyId = Nothing, sealed} v)
    on w d (readSecrets alice notes) `shouldReturn` Right (Map.fromList [(token, "kept")])

  it "answers that the vault is locked where this browser holds no key to it, and writes nothing" $ do
    (w, _) <- unlocked
    locked <- newDevice w.counter True
    on w locked (readSecrets alice notes) `shouldReturn` Left Locked
    on w locked (putSecret alice notes token "sk-1") >>= (`shouldSatisfy` isLeft)
    (\s -> s.stores) <$> serverState w.server `shouldReturn` 0
    deviceRecords locked `shouldReturn` mempty

  it "does not open the vault of another user with this user's key" $ do
    (w, d) <- unlocked
    -- Bob holds no key here: the page's session may be Bob's while a
    -- view still asks for Alice, whose key does not open Bob's vault.
    on w d (readSecrets bob notes) `shouldReturn` Left Locked
    on w d (putSecret bob notes token "sk-1") >>= (`shouldSatisfy` isLeft)
    (\s -> s.stores) <$> serverState w.server `shouldReturn` 0

  it "opens no item moved to another plugin's name, and does not write over it" $ do
    (w, d) <- unlocked
    on w d (putSecret alice notes token "the notes plugin's") `shouldReturn` Right ()
    v <- vaultNow w
    let moved = fromMaybe (panic "no item") (itemOf (ItemName "plugin-notes") v)
    setVault w.server (withItem (ItemName "plugin-mail") moved v)
    on w d (readSecrets alice mail) >>= \case
      Left (Failed _) -> pass
      other' -> expectationFailure ("read as the mail plugin's: " <> show other')
    on w d (putSecret alice mail token "mine") >>= (`shouldSatisfy` isLeft)
    itemOf (ItemName "plugin-mail") <$> vaultNow w `shouldReturn` Just moved

  it "reads, as a surface starts, the copy this browser holds and asks the server nothing; with no copy, it reads the vault" $ do
    (w, d) <- unlocked
    on w d (putSecret alice notes token "sk-1") `shouldReturn` Right ()
    -- The change kept the copy: nothing fetched for the read.
    before <- (\s -> s.fetches) <$> serverState w.server
    on w d (readSecretsHere alice notes) `shouldReturn` Right (Map.fromList [(token, "sk-1")])
    (\s -> s.fetches) <$> serverState w.server `shouldReturn` before
    -- Shown able to fail: a browser that holds no copy reads the vault,
    -- and keeps the copy for the next read.
    other' <- newDevice w.counter True
    withVaultKey other' alice w.vaultKey
    on w other' (readSecretsHere alice notes) `shouldReturn` Right (Map.fromList [(token, "sk-1")])
    afterFirst <- (\s -> s.fetches) <$> serverState w.server
    afterFirst `shouldSatisfy` (> before)
    on w other' (readSecretsHere alice notes) `shouldReturn` Right (Map.fromList [(token, "sk-1")])
    (\s -> s.fetches) <$> serverState w.server `shouldReturn` afterFirst
    -- Locked here, with the copy: locked, and nothing fetched.
    modifyIORef' other'.keys (Map.delete (vaultSlot alice).text)
    on w other' (readSecretsHere alice notes) `shouldReturn` Left Locked
    (\s -> s.fetches) <$> serverState w.server `shouldReturn` afterFirst
    -- A change on the first browser: the second's copy is older, and a
    -- refresh reads the vault and announces it.
    on w d (putSecret alice notes token "sk-2") `shouldReturn` Right ()
    withVaultKey other' alice w.vaultKey
    on w other' (readSecretsHere alice notes) `shouldReturn` Right (Map.fromList [(token, "sk-1")])
    on w other' (refreshSecrets alice notes)
    on w other' (readSecretsHere alice notes) `shouldReturn` Right (Map.fromList [(token, "sk-2")])

  it "asks the server once whether there is a vault, where there is none, and then reads what this browser keeps" $ do
    counter <- newIORef 0
    server <- newServer Nothing
    d <- newDevice counter True
    let w = World {server, counter, vaultKey = 0}
    on w d (readSecretsHere alice notes) `shouldReturn` Right mempty
    first' <- (\s -> s.fetches) <$> serverState server
    first' `shouldSatisfy` (> 0)
    on w d (readSecretsHere alice notes) `shouldReturn` Right mempty
    (\s -> s.fetches) <$> serverState server `shouldReturn` first'

  it "makes a change again when another device stored the vault first, at most three times in all" $ do
    (w, d) <- unlocked
    let elsewhere = vaultNow w >>= setVault w.server
    replicateM_ (maxTries - 1) (beforeNextStore w.server elsewhere)
    on w d (putSecret alice notes token "kept at last") `shouldReturn` Right ()
    itemSecrets w notes `shouldReturn` Just (Map.fromList [(token, "kept at last")])
    replicateM_ maxTries (beforeNextStore w.server elsewhere)
    on w d (putSecret alice notes token "never kept") >>= (`shouldSatisfy` isLeft)
    itemSecrets w notes `shouldReturn` Just (Map.fromList [(token, "kept at last")])

-- | 'Op' is a change a plugin asks for.
data Op = Put Manifest SecretName Text | Remove Manifest SecretName

instance Show Op where
  showsPrec _ op rest = described <> rest
    where
      described :: [Char]
      described = toS $ case op of
        Put m n v -> "Put " <> m.id.text <> " " <> n.text <> " " <> show v
        Remove m n -> "Remove " <> m.id.text <> " " <> n.text

-- | The plugin an op is of.
pluginOf :: Op -> Manifest
pluginOf = \case
  Put m _ _ -> m
  Remove m _ -> m

-- | Whether the plugin's manifest declares the op's name.
declares :: Op -> Bool
declares = \case
  Put m n _ -> n `elem` map (\s -> s.name) m.secrets
  Remove m n -> n `elem` map (\s -> s.name) m.secrets

-- | An op of one of the three plugins, with a name of any of them, so that
-- some are refused.
genOp :: Gen Op
genOp = do
  m <- elements [notes, mail, calendar]
  n <- SecretName <$> elements ["token", "other", "password", "key"]
  frequency [(5, Put m n <$> elements ["a", "b", "sk-ü", ""]), (3, pure (Remove m n))]

-- | What an op makes of a plugin's secrets.
applied :: Op -> Map SecretName Text -> Map SecretName Text
applied = \case
  Put _ n v -> Map.insert n v
  Remove _ n -> Map.delete n

-- | Run an op on a device, with the lock.
runOp :: UserId -> Op -> Served (Fake MemDOM) (Either Text ())
runOp u = \case
  Put m n v -> putSecret u m n v
  Remove m n -> removeSecret u m n

isolation :: Spec
isolation = describe "one plugin's change" $
  modifyMaxSuccess (const 60) . prop "leaves every other item and every wrap of the vault as it was, and gives the plugin what it asked for" $
    forAll (listOf1 genOp) $ \ops -> ioProperty $ do
      (w, d) <- unlocked
      results <- for ops $ \op -> do
        before <- vaultNow w
        result <- on w d (runOp alice op)
        after <- vaultNow w
        let mine = pluginItem (pluginOf op).id.text
            othersOf v = Map.filterWithKey (\n _ -> Just n /= mine) v.items
        pure
          ( counterexample (show op) $
              conjoin
                [ othersOf after === othersOf before
                , after.wraps === before.wraps
                , if declares op then isRight result === True else (isLeft result .&&. after === before)
                ]
          )
      -- What each plugin holds is what its own ops made of it.
      let expected m = foldl (flip applied) mempty [op | op <- ops, declares op, (pluginOf op).id == m.id]
      held <- for [notes, mail, calendar] $ \m -> (\s -> fromMaybe mempty s === expected m) <$> itemSecrets w m
      pure (conjoin results .&&. conjoin held)

twoDevices :: Spec
twoDevices = describe "two devices" $
  modifyMaxSuccess (const 60) . prop "that change the vault at once both keep their change, the last stored winning on a name" $
    forAll (listOf1 (declaredOp)) $ \fromA -> forAll (listOf1 declaredOp) $ \fromB -> forAll (vectorOf (length fromA) arbitrary) $ \between -> ioProperty $ do
      (w, a) <- unlocked
      b <- newDevice w.counter True
      withVaultKey b alice w.vaultKey
      -- The changes in the order the server stored them, each when it
      -- returned: a change B makes between A's fetch and A's store returns
      -- before A's.
      committed <- newIORef []
      remaining <- newIORef fromB
      let done op = \case
            Right () -> modifyIORef' committed (<> [op])
            Left why -> panic ("a change failed: " <> why)
          -- B's next change, made on B without the lock: A holds the one
          -- lock of the test's process, as each device holds its own.
          nextOfB =
            atomicModifyIORef' remaining (\case [] -> ([], Nothing); op : rest -> (rest, Just op)) >>= traverse_ (\op -> on w b (heldOp alice op) >>= done op)
      for_ (zip fromA between) $ \(op, interleave) -> do
        when interleave (beforeNextStore w.server nextOfB)
        on w a (runOp alice op) >>= done op
      -- What waits of B, and B's change put before a store that never came.
      modifyIORef' w.server (\s -> s {hooks = []})
      readIORef remaining >>= traverse_ (\op -> on w b (runOp alice op) >>= done op)
      order <- readIORef committed
      let expected m = foldl (flip applied) mempty [op | op <- order, (pluginOf op).id == m.id]
      held' <- for [notes, mail, calendar] $ \m -> (\s -> fromMaybe mempty s === expected m) <$> itemSecrets w m
      pure (length order === length fromA + length fromB .&&. conjoin held')

-- | A change made with the lock of the page held already: another page's,
-- run while this test's process holds its one lock of pages.
heldOp :: UserId -> Op -> Served (Fake MemDOM) (Either Text ())
heldOp u = \case
  Put m n v -> putSecretHeld u m n v
  Remove m n -> removeSecretHeld u m n

-- | A change of a name the plugin declares.
declaredOp :: Gen Op
declaredOp = genOp `suchThat` declares

-- | Two tabs of one browser are two pages: each holds a lock of its own
-- page, and both read and write the browser's one record of secrets. Tab
-- B's change is made while tab A is between its read of the record (and of
-- the browser's key) and its write. B must wait for A's write, and then
-- read it: otherwise A writes back what it read before B's change, and
-- B's change is lost, even one of another plugin's, while both report
-- success. The same holds for the browser's first key, which each tab
-- would make for itself.
twoTabs :: Spec
twoTabs = describe "two tabs of one browser, where the user has no vault" $
  modifyMaxSuccess (const 100) . prop "change what the browser keeps one after the other, so that neither writes over the other's change" $
    forAll (listOf declaredOp) $ \seeds -> forAll declaredOp $ \fromA -> forAll declaredOp $ \fromB -> ioProperty $ do
      counter <- newIORef 0
      server <- newServer Nothing
      d <- newDevice counter True
      let w = World {server, counter, vaultKey = 0}
      for_ seeds (\op -> on w d (runOp alice op) >>= either (panic . ("a change failed: " <>)) pure)
      answerOfB <- newEmptyTMVarIO
      let tabB = on w d (heldOp alice fromB) >>= atomically . putTMVar answerOfB
      -- B starts before A's first write, and A writes once B waits for
      -- the lock of the browser's tabs, or once B is done.
      beforeNextWrite d $ do
        void (forkIO tabB)
        waitingForLock d (not <$> isEmptyTMVar answerOfB)
      answerOfA <- on w d (runOp alice fromA)
      -- A wrote nothing: B was not started.
      unstarted <- atomicModifyIORef' d.writeHooks (\h -> ([], not (null h)))
      when unstarted tabB
      answerB <- atomically (takeTMVar answerOfB)
      let expected m = foldl (flip applied) mempty [op | op <- seeds <> [fromA, fromB], (pluginOf op).id == m.id]
      readBack <- for [notes, mail, calendar] $ \m -> (\r -> counterexample (toS m.id.text) (r === Right (expected m))) <$> on w d (readSecrets alice m)
      pure (answerOfA === Right () .&&. answerB === Right () .&&. conjoin readBack)

inThisBrowser :: Spec
inThisBrowser = describe "in this browser, where the user has no vault" $ do
  it "keeps them sealed with a key of this browser's, where it can seal (the demo)" $ do
    counter <- newIORef 0
    d <- newDevice counter True
    let here :: Fake MemDOM a -> IO a
        here = runMemDOM . onDevice d
    here (putSecret alice notes token "sk-1") `shouldReturn` Right ()
    here (putSecret alice mail token "sk-2") `shouldReturn` Right ()
    here (readSecrets alice notes) `shouldReturn` Right (Map.fromList [(token, "sk-1")])
    here (readSecrets alice mail) `shouldReturn` Right (Map.fromList [(token, "sk-2")])
    store <- storeOf d alice
    Map.keys store `shouldBe` [ItemName "plugin-mail", ItemName "plugin-notes"]
    all isSealed (Map.elems store) `shouldBe` True
    here (removeSecret alice notes token) `shouldReturn` Right ()
    here (removeSecret alice mail token) `shouldReturn` Right ()
    -- Nothing kept: the entries stay, with no secret, as the vault's item
    -- does.
    store' <- storeOf d alice
    Map.keys store' `shouldBe` [ItemName "plugin-mail", ItemName "plugin-notes"]
    here (readSecrets alice notes) `shouldReturn` Right mempty

  it "keeps them here for a user with a server but no vault yet, and folds them into the vault once there is one" $ do
    counter <- newIORef 0
    server <- newServer Nothing
    d <- newDevice counter True
    let w0 = World {server, counter, vaultKey = 0}
    on w0 d (putSecret alice notes token "typed here") `shouldReturn` Right ()
    on w0 d (putSecret alice notes other "only here") `shouldReturn` Right ()
    (\s -> s.stores) <$> serverState server `shouldReturn` 0
    -- The user makes a vault (elsewhere), which already holds a token.
    k <- atomicModifyIORef' counter (\n -> (n + 1, n + 1))
    let w = w0 {vaultKey = k}
        inVault = Map.fromList [(token, "the vault's")]
    setVault server (withItem (ItemName "plugin-notes") Item {keyId = Nothing, sealed = fakeSeal k (pluginSealingData "notes") (encodeOf inVault)} (userVault k))
    withVaultKey d alice k
    -- Read, both are given, the vault's winning on a name.
    on w d (readSecrets alice notes) `shouldReturn` Right (Map.fromList [(token, "the vault's"), (other, "only here")])
    -- Changed, what was kept here goes into the vault, and leaves here.
    on w d (removeSecret alice notes token) `shouldReturn` Right ()
    itemSecrets w notes `shouldReturn` Just (Map.fromList [(other, "only here")])
    Map.member (ItemName "plugin-notes") <$> storeOf d alice `shouldReturn` False
  where
    isSealed = \case
      SealedEntry _ -> True
      PlainEntry _ -> False

-- | What a device keeps of a user's secrets of plugins.
storeOf :: Device -> UserId -> IO (Map ItemName DeviceEntry)
storeOf d u = fromMaybe mempty . (decodeStrict =<<) . Map.lookup (deviceSecretsSlot u).text <$> deviceRecords d

registry :: Spec
registry = describe "the registry" $
  it "declares, for each plugin that keeps secrets, names a secret can have, once each, and an id an item can be named by" $
    for_ manifests $ \m -> unless (null m.secrets) $ do
      let names = map (\s -> s.name) m.secrets
      filter (not . secretNameOk) names `shouldBe` []
      ordNub names `shouldBe` names
      T.length m.id.text `shouldSatisfy` (<= maxPluginIdForSecrets)

-- | The text an item seals, for a test that plants one.
encodeOf :: Map SecretName Text -> Text
encodeOf = encodeSecrets
