-- | The plan that moves the writing assistant's settings from their legacy
-- places to the plugin's ("Wiki.Core.Assistant.Migrate"), run against a
-- model of those places: the vault's legacy item and the plugin's item,
-- the server's options, and each browser's record, slots of keys, cache
-- and "ask each time". A step changes the model as the application's
-- runner changes the real places ("Wiki.App.Assistant.Migrate"): a
-- conditional delete deletes only what was read, a key the plugin's
-- secrets hold keeps its value, a legacy place is taken into the plugin's
-- item once, options are taken only when the server has none. The
-- plugin's item is deleted by nothing: its last key forgotten, it stays
-- with no key, as the real store keeps it ("Wiki.App.Plugin.Host.Secrets";
-- only a reset of the password deletes the whole vault, which no run
-- does). A run reads the model, takes the plan's first step, and reads
-- again, as the runner does. Keys are forgotten from the plugin's item
-- between any two steps, on any browser that holds the vault key.
module Test.AssistantMigrate (spec) where

import Data.Aeson (Value)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Protolude
import Test.AssistantSplit (genSettings)
import Test.Cases (coverage)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Wiki.Core.Assistant (Mode (..), Settings, defaultSettings, withMode)
import Wiki.Core.Assistant.Migrate
import Wiki.Core.Assistant.Split (blankOptions, keysOf, optionsValue)
import Wiki.Core.Vault (Nonce (..), Sealed (..))
import Wiki.Core.Vault.Plugin (Contents (..), SecretName, foldIn, noContents)

--------------------------------------------------------------------------------
-- The model

-- | 'World' is every place the settings can be.
data World = World
  { item :: Maybe (Int, Settings)
  -- ^ The vault's legacy item, by the number of its seal.
  , plugin :: Maybe Contents
  -- ^ The plugin's item, if it was made.
  , serverOptions :: Maybe Value
  , devices :: Map Int Dev
  , seals :: Int
  , copiesMoved :: Set Int
  -- ^ The items, by number, a browser emptied its record of a copy of
  -- while that copy was the settings' last: the test's own account, kept
  -- apart from what the plugin's item says it took.
  }
  deriving stock (Eq, Show)

-- | 'Dev' is one browser.
data Dev = Dev
  { held :: Maybe Rec
  , keySlot :: Slot
  , vaultKept :: Bool
  , cached :: Bool
  , asks :: Bool
  }
  deriving stock (Eq, Show)

-- | 'Rec' is a browser's legacy record, by the number it was written
-- under.
data Rec = Rec
  { number :: Int
  , own :: Maybe Settings
  , follows :: Bool
  , copy :: Maybe (Int, Settings)
  -- ^ An L1 record's copy of the vault's item, by the number of the
  -- item's seal, which is its nonce.
  , askEachTime :: Bool
  , learnt :: Maybe Value
  }
  deriving stock (Eq, Show)

-- | Whether the browser holds the vault key.
unlocked :: Dev -> Bool
unlocked d = d.vaultKept || d.keySlot == VaultKey

sealOf :: Int -> Sealed
sealOf n = Sealed {nonce = Nonce (show n), ciphertext = "sealed"}

deviceOf :: Int -> World -> Dev
deviceOf d w = Map.findWithDefault (Dev Nothing NoKey False False False) d w.devices

-- | What the runner reads on a browser.
observe :: Int -> World -> Found
observe d w =
  Found
    { vault = case w.item of
        Nothing -> NoItem
        Just (n, s)
          | unlocked dev -> ItemOpened (sealOf n) s
          | otherwise -> ItemLocked
    , record =
        ( \r ->
            OnDevice
              { bytes = show r.number
              , source = "record:" <> show r.number
              , own = r.own
              , copy = if unlocked dev then snd <$> r.copy else Nothing
              , copied = show . fst <$> r.copy
              , follows = r.follows
              , unopenable = False
              , askEachTime = r.askEachTime
              , learnt = r.learnt
              }
        )
          <$> dev.held
    , slot = dev.keySlot
    , vaultSlot = dev.vaultKept
    , target = if unlocked dev then Just (pluginSecrets w) else Nothing
    , taken = if unlocked dev then maybe mempty (\c -> c.taken) w.plugin else mempty
    , options = Just w.serverOptions
    , local = dev.cached
    , askHere = dev.asks
    }
  where
    dev = deviceOf d w

-- | A step taken on a browser, as the runner takes it.
perform :: Int -> Step -> World -> World
perform d step w = case step of
  KeepAsk -> onDev (\x -> x {asks = True})
  CopyVaultKey -> onDev (\x -> if x.keySlot == VaultKey then x {vaultKept = True} else x)
  KeepKeys places
    | unlocked dev -> w {plugin = Just (foldl' taking (fromMaybe noContents w.plugin) places)}
    | otherwise -> w
  KeepLearnt _ -> onDev (\x -> x {cached = True})
  AdoptOptions v -> w {serverOptions = w.serverOptions <|> Just v}
  DropVaultItem s
    | (sealOf . fst <$> w.item) == Just s -> w {item = Nothing}
    | otherwise -> w
  DropRecord b -> case (\x -> x.held) (deviceOf d w) of
    Just r
      | show r.number == b ->
          (onDev (\x -> x {held = Nothing})) {copiesMoved = w.copiesMoved <> Set.fromList [n | lastCopyWorld w, Just (n, _) <- [r.copy]]}
    _ -> w
  DropSlotKey -> onDev (\x -> if isNothing x.held then x {keySlot = NoKey} else x)
  where
    dev = deviceOf d w
    onDev f = w {devices = Map.adjust f d w.devices}
    -- As the store takes a place, reading what was taken in the write.
    taking c (place, ks)
      | Set.member place c.taken = c
      | otherwise = Contents {secrets = foldIn c.secrets ks, taken = Set.insert place c.taken}

-- | The keys in the plugin's item; none when there is no item.
pluginSecrets :: World -> Map SecretName Text
pluginSecrets w = maybe mempty (\c -> c.secrets) w.plugin

-- | A key forgotten on a browser that holds the vault key, as the
-- plugin's field forgets it: the item stays, with the other keys.
forget :: Int -> SecretName -> World -> World
forget d k w
  | unlocked (deviceOf d w) = w {plugin = (\c -> c {secrets = Map.delete k c.secrets}) <$> w.plugin}
  | otherwise = w

-- | The legacy places that hold a key of this name and that the plugin's
-- item has not taken: those a run may still fill the name from. An L1
-- copy counts only while it is the settings' last copy.
untakenHolders :: SecretName -> World -> [Text]
untakenHolders k w =
  filter (`Set.notMember` takenNow) (["item:" <> show n | Just (n, s) <- [w.item], Map.member k (keysOf s)] <> ["record:" <> show r.number | r <- records, Map.member k (maybe mempty keysOf r.own)])
    -- A last copy of an item counts until a browser emptied its record
    -- of a copy of that item: the copies of one item are one place.
    <> ["copy:" <> show n | lastCopyWorld w, r <- records, Just (n, c) <- [r.copy], Set.notMember n w.copiesMoved, Map.member k (keysOf c)]
  where
    takenNow = maybe mempty (\c -> c.taken) w.plugin
    records = [r | Just r <- map (\x -> x.held) (Map.elems w.devices)]

-- | Whether an L1 copy is the settings' last copy: the vault has no
-- legacy item, and none was moved.
lastCopyWorld :: World -> Bool
lastCopyWorld w = isNothing w.item && not (tookItem w)

-- | Whether the plugin's item took a vault's legacy item.
tookItem :: World -> Bool
tookItem w = any ("item:" `T.isPrefixOf`) (maybe mempty (\c -> c.taken) w.plugin)

-- | One step of a browser's run, if its plan has one.
stepOn :: Int -> World -> Maybe (Step, World)
stepOn d w = case plan (observe d w) of
  [] -> Nothing
  s : _ -> Just (s, perform d s w)

-- | A browser's run, to its end, with every step and the world after it.
runOn :: Int -> World -> [(Step, World)]
runOn d = go (50 :: Int)
  where
    go 0 _ = []
    go n w = case stepOn d w of
      Nothing -> []
      Just (s, w') -> (s, w') : go (n - 1) w'

-- | The world at the end of a browser's run.
completed :: Int -> World -> World
completed d w = maybe w snd (lastMay (runOn d w))

-- | Every key's name that any place holds. An L1 record's copy of the
-- vault's settings counts only while it is their last copy: while the
-- vault holds the legacy item, or the plugin's item took one, the copy is
-- the vault's, and a key forgotten there since is not to come back from
-- it.
heldNames :: World -> Set SecretName
heldNames w =
  Set.unions
    ( Map.keysSet (pluginSecrets w)
        : maybe mempty (Map.keysSet . keysOf . snd) w.item
        : [Set.unions [maybe mempty (Map.keysSet . keysOf) r.own, if lastCopy then maybe mempty (Map.keysSet . keysOf . snd) r.copy else mempty] | Just r <- map (\x -> x.held) (Map.elems w.devices)]
    )
  where
    lastCopy = isNothing w.item && not (tookItem w)

-- | Settings that are not the defaults: in Ask mode.
settingsAsked :: Settings
settingsAsked = withMode Ask defaultSettings

-- | Whether a browser holds nothing legacy any more.
moved :: Int -> World -> Bool
moved d w = let x = deviceOf d w in isNothing x.held && x.keySlot == NoKey

--------------------------------------------------------------------------------
-- Generators

-- | One browser in one of the legacy states, or with none.
genDev :: (Int, Settings) -> Gen (Dev, Maybe Settings)
genDev item =
  oneof
    [ pure (Dev Nothing NoKey True False False, Nothing)
    , l1
    , l1Reloaded
    , ownRecord OwnKey True -- L2
    , ownRecord NoKey True -- L3
    , l4
    ]
  where
    -- The copy is of the vault's item, or of an older version of it. When
    -- the vault has no item (syncing was turned off elsewhere), the item is
    -- the one it last had: two browsers may hold copies of that one.
    aCopy = Just <$> oneof [pure item, (,) <$> chooseInt (100000, 200000) <*> genSettings]
    l1 = do
      copy <- aCopy
      inSlot <- arbitrary
      r <- record Nothing True copy
      pure (Dev (Just r) VaultKey (not inSlot) False False, Nothing)
    -- L1 on a browser that asks for an unlock each time, after a reload:
    -- the assistant's slot is empty, and the vault key is in the vault's
    -- slot once the browser is unlocked, which opens the copy.
    l1Reloaded = do
      copy <- aCopy
      r <- record Nothing True copy
      pure (Dev (Just Rec {number = r.number, own = r.own, follows = r.follows, copy = r.copy, askEachTime = True, learnt = r.learnt}) NoKey True False False, Nothing)
    ownRecord slot vaultSlot' = do
      s <- genSettings
      r <- record (Just s) False Nothing
      pure (Dev (Just r) slot vaultSlot' False False, Just s)
    -- The copy is sealed with the vault key, which this browser does not
    -- hold: it opens once the browser is unlocked.
    l4 = do
      typed <- oneof [pure Nothing, Just <$> genSettings]
      r <- record typed True (Just item)
      pure (Dev (Just r) OwnKey False False False, typed)
    record own follows copy = do
      askEachTime <- if follows then arbitrary else pure False
      learnt <- oneof [pure Nothing, Just . optionsValue <$> genSettings]
      n <- chooseInt (100, 10000)
      pure Rec {number = n, own, follows, copy, askEachTime, learnt}

-- | A world of one or two browsers, a vault item or none, keys in the
-- plugin's item or none, options on the server or none.
genWorld :: Int -> Gen World
genWorld n = do
  vaultItem <- oneof [pure Nothing, Just <$> genSettings]
  -- The item the vault has, or had before syncing was turned off.
  lastItem <- maybe genSettings pure vaultItem
  devs <- vectorOf n (fst <$> genDev (1, lastItem))
  plugin <- oneof [pure Nothing, pure (Just noContents), (\s -> Just noContents {secrets = keysOf s}) <$> genSettings]
  options <- oneof [pure Nothing, Just . optionsValue <$> genSettings]
  pure World {item = (1,) <$> vaultItem, plugin, serverOptions = options, devices = Map.fromList (zip [1 ..] (map dedupe (zip [1 ..] devs))), seals = 2, copiesMoved = mempty}
  where
    -- Each record its own number, so that a delete names one record.
    dedupe (i, d) = d {held = (\r -> Rec {number = r.number * 10 + i, own = r.own, follows = r.follows, copy = r.copy, askEachTime = r.askEachTime, learnt = r.learnt}) <$> d.held}

-- | A world whose browsers all hold the vault key.
genUnlocked :: Int -> Gen World
genUnlocked n = (\w -> w {devices = Map.map (\d -> if unlocked d then d else d {vaultKept = True}) w.devices}) <$> genWorld n

--------------------------------------------------------------------------------
-- Properties

spec :: Spec
spec = describe "Wiki.Core.Assistant.Migrate" $ do
  prop "moves everything from an unlocked browser, the plugin's keys winning, and empties the legacy places last" $
    forAll (genUnlocked 1) $ \w ->
      let found = observe 1 w
          end = completed 1 w
          steps = map fst (runOn 1 w)
          expectedOptions = w.serverOptions <|> mfilter (not . blankOptions) (optionsValue <$> legacyOptions found)
       in coverage . cover 20 (isJust (deviceOf 1 w).held) "a legacy record" . cover 10 (isJust w.item) "a legacy item" $
            moved 1 end
              .&&. isNothing end.item
              .&&. pluginSecrets end === foldIn (pluginSecrets w) (legacyKeys found)
              .&&. end.serverOptions === expectedOptions
              .&&. counterexample (show steps) (all isDrop (dropWhile (not . isDrop) steps))

  prop "keeps every key in the legacy places or the plugin's after every step" $
    forAll (genWorld 1) $ \w ->
      let atStart = heldNames w
       in conjoin [counterexample (show s) (atStart `Set.isSubsetOf` heldNames w') | (s, w') <- runOn 1 w]

  prop "ends a run stopped after any step and run again as a run never stopped" $
    forAll (genWorld 1) $ \w ->
      conjoin [completed 1 w' === completed 1 w | (_, w') <- runOn 1 w]

  prop "two unlocked browsers moving at once keep every key, and each name's value is one a run in turn gives" $
    forAll (genUnlocked 2) $ \w ->
      forAll (listOf (elements [1, 2 :: Int])) $ \schedule ->
        let together = interleaved schedule w
            ab = completed 2 (completed 1 w)
            ba = completed 1 (completed 2 w)
         in moved 1 together
              .&&. moved 2 together
              .&&. isNothing together.item
              .&&. Map.keysSet (pluginSecrets together) === Map.keysSet (pluginSecrets ab)
              .&&. conjoin [counterexample (show k) (Just v `elem` [Map.lookup k (pluginSecrets ab), Map.lookup k (pluginSecrets ba)]) | (k, v) <- Map.toList (pluginSecrets together)]
              .&&. counterexample "options" (together.serverOptions `elem` [ab.serverOptions, ba.serverOptions])

  prop "two unlocked browsers that follow the vault and hold nothing of their own end as one run" $
    forAll (genUnlocked 2) $ \w0 ->
      let w = w0 {devices = Map.map followsOnly w0.devices}
          followsOnly d = d {held = (\r -> Rec {number = r.number, own = Nothing, follows = True, copy = w0.item, askEachTime = r.askEachTime, learnt = r.learnt}) <$> d.held}
       in forAll (listOf (elements [1, 2 :: Int])) $ \schedule ->
            let together = interleaved schedule w
                inTurn = completed 2 (completed 1 w)
             in (pluginSecrets together, together.serverOptions, together.item) === (pluginSecrets inTurn, inTurn.serverOptions, inTurn.item)

  prop "moves the legacy item an earlier tab adds again, the plugin's keys winning" $
    forAll (genUnlocked 1) $ \w ->
      forAll genSettings $ \readdedSettings ->
        let first' = completed 1 w
            readded = first' {item = Just (first'.seals, readdedSettings), seals = first'.seals + 1}
            end = completed 1 readded
         in isNothing end.item .&&. pluginSecrets end === foldIn (pluginSecrets first') (keysOf readdedSettings)

  it "gives the options to adopt before the server's are known, and empties nothing until they are" $ do
    let s = settingsAsked
        found' = Found {vault = NoItem, record = Just OnDevice {bytes = "r", source = "record:r", own = Just s, copy = Nothing, copied = Nothing, follows = False, unopenable = False, askEachTime = False, learnt = Nothing}, slot = OwnKey, vaultSlot = True, target = Just mempty, taken = Set.fromList ["record:r"], options = Nothing, local = False, askHere = False}
    plan found' `shouldBe` [AdoptOptions (optionsValue s)]
    plan found' {options = Just (Just (optionsValue s))} `shouldBe` [DropRecord "r"]

  it "takes a legacy place into the plugin's item before anything is emptied, even one with no key" $ do
    let found' = Found {vault = NoItem, record = Just OnDevice {bytes = "r", source = "record:r", own = Just defaultSettings, copy = Nothing, copied = Nothing, follows = False, unopenable = False, askEachTime = False, learnt = Nothing}, slot = OwnKey, vaultSlot = True, target = Just mempty, taken = mempty, options = Just Nothing, local = False, askHere = False}
    plan found' `shouldBe` [KeepKeys [("record:r", mempty)]]

  prop "never brings back a key forgotten once every legacy place that held it was taken, on either browser" $
    forAll (genUnlocked 2) $ \w ->
      forAll (listOf genMove) $ \moves ->
        let go (world, watched, checks) = \case
              Left d -> case stepOn d world of
                Nothing -> (world, watched, checks)
                Just (s, world') -> (world', watched, [counterexample ("after " <> show s <> ": " <> show k) (Map.notMember k (pluginSecrets world')) | k <- watched] <> checks)
              Right (d, i) ->
                let names = Map.keys (pluginSecrets world)
                 in case atMay names (i `mod` max 1 (length names)) of
                      Just k
                        | unlocked (deviceOf d world) ->
                            let world' = forget d k world
                             in (world', [k | null (untakenHolders k world')] <> watched, checks)
                      _ -> (world, watched, checks)
            (end, watchedAtEnd, checked) = foldl' go (w, [], []) moves
            finished = completed 2 (completed 1 end)
         in coverage . cover 10 (not (null watchedAtEnd)) "a key forgotten after its places were taken" $
              conjoin checked .&&. conjoin [counterexample ("at the end: " <> show k) (Map.notMember k (pluginSecrets finished)) | k <- watchedAtEnd]

  prop "takes the copies two browsers keep of one item once: a key forgotten after the first moved does not come back from the second" $
    -- Syncing was turned off elsewhere: the vault has no item, and both
    -- browsers keep a copy of the one it last had.
    forAll genSettings $ \lost -> forAll (chooseInt (100, 10000)) $ \n ->
      let copyOf i = Dev (Just Rec {number = n * 10 + i, own = Nothing, follows = True, copy = Just (1, lost), askEachTime = False, learnt = Nothing}) VaultKey True False False
          w = World {item = Nothing, plugin = Nothing, serverOptions = Just (optionsValue lost), devices = Map.fromList [(1, copyOf 1), (2, copyOf 2)], seals = 2, copiesMoved = mempty}
          first' = completed 1 w
          forgotten = foldl' (flip (forget 1)) first' (Map.keys (pluginSecrets first'))
          end = completed 2 forgotten
       in coverage . cover 50 (not (Map.null (keysOf lost))) "keys in the copy" $
            pluginSecrets first' === keysOf lost .&&. pluginSecrets end === mempty .&&. moved 2 end

  prop "waits on a locked browser without emptying a place that holds a key, and moves it all once unlocked" $
    forAll (genWorld 1) $ \w0 ->
      let w = w0 {devices = Map.map (\d -> d {vaultKept = False, keySlot = if d.keySlot == VaultKey then OwnKey else d.keySlot}) w0.devices}
          stopped = completed 1 w
          unlockedAfter = stopped {devices = Map.map (\d -> d {vaultKept = True}) stopped.devices}
          end = completed 1 unlockedAfter
       in coverage . cover 10 (isJust w.item) "a locked legacy item" $
            (heldNames w `Set.isSubsetOf` heldNames stopped)
              .&&. (isNothing w.item || isJust stopped.item)
              .&&. moved 1 end
              .&&. isNothing end.item
  where
    -- A browser's next step, or a key forgotten on a browser.
    genMove = oneof [Left <$> elements [1, 2 :: Int], (\d i -> Right (d, i)) <$> elements [1, 2 :: Int] <*> chooseInt (0, 5)]
    -- Two runs at once: each turn, the browser the schedule names takes
    -- its next step; then each runs to its end.
    interleaved schedule w = completed 2 (completed 1 (foldl' (\x d -> maybe x snd (stepOn d x)) w schedule))
