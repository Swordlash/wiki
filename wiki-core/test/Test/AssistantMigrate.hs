-- | The plan that moves the writing assistant's settings from their legacy
-- places to the plugin's ("Wiki.Core.Assistant.Migrate"), run against a
-- model of those places: the vault's legacy item and the plugin's item,
-- the server's options, and each browser's record, slots of keys, cache
-- and "ask each time". A step changes the model as the application's
-- runner changes the real places ("Wiki.App.Assistant.Migrate"): a
-- conditional delete deletes only what was read, a key the plugin's
-- secrets hold keeps its value, options are taken only when the server
-- has none. A run reads the model, takes the plan's first step, and reads
-- again, as the runner does.
module Test.AssistantMigrate (spec) where

import Data.Aeson (Value)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
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
import Wiki.Core.Vault.Plugin (SecretName, foldIn)

--------------------------------------------------------------------------------
-- The model

-- | 'World' is every place the settings can be.
data World = World
  { item :: Maybe (Int, Settings)
  -- ^ The vault's legacy item, by the number of its seal.
  , plugin :: Map SecretName Text
  -- ^ The plugin's item; empty is none.
  , serverOptions :: Maybe Value
  , devices :: Map Int Dev
  , seals :: Int
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
  , copy :: Maybe Settings
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
    , pluginItem = not (Map.null w.plugin)
    , record =
        ( \r ->
            OnDevice
              { bytes = show r.number
              , own = r.own
              , copy = if unlocked dev then r.copy else Nothing
              , follows = r.follows
              , unopenable = False
              , askEachTime = r.askEachTime
              , learnt = r.learnt
              }
        )
          <$> dev.held
    , slot = dev.keySlot
    , vaultSlot = dev.vaultKept
    , target = if unlocked dev then Just w.plugin else Nothing
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
  KeepKeys ks
    | unlocked dev -> w {plugin = foldIn w.plugin ks}
    | otherwise -> w
  KeepLearnt _ -> onDev (\x -> x {cached = True})
  AdoptOptions v -> w {serverOptions = w.serverOptions <|> Just v}
  DropVaultItem s
    | (sealOf . fst <$> w.item) == Just s -> w {item = Nothing}
    | otherwise -> w
  DropRecord b -> onDev (\x -> if ((\r -> show r.number) <$> x.held) == Just b then x {held = Nothing} else x)
  DropSlotKey -> onDev (\x -> if isNothing x.held then x {keySlot = NoKey} else x)
  where
    dev = deviceOf d w
    onDev f = w {devices = Map.adjust f d w.devices}

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
-- vault holds the legacy item or the plugin's, the copy is the vault's,
-- and a key forgotten there since is not to come back from it.
heldNames :: World -> Set SecretName
heldNames w =
  Set.unions
    ( Map.keysSet w.plugin
        : maybe mempty (Map.keysSet . keysOf . snd) w.item
        : [Set.unions [maybe mempty (Map.keysSet . keysOf) r.own, if lastCopy then maybe mempty (Map.keysSet . keysOf) r.copy else mempty] | Just r <- map (\x -> x.held) (Map.elems w.devices)]
    )
  where
    lastCopy = isNothing w.item && Map.null w.plugin

-- | Settings that are not the defaults: in Ask mode.
settingsAsked :: Settings
settingsAsked = withMode Ask defaultSettings

-- | Whether a browser holds nothing legacy any more.
moved :: Int -> World -> Bool
moved d w = let x = deviceOf d w in isNothing x.held && x.keySlot == NoKey

--------------------------------------------------------------------------------
-- Generators

-- | One browser in one of the legacy states, or with none.
genDev :: Maybe Settings -> Gen (Dev, Maybe Settings)
genDev vaultItem =
  oneof
    [ pure (Dev Nothing NoKey True False False, Nothing)
    , l1
    , ownRecord OwnKey True -- L2
    , ownRecord NoKey True -- L3
    , l4
    ]
  where
    l1 = do
      -- The copy is the vault's item, or an older version of it, or the
      -- last copy when the vault has none.
      copy <- maybe (Just <$> genSettings) (pure . Just) vaultItem
      inSlot <- arbitrary
      r <- record Nothing True copy
      pure (Dev (Just r) VaultKey (not inSlot) False False, Nothing)
    ownRecord slot vaultSlot' = do
      s <- genSettings
      r <- record (Just s) False Nothing
      pure (Dev (Just r) slot vaultSlot' False False, Just s)
    -- The copy is sealed with the vault key, which this browser does not
    -- hold: it opens once the browser is unlocked.
    l4 = do
      typed <- oneof [pure Nothing, Just <$> genSettings]
      copy <- maybe (Just <$> genSettings) (pure . Just) vaultItem
      r <- record typed True copy
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
  devs <- vectorOf n (fst <$> genDev vaultItem)
  plugin <- oneof [pure mempty, keysOf <$> genSettings]
  options <- oneof [pure Nothing, Just . optionsValue <$> genSettings]
  pure World {item = (1,) <$> vaultItem, plugin, serverOptions = options, devices = Map.fromList (zip [1 ..] (map dedupe (zip [1 ..] devs))), seals = 2}
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
              .&&. end.plugin === foldIn w.plugin (legacyKeys found)
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
              .&&. Map.keysSet together.plugin === Map.keysSet ab.plugin
              .&&. conjoin [counterexample (show k) (Just v `elem` [Map.lookup k ab.plugin, Map.lookup k ba.plugin]) | (k, v) <- Map.toList together.plugin]
              .&&. counterexample "options" (together.serverOptions `elem` [ab.serverOptions, ba.serverOptions])

  prop "two unlocked browsers that follow the vault and hold nothing of their own end as one run" $
    forAll (genUnlocked 2) $ \w0 ->
      let w = w0 {devices = Map.map followsOnly w0.devices}
          followsOnly d = d {held = (\r -> Rec {number = r.number, own = Nothing, follows = True, copy = snd <$> w0.item, askEachTime = r.askEachTime, learnt = r.learnt}) <$> d.held}
       in forAll (listOf (elements [1, 2 :: Int])) $ \schedule ->
            let together = interleaved schedule w
                inTurn = completed 2 (completed 1 w)
             in (together.plugin, together.serverOptions, together.item) === (inTurn.plugin, inTurn.serverOptions, inTurn.item)

  prop "moves the legacy item an earlier tab adds again, the plugin's keys winning" $
    forAll (genUnlocked 1) $ \w ->
      forAll genSettings $ \readdedSettings ->
        let first' = completed 1 w
            readded = first' {item = Just (first'.seals, readdedSettings), seals = first'.seals + 1}
            end = completed 1 readded
         in isNothing end.item .&&. end.plugin === foldIn first'.plugin (keysOf readdedSettings)

  it "gives the options to adopt before the server's are known, and empties nothing until they are" $ do
    let s = settingsAsked
        found' = Found {vault = NoItem, pluginItem = False, record = Just OnDevice {bytes = "r", own = Just s, copy = Nothing, follows = False, unopenable = False, askEachTime = False, learnt = Nothing}, slot = OwnKey, vaultSlot = True, target = Just mempty, options = Nothing, local = False, askHere = False}
    plan found' `shouldBe` [AdoptOptions (optionsValue s)]
    plan found' {options = Just (Just (optionsValue s))} `shouldBe` [DropRecord "r"]

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
    -- Two runs at once: each turn, the browser the schedule names takes
    -- its next step; then each runs to its end.
    interleaved schedule w = completed 2 (completed 1 (foldl' (\x d -> maybe x snd (stepOn d x)) w schedule))
