-- | The machines of the move of the assistant's legacy settings: the
-- assistant's settings as a surface holds them
-- ("Wiki.App.Plugin.Assistant.Settings"), the shell's runs of the move
-- ("Wiki.App.View.Migration.Machine"), and the options a move finds, as
-- the plugins' state adopts them ("Wiki.App.View.Plugins.Machine").
module Test.AssistantSettings (spec) where

import Data.Aeson (Value (..), object, (.=))
import Data.Map.Strict qualified as Map
import Protolude
import Test.Hspec
import Test.QuickCheck (Gen, chooseInt, counterexample, forAll, frequency, listOf, (.&&.), (===))
import Wiki.Api.Plugins (PluginChoice (..), PluginId (..), PluginState (..), Plugins (..))
import Wiki.App.Assistant.Migrate (Outcome (..))
import Wiki.App.Plugin (SecretName (..), Unavailable (..))
import Wiki.App.Plugin.Assistant.Settings qualified as S
import Wiki.App.View.Migration.Machine qualified as M
import Wiki.App.View.Plugins.Machine qualified as P
import Wiki.Core.Assistant (Mode (..), Provider (..), Settings (..), chooseProvider, defaultSettings, withMode, withReached)
import Wiki.Core.Assistant.Split (optionsValue)
import Wiki.Core.Id (UserId (..))

spec :: Spec
spec = do
  settingsMachine
  migrationMachine
  adopting

alice, bob :: UserId
alice = UserId "user-alice"
bob = UserId "user-bob"

--------------------------------------------------------------------------------
-- The assistant's settings

-- | What a run of the settings' machine meets: the keys as the secrets
-- hold them now, which change; the asks out, in the order made.
data Sim = Sim
  { model :: S.Model
  , keys :: Map SecretName Text
  , cache :: Maybe Value
  , out :: [S.Effect]
  , raised :: [Maybe Value]
  }

-- | A move of the run.
data Move = Change Int | KeysChanged Int | Changed' Int | Answer Int
  deriving stock (Show)

genMove :: Gen Move
genMove =
  frequency
    [ (2, KeysChanged <$> chooseInt (0, 3))
    , (2, Changed' <$> chooseInt (0, 3))
    , (5, Answer <$> chooseInt (0, 3))
    , (1, Change <$> chooseInt (0, 3))
    ]

keysNumbered :: Int -> Map SecretName Text
keysNumbered n = Map.fromList [(SecretName "anthropic", "sk-" <> show n) | n > 0]

-- | A change of the settings, by number: options, or what was learnt.
changeOf :: Int -> Settings -> Settings
changeOf = \case
  0 -> chooseProvider OpenAI
  1 -> withMode Ask
  2 -> withReached Anthropic False
  _ -> withReached OpenAI False

-- | Step the machine, and note its asks.
stepSim :: S.Event -> Sim -> Sim
stepSim e s =
  let (m, effects) = S.step e s.model
   in s {model = m, out = s.out <> [x | x <- effects, isAsk x], raised = s.raised <> [o | S.SetOptions o <- effects]}
  where
    isAsk = \case
      S.ReadKeys _ -> True
      S.ReadCache _ -> True
      S.SaveCache _ _ -> True
      _ -> False

-- | Answer the ask out at this place, as the host would now.
answer :: Int -> Sim -> Sim
answer i s = case splitAt (i `mod` max 1 (length s.out)) s.out of
  (earlier, x : later) ->
    let s' = s {out = earlier <> later}
     in case x of
          S.ReadKeys n -> stepSim (S.KeysRead n (Right s.keys)) s'
          S.ReadCache n -> stepSim (S.CacheRead n s.cache) s'
          S.SaveCache n v -> stepSim (S.CacheSaved n (Right ())) s' {cache = Just v}
          _ -> s'
  _ -> s

runMoves :: [Move] -> Sim
runMoves = foldl' moveOn start
  where
    start = stepSim (S.Opened (1, Just alice) (Just (optionsValue defaultSettings)) Nothing) Sim {model = S.initial (1, Just alice) Nothing, keys = keysNumbered 1, cache = Nothing, out = [], raised = []}
    moveOn s = \case
      KeysChanged n -> stepSim S.SecretsChanged s {keys = keysNumbered n}
      Changed' n -> stepSim (S.Changed (changeOf n)) s
      Change n -> stepSim (S.Changed (changeOf n)) s
      Answer i -> answer i s

-- | Answer every ask, oldest first, until none is out.
drain :: Sim -> Sim
drain s = if null s.out then s else drain (answer 0 s)

settingsMachine :: Spec
settingsMachine = describe "Wiki.App.Plugin.Assistant.Settings" $ do
  it "keeps one read of the keys, one of the cache and one write of the cache out at a time, and ends with the keys the secrets hold and the cache last written" $
    forAll (listOf genMove) $ \moves ->
      let s = runMoves moves
          end = drain s
          kinds = map kindOf s.out
       in counterexample (show moves) $
            (length (filter (== "keys") kinds) <= 1)
              .&&. (length (filter (== "cache") kinds) <= 1)
              .&&. (length (filter (== "save") kinds) <= 1)
              .&&. (end.model.keys === Just (Right end.keys))
              .&&. (join end.model.learnt === end.cache)

  it "gives the host the options a change makes, and keeps them until the host gives others" $ do
    let s0 = drain (runMoves [])
        s1 = stepSim (S.Changed (chooseProvider OpenAI)) s0
    s1.raised `shouldBe` [Just (optionsValue (chooseProvider OpenAI defaultSettings))]
    (\x -> x.provider) <$> S.settings s1.model `shouldBe` Just OpenAI

  it "reads again for another reader, and drops an answer asked for the former one" $ do
    let s0 = runMoves []
        -- Bob signs in while Alice's reads are out.
        (m1, e1) = S.step (S.Given (2, Just bob) Nothing) s0.model
        (m2, _) = S.step (S.KeysRead 0 (Right (keysNumbered 3))) m1
    e1 `shouldBe` []
    m2.keys `shouldBe` Nothing
    m2.keysOut `shouldSatisfy` isJust

  it "says the keys are locked, and has no key, while the vault is locked" $ do
    let s0 = runMoves []
        (m1, _) = S.step (S.KeysRead 0 (Left Locked)) s0.model
        (m2, _) = S.step (S.CacheRead 1 Nothing) m1
    S.lockedKeys m2 `shouldBe` Just Locked
    S.settings m2 `shouldBe` Just defaultSettings
  where
    kindOf = \case
      S.ReadKeys _ -> "keys" :: Text
      S.ReadCache _ -> "cache"
      _ -> "save"

--------------------------------------------------------------------------------
-- The shell's runs of the move

migrationMachine :: Spec
migrationMachine = describe "Wiki.App.View.Migration.Machine" $ do
  it "runs for the reader given, one run at a time, and again for a change heard while one is out" $ do
    let (m1, e1) = M.step (M.Given 1 (Just alice)) M.initial
        (m2, e2) = M.step M.Nudged m1
        (m3, e3) = M.step (M.Answered 0 (Right Waiting)) m2
    e1 `shouldBe` [M.Run 0 alice]
    e2 `shouldBe` []
    e3 `shouldBe` [M.Run 1 alice]
    m3.out `shouldBe` Just (1, 1)

  it "gives the options a run found, with the session they were found in" $ do
    let (m1, _) = M.step (M.Given 1 (Just alice)) M.initial
        (_, e2) = M.step (M.Answered 0 (Right (Adopt (String "options")))) m1
    e2 `shouldBe` [M.AdoptOptions 1 (String "options")]

  it "adopts nothing a run of an earlier reader found, and runs for the new reader once it answers" $ do
    let (m1, _) = M.step (M.Given 1 (Just alice)) M.initial
        (m2, e2) = M.step (M.Given 2 (Just bob)) m1
        (_, e3) = M.step (M.Answered 0 (Right (Adopt (String "alice's")))) m2
    e2 `shouldBe` []
    e3 `shouldBe` [M.Run 1 bob]

  it "runs no more once nothing is left, until another session" $ do
    let (m1, _) = M.step (M.Given 1 (Just alice)) M.initial
        (m2, _) = M.step (M.Answered 0 (Right Moved)) m1
        (_, e3) = M.step M.Nudged m2
        (_, e4) = M.step (M.Given 2 (Just alice)) m2
    e3 `shouldBe` []
    e4 `shouldBe` [M.Run 1 alice]

  it "runs for nobody" $
    snd (M.step (M.Given 1 Nothing) M.initial) `shouldBe` []

--------------------------------------------------------------------------------
-- The options a move finds

adopting :: Spec
adopting = describe "the plugins' state adopting the options a move found" $ do
  let assistant = PluginId "assistant"
      found = object ["provider" .= ("openai" :: Text)]
      kept = object ["provider" .= ("anthropic" :: Text)]
      none = Plugins {offForWiki = mempty, mine = mempty}
      withOptions = Plugins {offForWiki = mempty, mine = Map.singleton assistant PluginChoice {state = PluginDefault, options = Just kept}}
      writes effects = [o | P.WriteOptions _ _ o <- effects]
  it "writes them when the server holds none for the plugin" $
    writes (snd (P.step (P.OptionsAdopted 0 assistant found) (P.initialWith (Just none) 0))) `shouldBe` [Just found]

  it "writes nothing when the server holds options for the plugin, which another browser moved or the user chose" $
    -- Shown able to fail: the same step writes when there are none (above).
    writes (snd (P.step (P.OptionsAdopted 0 assistant found) (P.initialWith (Just withOptions) 0))) `shouldBe` []

  it "waits for what is on to be read, then writes them only if the read found none" $ do
    let (m1, e1) = P.step (P.OptionsAdopted 0 assistant found) (P.initial 0)
        (m2, e2) = P.step P.Refresh m1
        ticket = fromMaybe (panic "no read") (head [a | P.Load a <- e2])
    writes e1 `shouldBe` []
    writes (snd (P.step (P.Loaded ticket (Right none)) m2)) `shouldBe` [Just found]
    writes (snd (P.step (P.Loaded ticket (Right withOptions)) m2)) `shouldBe` []

  it "drops options found in an earlier session" $
    writes (snd (P.step (P.OptionsAdopted 0 assistant found) (P.initialWith (Just none) 1))) `shouldBe` []
