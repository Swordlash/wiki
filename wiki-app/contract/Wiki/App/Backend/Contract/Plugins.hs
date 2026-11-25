-- | The contract tests of 'MonadPlugins': what every backend does with a
-- user's choices for the plugins, as tests of the rules of
-- "Wiki.App.Backend.Plugins" and "Wiki.Api.Plugins": a plugin's state and
-- its options are set apart, the options are bounded, and @null@ is no
-- options.
--
-- Like the backend's contract ("Wiki.App.Backend.Contract", whose
-- 'Fixture' it takes), it runs against the in-memory backend and the HTTP
-- backend against the server's API, so that the demo keeps a plugin's
-- options by the server's rules.
module Wiki.App.Backend.Contract.Plugins (spec) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Protolude hiding (reader)
import Test.HUnit (assertFailure)
import Test.Hspec
import Wiki.Api.Plugins (PluginChoice (..), PluginId (..), PluginState (..), Plugins (..), maxOptionsBytes, maxPluginsPerUser, optionsNull, optionsTooLarge)
import Wiki.App.Backend (WikiError (..))
import Wiki.App.Backend.Contract (As (..), Fixture (..))
import Wiki.App.Backend.Plugins

editor, reader :: As
editor = As "editor"
reader = As "reader"

assistant, errors :: PluginId
assistant = PluginId "assistant"
errors = PluginId "errors"

-- | The contract, for a backend made afresh for each example, as in
-- "Wiki.App.Backend.Contract".
spec :: forall m -> (MonadPlugins m) => (ActionWith (Fixture m) -> IO ()) -> Spec
spec _ withFixture = around withFixture $ describe "MonadPlugins contract" $ do
  options
  refusing

--------------------------------------------------------------------------------
-- Helpers

ok :: (HasCallStack, Show e) => Either e a -> IO a
ok = either (\e -> assertFailure ("expected success, got " <> show e)) pure

shouldFailWith :: (HasCallStack, Show a) => Either WikiError a -> WikiError -> Expectation
shouldFailWith result e = case result of
  Left e' -> e' `shouldBe` e
  Right a -> expectationFailure ("expected " <> show e <> ", got Right " <> show a)

chose :: PluginState -> Maybe Value -> PluginChoice
chose s o = PluginChoice {state = s, options = o}

-- | Some options, as a plugin keeps them.
someOptions :: Value
someOptions = object ["provider" .= ("anthropic" :: Text), "mode" .= ("ask" :: Text)]

-- | A string whose JSON, quotes included, is this many bytes long.
sized :: Int -> Value
sized n = String (T.replicate (n - 2) "a")

--------------------------------------------------------------------------------
-- Options

options :: (MonadPlugins m) => SpecWith (Fixture m)
options = describe "a plugin's options" $ do
  it "are set and cleared apart from its state, and read with the choices" $ \(Fixture run) -> do
    run editor (setPluginOptions assistant someOptions) `shouldReturn` Right (chose PluginDefault (Just someOptions))
    _ <- run editor (setPluginState assistant PluginOff) >>= ok
    mine <- (\p -> p.mine) <$> (run editor pluginsNow >>= ok)
    mine `shouldBe` Map.fromList [(assistant, chose PluginOff (Just someOptions))]
    -- Another user's choices are their own.
    ((\p -> p.mine) <$> (run reader pluginsNow >>= ok)) `shouldReturn` mempty
    -- Replaced whole, then cleared; the state stays as it was.
    let other = object ["v" .= (2 :: Int)]
    run editor (setPluginOptions assistant other) `shouldReturn` Right (chose PluginOff (Just other))
    run editor (clearPluginOptions assistant) `shouldReturn` Right (chose PluginOff Nothing)

  it "keep a plugin given back to its default, until they are cleared too" $ \(Fixture run) -> do
    _ <- run editor (setPluginOptions errors (toJSON [1, 2, 3 :: Int])) >>= ok
    _ <- run editor (setPluginState errors PluginOn) >>= ok
    run editor (setPluginState errors PluginDefault) `shouldReturn` Right (chose PluginDefault (Just (toJSON [1, 2, 3 :: Int])))
    run editor (clearPluginOptions errors) `shouldReturn` Right (chose PluginDefault Nothing)
    ((\p -> p.mine) <$> (run editor pluginsNow >>= ok)) `shouldReturn` mempty
    -- Clearing what is not there changes nothing.
    run editor (clearPluginOptions errors) `shouldReturn` Right (chose PluginDefault Nothing)

  it "are written where the user has none only, when asked so, and a state alone is none" $ \(Fixture run) -> do
    _ <- run editor (setPluginState assistant PluginOn) >>= ok
    run editor (setPluginOptionsIfNone assistant someOptions) `shouldReturn` Right (chose PluginOn (Just someOptions))
    let other = object ["v" .= (2 :: Int)]
    run editor (setPluginOptionsIfNone assistant other) >>= \case
      Left (Conflict _) -> pass
      r -> expectationFailure ("expected a conflict, got " <> show r)
    ((\p -> p.mine) <$> (run editor pluginsNow >>= ok)) `shouldReturn` Map.fromList [(assistant, chose PluginOn (Just someOptions))]
    run editor (setPluginOptionsIfNone errors other) `shouldReturn` Right (chose PluginDefault (Just other))
    run editor (setPluginOptionsIfNone errors Null) >>= (`shouldFailWith` Invalid optionsNull)

  it "are at most 16 kB of JSON" $ \(Fixture run) -> do
    run editor (setPluginOptions assistant (sized maxOptionsBytes)) `shouldReturn` Right (chose PluginDefault (Just (sized maxOptionsBytes)))
    run editor (setPluginOptions assistant (sized (maxOptionsBytes + 1))) >>= (`shouldFailWith` Invalid optionsTooLarge)
    -- What was kept stays.
    ((\p -> p.mine) <$> (run editor pluginsNow >>= ok)) `shouldReturn` Map.fromList [(assistant, chose PluginDefault (Just (sized maxOptionsBytes)))]

--------------------------------------------------------------------------------
-- Refusals

refusing :: (MonadPlugins m) => SpecWith (Fixture m)
refusing = describe "refuses" $ do
  it "null as options, which DELETE clears" $ \(Fixture run) -> do
    run editor (setPluginOptions assistant Null) >>= (`shouldFailWith` Invalid optionsNull)
    ((\p -> p.mine) <$> (run editor pluginsNow >>= ok)) `shouldReturn` mempty

  it "options by nobody signed in" $ \(Fixture run) -> do
    run Anonymous (setPluginOptions assistant someOptions) >>= (`shouldFailWith` Forbidden)
    run Anonymous (clearPluginOptions assistant) >>= (`shouldFailWith` Forbidden)
    -- The body is read before anyone is asked for: a body refused is
    -- refused for nobody signed in too.
    run Anonymous (setPluginOptions assistant Null) >>= (`shouldFailWith` Invalid optionsNull)
    run Anonymous (setPluginOptions assistant (sized (maxOptionsBytes + 1))) >>= (`shouldFailWith` Invalid optionsTooLarge)

  it "a first choice past the most a user keeps, options or state alike" $ \(Fixture run) -> do
    for_ [PluginId ("p" <> show n) | n <- [1 .. maxPluginsPerUser]] $ \p ->
      run reader (setPluginOptions p (object [])) >>= ok
    why <- run reader (setPluginState (PluginId "one-more") PluginOn)
    run reader (setPluginOptions (PluginId "one-more") (object [])) `shouldReturn` why
    isLeft why `shouldBe` True
    -- A plugin kept already may change, and one cleared makes room.
    run reader (setPluginOptions (PluginId "p1") someOptions) `shouldReturn` Right (chose PluginDefault (Just someOptions))
    _ <- run reader (clearPluginOptions (PluginId "p1")) >>= ok
    run reader (setPluginOptions (PluginId "one-more") (object [])) `shouldReturn` Right (chose PluginDefault (Just (object [])))
