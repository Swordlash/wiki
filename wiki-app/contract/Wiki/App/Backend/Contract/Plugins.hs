-- | The contract tests of 'MonadPlugins': what every backend does with a
-- user's choices for the plugins, as tests of the rules of
-- "Wiki.App.Backend.Plugins" and "Wiki.Api.Plugins": a plugin's state and
-- its options are set apart, the options are bounded, and @null@ is no
-- options. And the external plugins ("Wiki.Api.Plugins.External"): who
-- may add them, the uploads that install and replace them, the removals,
-- and each refusal in the server's words, so that the in-memory backend
-- the settings' tests run over refuses as the server does.
--
-- Like the backend's contract ("Wiki.App.Backend.Contract", whose
-- 'Fixture' it takes), it runs against the in-memory backend and the HTTP
-- backend against the server's API, so that the demo keeps a plugin's
-- options by the server's rules.
module Wiki.App.Backend.Contract.Plugins (spec) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Protolude hiding (reader)
import Test.HUnit (assertFailure)
import Test.Hspec
import Wiki.Api.Plugins (PluginChoice (..), PluginId (..), PluginState (..), Plugins (..), maxOptionsBytes, maxPluginsPerUser, optionsNull, optionsTooLarge)
import Wiki.Api.Plugins.External (ExternalManifest (..), ExternalPlugin (..), ExternalPlugins (..), ExternalPolicy (..), Owner (..), maxBundleBytes, maxOwnPlugins)
import Wiki.App.Backend (WikiError (..))
import Wiki.App.Backend.Contract (As (..), Fixture (..), Run)
import Wiki.App.Backend.Plugins

admin, editor, reader :: As
admin = As "admin"
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
  external

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

--------------------------------------------------------------------------------
-- External plugins

-- | A manifest of a plugin of this id, as its file holds it.
manifestOf :: Text -> Text
manifestOf pid = "{\"manifest\": 1, \"id\": \"" <> pid <> "\", \"name\": \"Doubled words\", \"version\": \"1.0.0\", \"api\": 1, \"contributes\": {\"diagnostics\": {}}, \"permissions\": [\"document.read\"], \"hosts\": [\"https://api.example.org\"]}"

-- | A plugin's code.
bundleOf :: Text -> ByteString
bundleOf v = encodeUtf8 ("wikiPlugin.plugin({ diagnostics: () => [] }); // " <> v)

-- | What an installed plugin is, but for when it was installed, which is
-- the backend's clock's.
seenAs :: ExternalPlugin -> (Text, Text, Int, Bool, [Text])
seenAs p = (p.manifest.id.text, p.bundle, p.size, p.mine, p.manifest.hosts)

-- | The plugins listed to a reader, as 'seenAs' sees each.
listed :: (MonadPlugins m) => Run m -> As -> IO (ExternalPolicy, [(Text, Text, Int, Bool, [Text])])
listed run as' = run as' externalPluginsNow >>= ok <&> \l -> (l.policy, map seenAs l.external)

external :: (MonadPlugins m) => SpecWith (Fixture m)
external = describe "external plugins" $ do
  it "are off at first, and the policy is an administrator's to set" $ \(Fixture run) -> do
    listed run editor `shouldReturn` (ExternalOff, [])
    run editor (setExternalPolicy ExternalUsers) >>= (`shouldFailWith` Forbidden)
    (\l -> l.policy) <$> (run admin (setExternalPolicy ExternalWiki) >>= ok) `shouldReturn` ExternalWiki
    listed run editor `shouldReturn` (ExternalWiki, [])
    listed run Anonymous `shouldReturn` (ExternalWiki, [])

  it "are installed for everyone by an administrator, whatever the policy, and replaced by the hash of the bundle they replace" $ \(Fixture run) -> do
    run editor (installExternal TheWiki (manifestOf "doubled") (bundleOf "1") Nothing) >>= (`shouldFailWith` Forbidden)
    first' <- run admin (installExternal TheWiki (manifestOf "doubled") (bundleOf "1") Nothing) >>= ok
    seenAs first' `shouldSatisfy` (\(i, h, n, mine, hosts) -> i == "doubled" && T.length h == 64 && n == BS.length (bundleOf "1") && not mine && hosts == ["https://api.example.org"])
    -- Listed to the administrator under the policy off, and to a user
    -- once it is on.
    listed run admin `shouldReturn` (ExternalOff, [seenAs first'])
    listed run editor `shouldReturn` (ExternalOff, [])
    _ <- run admin (setExternalPolicy ExternalWiki) >>= ok
    listed run editor `shouldReturn` (ExternalWiki, [seenAs first'])
    -- Again without replaces, or with a hash it does not have.
    run admin (installExternal TheWiki (manifestOf "doubled") (bundleOf "2") Nothing)
      >>= (`shouldFailWith` Conflict ("A plugin with the id doubled is installed already: to replace it, send its bundle's hash, " <> first'.bundle <> ", as replaces."))
    run admin (installExternal TheWiki (manifestOf "doubled") (bundleOf "2") (Just "0000"))
      >>= (`shouldFailWith` Conflict ("The plugin doubled was replaced since: its bundle's hash is now " <> first'.bundle <> "."))
    run admin (installExternal TheWiki (manifestOf "other") (bundleOf "2") (Just first'.bundle))
      >>= (`shouldFailWith` Conflict "No plugin with the id other is installed, so none is replaced: send the upload without replaces.")
    second' <- run admin (installExternal TheWiki (manifestOf "doubled") (bundleOf "2") (Just first'.bundle)) >>= ok
    second'.bundle `shouldNotBe` first'.bundle
    listed run editor `shouldReturn` (ExternalWiki, [seenAs second'])
    -- The hash seen before the replacement is stale now.
    run admin (installExternal TheWiki (manifestOf "doubled") (bundleOf "3") (Just first'.bundle))
      >>= (`shouldFailWith` Conflict ("The plugin doubled was replaced since: its bundle's hash is now " <> second'.bundle <> "."))

  it "are removed for everyone by an administrator only, and once" $ \(Fixture run) -> do
    _ <- run admin (installExternal TheWiki (manifestOf "doubled") (bundleOf "1") Nothing) >>= ok
    run editor (removeExternal TheWiki (PluginId "doubled")) >>= (`shouldFailWith` Forbidden)
    run admin (removeExternal TheWiki (PluginId "doubled")) `shouldReturn` Right ()
    listed run admin `shouldReturn` (ExternalOff, [])
    run admin (removeExternal TheWiki (PluginId "doubled")) >>= (`shouldFailWith` NotFound)

  it "are a user's own while users may add theirs, listed to their owner only, and never with an id of the wiki's" $ \(Fixture run) -> do
    run editor (installExternal Mine (manifestOf "mine") (bundleOf "1") Nothing) >>= (`shouldFailWith` Forbidden)
    _ <- run admin (setExternalPolicy ExternalUsers) >>= ok
    run Anonymous (installExternal Mine (manifestOf "mine") (bundleOf "1") Nothing) >>= (`shouldFailWith` Forbidden)
    own <- run editor (installExternal Mine (manifestOf "mine") (bundleOf "1") Nothing) >>= ok
    own.mine `shouldBe` True
    listed run editor `shouldReturn` (ExternalUsers, [seenAs own])
    listed run reader `shouldReturn` (ExternalUsers, [])
    -- An id the wiki has is the wiki's.
    _ <- run admin (installExternal TheWiki (manifestOf "spell") (bundleOf "1") Nothing) >>= ok
    run editor (installExternal Mine (manifestOf "spell") (bundleOf "1") Nothing)
      >>= (`shouldFailWith` Conflict "The wiki has a plugin with the id spell, installed for everyone: a plugin of your own needs another id.")
    -- Under the policy wiki, the user's own are kept and not listed, and
    -- removed all the same.
    _ <- run admin (setExternalPolicy ExternalWiki) >>= ok
    map (\(i, _, _, mine, _) -> (i, mine)) . snd <$> listed run editor `shouldReturn` [("spell", False)]
    run editor (removeExternal Mine (PluginId "mine")) `shouldReturn` Right ()
    run editor (removeExternal Mine (PluginId "mine")) >>= (`shouldFailWith` NotFound)

  it "are at most twenty of a user's own" $ \(Fixture run) -> do
    _ <- run admin (setExternalPolicy ExternalUsers) >>= ok
    for_ [1 .. maxOwnPlugins] $ \n -> run editor (installExternal Mine (manifestOf ("p" <> show n)) (bundleOf (show n)) Nothing) >>= ok
    run editor (installExternal Mine (manifestOf "one-more") (bundleOf "x") Nothing)
      >>= (`shouldFailWith` Conflict "You have 20 plugins installed already, the most it may: remove one first.")

  it "refuse a manifest or a bundle that breaks the rules, with the rule, before anything else" $ \(Fixture run) -> do
    let refusedWith as' manifest bundle = run as' (installExternal TheWiki manifest bundle Nothing)
    refusedWith admin "{}" (bundleOf "1") >>= (`shouldFailWith` Invalid "A plugin's manifest has the field manifest.")
    refusedWith admin (manifestOf "errors") (bundleOf "1") >>= (`shouldFailWith` Invalid "The id errors is a plugin's built into the wiki: choose another.")
    refusedWith admin (manifestOf "doubled") "  " >>= (`shouldFailWith` Invalid "A plugin's bundle is empty.")
    refusedWith admin (manifestOf "doubled") (BS.replicate (maxBundleBytes + 1) 32) >>= (`shouldFailWith` Invalid "A plugin's bundle is at most 2097152 bytes (2 MiB).")
    -- The upload is read before anyone is asked for.
    refusedWith editor "{}" (bundleOf "1") >>= (`shouldFailWith` Invalid "A plugin's manifest has the field manifest.")
    listed run admin `shouldReturn` (ExternalOff, [])
