-- | External plugins' rules ("Wiki.Api.Plugins.External"): the manifests
-- accepted and each one refused with its reason, the bundles, the policy's
-- names, the frame's address, and the frame's Content-Security-Policy
-- ("Wiki.Api.ContentPolicy.pluginFramePolicy"), which no host a manifest
-- may name can change beyond its @connect-src@.
module Test.ExternalPlugins (spec) where

import Data.Aeson (Value, encode, object, toJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import Protolude
import Test.Hspec
import Test.QuickCheck
import Wiki.Api.ContentPolicy (directives, pluginFramePolicy)
import Wiki.Api.Plugins (PluginId (..))
import Wiki.Api.Plugins.External

-- | The manifest of the guide's example, as JSON.
doubledWords :: [(Text, Value)]
doubledWords =
  [ ("manifest", toJSON (1 :: Int))
  , ("id", "doubled-words")
  , ("name", "Doubled words")
  , ("version", "1.0.0")
  , ("description", "Marks a word written twice.")
  , ("api", toJSON (1 :: Int))
  , ("contributes", object ["diagnostics" .= object []])
  , ("permissions", toJSON ["document.read" :: Text])
  , ("hosts", toJSON ["https://api.languagetool.org" :: Text])
  ]

-- | A manifest's bytes, with some of the example's fields replaced or
-- taken out ('Nothing').
manifestWith :: [(Text, Maybe Value)] -> ByteString
manifestWith changes =
  BSL.toStrict . encode . object $
    [(Key.fromText k, v) | (k, v) <- doubledWords, isNothing (lookup' k)]
      <> [(Key.fromText k, v) | (k, Just v) <- changes]
  where
    lookup' k = find ((== k) . fst) changes

-- | The reason a manifest is refused, or a failure that says it was not.
refusal :: (HasCallStack) => ByteString -> IO Text
refusal bytes = case parseManifest bytes of
  Left why -> pure why
  Right m -> expectationFailure ("accepted: " <> show m) >> pure ""

refusedFor :: (HasCallStack) => [(Text, Maybe Value)] -> Text -> Expectation
refusedFor changes fragment = do
  why <- refusal (manifestWith changes)
  unless (fragment `T.isInfixOf` why) (expectationFailure (toS ("refused, but not for " <> fragment <> ": " <> why)))

spec :: Spec
spec = describe "Wiki.Api.Plugins.External" $ do
  manifests
  bundles
  policies
  frames

manifests :: Spec
manifests = describe "a plugin's manifest" $ do
  it "reads the guide's example" $
    parseManifest (manifestWith [])
      `shouldBe` Right
        ExternalManifest
          { id = PluginId "doubled-words"
          , name = "Doubled words"
          , version = "1.0.0"
          , description = "Marks a word written twice."
          , api = 1
          , contributes = Set.singleton Diagnostics
          , permissions = Set.singleton DocumentRead
          , hosts = ["https://api.languagetool.org"]
          }

  it "takes no description, no hosts, and a host's address as an origin in lower case" $ do
    m <- either (\why -> expectationFailure (toS why) >> panic "refused") pure (parseManifest (manifestWith [("description", Nothing), ("hosts", Just (toJSON ["HTTPS://Example.org/api/v1" :: Text, "http://localhost:8787"]))]))
    m.description `shouldBe` ""
    m.hosts `shouldBe` ["https://example.org", "http://localhost:8787"]
    parseManifest (manifestWith [("hosts", Nothing)]) `shouldSatisfy` either (const False) (\x -> null x.hosts)

  it "writes itself as it reads itself" $
    case parseManifest (manifestWith []) of
      Left why -> expectationFailure (toS why)
      Right m -> parseManifest (BSL.toStrict (encode m)) `shouldBe` Right m

  it "refuses what is not one object of JSON, or is too large" $ do
    why <- refusal "not json"
    why `shouldSatisfy` T.isInfixOf "is JSON"
    refusal "[1, 2]" >>= (`shouldSatisfy` T.isInfixOf "a JSON object")
    refusedFor [("description", Just (toJSON (T.replicate (maxManifestBytes + 1) "a")))] "at most 16384 bytes"

  it "refuses a field it does not know, so that a later format is not half read" $
    refusedFor [("icon", Just "x.svg")] "no field icon"

  it "refuses a missing field, and one of the wrong kind" $ do
    for_ ["manifest", "id", "name", "version", "api", "contributes"] $ \f ->
      refusedFor [(f, Nothing)] ("has the field " <> f)
    refusedFor [("api", Just "1")] "is a whole number"
    refusedFor [("name", Just (toJSON (3 :: Int)))] "is a string"

  it "refuses another format, and a protocol this wiki does not speak" $ do
    refusedFor [("manifest", Just (toJSON (2 :: Int)))] "manifests of format 1"
    refusedFor [("api", Just (toJSON (2 :: Int)))] "speaks version 2"

  it "refuses an id that is not one, and the id of a plugin built into the wiki" $ do
    refusedFor [("id", Just "Doubled Words")] "The manifest's id"
    for_ (Set.toList reservedPluginIds) $ \p -> refusedFor [("id", Just (toJSON p.text))] "built into the wiki"

  it "keeps a name and a description to one line of their lengths" $ do
    refusedFor [("name", Just "   ")] "1 to 80 characters"
    refusedFor [("name", Just (toJSON (T.replicate 81 "n")))] "1 to 80 characters"
    refusedFor [("name", Just "two\nlines")] "one line"
    refusedFor [("description", Just (toJSON (T.replicate 301 "d")))] "0 to 300 characters"

  it "takes a version of three numbers" $ do
    for_ ["1.0", "1.0.0.0", "1.0.x", "v1.0.0", "1..0", "1234567890.0.0"] $ \v ->
      refusedFor [("version", Just (toJSON (v :: Text)))] "three numbers"
    parseManifest (manifestWith [("version", Just "10.2.33")]) `shouldSatisfy` isRight

  it "refuses an agent, which is not supported yet, and any other contribution" $ do
    refusedFor [("contributes", Just (object ["agent" .= object []]))] "agent yet"
    refusedFor [("contributes", Just (object ["diagnostics" .= object [], "agent" .= object []]))] "agent yet"
    refusedFor [("contributes", Just (object ["block" .= object []]))] "not block"
    refusedFor [("contributes", Just (object []))] "contributes something"
    refusedFor [("contributes", Just (object ["diagnostics" .= True]))] "an empty object"

  it "asks for exactly the permissions its contributions need" $ do
    refusedFor [("permissions", Nothing)] "need the permission document.read"
    refusedFor [("permissions", Just (toJSON ["document.read", "network" :: Text]))] "not network"
    refusedFor [("permissions", Just (toJSON ["document.read", "document.read" :: Text]))] "each permission once"

  it "names at most ten hosts, each an origin, each once" $ do
    refusedFor [("hosts", Just (toJSON ["https://example.org; script-src *" :: Text]))] "is not an origin"
    refusedFor [("hosts", Just (toJSON ["*" :: Text]))] "is not an origin"
    refusedFor [("hosts", Just (toJSON ["https:" :: Text]))] "is not an origin"
    refusedFor [("hosts", Just (toJSON ["ftp://example.org" :: Text]))] "is not an origin"
    refusedFor [("hosts", Just (toJSON ["https://example.org", "https://EXAMPLE.org/" :: Text]))] "each host once"
    refusedFor [("hosts", Just (toJSON ["https://h" <> show i <> ".example.org" :: Text | i <- [1 .. maxHosts + 1 :: Int]]))] "at most 10 hosts"

  it "lets no host it accepts change the frame's policy beyond connect-src" $
    checkCoverage $
      forAll (choose (1, 3) >>= (`vectorOf` hostText)) $ \typed ->
        let parsed = parseManifest (manifestWith [("hosts", Just (toJSON typed))])
         in cover 2 (isRight parsed) "some hosts accepted" $ case parsed of
              Left _ -> property True
              Right m ->
                let policy = pluginFramePolicy m.hosts ["aGFzaA=="]
                 in counterexample (toS policy) $
                      map fst (directives policy) === frameDirectives
                        .&&. directiveIn "connect-src" (directives policy) === Just m.hosts

bundles :: Spec
bundles = describe "a plugin's bundle" $ do
  it "is JavaScript in UTF-8, kept as it is sent" $ do
    let code = "self.onmessage = () => {};\r\n// ünïcode </script>" :: Text
    checkBundle (encodeUtf8 code) `shouldBe` Right code
  it "is refused empty, in another encoding, or over 2 MiB" $ do
    checkBundle " \n\t" `shouldBe` Left "A plugin's bundle is empty."
    checkBundle "\xff\xfe" `shouldBe` Left "A plugin's bundle is JavaScript in UTF-8."
    checkBundle (BS.replicate (maxBundleBytes + 1) 0x61) `shouldSatisfy` either (T.isInfixOf "2 MiB") (const False)
    checkBundle (BS.replicate maxBundleBytes 0x61) `shouldSatisfy` isRight

  it "asks for strict mode exactly when its directive prologue holds a \"use strict\" directive" $ do
    let comments = T.replicate (prologueWindow `div` 8) "/* ab */"
    for_
      [ ("\"use strict\";\nwikiPlugin.plugin({});", True)
      , ("'use strict';", True)
      , ("\"use strict\"", True)
      , ("// a comment\n/* another\n */ \"use strict\";", True)
      , ("#!/usr/bin/env node\n\"use strict\";", True)
      , ("\"use asm\"; \"use strict\";", True)
      , ("\"use asm\"\n'use strict'\nf();", True)
      , ("\"use strict\"\nf();", True)
      , ("\"use strict\"\n\"another\";", True)
      , ("\"use strict\"; \"a\" + b;", True)
      , ("\t\x00A0\xFEFF\x2028\"use strict\";", True)
      , ("", False)
      , ("wikiPlugin.plugin({});", False)
      , ("x = 1; \"use strict\";", False)
      , ("'use\\x20strict';", False)
      , ("\"use \\\nstrict\";", False)
      , ("\"use strict\" + x;", False)
      , ("\"use strict\".length;", False)
      , ("(\"use strict\");", False)
      , ("\"use strict\"\n(f)();", False)
      , ("\"use strict\"\n+ x;", False)
      , ("\"use strict\"\ninstanceof X;", False)
      , ("\"use strict\n\";", False)
      , ("\"use strict", False)
      , ("/* never closed \"use strict\";", False)
      , ("\"use strict\" /* never closed", False)
      , ("#!\"use strict\";", False)
      , (" #!/usr/bin/env node\n\"use strict\";", False)
      , ("\"a\"; f(); \"use strict\";", False)
      , (comments <> "\"use strict\";", False)
      , (T.drop 16 comments <> "\"use strict\";", True)
      ]
      $ \(code, strict) -> (code, strictBundle code) `shouldBe` (code, strict)
    -- Past the window, a statement not yet ended is no directive.
    strictBundle (T.replicate (prologueWindow - T.length "\"use strict\"") " " <> "\"use strict\"" <> "\n.length") `shouldBe` False

policies :: Spec
policies = describe "who may add plugins" $ do
  it "is off, wiki or users in JSON" $
    map (Aeson.encode . toJSON) [ExternalOff, ExternalWiki, ExternalUsers] `shouldBe` ["\"off\"", "\"wiki\"", "\"users\""]
  it "reads its names back, and refuses another" $ do
    Aeson.decode "\"users\"" `shouldBe` Just ExternalUsers
    (Aeson.decode "\"everyone\"" :: Maybe ExternalPolicy) `shouldBe` Nothing

frames :: Spec
frames = describe "a plugin's frame" $ do
  it "has an address of its owner, its id and its bundle's hash" $ do
    m <- either (\why -> expectationFailure (toS why) >> panic "refused") pure (parseManifest (manifestWith []))
    let installed = ExternalPlugin {manifest = m, bundle = T.replicate 64 "a", size = 10, mine = False, installedAt = UTCTime (fromGregorian 2026 11 25) 0}
    framePath installed `shouldBe` "plugin-frame/wiki/doubled-words/" <> T.replicate 64 "a" <> "/diagnostics"
    framePath installed {mine = True} `shouldBe` "plugin-frame/mine/doubled-words/" <> T.replicate 64 "a" <> "/diagnostics"

  it "runs the two scripts its hashes name, in a sandbox, a worker from a blob, and connects to the hosts given alone" $
    pluginFramePolicy ["https://api.languagetool.org"] ["cnVudGltZQ==", "Y29kZQ=="]
      `shouldBe` "sandbox allow-scripts; default-src 'none'; script-src 'sha256-cnVudGltZQ==' 'sha256-Y29kZQ=='; worker-src blob:; connect-src https://api.languagetool.org; frame-src 'none'; base-uri 'none'; form-action 'none'; frame-ancestors 'self'; webrtc 'block'"

  it "connects nowhere when the manifest names no host" $
    directiveIn "connect-src" (directives (pluginFramePolicy [] ["aGFzaA=="])) `shouldBe` Nothing

-- | The directives of a frame's policy, in order, with a host to connect to.
frameDirectives :: [Text]
frameDirectives = ["sandbox", "default-src", "script-src", "worker-src", "connect-src", "frame-src", "base-uri", "form-action", "frame-ancestors", "webrtc"]

-- | Text a manifest might give as a host: most often an address, of the
-- kinds a policy takes, and otherwise something near one, with the
-- characters that would change a policy were they let into it.
hostText :: Gen Text
hostText = frequency [(6, address), (1, toS <$> listOf (elements "htps:/*.;' \"exampl0-9org,\n"))]
  where
    address = do
      scheme <- frequency [(8, elements ["https://", "http://", "HTTPS://"]), (1, elements ["ws://", ""])]
      wildcard <- frequency [(4, pure ""), (1, pure "*.")]
      labels <- choose (1, 3) >>= (`vectorOf` (toS <$> listOf1 (frequency [(20, elements "abz09-"), (1, elements "*;' ")])))
      port <- frequency [(6, elements ["", ":8080", ":*"]), (1, elements [":0", ":99999", ": 1"])]
      path <- frequency [(4, elements ["", "/", "?q=1"]), (1, pure "/api; script-src *")]
      pure (scheme <> wildcard <> T.intercalate "." labels <> port <> path)

-- | A directive's sources in a policy's directives.
directiveIn :: Text -> [(Text, [Text])] -> Maybe [Text]
directiveIn name = fmap snd . find ((== name) . fst)
