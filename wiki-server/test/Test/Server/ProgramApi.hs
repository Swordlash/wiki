{-# OPTIONS_GHC -Wno-orphans #-}

-- | The OpenAPI document of the API for programs ("Wiki.Server.ProgramApi")
-- against what the server does, so that neither drifts from the other:
--
-- * every JSON body the API sends or takes matches its schema in the
--   document (servant-openapi3's check of each 'ToJSON' against its
--   'ToSchema', with values QuickCheck makes);
-- * the document is the golden file @test\/golden\/openapi.json@, so that a
--   change of the API shows in review: run the suite with
--   @WIKI_API_GOLDEN=accept@ to write it again, and read the diff before
--   committing it;
-- * the server serves the document the type makes, which reads back as
--   OpenAPI 3, whose references all resolve, under the listener's path;
-- * each operation of the document, asked by a real request of the demo's
--   wiki, answers with a status the document lists for it.
--
-- The 'Arbitrary' instances are orphans, made for this test alone.
module Test.Server.ProgramApi (unitSpec, spec) where

import Control.Lens ((^.), (^..))
import Data.Aeson (ToJSON, Value (..), eitherDecode, eitherDecodeFileStrict', toJSON)
import Data.Aeson.Encode.Pretty (Config (..), Indent (..), defConfig, encodePretty')
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteArray.Encoding (Base (..), convertToBase)
import Data.ByteString.Lazy qualified as BSL
import Data.HashMap.Strict.InsOrd.Compat qualified as InsOrd
import Data.List (lookup)
import Data.OpenApi (OpenApi, Operation, PathItem, Referenced (..), ToSchema)
import Data.OpenApi qualified as O
import Data.OpenApi.Declare (runDeclare)
import Data.OpenApi.Schema.Validation (validateJSON, validateToJSON)
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Network.HTTP.Types (Header, methodGet, methodPost, methodPut)
import Protolude
import Servant.OpenApi.Internal.Test (maybeCounterExample, prettyValidateWith, props)
import Servant.OpenApi.Internal.TypeLevel.API (Nub)
import System.Environment (lookupEnv)
import Test.Database (Backing)
import Test.Hspec
import Test.QuickCheck (Arbitrary (..), Gen, choose, elements, listOf, oneof, resize, sized)
import Test.Server.Fixture
import Wiki.Api.Error (ApiError (..), Staleness)
import Wiki.Core.PageKind (PageKind (..))
import Wiki.Core.Search (Piece (..), Snippet (..))
import Wiki.Server.ContentsApi (Contents (..), Heading (..), Written (..))
import Wiki.Server.ListApi (HitEntry (..), HitSpace (..), Hits (..), NewSpace (..), PageEntry (..), SpaceEntry (..), TopLevel (..))
import Wiki.Server.ProgramApi (OpenApiDocument (..), ProgramApi, programDocument)
import Wiki.Server.ProgramApi.Combinators (JsonBodies, RefusalBody (..))

-- | What needs no server: the schemas, and the golden file.
unitSpec :: Spec
unitSpec = describe "the OpenAPI document of the API for programs" $ do
  describe "every JSON body matches its schema" $ do
    props (Proxy @'[ToJSON, ToSchema]) (maybeCounterExample . prettyValidateWith validateToJSON) (Proxy @(Nub (RefusalBody ': JsonBodies ProgramApi)))
    it "lists every JSON body of the API" $
      -- The bodies the type sends and takes as JSON, which the check above
      -- covers: one more there means an Arbitrary instance here.
      length (typeReps (Proxy @(Nub (JsonBodies ProgramApi)))) `shouldBe` 7
    it "gives the application's own refusals the schema of a refusal" $ do
      let (defs, schema) = runDeclare (O.declareSchema (Proxy @RefusalBody)) mempty
          errors = [NotFound, Forbidden, Conflict "c", Invalid "i", Unauthenticated, TooManyRequests "t", SecondFactorRequired] <> map (`Stale` "s") [minBound .. maxBound :: Staleness]
      for_ errors $ \e -> (e, validateJSON defs schema (toJSON e)) `shouldBe` (e, [])

  it "is the golden file test/golden/openapi.json" $ do
    let path = "test/golden/openapi.json"
        document = programDocument "/"
    accept <- (== Just "accept") <$> lookupEnv "WIKI_API_GOLDEN"
    when accept $ BSL.writeFile path (encodePretty' defConfig {confIndent = Spaces 2, confCompare = compare} document <> "\n")
    golden <- either (panic . toS) identity <$> eitherDecodeFileStrict' path
    (golden :: Value) `shouldBe` toJSON document

  it "describes every operation, names it once, and resolves every reference" $ do
    let document = programDocument "/"
        ops = operations document
        refs = references (toJSON document)
        named = map ("#/components/schemas/" <>) (InsOrd.keys (document ^. O.components . O.schemas))
    sort (map (\(p, m, _) -> (p, m)) ops) `shouldBe` sort operationsServed
    let names = mapMaybe (\(_, _, op) -> op ^. O.operationId) ops
    (length names, length (ordNub names)) `shouldBe` (length ops, length ops)
    for_ ops $ \(p, m, op) -> do
      (p, m, isJust (op ^. O.summary), isJust (op ^. O.description), null (op ^. O.security)) `shouldBe` (p, m, True, True, False)
      -- A refusal of the sign-in, and the answers' statuses, each with a description.
      (p, m, all (\(_, r) -> either (const True) (not . T.null . (^. O.description)) (inlined r)) (InsOrd.toList (op ^. O.responses . O.responses))) `shouldBe` (p, m, True)
      -- servant-openapi3's own descriptions of a refusal (`address` not
      -- found, Invalid `If-Match`), which the type's replace, are gone.
      [(p, m, code) | (code, Inline r) <- InsOrd.toList (op ^. O.responses . O.responses), "`" `T.isInfixOf` (r ^. O.description)] `shouldBe` []
    filter (`notElem` named) refs `shouldBe` []

  it "gives the address it is served under as its server" $ do
    (programDocument "/wiki/" ^.. O.servers . traverse . O.url) `shouldBe` ["/wiki"]
    (programDocument "/" ^.. O.servers . traverse . O.url) `shouldBe` []
  where
    inlined = \case
      Inline r -> Right r
      Ref x -> Left x
    -- Every reference in a JSON document, as written.
    references :: Value -> [Text]
    references = \case
      Object o -> [r | Just (String r) <- [KeyMap.lookup "$ref" o]] <> concatMap references (KeyMap.elems o)
      Array a -> concatMap references (toList a)
      _ -> []

-- | The operations the API has, by their path in the document and their
-- method: a new one is added here, and to 'asked' below.
operationsServed :: [(FilePath, Text)]
operationsServed =
  [ ("/api/markdown/s/{address}", "GET")
  , ("/api/markdown/s/{address}", "PUT")
  , ("/api/markdown/reference", "GET")
  , ("/api/contents/s/{address}", "GET")
  , ("/api/list", "GET")
  , ("/api/list/s/{space}", "GET")
  , ("/api/list/s/{space}", "PUT")
  , ("/api/search", "GET")
  , ("/api/openapi.json", "GET")
  ]

-- | What needs a server: the document served, and an answer to each of the
-- document's operations.
spec :: Backing -> Spec
spec backing = describe "the OpenAPI document of the API for programs, served" $ do
  it "is what the type makes, and reads as OpenAPI 3" $ withDemoRunning backing identity $ \r -> do
    reply <- sendBare r methodGet (r.public <> "/api/openapi.json") [] ""
    (reply.status, header "Content-Type" reply) `shouldBe` (200, Just "application/json")
    eitherDecode reply.body `shouldBe` Right (toJSON (programDocument "/"))
    (eitherDecode reply.body :: Either [Char] OpenApi) `shouldSatisfy` isRight

  it "answers every operation it describes with a status it lists" $ withDemoRunning backing identity $ \r -> do
    givePassword r "user-admin" "admin's password"
    for_ (operations (programDocument "/")) $ \(p, m, op) -> case lookup (p, m) asked of
      Nothing -> expectationFailure ("no request asks " <> toS m <> " " <> p <> ": add one to asked")
      Just Asked {address, headers, body, signedIn, anonymous} -> do
        let listed = InsOrd.keys (op ^. O.responses . O.responses)
        -- Signed in, the answer the request is made for; without a
        -- credential, the sign-in's or the same. Each among the
        -- operation's statuses.
        signedIn' <- sendBare r (encodeUtf8 m) (r.public <> address) (admin : headers) body
        (m, address, signedIn'.status, signedIn'.status `elem` listed) `shouldBe` (m, address, signedIn, True)
        anonymous' <- sendBare r (encodeUtf8 m) (r.public <> address) headers body
        (m, address, "anonymous" :: Text, anonymous'.status, anonymous'.status `elem` listed) `shouldBe` (m, address, "anonymous", anonymous, True)

  -- What the API answered before it was a servant type, and servant's own
  -- combinators would answer otherwise ("Wiki.Server.ProgramApi.Combinators").
  it "answers as the API did before it was a servant type" $ withDemoRunning backing identity $ \r -> do
    givePassword r "user-admin" "admin's password"
    let page = "/api/markdown/s/blog/p/hello-world/"
        asAdmin method address hs = sendBare r method (r.public <> address) (admin : hs)
    -- Accept is not read: the Markdown is the Markdown.
    read' <- asAdmin methodGet page [("Accept", "application/json")] ""
    (read'.status, header "Content-Type" read') `shouldBe` (200, Just "text/markdown; charset=utf-8")
    -- Any Content-Type is taken, as curl --data-binary sends it; a save
    -- and a new page answer without one.
    for_ [[("Content-Type", "application/x-www-form-urlencoded")], [("Content-Type", "no media type")], []] $ \hs -> do
      saved <- asAdmin methodPut page hs (read'.body <> "\nMore.\n")
      (hs, saved.status, header "Content-Type" saved) `shouldBe` (hs, 204, Nothing)
    created <- asAdmin methodPut "/api/markdown/s/blog/p/hello-world/below/" [("If-None-Match", "*")] "# Below\n"
    (created.status, header "Content-Type" created, header "Location" created) `shouldBe` (201, Nothing, Just "/api/markdown/s/blog/p/hello-world/below/")
    -- A flag alone is true; a value that is not UTF-8 is refused.
    top <- asAdmin methodGet (page <> "?top=true") [] ""
    (.body) <$> asAdmin methodGet (page <> "?top") [] "" `shouldReturn` top.body
    notUtf8 <- asAdmin methodGet (page <> "?section=%FF") [] ""
    (notUtf8.status, notUtf8.body) `shouldBe` (400, "{\"error\":\"Invalid\",\"message\":\"The parameter section is not UTF-8 text.\"}")
    -- The router reads the address with its query: a version's address is
    -- no page's.
    (.status) <$> asAdmin methodGet (page <> "?version=2026-01-01T00:00:00Z") [] "" `shouldReturn` 404
    -- A method the address does not take, where no page is: 404, as any
    -- request there; at a page, 405.
    (.status) <$> asAdmin methodPost (page <> "history/") [] "" `shouldReturn` 404
    (.status) <$> asAdmin methodPost page [] "" `shouldReturn` 405
    -- An address that names nothing signs in first.
    for_ ["/api/markdown/x", "/api/contents/x", "/api/list/x"] $ \address -> do
      bare <- sendBare r methodGet (r.public <> address) [] ""
      (address, bare.status) `shouldBe` (address, 401)
      (.status) <$> asAdmin methodGet address [] "" `shouldReturn` 404
  where
    admin = ("Authorization", "Basic " <> convertToBase Base64 (encodeUtf8 ("admin:admin's password" :: Text)))

-- | 'Asked' is a request of the demo's wiki for an operation of the
-- document: its address, its headers besides the credential, its body,
-- and the statuses it is answered with, signed in as the administrator
-- and without a credential.
data Asked = Asked
  { address :: Text
  , headers :: [Header]
  , body :: LByteString
  , signedIn :: Int
  , anonymous :: Int
  }

-- | A request for each operation of the document.
asked :: [((FilePath, Text), Asked)]
asked =
  [ (("/api/markdown/s/{address}", "GET"), Asked "/api/markdown/s/blog/p/hello-world/" [] "" 200 401)
  , (("/api/markdown/s/{address}", "PUT"), Asked "/api/markdown/s/blog/p/a-new-page/?title=A%20new%20page" [("If-None-Match", "*")] "# A new page\n" 201 401)
  , (("/api/markdown/reference", "GET"), Asked "/api/markdown/reference" [] "" 200 200)
  , (("/api/contents/s/{address}", "GET"), Asked "/api/contents/s/blog/p/hello-world/" [] "" 200 401)
  , (("/api/list", "GET"), Asked "/api/list/?depth=1" [] "" 200 401)
  , (("/api/list/s/{space}", "GET"), Asked "/api/list/s/blog/" [] "" 200 401)
  , (("/api/list/s/{space}", "PUT"), Asked "/api/list/s/blog/notes/" [("Content-Type", "application/json")] "{\"title\": \"Notes\"}" 201 401)
  , (("/api/search", "GET"), Asked "/api/search?q=wiki" [] "" 200 401)
  , (("/api/openapi.json", "GET"), Asked "/api/openapi.json" [] "" 200 200)
  ]

-- | The document's operations: path, method, and the operation.
operations :: OpenApi -> [(FilePath, Text, Operation)]
operations document =
  [ (p, m, op)
  | (p, item) <- InsOrd.toList (document ^. O.paths)
  , (m, Just op) <- methods item
  ]
  where
    methods :: PathItem -> [(Text, Maybe Operation)]
    methods item = [("GET", item ^. O.get), ("PUT", item ^. O.put), ("POST", item ^. O.post), ("DELETE", item ^. O.delete), ("PATCH", item ^. O.patch), ("HEAD", item ^. O.head_), ("OPTIONS", item ^. O.options)]

-- | The types of a list, to count them.
class TypeReps (xs :: [Type]) where
  typeReps :: Proxy xs -> [TypeRep]

instance TypeReps '[] where
  typeReps _ = []

instance (Typeable x, TypeReps xs) => TypeReps (x ': xs) where
  typeReps _ = typeRep (Proxy @x) : typeReps (Proxy @xs)

--------------------------------------------------------------------------------
-- Values to check the schemas with

text :: Gen Text
text = T.pack <$> listOf arbitrary

maybeText :: Gen (Maybe Text)
maybeText = oneof [pure Nothing, Just <$> text]

-- | A tree of at most this many levels below, few at each.
few :: Gen a -> Gen [a]
few g = choose (0, 3) >>= \n -> replicateM n g

instance Arbitrary RefusalBody where
  arbitrary = RefusalBody <$> text <*> maybeText <*> maybeText

instance Arbitrary Written where
  arbitrary = Written <$> maybeText <*> maybeText <*> maybeText <*> text

instance Arbitrary Contents where
  arbitrary = Contents <$> text <*> text <*> maybeText <*> maybeText <*> sized (\n -> few (heading (min n 3)))
    where
      heading :: Int -> Gen Heading
      heading depth = Heading <$> choose (1, 6) <*> text <*> text <*> arbitrary <*> maybeText <*> (if depth <= 0 then pure [] else few (heading (depth - 1)))

instance Arbitrary PageKind where
  arbitrary = oneof [pure Document, pure Presentation, OtherKind <$> text]

instance Arbitrary Snippet where
  arbitrary = Snippet <$> listOf (Piece <$> text <*> arbitrary)

time :: Gen UTCTime
time = UTCTime <$> (fromGregorian <$> choose (1990, 2100) <*> choose (1, 12) <*> choose (1, 28)) <*> (secondsToDiffTime <$> choose (0, 86399))

pageEntry :: Int -> Gen PageEntry
pageEntry depth = PageEntry <$> text <*> arbitrary <*> text <*> time <*> arbitrary <*> (if depth <= 0 then pure Nothing else oneof [pure Nothing, Just <$> few (pageEntry (depth - 1))])

spaceEntry :: Int -> Gen SpaceEntry
spaceEntry depth =
  SpaceEntry
    <$> text
    <*> text
    <*> maybeText
    <*> maybeText
    <*> arbitrary
    <*> (if depth <= 0 then pure Nothing else oneof [pure Nothing, Just <$> few (spaceEntry (depth - 1))])
    <*> oneof [pure Nothing, Just <$> few (pageEntry 2)]

instance Arbitrary SpaceEntry where
  arbitrary = spaceEntry 2

instance Arbitrary TopLevel where
  arbitrary = TopLevel <$> resize 3 (few (spaceEntry 1))

instance Arbitrary HitEntry where
  arbitrary = HitEntry <$> text <*> arbitrary <*> (HitSpace <$> text <*> text) <*> text <*> text <*> arbitrary

instance Arbitrary Hits where
  arbitrary = Hits <$> few arbitrary

instance Arbitrary NewSpace where
  arbitrary = NewSpace <$> maybeText <*> arbitrary

-- | The document is one value: the one the server serves.
instance Arbitrary OpenApiDocument where
  arbitrary = elements [OpenApiDocument (programDocument "/"), OpenApiDocument (programDocument "/wiki/")]
