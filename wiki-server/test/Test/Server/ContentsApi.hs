-- | A page's table of contents and one section of a page, in the API for
-- programs ("Wiki.Server.ContentsApi"), over real HTTP, as @curl -u
-- login:password@ drives them.
--
-- Every request goes through 'sendBare', without the application's marker
-- header, since a program's request has none. The demo's wiki is the
-- server's: Blog and Docs public, Team private; the editor edits Blog
-- through their group, the reader views Team's Projects; nobody has a
-- password until the administrator gives one.
module Test.Server.ContentsApi (spec) where

import Data.Aeson (FromJSON, Value (..), decodeStrict, eitherDecode, encode)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteArray.Encoding (Base (..), convertToBase)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Types (Header, Method, hContentType, methodGet, methodPost, methodPut)
import Protolude
import Test.Database (Backing)
import Test.Hspec
import Test.Server.Fixture
import Wiki.Api.Encrypted (EncryptSpace (..), NewGrant (..), NewSealedPage (..))
import Wiki.Api.Types qualified as Api
import Wiki.Api.Vault (NewPublicKey (..), VaultChange (..))
import Wiki.Core.Encrypted (Encapsulated (..), PageKey (..), SealedContent (..), VersionTag (..), firstChain)
import Wiki.Core.Id (PageId (..), SpaceId (..), UserId (..))
import Wiki.Core.PageKind qualified as Kind
import Wiki.Core.Slug (slugText)
import Wiki.Core.Space qualified as S
import Wiki.Core.Vault
import Wiki.Server.Vault (keyIdOf)
import Wiki.Server.WebAuthn (toBase64Url)

spec :: Backing -> Spec
spec backing = describe "a page's contents and sections, for programs" $ do
  contents backing
  sections backing
  refusals backing

-- | A page's table of contents, read back from its JSON. A key left out is
-- 'Nothing'.
data Contents = Contents
  { title :: Text
  , version :: Text
  , markdown :: Text
  , top :: Maybe Text
  , headings :: [Heading]
  }
  deriving stock (Show, Generic)

instance FromJSON Contents

data Heading = Heading
  { level :: Int
  , text :: Text
  , anchor :: Text
  , inSpoiler :: Bool
  , section :: Text
  , children :: [Heading]
  }
  deriving stock (Show, Generic)

instance FromJSON Heading

-- | The demo with passwords for the editor, the reader and the admin.
withPasswords :: Backing -> (Running -> IO a) -> IO a
withPasswords backing test = withDemoRunning backing identity $ \r -> do
  givePassword r "user-editor" "editor's password"
  givePassword r "user-reader" "reader's password"
  givePassword r "user-admin" "admin's password"
  test r

-- | The @Authorization@ header of a login and a password, as @curl -u@
-- sends it.
basic :: Text -> Text -> Header
basic login password = ("Authorization", "Basic " <> convertToBase Base64 (encodeUtf8 (login <> ":" <> password)))

ada, edgar, rita :: Header
ada = basic "admin" "admin's password"
edgar = basic "editor" "editor's password"
rita = basic "reader" "reader's password"

-- | A request to an address of the API (from @api\/@ on), as a program
-- sends it: with these headers only.
request :: Running -> Method -> Text -> [Header] -> LByteString -> IO Reply
request r method path = sendBare r method (r.public <> "/" <> path)

-- | The table of contents of a page, by its address after @api\/contents\/@.
contentsOf :: (HasCallStack) => Running -> Header -> Text -> IO Contents
contentsOf r who path = do
  reply <- request r methodGet ("api/contents/" <> path) [who] ""
  reply.status `shouldBe` 200
  header "Content-Type" reply `shouldBe` Just "application/json"
  either (\e -> expectationFailure e >> panic "unreachable") pure (eitherDecode reply.body)

-- | A request to an address the API gave (it starts with a slash).
follow :: Running -> Header -> Text -> IO Reply
follow r who address = sendBare r methodGet (r.public <> address) [who] ""

-- | The body of a reply as text.
bodyText :: Reply -> Text
bodyText reply = decodeUtf8 (BSL.toStrict reply.body)

-- | The error a refusal names, from its JSON body.
errorName :: Reply -> Maybe Text
errorName reply = case decodeStrict (BSL.toStrict reply.body) of
  Just (Object o) | Just (String name) <- KeyMap.lookup "error" o -> Just name
  _ -> Nothing

-- | The headings at every depth, in order.
everyHeading :: [Heading] -> [Heading]
everyHeading = concatMap (\h -> h : everyHeading h.children)

-- | The text between two marks of a text: from the first's start to the
-- second's, or to the end without one.
between :: Text -> Maybe Text -> Text -> Text
between start stop whole =
  let rest = snd (T.breakOn start whole)
   in maybe rest (\t -> fst (T.breakOn t rest)) stop

contents :: Backing -> Spec
contents backing = describe "a page's table of contents" $ do
  it "lists the headings as a tree, with the anchors the rendered page links to" $ withPasswords backing $ \r -> do
    c <- contentsOf r ada "s/team/p/team-handbook/"
    c.title `shouldBe` "Team handbook"
    c.markdown `shouldBe` "/api/markdown/s/team/p/team-handbook/"
    c.top `shouldBe` Nothing
    [(h.level, h.text, h.anchor, map (\x -> x.anchor) h.children) | h <- c.headings]
      `shouldBe` [(1, "Team handbook", "team-handbook", ["the-week", "before-your-first-day", "meetings"])]
    map (\h -> h.section) (everyHeading c.headings)
      `shouldBe` map ("/api/markdown/s/team/p/team-handbook/?section=" <>) ["team-handbook", "the-week", "before-your-first-day", "meetings"]
    -- The version is the Markdown's ETag.
    page <- request r methodGet "api/markdown/s/team/p/team-handbook/" [ada] ""
    answer <- request r methodGet "api/contents/s/team/p/team-handbook/" [ada] ""
    (header "ETag" page, header "ETag" answer) `shouldBe` (Just (encodeUtf8 c.version), Just (encodeUtf8 c.version))
    -- The page places its table of contents with [[!toc]]: the server's
    -- render links its entries to the same anchors, in the same order, and
    -- gives each heading the same id.
    session <- cookie <$> signInAs r "admin" "admin's password"
    rendered <- bodyText <$> getUrlWith r (r.public <> "/s/team/p/team-handbook/") [session]
    let nav = between "class=\"toc\"" (Just "</nav>") rendered
        links' = [T.takeWhile (/= '"') l | l <- drop 1 (T.splitOn "href=\"#" nav)]
    links' `shouldBe` map (\h -> h.anchor) (everyHeading c.headings)
    for_ (everyHeading c.headings) $ \h -> rendered `shouldSatisfy` T.isInfixOf ("id=\"" <> h.anchor <> "\"")

  it "gives the address of the text before the first heading, and duplicate headings their own anchors" $ withPasswords backing $ \r -> do
    let source = "Before the title.\n\n# Hello\n\nText.\n\n## Part\n\nOne.\n\n## Part\n\nTwo.\n\n```\n# not a heading\n```\n" :: Text
    (.status) <$> request r methodPut "api/markdown/s/blog/p/hello-world/" [edgar] (toS (encodeUtf8 source)) `shouldReturn` 204
    c <- contentsOf r edgar "s/blog/p/hello-world/"
    c.top `shouldBe` Just "/api/markdown/s/blog/p/hello-world/?top=true"
    map (\h -> (h.level, h.anchor)) (everyHeading c.headings) `shouldBe` [(1, "hello"), (2, "part"), (2, "part-2")]

  it "lists a block spoiler's headings under the section that holds the spoiler, each section within the spoiler" $ withPasswords backing $ \r -> do
    let source = "# A\n\nLead.\n\n[[!spoiler]]{s}(More)\n## Hidden\n\nInside.\n[[/spoiler]]{s}\n\n## Next\n\nText.\n" :: Text
    (.status) <$> request r methodPut "api/markdown/s/blog/p/hello-world/" [edgar] (toS (encodeUtf8 source)) `shouldReturn` 204
    c <- contentsOf r edgar "s/blog/p/hello-world/"
    [(h.anchor, h.inSpoiler, [(x.anchor, x.inSpoiler) | x <- h.children]) | h <- c.headings] `shouldBe` [("a", False, [("hidden", True), ("next", False)])]
    hidden <- maybe (expectationFailure "no hidden heading" >> pure "") (fmap bodyText . follow r edgar . (\h -> h.section)) (find (\h -> h.anchor == "hidden") (everyHeading c.headings))
    hidden `shouldBe` "## Hidden\n\nInside.\n"
    -- The section that holds the spoiler holds all of it, markers too.
    (bodyText <$> request r methodGet "api/markdown/s/blog/p/hello-world/?section=a&own=true" [edgar] "") `shouldReturn` "# A\n\nLead.\n\n[[!spoiler]]{s}(More)\n## Hidden\n\nInside.\n[[/spoiler]]{s}\n\n"

  it "leads from each address it gives to what it names, for headings whose ids are ~top and top" $ withPasswords backing $ \r -> do
    let source = "Before the title.\n\n# One {id=\"~top\"}\n\nA.\n\n# Two {#top}\n\nB.\n" :: Text
    (.status) <$> request r methodPut "api/markdown/s/blog/p/hello-world/" [edgar] (toS (encodeUtf8 source)) `shouldReturn` 204
    c <- contentsOf r edgar "s/blog/p/hello-world/"
    map (\h -> h.anchor) c.headings `shouldBe` ["~top", "top"]
    sections' <- for c.headings $ \h -> bodyText <$> follow r edgar h.section
    sections' `shouldBe` ["# One {id=\"~top\"}\n\nA.\n\n", "# Two {#top}\n\nB.\n"]
    top <- maybe (expectationFailure "no top" >> pure "") (fmap bodyText . follow r edgar) c.top
    top `shouldBe` "Before the title.\n\n"

  it "reads a space's home page at the space's address" $ withPasswords backing $ \r -> do
    c <- contentsOf r rita "s/team/projects/"
    c.markdown `shouldBe` "/api/markdown/s/team/projects/"

sections :: Backing -> Spec
sections backing = describe "a section of a page" $ do
  it "is the slice of the page's Markdown the contents' address names, with the page's ETag" $ withPasswords backing $ \r -> do
    whole <- request r methodGet "api/markdown/s/team/p/team-handbook/" [ada] ""
    let source = bodyText whole
    c <- contentsOf r ada "s/team/p/team-handbook/"
    let bySection = Map.fromList [(h.anchor, h.section) | h <- everyHeading c.headings]
        sectionOf anchor = follow r ada (Map.findWithDefault "" anchor bySection)
    week <- sectionOf "the-week"
    week.status `shouldBe` 200
    header "Content-Type" week `shouldBe` Just "text/markdown; charset=utf-8"
    header "ETag" week `shouldBe` header "ETag" whole
    bodyText week `shouldBe` between "## The week" (Just "## Before your first day") source
    bodyText week `shouldSatisfy` T.isPrefixOf "## The week\n\n| Day"
    -- The title's section, with its subsections, is the whole page.
    (.body) <$> sectionOf "team-handbook" `shouldReturn` whole.body
    (bodyText <$> sectionOf "meetings") `shouldReturn` between "## Meetings" Nothing source

  it "stops at the next heading of any level with own=true: the text between the title and the first subsection" $ withPasswords backing $ \r -> do
    page <- request r methodGet "api/markdown/s/team/p/team-handbook/" [ada] ""
    own <- request r methodGet "api/markdown/s/team/p/team-handbook/?section=team-handbook&own=true" [ada] ""
    own.status `shouldBe` 200
    bodyText own `shouldBe` between "# Team handbook" (Just "## The week") (bodyText page)
    bodyText own `shouldSatisfy` T.isSuffixOf "[[!toc]]\n\n"
    whole <- request r methodGet "api/markdown/s/team/p/team-handbook/?section=team-handbook&own=false" [ada] ""
    whole.body `shouldBe` page.body

  it "reads the text before the first heading with top=true, empty where the page starts with its heading" $ withPasswords backing $ \r -> do
    (.status) <$> request r methodPut "api/markdown/s/blog/p/hello-world/" [edgar] "Before the title.\n\n# Hello\n\nText.\n" `shouldReturn` 204
    (bodyText <$> request r methodGet "api/markdown/s/blog/p/hello-world/?top=true" [edgar] "") `shouldReturn` "Before the title.\n\n"
    (bodyText <$> request r methodGet "api/markdown/s/blog/p/hello-world/?top" [edgar] "") `shouldReturn` "Before the title.\n\n"
    (bodyText <$> request r methodGet "api/markdown/s/blog/p/hello-world/?section=hello" [edgar] "") `shouldReturn` "# Hello\n\nText.\n"
    none <- request r methodGet "api/markdown/s/team/p/team-handbook/?top=true" [ada] ""
    (none.status, none.body) `shouldBe` (200, "")
    -- A section and the top at once are refused.
    (.status) <$> request r methodGet "api/markdown/s/blog/p/hello-world/?section=hello&top=true" [edgar] "" `shouldReturn` 400

  it "answers 404 to an anchor no section has, naming the anchors there are" $ withPasswords backing $ \r -> do
    missing <- request r methodGet "api/markdown/s/team/p/team-handbook/?section=no-such-part" [ada] ""
    missing.status `shouldBe` 404
    errorName missing `shouldBe` Just "NotFound"
    bodyText missing `shouldSatisfy` T.isInfixOf "Its anchors: team-handbook, the-week, before-your-first-day, meetings."
    bodyText missing `shouldSatisfy` T.isInfixOf "?top=true"
    (.status) <$> request r methodGet "api/markdown/s/team/p/team-handbook/?section=team-handbook&own=maybe" [ada] "" `shouldReturn` 400

refusals :: Backing -> Spec
refusals backing = describe "refusals" $ do
  it "asks for a sign-in, and hides a page the user may not read" $ withPasswords backing $ \r -> do
    for_ ["api/contents/s/team/p/team-handbook/", "api/markdown/s/team/p/team-handbook/?section=team-handbook"] $ \path -> do
      (.status) <$> request r methodGet path [] "" `shouldReturn` 401
      hidden <- request r methodGet path [edgar] ""
      (hidden.status, errorName hidden) `shouldBe` (404, Just "NotFound")
      (.status) <$> request r methodGet path [ada] "" `shouldReturn` 200
    -- The reader sees Team's Projects, and nothing else of Team.
    (.status) <$> request r methodGet "api/contents/s/team/projects/p/the-wiki/" [rita] "" `shouldReturn` 200
    (.status) <$> request r methodGet "api/markdown/s/team/projects/p/the-wiki/?section=milestones" [rita] "" `shouldReturn` 200
    (.status) <$> request r methodGet "api/contents/s/team/p/team-handbook/" [rita] "" `shouldReturn` 404
    (.status) <$> request r methodGet "api/contents/s/team/p/no-such-page/" [ada] "" `shouldReturn` 404
    -- A section of a page that is not there is the page's 404.
    missing <- request r methodGet "api/markdown/s/team/p/no-such-page/?top=true" [ada] ""
    (missing.status, errorName missing) `shouldBe` (404, Just "NotFound")

  it "refuses a page of an encrypted space, which the server cannot read" $ withPasswords backing $ \r -> do
    slug <- encryptedSpace r
    for_ ["api/contents/s/" <> slug <> "/", "api/markdown/s/" <> slug <> "/?top=true"] $ \path -> do
      reply <- request r methodGet path [ada] ""
      reply.status `shouldBe` 409
      errorName reply `shouldBe` Just "Stale"

  it "only reads: 405 to another method, and a section's PUT writes nothing" $ withPasswords backing $ \r -> do
    posted <- request r methodPost "api/contents/s/blog/p/hello-world/" [edgar] ""
    (posted.status, header "Allow" posted) `shouldBe` (405, Just "GET, HEAD")
    unchanged <- request r methodGet "api/markdown/s/blog/p/hello-world/" [edgar] ""
    written <- request r methodPut "api/markdown/s/blog/p/hello-world/?section=why-a-wiki" [edgar] "## Why a wiki\n\nReplaced.\n"
    (written.status, header "Allow" written) `shouldBe` (405, Just "GET, HEAD")
    (.body) <$> request r methodGet "api/markdown/s/blog/p/hello-world/" [edgar] "" `shouldReturn` unchanged.body
    -- A PUT to a free address creates a page there, but not with ?section=.
    nowhere <- request r methodPut "api/markdown/s/blog/p/new-page/?section=x" [edgar] "# New page\n"
    (nowhere.status, header "Allow" nowhere) `shouldBe` (405, Just "GET, HEAD")
    (.status) <$> request r methodGet "api/markdown/s/blog/p/new-page/" [edgar] "" `shouldReturn` 404
    (.status) <$> request r methodPut "api/markdown/s/blog/p/new-page/?top=true" [edgar] "# New page\n" `shouldReturn` 405
    (.status) <$> request r methodGet "api/markdown/s/blog/p/new-page/" [edgar] "" `shouldReturn` 404

-- | A space the admin has encrypted, with its home page, as the browser
-- encrypts one (the ciphertexts are bytes of no meaning): its slug. The
-- same as the Markdown's tests make ("Test.Server.MarkdownApi").
encryptedSpace :: Running -> IO Text
encryptedSpace r = do
  token <- signInAs r "admin" "admin's password"
  let as' method path body = send r method (r.public <> "/" <> path) [cookie token, (hContentType, "application/json")] body
      bytesOf n c = toBase64Url (BS.replicate n c)
      public = PublicKey (bytesOf 32 65)
      keyId = fromMaybe (KeyId "") (keyIdOf public)
      vault = Vault {format = vaultFormat, salt = Salt (bytesOf 32 1), items = Map.singleton identityItem Item {keyId = Just keyId, sealed = Sealed (Nonce (bytesOf 12 2)) (Ciphertext (bytesOf 64 3))}, wraps = Map.singleton ByPassword (WrappedKey (Nonce (bytesOf 12 4)) (Ciphertext (bytesOf 48 5)))}
      spaceKey = KeyId (bytesOf 16 6)
  (.status) <$> as' methodPut "api/v1/assistant/vault" (encode VaultChange {vault, publicKey = Just NewPublicKey {keyId, publicKey = public}}) `shouldReturn` 200
  made :: S.Space <- as' methodPost "api/v1/spaces" (encode Api.NewSpace {parent = Nothing, name = "Vault room"}) >>= jsonAs 201
  (.status)
    <$> as'
      methodPost
      ("api/v1/encrypted/spaces/" <> made.id.text)
      (encode EncryptSpace {keyId = spaceKey, grant = NewGrant {recipient = UserId "user-admin", recipientKeyId = keyId, enc = Encapsulated (bytesOf 32 7), sealed = Ciphertext (bytesOf 48 8)}, homePage = NewSealedPage {id = PageId "4c1a6d2f-3e5b-4f7c-9dae-1b2c3d4e5f60", parent = Nothing, pageKey = PageKey {keyId = KeyId (bytesOf 16 20), spaceKeyId = spaceKey, wrapped = WrappedKey (Nonce (bytesOf 12 21)) (Ciphertext (bytesOf 48 22))}, content = SealedContent {tag = VersionTag (bytesOf 16 23), keyId = KeyId (bytesOf 16 20), chain = Just (firstChain (VersionTag (bytesOf 16 30))), title = Sealed (Nonce (bytesOf 12 24)) (Ciphertext (bytesOf 40 25)), body = Sealed (Nonce (bytesOf 12 26)) (Ciphertext (bytesOf 60 27))}, kind = Kind.Document}})
    `shouldReturn` 200
  pure (slugText made.slug)
