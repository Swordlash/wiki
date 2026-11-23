-- | The wiki's MCP server ("Wiki.Server.McpApi") over real HTTP, as an
-- assistant's client sends it: a JSON-RPC message in each @POST@ to
-- @\/mcp@, with the key as @Authorization: Bearer@, in the protocol's
-- current revision (each request with its version in @_meta@) or in the
-- older one (an @initialize@ first). Each tool is compared with what the
-- HTTP API for programs gives for the same thing, read with the same key.
-- The request's log line is read with the listener's application called
-- in process, where the line is written before the call returns.
--
-- The demo's wiki is the server's: Blog and Docs public, Team private; the
-- editor edits Blog through their group, the reader views Team's
-- Projects, the admin is an administrator.
module Test.Server.McpApi (spec) where

import Data.Aeson (Object, Value (..), decode, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text qualified as T
import Data.Time (addUTCTime, getCurrentTime)
import Database.Beam (runUpdate, update, val_, (<-.), (==.))
import Network.HTTP.Types (Header, methodGet, methodPost, methodPut, statusCode)
import Network.Wai (Request (..), defaultRequest, responseStatus, responseToStream, setRequestBodyChunks)
import Network.Wai.Internal (ResponseReceived (..))
import Protolude
import Test.Database (Backing)
import Test.Hspec
import Test.Server.Fixture
import Test.Server.Http (withApiEnv)
import Test.Server.MarkdownApi (encryptedSpace)
import Test.Storage.Fixture (withDemo)
import Web.HttpApiData (toHeader)
import Wiki.Api (AdminApi (..), PagesApi (..), SpacesApi (..), WikiApi (..))
import Wiki.Api.ApiKeys (ApiKeyId (..), ApiKeyInfo (..), KeyRight (..), MadeApiKey (..), NewApiKey (..))
import Wiki.Api.Links (links)
import Wiki.Api.Version (PageVersion (..))
import Wiki.Core.Assistant.Reference (markdownReference)
import Wiki.Core.Id (SpaceId, UserId (..))
import Wiki.Core.Markdown.Sections (outline, sectionRange, sectionText)
import Wiki.Core.Page qualified as P
import Wiki.Core.Path (parsePath)
import Wiki.Core.Space qualified as S
import Wiki.Core.Version (Author (..), VersionInfo (..))
import Wiki.Server.Api (Env (..))
import Wiki.Server.ApiKeys (makeApiKey, revokeApiKey)
import Wiki.Server.App (Finding (..), Listener (..), listenerApp)
import Wiki.Server.Log (silentLogger)
import Wiki.Server.McpApi (instructions, maxResult)
import Wiki.Server.Session (CookieSettings (..))
import Wiki.Server.Storage (withStorageAside, writing)
import Wiki.Server.Storage.Schema (ApiKeys (..), WikiDb (..), wikiDb)

spec :: Backing -> Spec
spec backing = describe "the MCP server" $ do
  eras backing
  signingIn backing
  keyRights backing
  tools backing
  writing' backing
  hidden backing
  logging backing
  underBasePath backing

-- | The demo with passwords for the editor, the reader and the admin.
withPasswords :: Backing -> (Running -> IO a) -> IO a
withPasswords backing test = withDemoRunning backing identity $ \r -> do
  givePassword r "user-editor" "editor's password"
  givePassword r "user-reader" "reader's password"
  givePassword r "user-admin" "admin's password"
  test r

-- | A key of a user with a right, made as the console makes one.
keyOf :: (HasCallStack) => Running -> Text -> KeyRight -> IO MadeApiKey
keyOf r user right = do
  now <- getCurrentTime
  made <- withStorageAside r.place $ \st -> makeApiKey st now (UserId user) (right == AdminRight) NewApiKey {name = "An assistant", right, expiresAt = Nothing, password = Nothing}
  either (\e -> panic ("no key: " <> show e)) pure made

bearer :: MadeApiKey -> Header
bearer k = ("Authorization", "Bearer " <> encodeUtf8 k.key)

-- | A JSON-RPC request.
rpc :: Text -> Value -> Value
rpc method params = object ["jsonrpc" .= ("2.0" :: Text), "id" .= (1 :: Int), "method" .= method, "params" .= params]

-- | A request of the protocol's current revision (2026-07-28): the
-- revision and the client in the params' @_meta@, and the headers that
-- repeat the method and, for a tool's call, the tool's name.
modern :: Text -> Maybe Text -> [Pair] -> ([Header], Value)
modern method name params = (headers, rpc method (object (meta : params)))
  where
    meta =
      "_meta"
        .= object
          [ "io.modelcontextprotocol/protocolVersion" .= ("2026-07-28" :: Text)
          , "io.modelcontextprotocol/clientInfo" .= object ["name" .= ("a test" :: Text), "version" .= ("1" :: Text)]
          , "io.modelcontextprotocol/clientCapabilities" .= object []
          ]
    headers = [("MCP-Protocol-Version", "2026-07-28"), ("Mcp-Method", encodeUtf8 method)] <> [("Mcp-Name", encodeUtf8 n) | Just n <- [name]]

-- | A @POST@ to the MCP server of the public listener, as a client sends
-- it.
post :: Running -> [Header] -> Value -> IO Reply
post r hs body = sendBare r methodPost (r.public <> "/mcp") ([("Content-Type", "application/json"), ("Accept", "application/json, text/event-stream")] <> hs) (encode body)

-- | The JSON-RPC result of a reply of status 200.
resultOf :: (HasCallStack) => Reply -> IO Object
resultOf reply = case (reply.status, decode reply.body) of
  (200, Just (Object o)) | Just (Object result) <- KeyMap.lookup "result" o -> pure result
  _ -> expectationFailure ("not a result: " <> show reply) $> mempty

-- | A tool's result: whether it is an error, and its texts.
data Called = Called
  { failed :: Bool
  , texts :: [Text]
  }
  deriving stock (Show)

calledOf :: (HasCallStack) => Reply -> IO Called
calledOf reply = do
  result <- resultOf reply
  let texts = case KeyMap.lookup "content" result of
        Just (Array blocks) -> [t | Object b <- toList blocks, Just (String t) <- [KeyMap.lookup "text" b]]
        _ -> []
  pure Called {failed = KeyMap.lookup "isError" result == Just (Bool True), texts}

-- | Call a tool with a key, in the current revision.
call :: (HasCallStack) => Running -> MadeApiKey -> Text -> [Pair] -> IO Called
call r k tool args = do
  let (hs, body) = modern "tools/call" (Just tool) ["name" .= tool, "arguments" .= object args]
  post r (bearer k : hs) body >>= calledOf

-- | A call that succeeds: its texts.
okCall :: (HasCallStack) => Running -> MadeApiKey -> Text -> [Pair] -> IO [Text]
okCall r k tool args = do
  called <- call r k tool args
  when called.failed $ expectationFailure ("the tool failed: " <> show called)
  pure called.texts

-- | A call that fails: its sentence.
failedCall :: (HasCallStack) => Running -> MadeApiKey -> Text -> [Pair] -> IO Text
failedCall r k tool args = do
  called <- call r k tool args
  unless called.failed $ expectationFailure ("the tool did not fail: " <> show called)
  pure (T.unwords called.texts)

-- | The names of the tools a key is listed, in the current revision.
toolNames :: (HasCallStack) => Running -> MadeApiKey -> IO [Text]
toolNames r k = do
  let (hs, body) = modern "tools/list" Nothing []
  result <- post r (bearer k : hs) body >>= resultOf
  pure $ case KeyMap.lookup "tools" result of
    Just (Array ts) -> [n | Object t <- toList ts, Just (String n) <- [KeyMap.lookup "name" t]]
    _ -> []

-- | A text as JSON.
jsonOf :: (HasCallStack) => Text -> IO Value
jsonOf t = maybe (expectationFailure ("not JSON: " <> toS t) $> Null) pure (decode (toS (encodeUtf8 t)))

-- | A field of a JSON object, as text.
field :: Text -> Value -> Maybe Text
field name = \case
  Object o | Just (String s) <- KeyMap.lookup (Key.fromText name) o -> Just s
  _ -> Nothing

-- | A request to the HTTP API for programs, after @api\/@, with a key.
api :: Running -> MadeApiKey -> Text -> IO Reply
api r k path = sendBare r methodGet (r.public <> "/api/" <> path) [bearer k] ""

putApi :: Running -> MadeApiKey -> Text -> LByteString -> IO Reply
putApi r k path = sendBare r methodPut (r.public <> "/api/" <> path) [bearer k]

-- | The text of a reply's body.
bodyText :: Reply -> Text
bodyText reply = decodeUtf8 (BSL.toStrict reply.body)

-- | The HTTP API's JSON with its addresses written as the tools write
-- them: a page's path for the Markdown's address, a space's path for its
-- listing's; and without the addresses the tools leave out (a page's
-- top, a section's).
asTools :: Value -> Value
asTools = \case
  Object o -> Object (KeyMap.mapMaybeWithKey entry o)
  Array a -> Array (map asTools a)
  v -> v
  where
    entry k v = case (k, v) of
      ("markdown", String s) | Just rest <- T.stripPrefix "/api/markdown" s -> Just (String rest)
      ("home", String s) | Just rest <- T.stripPrefix "/api/markdown" s -> Just (String rest)
      ("list", String s) | Just rest <- T.stripPrefix "/api/list/s" s -> Just (String (T.dropAround (== '/') rest))
      ("markdown", String _) -> Nothing
      ("top", _) -> Nothing
      ("section", String _) -> Nothing
      _ -> Just (asTools v)

-- | A JSON object without one of its keys.
without :: Text -> Value -> Value
without name = \case
  Object o -> Object (KeyMap.delete (Key.fromText name) o)
  v -> v

eras :: Backing -> Spec
eras backing = describe "both of the protocol's eras" $ do
  it "lists and calls the tools in the current revision, each request on its own" $ withPasswords backing $ \r -> do
    k <- keyOf r "user-editor" WriteRight
    names <- toolNames r k
    names `shouldBe` ["wiki_list", "wiki_search", "wiki_read_page", "wiki_page_outline", "wiki_read_section", "wiki_write_page", "wiki_create_page", "wiki_markdown_reference"]
    page <- api r k "markdown/s/blog/p/hello-world/"
    texts <- okCall r k "wiki_read_page" ["path" .= ("/s/blog/p/hello-world/" :: Text)]
    drop 1 texts `shouldBe` [bodyText page]
    -- The server's instructions, as server/discover gives them.
    let (hs, body) = modern "server/discover" Nothing []
    discovered <- post r (bearer k : hs) body >>= resultOf
    KeyMap.lookup "instructions" discovered `shouldBe` Just (String instructions)
    T.length instructions `shouldSatisfy` (< 4096)

  it "lists and calls the tools after an initialize of an older revision" $ withPasswords backing $ \r -> do
    k <- keyOf r "user-editor" WriteRight
    let legacy = [bearer k, ("MCP-Protocol-Version", "2025-11-25")]
    initialized <- post r [bearer k] (rpc "initialize" (object ["protocolVersion" .= ("2025-11-25" :: Text), "capabilities" .= object [], "clientInfo" .= object ["name" .= ("a test" :: Text), "version" .= ("1" :: Text)]])) >>= resultOf
    KeyMap.lookup "protocolVersion" initialized `shouldBe` Just (String "2025-11-25")
    -- mcp-server 0.3.0.1 puts the instructions inside serverInfo, where
    -- the older revisions do not look for them: their InitializeResult has
    -- them at its top level. A client of those revisions does not see
    -- them, until the library puts them there.
    (field "instructions" =<< KeyMap.lookup "serverInfo" initialized) `shouldBe` Just instructions
    KeyMap.lookup "instructions" initialized `shouldBe` Nothing
    listed <- post r legacy (rpc "tools/list" (object [])) >>= resultOf
    case KeyMap.lookup "tools" listed of
      Just (Array ts) -> length ts `shouldBe` 8
      other -> expectationFailure ("no tools: " <> show other)
    page <- api r k "markdown/s/blog/p/hello-world/"
    called <- post r legacy (rpc "tools/call" (object ["name" .= ("wiki_read_page" :: Text), "arguments" .= object ["path" .= ("/s/blog/p/hello-world/" :: Text)]])) >>= calledOf
    (called.failed, drop 1 called.texts) `shouldBe` (False, [bodyText page])

signingIn :: Backing -> Spec
signingIn backing = describe "signing in" $ do
  it "refuses no key, and a wrong, an expired or a revoked key, counting the keys as failed ones of the HTTP API" $ withPasswords backing $ \r -> do
    k <- keyOf r "user-editor" WriteRight
    expiring <- keyOf r "user-editor" WriteRight
    past <- addUTCTime (-1) <$> getCurrentTime
    withStorageAside r.place $ \st -> writing st (runUpdate (update wikiDb.api_keys (\x -> x.expires_at <-. val_ (Just past)) (\x -> x.id ==. val_ expiring.info.id.text)))
    revoked <- keyOf r "user-editor" WriteRight
    now <- getCurrentTime
    _ <- withStorageAside r.place $ \st -> revokeApiKey st now (UserId "user-editor") revoked.info.id
    let (hs, body) = modern "tools/list" Nothing []
        wrong n = k {key = T.dropEnd 2 k.key <> T.justifyRight 2 'a' (show (n :: Int) <> "")}
    -- No key, and a password, are not counted: as on the HTTP API, a
    -- request without a key guesses nothing.
    for_ [1 .. 35 :: Int] $ \_ -> do
      refused <- post r hs body
      refused.status `shouldBe` 401
      header "WWW-Authenticate" refused `shouldBe` Just "Bearer realm=\"wiki\""
    (.status) <$> post r (("Authorization", "Basic ZWRpdG9yOmVkaXRvcidzIHBhc3N3b3Jk") : hs) body `shouldReturn` 401
    -- A right key sent as X-API-Key, which the HTTP API takes: here only
    -- Authorization: Bearer is read.
    (.status) <$> post r (("X-API-Key", encodeUtf8 k.key) : hs) body `shouldReturn` 401
    -- An expired key, a revoked one and 28 wrong ones: thirty failures.
    for_ (expiring : revoked : map wrong [1 .. 28]) $ \bad ->
      (.status) <$> post r (bearer bad : hs) body `shouldReturn` 401
    -- The address's next failed key, on the HTTP API, is limited: the
    -- failures were counted in the same limiter.
    limited <- api r (wrong 29) "markdown/s/blog/p/hello-world/"
    limited.status `shouldBe` 429
    -- A right key still signs in.
    toolNames r k `shouldNotReturn` []

  it "refuses every browser's page, the wiki's own origin included" $ withPasswords backing $ \r -> do
    k <- keyOf r "user-editor" WriteRight
    let (hs, body) = modern "tools/list" Nothing []
    for_ ["https://elsewhere.example", r.public] $ \origin ->
      (.status) <$> post r (("Origin", encodeUtf8 origin) : bearer k : hs) body `shouldReturn` 403
    (.status) <$> post r (bearer k : hs) body `shouldReturn` 200

  it "refuses a lapsed administrator's admin key, as the HTTP API does" $ withPasswords backing $ \r -> do
    k <- keyOf r "user-admin" AdminRight
    toolNames r k `shouldNotReturn` []
    let administrator yes = sendJson r methodPut (r.admin <> "/admin/" <> links.admin.setAdministrator (UserId "user-admin")) (object ["administrator" .= yes])
    (.status) <$> administrator False `shouldReturn` 204
    let (hs, body) = modern "tools/list" Nothing []
    lapsed <- post r (bearer k : hs) body
    lapsed.status `shouldBe` 403
    bodyText lapsed `shouldSatisfy` T.isInfixOf "not an administrator"

  it "is not served on the admin listener" $ withPasswords backing $ \r -> do
    k <- keyOf r "user-admin" AdminRight
    let (hs, body) = modern "tools/list" Nothing []
    for_ [r.admin <> "/admin/mcp", r.admin <> "/mcp"] $ \at ->
      (.status) <$> sendBare r methodPost at (("Content-Type", "application/json") : bearer k : hs) (encode body) `shouldReturn` 404

keyRights :: Backing -> Spec
keyRights backing = describe "a key's right" $ do
  it "with the right read, lists the tools that read only, and refuses a write, writing nothing" $ withPasswords backing $ \r -> do
    k <- keyOf r "user-editor" ReadRight
    toolNames r k `shouldReturn` ["wiki_list", "wiki_search", "wiki_read_page", "wiki_page_outline", "wiki_read_section", "wiki_markdown_reference"]
    unchanged <- api r k "markdown/s/blog/p/hello-world/"
    let version = maybe "" decodeUtf8 (header "ETag" unchanged)
    refused <- failedCall r k "wiki_write_page" ["path" .= ("/s/blog/p/hello-world/" :: Text), "markdown" .= ("Not with this key." :: Text), "base_version" .= version]
    refused `shouldSatisfy` T.isInfixOf "the right read"
    later <- api r k "markdown/s/blog/p/hello-world/"
    (later.body, header "ETag" later) `shouldBe` (unchanged.body, header "ETag" unchanged)
    _ <- failedCall r k "wiki_create_page" ["path" .= ("/s/blog/p/hello-world/by-reader/" :: Text), "markdown" .= ("Not with this key." :: Text)]
    (.status) <$> api r k "markdown/s/blog/p/hello-world/by-reader/" `shouldReturn` 404

tools :: Backing -> Spec
tools backing = describe "each tool gives what the HTTP API gives" $ do
  it "lists the tree, the whole wiki's and a space's" $ withPasswords backing $ \r -> do
    k <- keyOf r "user-reader" ReadRight
    whole <- okCall r k "wiki_list" [] >>= traverse jsonOf
    whole' <- api r k "list/" >>= jsonAs 200
    whole `shouldBe` [asTools whole']
    space <- okCall r k "wiki_list" ["space" .= ("team/projects" :: Text), "depth" .= (1 :: Int)] >>= traverse jsonOf
    space' <- api r k "list/s/team/projects/?depth=1" >>= jsonAs 200
    space `shouldBe` [asTools space']
    -- A path the listing gives reads with wiki_read_page.
    case space of
      [listed] | Just home <- field "home" listed -> void (okCall r k "wiki_read_page" ["path" .= home])
      _ -> expectationFailure ("no home page listed: " <> show space)

  it "searches" $ withPasswords backing $ \r -> do
    k <- keyOf r "user-editor" ReadRight
    found <- okCall r k "wiki_search" ["query" .= ("hello" :: Text)] >>= traverse jsonOf
    found' <- api r k "search?q=hello" >>= jsonAs 200
    let hitsOf v = case v of
          Object o | Just (Array hs) <- KeyMap.lookup "hits" o -> toList hs
          _ -> []
        mine = [(field "title" h, field "path" h) | v <- found, h <- hitsOf v]
        theirs = [(field "title" h, T.stripPrefix "/api/markdown" =<< field "markdown" h) | h <- hitsOf found']
    mine `shouldBe` theirs
    mine `shouldNotBe` []
    case mine of
      (_, Just path) : _ -> void (okCall r k "wiki_read_page" ["path" .= path])
      _ -> expectationFailure "no hit"
    _ <- failedCall r k "wiki_search" ["query" .= ("  " :: Text)]
    pass

  it "reads a page, its outline and its sections, with the page's version" $ withPasswords backing $ \r -> do
    k <- keyOf r "user-editor" WriteRight
    (.status) <$> putApi r k "markdown/s/blog/p/hello-world/" "Before.\n\n# One\n\nText.\n\n## Two\n\nMore.\n\n[[!spoiler]]{s}(More)\n### Inside\n\nHidden.\n[[/spoiler]]{s}\n\n# Three\n\nLast.\n" `shouldReturn` 204
    page <- api r k "markdown/s/blog/p/hello-world/"
    let version = maybe "" decodeUtf8 (header "ETag" page)
    read' <- okCall r k "wiki_read_page" ["path" .= ("/s/blog/p/hello-world/" :: Text)]
    case read' of
      [about, body] -> do
        body `shouldBe` bodyText page
        meta <- jsonOf about
        (field "version" meta, field "path" meta) `shouldBe` (Just version, Just "/s/blog/p/hello-world/")
      other -> expectationFailure ("not a page: " <> show other)
    -- A full address of the wiki names the same page.
    drop 1 <$> okCall r k "wiki_read_page" ["path" .= (r.public <> "/s/blog/p/hello-world/")] `shouldReturn` [bodyText page]
    outline' <- okCall r k "wiki_page_outline" ["path" .= ("/s/blog/p/hello-world/" :: Text)] >>= traverse jsonOf
    contents <- api r k "contents/s/blog/p/hello-world/" >>= jsonAs 200
    outline' `shouldBe` [asTools (without "markdown" contents)]
    for_ [("one", False), ("one", True), ("inside", False), ("three", False)] $ \(anchor, own) -> do
      part <- api r k ("markdown/s/blog/p/hello-world/?section=" <> anchor <> (if own then "&own=true" else ""))
      okCall r k "wiki_read_section" ["path" .= ("/s/blog/p/hello-world/" :: Text), "section" .= anchor, "own" .= own] >>= \case
        [about, text] -> do
          text `shouldBe` bodyText part
          field "version" <$> jsonOf about `shouldReturn` Just version
        other -> expectationFailure ("not a section: " <> show other)
    missing <- failedCall r k "wiki_read_section" ["path" .= ("/s/blog/p/hello-world/" :: Text), "section" .= ("nowhere" :: Text)]
    missing `shouldSatisfy` T.isInfixOf "one, two"

  it "says to read a page too long for one answer by its outline and sections" $ withPasswords backing $ \r -> do
    k <- keyOf r "user-editor" WriteRight
    (.status) <$> putApi r k "markdown/s/blog/p/hello-world/" (toS (encodeUtf8 ("# Long\n\n" <> T.replicate maxResult "x" <> "\n"))) `shouldReturn` 204
    long <- failedCall r k "wiki_read_page" ["path" .= ("/s/blog/p/hello-world/" :: Text)]
    long `shouldSatisfy` T.isInfixOf "wiki_page_outline"

  it "creates a page where there is none, and only there" $ withPasswords backing $ \r -> do
    k <- keyOf r "user-editor" WriteRight
    made <- okCall r k "wiki_create_page" ["path" .= ("/s/blog/p/hello-world/notes/" :: Text), "markdown" .= ("# Notes\n\nFrom an assistant.\n" :: Text), "title" .= ("Our notes" :: Text)]
    page <- api r k "markdown/s/blog/p/hello-world/notes/"
    (page.status, page.body) `shouldBe` (200, "# Notes\n\nFrom an assistant.\n")
    case made of
      [about] -> do
        meta <- jsonOf about
        (field "path" meta, field "version" meta) `shouldBe` (Just "/s/blog/p/hello-world/notes/", decodeUtf8 <$> header "ETag" page)
      other -> expectationFailure ("not a new page: " <> show other)
    again <- failedCall r k "wiki_create_page" ["path" .= ("/s/blog/p/hello-world/notes/" :: Text), "markdown" .= ("Over it." :: Text)]
    again `shouldSatisfy` T.isInfixOf "already"
    (.body) <$> api r k "markdown/s/blog/p/hello-world/notes/" `shouldReturn` page.body
    nowhere <- failedCall r k "wiki_create_page" ["path" .= ("/s/blog/p/no-such-page/below/" :: Text), "markdown" .= ("Lost." :: Text)]
    nowhere `shouldSatisfy` T.isInfixOf "/s/blog/p/no-such-page/"
    notASlug <- failedCall r k "wiki_create_page" ["path" .= ("/s/blog/p/Not A Slug/" :: Text), "markdown" .= ("Lost." :: Text)]
    notASlug `shouldSatisfy` T.isInfixOf "not a slug"

  it "gives the Markdown reference's sections, and one of them whole" $ withPasswords backing $ \r -> do
    k <- keyOf r "user-reader" ReadRight
    listed <- T.unlines <$> okCall r k "wiki_markdown_reference" []
    listed `shouldSatisfy` T.isInfixOf "- notes-and-boxes: Notes and boxes. "
    listed `shouldSatisfy` T.isInfixOf "- lists: Lists. Bulleted lists use"
    reference <- bodyText <$> api r k "markdown/reference"
    reference `shouldBe` markdownReference
    let whole = sectionText reference <$> sectionRange (outline reference) "notes-and-boxes" False
    okCall r k "wiki_markdown_reference" ["section" .= ("notes-and-boxes" :: Text)] `shouldReturn` toList whole
    okCall r k "wiki_markdown_reference" ["section" .= ("Notes and boxes" :: Text)] `shouldReturn` toList whole
    _ <- failedCall r k "wiki_markdown_reference" ["section" .= ("no-such-section" :: Text)]
    pass

writing' :: Backing -> Spec
writing' backing = describe "writing a page" $ do
  it "saves over the version read, a version in the page's history, and nothing over a version saved since" $ withPasswords backing $ \r -> do
    k <- keyOf r "user-editor" WriteRight
    first' <- api r k "markdown/s/blog/p/hello-world/"
    let version1 = maybe "" decodeUtf8 (header "ETag" first')
        path = "path" .= ("/s/blog/p/hello-world/" :: Text)
    history1 <- versionsOf r
    written <- okCall r k "wiki_write_page" [path, "markdown" .= ("# Hello\n\nBy an assistant.\n" :: Text), "base_version" .= version1]
    saved <- api r k "markdown/s/blog/p/hello-world/"
    saved.body `shouldBe` "# Hello\n\nBy an assistant.\n"
    version2 <- case written of
      [about] -> do
        version2 <- field "version" <$> jsonOf about
        version2 `shouldBe` (decodeUtf8 <$> header "ETag" saved)
        pure version2
      other -> expectationFailure ("not a write: " <> show other) $> Nothing
    history2 <- versionsOf r
    length history2 `shouldBe` length history1 + 1
    map (\v -> (Just (decodeUtf8 (toHeader (PageVersion v.updatedAt))), (\a -> a.id) <$> v.author)) (take 1 history2) `shouldBe` [(version2, Just (UserId "user-editor"))]
    -- Over the first version again: refused, and nothing is written.
    stale <- failedCall r k "wiki_write_page" [path, "markdown" .= ("# Hello\n\nOver it.\n" :: Text), "base_version" .= version1]
    stale `shouldSatisfy` T.isInfixOf "saved since"
    later <- api r k "markdown/s/blog/p/hello-world/"
    (later.body, header "ETag" later) `shouldBe` (saved.body, header "ETag" saved)
    length <$> versionsOf r `shouldReturn` length history2
    -- The version without its quotes is the same version.
    _ <- okCall r k "wiki_write_page" [path, "markdown" .= ("# Hello\n\nAgain.\n" :: Text), "base_version" .= maybe "" (T.dropAround (== '"')) version2]
    -- A version that is not one, and no version: refused.
    _ <- failedCall r k "wiki_write_page" [path, "markdown" .= ("x" :: Text), "base_version" .= ("yesterday" :: Text)]
    _ <- failedCall r k "wiki_write_page" [path, "markdown" .= ("x" :: Text)]
    (.body) <$> api r k "markdown/s/blog/p/hello-world/" `shouldReturn` "# Hello\n\nAgain.\n"
  where
    -- The page's history, newest first, as the application lists it to
    -- the editor's session.
    versionsOf :: Running -> IO [VersionInfo]
    versionsOf r = do
      session <- cookie <$> signInAs r "editor" "editor's password"
      blog :: SpaceId <- (\(s :: S.Space) -> s.id) <$> ok200 (getUrlWith r (r.public <> "/" <> links.spaces.atPath (slugs "blog")) [session])
      page :: P.Page <- ok200 (getUrlWith r (r.public <> "/" <> links.spaces.pageAt blog (slugs "hello-world")) [session])
      ok200 (getUrlWith r (r.public <> "/" <> links.pages.versions page.id Nothing Nothing) [session])
    slugs = either (panic . show) identity . parsePath

hidden :: Backing -> Spec
hidden backing = describe "what the key's user may not read" $ do
  it "is not found in a private space, as a page that is not there" $ withPasswords backing $ \r -> do
    k <- keyOf r "user-editor" WriteRight
    for_ ["/s/team/p/team-handbook/", "/s/team/p/no-such-page/"] $ \path -> do
      refused <- failedCall r k "wiki_read_page" ["path" .= (path :: Text)]
      refused `shouldBe` ("There is no page at " <> path <> " that the user may read: find pages with wiki_list or wiki_search.")
    -- The reader may read Team's Projects.
    readerKey <- keyOf r "user-reader" ReadRight
    void (okCall r readerKey "wiki_read_page" ["path" .= ("/s/team/projects/p/the-wiki/" :: Text)])

  it "is refused a write where the user may read and not edit, and nothing is written" $ withPasswords backing $ \r -> do
    k <- keyOf r "user-editor" WriteRight
    tour <- api r k "markdown/s/docs/p/tour/"
    refused <- failedCall r k "wiki_write_page" ["path" .= ("/s/docs/p/tour/" :: Text), "markdown" .= ("Not mine to write." :: Text), "base_version" .= maybe "" decodeUtf8 (header "ETag" tour)]
    refused `shouldSatisfy` T.isInfixOf "not write it"
    later <- api r k "markdown/s/docs/p/tour/"
    (later.body, header "ETag" later) `shouldBe` (tour.body, header "ETag" tour)

  it "is refused in an encrypted space, which the server cannot read" $ withPasswords backing $ \r -> do
    slug <- encryptedSpace r
    k <- keyOf r "user-admin" AdminRight
    let path = "path" .= ("/s/" <> slug <> "/")
    failedCall r k "wiki_read_page" [path] >>= (`shouldSatisfy` T.isInfixOf "encrypted")
    failedCall r k "wiki_write_page" [path, "markdown" .= ("Plain." :: Text), "base_version" .= ("\"2026-11-18T09:30:12.345678Z\"" :: Text)] >>= (`shouldSatisfy` T.isInfixOf "encrypted")

logging :: Backing -> Spec
logging backing = describe "the request's log line" $ do
  it "names the key's user and the key's id, never the key" $ withDemo backing $ \st -> withApiEnv st $ \env0 -> do
    lines' <- newIORef []
    let logger = silentLogger (\line -> modifyIORef' lines' (line :))
        env = env0 {logger}
        listener = Listener {name = "public", mount = "/", finding = BySession CookieSettings {path = "/", secure = False}}
        app = listenerApp logger env Nothing listener
        (hs, body) = modern "tools/list" Nothing []
    now <- getCurrentTime
    made <- either (panic . show) identity <$> makeApiKey st now (UserId "user-editor") False NewApiKey {name = "Logged", right = ReadRight, expiresAt = Nothing, password = Nothing}
    chunks <- newIORef [BSL.toStrict (encode body)]
    status <- newIORef 0
    let request' = setRequestBodyChunks (atomicModifyIORef' chunks (\case c : cs -> (cs, c); [] -> ([], ""))) defaultRequest {requestMethod = methodPost, rawPathInfo = "/mcp", pathInfo = ["mcp"], requestHeaders = bearer made : ("Content-Type", "application/json") : hs}
    _ <- app request' $ \response -> writeIORef status (statusCode (responseStatus response)) $> ResponseReceived
    readIORef status `shouldReturn` 200
    logged <- readIORef lines'
    filter (T.isInfixOf (T.drop 5 made.key)) logged `shouldBe` []
    filter (\l -> T.isInfixOf ("principal=user:user-editor key=" <> made.info.id.text <> " ") l && T.isInfixOf "route=/mcp" l) logged `shouldSatisfy` ((== 1) . length)

underBasePath :: Backing -> Spec
underBasePath backing = describe "under a base path" $ do
  it "is at the base path's mcp, and takes a page's path with the base path or without it" $ withDemo backing $ \st -> withApiEnv st $ \env -> do
    let listener = Listener {name = "public", mount = "/wiki/", finding = BySession CookieSettings {path = "/wiki/", secure = False}}
        app = listenerApp env.logger env Nothing listener
    now <- getCurrentTime
    made <- either (panic . show) identity <$> makeApiKey st now (UserId "user-editor") False NewApiKey {name = "Under a path", right = ReadRight, expiresAt = Nothing, password = Nothing}
    -- The listener's application called in process, with a body: the
    -- status and the body of its answer.
    let posted (hs, body) = do
          chunks <- newIORef [BSL.toStrict (encode body)]
          answered <- newIORef (0, "")
          let request' = setRequestBodyChunks (atomicModifyIORef' chunks (\case c : cs -> (cs, c); [] -> ([], ""))) defaultRequest {requestMethod = methodPost, rawPathInfo = "/wiki/mcp", pathInfo = ["wiki", "mcp"], requestHeaders = bearer made : ("Content-Type", "application/json") : hs}
          _ <- app request' $ \response -> do
            let (status, _, withBody) = responseToStream response
            bytes <- newIORef mempty
            withBody $ \streaming -> streaming (\b -> modifyIORef' bytes (<> b)) pass
            written <- readIORef bytes
            writeIORef answered (statusCode status, toLazyByteString written)
            pure ResponseReceived
          readIORef answered
    for_ ["/s/blog/p/hello-world/", "/wiki/s/blog/p/hello-world/", "https://wiki.example/wiki/s/blog/p/hello-world/"] $ \path -> do
      (status, reply) <- posted (modern "tools/call" (Just "wiki_read_page") ["name" .= ("wiki_read_page" :: Text), "arguments" .= object ["path" .= (path :: Text)]])
      called <- calledOf Reply {status, headers = [], body = reply}
      (called.failed, length called.texts) `shouldBe` (False, 2)
