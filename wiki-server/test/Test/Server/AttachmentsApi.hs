{-# LANGUAGE DeriveAnyClass #-}

-- | A page's attachments in the API for programs
-- ("Wiki.Server.AttachmentsApi"), over real HTTP, as @curl@ drives it:
-- the list, a file attached, read back byte for byte and taken off, the
-- names, the limit on a file's size, a name that is taken, and who may do
-- what. The demo's wiki is the server's: Blog public, Team private; the
-- editor edits Blog, the reader views Team's Projects.
module Test.Server.AttachmentsApi (unitSpec, spec) where

import Data.Aeson (FromJSON, Value (..), decodeStrict, eitherDecode)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteArray.Encoding (Base (..), convertToBase)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Char (isAsciiLower, isAsciiUpper)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Network.HTTP.Types (Header, Method, methodDelete, methodGet, methodPut)
import Protolude
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openBinaryTempFile)
import Test.Database (Backing)
import Test.Hspec
import Test.Server.Fixture
import Test.Server.MarkdownApi (encryptedSpace)
import Wiki.Api (SpacesApi (..), WikiApi (..))
import Wiki.Api.ApiKeys (KeyRight (..), MadeApiKey (..), NewApiKey (..))
import Wiki.Api.Links (attachmentUrl, links)
import Wiki.App.Backend (maxAttachmentSize)
import Wiki.Core.Attachment (Attachment (..), FileName (..))
import Wiki.Core.Id (SpaceId, UserId (..))
import Wiki.Core.Page qualified as P
import Wiki.Core.Path (parsePath)
import Wiki.Core.Space qualified as S
import Wiki.Server.ApiKeys (makeApiKey)
import Wiki.Server.AttachmentsApi (Spooled (..), spool)
import Wiki.Server.Storage (attachmentTooLarge, withStorageAside)

-- | What needs no server: the reading of an upload's body.
unitSpec :: Spec
unitSpec = describe "an upload's body, read a chunk at a time" $ do
  it "is written whole when it is within the limit, the limit itself included" $ do
    (spooled, pulled, written) <- spoolOf 12 ["abcd", "efgh", "ijkl"]
    (spooled, pulled, written) `shouldBe` (Spooled, 4, "abcdefghijkl")

  it "stops at the first chunk past the limit, which it neither writes nor follows" $ do
    (spooled, pulled, written) <- spoolOf 10 ["abcd", "efgh", "ijkl", "mnop", "qrst"]
    -- Two chunks written, the third read and refused, the rest never asked for.
    (spooled, pulled, written) `shouldBe` (TooLarge, 3, "abcdefgh")
  where
    -- The outcome, how many chunks were asked for, and what was written,
    -- for a body of these chunks (then the empty chunk of its end).
    spoolOf limit chunks = do
      left <- newIORef (chunks <> [""])
      pulled <- newIORef (0 :: Int)
      let next = do
            modifyIORef' pulled (+ 1)
            readIORef left >>= \case
              c : rest -> modifyIORef' left (const rest) $> c
              [] -> pure ""
      dir <- getTemporaryDirectory
      bracket (openBinaryTempFile dir "spool") (\(file, h) -> hClose h *> removeFile file) $ \(file, h) -> do
        spooled <- spool limit next h
        hClose h
        (spooled,,) <$> readIORef pulled <*> BS.readFile file

spec :: Backing -> Spec
spec backing = describe "a page's attachments in the API for programs" $ do
  roundTrips backing
  names backing
  takenNames backing
  deleting backing
  limits backing
  permissions backing

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

editor, rita, admin :: Header
editor = basic "editor" "editor's password"
rita = basic "reader" "reader's password"
admin = basic "admin" "admin's password"

-- | The Blog's page the editor edits, after @api\/attachments\/s\/@.
hello :: Text
hello = "blog/p/hello-world/"

-- | A request to the attachments of a page, by its address after
-- @api\/attachments\/s\/@ and a query, as a program sends it.
request :: Running -> Method -> Text -> Text -> [Header] -> LByteString -> IO Reply
request r method page query = sendBare r method (r.public <> "/api/attachments/s/" <> page <> query)

-- | The query of a file's name, percent-encoded as JavaScript's
-- encodeURIComponent writes it, and wiki.sh: every byte of its UTF-8 but
-- letters, digits and @-_.~@ (a space is @%20@, a plus @%2B@).
nameQuery :: Text -> Text
nameQuery n = "?name=" <> foldMap escaped (BS.unpack (encodeUtf8 n))
  where
    escaped b
      | isAsciiUpper c || isAsciiLower c || isDigit c || c `elem` ("-_.~" :: [Char]) = T.singleton c
      | otherwise = T.pack ['%', hex (b `div` 16), hex (b `mod` 16)]
      where
        c = chr (fromIntegral b)
    hex k = "0123456789ABCDEF" `T.index` fromIntegral k

-- | A file attached, as @curl --data-binary@ with its type sends it.
attach :: Running -> Header -> Text -> Text -> Text -> LByteString -> IO Reply
attach r who page n mediaType = request r methodPut page (nameQuery n) (who : [("Content-Type", encodeUtf8 mediaType)])

-- | A file's bytes, read back.
fetch :: Running -> Header -> Text -> Text -> IO Reply
fetch r who page n = request r methodGet page (nameQuery n) [who] ""

-- | 'Entry' is a file as the API lists it, read back.
data Entry = Entry
  { name :: Text
  , size :: Integer
  , mediaType :: Text
  , version :: Maybe Text
  , address :: Text
  , markdown :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

newtype Listed = Listed {attachments :: [Entry]}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

-- | A file's entry, from an answer's JSON.
decoded :: Reply -> Either [Char] Entry
decoded reply = eitherDecode reply.body

-- | A page's files, as the API lists them to a user.
listed :: (HasCallStack) => Running -> Header -> Text -> IO [Entry]
listed r who page = (\(l :: Listed) -> l.attachments) <$> (request r methodGet page "" [who] "" >>= jsonAs 200)

-- | The bytes of every value, in order, this many times over.
everyByte :: Int -> LByteString
everyByte n = BSL.concat (replicate n (BSL.pack [0 .. 255]))

-- | The error and the message of a refusal.
refusalOf :: Reply -> (Int, Maybe Text, Maybe Text)
refusalOf reply = case decodeStrict (BSL.toStrict reply.body) of
  Just (Object o) -> (reply.status, text (KeyMap.lookup "error" o), text (KeyMap.lookup "message" o))
  _ -> (reply.status, Nothing, Nothing)
  where
    text = \case
      Just (String t) -> Just t
      _ -> Nothing

-- | The Blog's page as the editor's session reads it through the
-- application's own API: the browser's view of the same store.
browsersPage :: Running -> IO (P.Page, Header)
browsersPage r = do
  session <- cookie <$> signInAs r "editor" "editor's password"
  blog :: SpaceId <- (\(s :: S.Space) -> s.id) <$> ok200 (getUrlWith r (r.public <> "/" <> links.spaces.atPath (slugs "blog")) [session])
  (,session) <$> ok200 (getUrlWith r (r.public <> "/" <> links.spaces.pageAt blog (slugs "hello-world")) [session])
  where
    slugs = either (panic . show) identity . parsePath

roundTrips :: Backing -> Spec
roundTrips backing = describe "a file attached and read back" $ do
  it "is listed with its Markdown and read back byte for byte, as the browser's route serves it" $ withPasswords backing $ \r -> do
    listed r editor hello `shouldReturn` []
    earlier <- sendBare r methodGet (r.public <> "/api/markdown/s/" <> hello) [editor] ""
    let bytes = everyByte 40
    made <- attach r editor hello "every-byte.bin" "application/octet-stream" bytes
    made.status `shouldBe` 201
    entry :: Entry <- json made
    entry
      `shouldBe` Entry
        { name = "every-byte.bin"
        , size = fromIntegral (BSL.length bytes)
        , mediaType = "application/octet-stream"
        , version = decodeUtf8 <$> header "ETag" made
        , address = "/api/attachments/s/blog/p/hello-world/?name=every-byte.bin"
        , markdown = "[every-byte.bin](attachment:every-byte.bin)"
        }
    header "Location" made `shouldBe` Just (encodeUtf8 entry.address)
    png <- attach r editor hello "chart.png" "image/png" "\137PNG not really"
    (\(e :: Entry) -> e.markdown) <$> json png `shouldReturn` "![chart.png](attachment:chart.png)"
    map (\e -> e.name) <$> listed r editor hello `shouldReturn` ["every-byte.bin", "chart.png"]

    read' <- sendBare r methodGet (r.public <> entry.address) [editor] ""
    (read'.status, read'.body) `shouldBe` (200, bytes)
    -- The same headers as the browser's route for the same file, but for
    -- how long a cache may keep it: the browser's address names one
    -- upload, this one a name whose file may be replaced.
    (page, session) <- browsersPage r
    map (\a -> a.name.text) page.attachments `shouldBe` ["every-byte.bin", "chart.png"]
    for_ page.attachments $ \a -> do
      browser <- getUrlWith r (r.public <> "/" <> attachmentUrl a) [session]
      api <- fetch r editor hello a.name.text
      (a.name.text, browser.status, api.status) `shouldBe` (a.name.text, 200, 200)
      let protective = ["Content-Type", "X-Content-Type-Options", "Content-Disposition", "Content-Security-Policy", "ETag"]
      (a.name.text, map (`header` api) protective) `shouldBe` (a.name.text, map (`header` browser) protective)
      header "Cache-Control" api `shouldBe` Just "private, no-cache"
      api.body `shouldBe` browser.body
    -- The page itself is as it was: its text and its version.
    later <- sendBare r methodGet (r.public <> "/api/markdown/s/" <> hello) [editor] ""
    (later.body, header "ETag" later) `shouldBe` (earlier.body, header "ETag" earlier)

  it "answers 304 to its ETag, and a range of its bytes" $ withPasswords backing $ \r -> do
    made <- attach r editor hello "every-byte.bin" "application/octet-stream" (everyByte 1)
    let tag = fromMaybe "" (header "ETag" made)
    (.status) <$> request r methodGet hello "?name=every-byte.bin" [editor, ("If-None-Match", tag)] "" `shouldReturn` 304
    ranged <- request r methodGet hello "?name=every-byte.bin" [editor, ("Range", "bytes=10-19")] ""
    (ranged.status, ranged.body) `shouldBe` (206, BSL.pack [10 .. 19])

  it "takes the page's address with its slashes escaped, as the plain one" $ withPasswords backing $ \r -> do
    let escaped = "blog%2Fp%2Fhello-world%2F"
    (.status) <$> attach r editor escaped "notes.txt" "text/plain" "From an escaped address.\n" `shouldReturn` 201
    (.body) <$> fetch r editor hello "notes.txt" `shouldReturn` "From an escaped address.\n"
    listed r editor escaped >>= (`shouldBe` []) . filter (\e -> e.name /= "notes.txt")
    (.status) <$> request r methodDelete escaped "?name=notes.txt" [editor] "" `shouldReturn` 204
    listed r editor hello `shouldReturn` []

  it "lists a space's home page, and nothing under a page that is not there" $ withPasswords backing $ \r -> do
    (.status) <$> request r methodGet "blog/" "" [editor] "" `shouldReturn` 200
    (.status) <$> request r methodGet "blog/p/no-such-page/" "" [editor] "" `shouldReturn` 404
    (.status) <$> request r methodPut "blog/p/no-such-page/" "?name=x.txt" [editor] "x" `shouldReturn` 404
    -- A segment after a page's address names a page under it, never a file.
    (.status) <$> request r methodGet (hello <> "every-byte.bin/") "" [editor] "" `shouldReturn` 404

names :: Backing -> Spec
names backing = describe "a file's name" $ do
  it "is taken byte for byte, percent-decoded once, and compared exactly" $ withPasswords backing $ \r -> do
    let kept = ["meeting notes.txt", "a+b.txt", "żółw — notatki.md", ".hidden", "Readme.txt", "README.txt", "100%.txt", "a&b=c?.txt"]
    for_ (zip [1 :: Int ..] kept) $ \(i, n) -> do
      made <- attach r editor hello n "text/plain" (show i)
      (n, made.status) `shouldBe` (n, 201)
      (n, (\e -> e.name) <$> decoded made) `shouldBe` (n, Right n)
    map (\e -> e.name) <$> listed r editor hello `shouldReturn` kept
    for_ (zip [1 :: Int ..] kept) $ \(i, n) -> do
      back <- fetch r editor hello n
      (n, back.status, back.body) `shouldBe` (n, 200, show i)
    -- A plus in the query is a plus, as sent; a space is %20.
    (.body) <$> request r methodGet hello "?name=a+b.txt" [editor] "" `shouldReturn` "2"
    (.status) <$> request r methodGet hello "?name=meeting+notes.txt" [editor] "" `shouldReturn` 404
    -- Names that differ in case are two files; no other case finds either.
    (.status) <$> fetch r editor hello "readme.txt" `shouldReturn` 404
    -- An escape is decoded once: %2541 is a %41 in the name, not an A.
    (.status) <$> request r methodGet hello "?name=%2541.txt" [editor] "" `shouldReturn` 404

  it "is refused with the application's words when its rule refuses it, and nothing is stored" $ withPasswords backing $ \r -> do
    let refused =
          [ ("?name=a%2Fb.txt", "An attachment's name cannot have slashes or control characters in it.")
          , ("?name=a%5Cb.txt", "An attachment's name cannot have slashes or control characters in it.")
          , ("?name=a%01b.txt", "An attachment's name cannot have slashes or control characters in it.")
          , ("?name=", "An attachment's name cannot be blank.")
          , ("?name=%20%20", "An attachment's name cannot be blank.")
          , ("?name=%20x.txt", "An attachment's name cannot start or end with white space: this one would be stored as \"x.txt\". Send that name.")
          , ("?name=%FF.txt", "The parameter name is not UTF-8 text.")
          , ("?name", "The parameter name needs a value: ?name=diagram.png.")
          , ("?name=a.txt&name=b.txt", "The query has the parameter name more than once: name one file.")
          , ("", "Give the file's name in the query: ?name=diagram.png.")
          ]
    for_ refused $ \(query, why) -> do
      reply <- request r methodPut hello query [editor, ("Content-Type", "text/plain")] "x"
      (query, refusalOf reply) `shouldBe` (query, (400, Just "Invalid", Just why))
    listed r editor hello `shouldReturn` []

  it "is refused with a form's media type, which is not the file's" $ withPasswords backing $ \r -> do
    for_ ["application/x-www-form-urlencoded", "multipart/form-data; boundary=x"] $ \t -> do
      reply <- attach r editor hello "plot.png" t "x"
      (t, reply.status) `shouldBe` (t, 415)
    -- Without a Content-Type the file has none: application/octet-stream.
    bare <- request r methodPut hello "?name=plain" [editor] "x"
    (bare.status, (\e -> e.mediaType) <$> decoded bare) `shouldBe` (201, Right "application/octet-stream")
    map (\e -> e.name) <$> listed r editor hello `shouldReturn` ["plain"]

takenNames :: Backing -> Spec
takenNames backing = describe "a name that is taken" $ do
  it "is replaced only over the version read, in its place" $ withPasswords backing $ \r -> do
    first' <- attach r editor hello "plan.txt" "text/plain" "First plan.\n" >>= jsonAs @Entry 201
    _ <- attach r editor hello "other.txt" "text/plain" "Other.\n" >>= jsonAs @Entry 201
    let current = encodeUtf8 (fromMaybe "" first'.version)
        stale = "\"0000000000000000000000000000000000000000000000000000000000000000\""
        unchanged = do
          (.body) <$> fetch r editor hello "plan.txt" `shouldReturn` "First plan.\n"
          map (\e -> e.name) <$> listed r editor hello `shouldReturn` ["plan.txt", "other.txt"]
    plain <- attach r editor hello "plan.txt" "text/plain" "Over it.\n"
    refusalOf plain `shouldSatisfy` (\(s, e, _) -> (s, e) == (428, Just "Conflict"))
    unchanged
    createOnly <- request r methodPut hello "?name=plan.txt" [editor, ("If-None-Match", "*")] "Over it.\n"
    createOnly.status `shouldBe` 412
    unchanged
    old <- request r methodPut hello "?name=plan.txt" [editor, ("If-Match", stale)] "Over it.\n"
    old.status `shouldBe` 412
    unchanged
    nothingThere <- request r methodPut hello "?name=no-plan.txt" [editor, ("If-Match", current)] "Over it.\n"
    nothingThere.status `shouldBe` 412
    unchanged
    replaced <- request r methodPut hello "?name=plan.txt" [editor, ("If-Match", current), ("Content-Type", "text/markdown")] "Second plan.\n"
    replaced.status `shouldBe` 200
    entry :: Entry <- json replaced
    (entry.mediaType, entry.version == first'.version, decodeUtf8 <$> header "ETag" replaced) `shouldBe` ("text/markdown", False, entry.version)
    (.body) <$> fetch r editor hello "plan.txt" `shouldReturn` "Second plan.\n"
    map (\e -> e.name) <$> listed r editor hello `shouldReturn` ["plan.txt", "other.txt"]
    -- The version read before the replace is stale now.
    (.status) <$> request r methodPut hello "?name=plan.txt" [editor, ("If-Match", current)] "Third.\n" `shouldReturn` 412

  it "refuses a condition it does not take" $ withPasswords backing $ \r -> do
    for_ [[("If-Match", "*")], [("If-Match", "W/\"abc\"")], [("If-None-Match", "\"abc\"")], [("If-Match", "\"abc\""), ("If-None-Match", "*")]] $ \hs -> do
      reply <- request r methodPut hello "?name=x.txt" (editor : hs) "x"
      (hs, reply.status) `shouldBe` (hs, 400)
    listed r editor hello `shouldReturn` []

deleting :: Backing -> Spec
deleting backing = describe "a file taken off" $ do
  it "is gone from the list and from the browser's page, and its address answers 404" $ withPasswords backing $ \r -> do
    _ <- attach r editor hello "gone.txt" "text/plain" "Soon gone.\n" >>= jsonAs @Entry 201
    (.status) <$> request r methodDelete hello "?name=gone.txt" [editor] "" `shouldReturn` 204
    listed r editor hello `shouldReturn` []
    (page, _) <- browsersPage r
    page.attachments `shouldBe` []
    (.status) <$> fetch r editor hello "gone.txt" `shouldReturn` 404
    (.status) <$> request r methodDelete hello "?name=gone.txt" [editor] "" `shouldReturn` 404
    (.status) <$> request r methodDelete hello "" [editor] "" `shouldReturn` 400

limits :: Backing -> Spec
limits backing = describe "a file over the application's limit" $ do
  it "is refused by its length before its body is read, with the application's words, and nothing is stored" $ withPasswords backing $ \r -> do
    _ <- attach r editor hello "small.txt" "text/plain" "Small.\n" >>= jsonAs @Entry 201
    earlier <- listed r editor hello
    -- Expect: 100-continue, as curl sends it with a large file: the body
    -- goes only once the server asks for it, which it does not.
    reply <- request r methodPut hello "?name=big.bin" [editor, ("Content-Type", "application/octet-stream"), ("Expect", "100-continue")] (BSL.replicate (fromIntegral maxAttachmentSize + 1) 0)
    later <- listed r editor hello
    (refusalOf reply, later) `shouldBe` ((413, Just "Invalid", Just attachmentTooLarge), earlier)

  it "is taken at the limit itself" $ withPasswords backing $ \r -> do
    reply <- attach r editor hello "full.bin" "application/octet-stream" (BSL.replicate (fromIntegral maxAttachmentSize) 7)
    (reply.status, (\e -> e.size) <$> decoded reply) `shouldBe` (201, Right (fromIntegral maxAttachmentSize))

permissions :: Backing -> Spec
permissions backing = describe "who may do what" $ do
  it "lets a key with the right read list and fetch, and refuses it a write" $ withPasswords backing $ \r -> do
    _ <- attach r editor hello "kept.txt" "text/plain" "Kept.\n" >>= jsonAs @Entry 201
    earlier <- listed r editor hello
    k <- keyOf r "user-editor" ReadRight
    let key = ("Authorization", "Bearer " <> encodeUtf8 k.key)
    (.status) <$> request r methodGet hello "" [key] "" `shouldReturn` 200
    (.body) <$> fetch r key hello "kept.txt" `shouldReturn` "Kept.\n"
    (.status) <$> request r methodHead hello "?name=kept.txt" [key] "" `shouldReturn` 200
    put' <- attach r key hello "new.txt" "text/plain" "New.\n"
    deleted <- request r methodDelete hello "?name=kept.txt" [key] ""
    later <- listed r editor hello
    (put'.status, deleted.status, later) `shouldBe` (403, 403, earlier)

  it "refuses a user who may read the page but not edit it" $ withPasswords backing $ \r -> do
    let wiki = "team/projects/p/the-wiki/"
    _ <- attach r admin wiki "spec.txt" "text/plain" "Spec.\n" >>= jsonAs @Entry 201
    earlier <- listed r admin wiki
    listed r rita wiki `shouldReturn` earlier
    (.body) <$> fetch r rita wiki "spec.txt" `shouldReturn` "Spec.\n"
    put' <- attach r rita wiki "mine.txt" "text/plain" "Mine.\n"
    replaced <- request r methodPut wiki "?name=spec.txt" [rita, ("If-Match", encodeUtf8 (fromMaybe "" (head earlier >>= \e -> e.version)))] "Mine.\n"
    deleted <- request r methodDelete wiki "?name=spec.txt" [rita] ""
    later <- listed r admin wiki
    (put'.status, replaced.status, deleted.status, later) `shouldBe` (403, 403, 403, earlier)

  it "hides a private space's pages, as pages not there" $ withPasswords backing $ \r -> do
    let wiki = "team/projects/p/the-wiki/"
    _ <- attach r admin wiki "spec.txt" "text/plain" "Spec.\n" >>= jsonAs @Entry 201
    earlier <- listed r admin wiki
    for_ [(methodGet, ""), (methodGet, "?name=spec.txt"), (methodPut, "?name=x.txt"), (methodDelete, "?name=spec.txt")] $ \(m, query) -> do
      reply <- request r m wiki query [editor] "x"
      (m, query, reply.status) `shouldBe` (m, query, 404)
    listed r admin wiki `shouldReturn` earlier

  it "refuses a page of an encrypted space, whose files the server cannot read" $ withPasswords backing $ \r -> do
    slug <- encryptedSpace r
    for_ [(methodGet, ""), (methodGet, "?name=x.txt"), (methodPut, "?name=x.txt"), (methodDelete, "?name=x.txt")] $ \(m, query) -> do
      reply <- request r m (slug <> "/") query [admin, ("Content-Type", "text/plain")] "x"
      (m, query, refusalOf reply) `shouldSatisfy` (\(_, _, (s, e, _)) -> (s, e) == (409, Just "Stale"))

  it "asks for a credential, and answers a method the address does not take with 405" $ withPasswords backing $ \r -> do
    (.status) <$> request r methodGet hello "" [] "" `shouldReturn` 401
    (.status) <$> request r methodPut hello "?name=x.txt" [] "x" `shouldReturn` 401
    (.status) <$> request r "POST" hello "?name=x.txt" [editor] "x" `shouldReturn` 405
    -- An address that is no page's (a page's history) is 404 for any method.
    (.status) <$> request r "POST" (hello <> "history/") "" [editor] "x" `shouldReturn` 404
    (.status) <$> request r methodGet "nothing-here" "" [editor] "" `shouldReturn` 404
    (.status) <$> sendBare r methodGet (r.public <> "/api/attachments/x") [editor] "" `shouldReturn` 404
    (.status) <$> sendBare r methodGet (r.public <> "/api/attachments/x") [] "" `shouldReturn` 401
  where
    methodHead = "HEAD"

-- | An API key of a user's, with a right, made in the storage.
keyOf :: (HasCallStack) => Running -> Text -> KeyRight -> IO MadeApiKey
keyOf r user right = do
  now <- getCurrentTime
  made <- withStorageAside r.place $ \st -> makeApiKey st now (UserId user) (right == AdminRight) NewApiKey {name = "A program", right, expiresAt = Nothing, password = Nothing}
  either (\e -> panic ("no key: " <> show e)) pure made
