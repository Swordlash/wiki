-- | The API for a page's Markdown (WP5.138 of docs/PLAN.md), over real
-- HTTP, as @curl -u login:password@ drives it: signing in with HTTP Basic
-- alone, a page's Markdown read and written whole, pages and spaces
-- created at their addresses, the Markdown reference the writing assistant
-- is given, and the listing and the search that find the pages to read.
--
-- Every request goes through 'sendBare', without the application's marker
-- header, since a program's request has none. The demo's wiki is the
-- server's: Blog and Docs public, Team private; the editor edits Blog
-- through their group, the reader views Team's Projects; nobody has a
-- password until the administrator gives one.
module Test.Server.MarkdownApi (spec, encryptedSpace) where

import Crypto.OPAQUE qualified as O
import Data.Aeson (FromJSON, Value (..), decodeStrict, eitherDecode, encode)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteArray.Encoding (Base (..), convertToBase)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime)
import Network.HTTP.Types (Header, Method, hContentType, methodGet, methodPost, methodPut)
import Protolude
import Test.Database (Backing)
import Test.Hspec
import Test.Server.Fixture
import Test.Server.Hygiene (setUp)
import Web.HttpApiData (toHeader)
import Wiki.Api (AdminApi (..), PagesApi (..), SpacesApi (..), WikiApi (..))
import Wiki.Api.Account (Account, AccountDisabled (..))
import Wiki.Api.Encrypted (EncryptSpace (..), NewGrant (..), NewSealedPage (..))
import Wiki.Api.Error (ApiError (..))
import Wiki.Api.Links (links)
import Wiki.Api.Opaque (PasswordRecord (..), RegistrationChallenge (..), RegistrationFor (..), RegistrationStart (..), Stretch (..), messageOf)
import Wiki.Api.Opaque qualified as Opaque
import Wiki.Api.Types qualified as Api
import Wiki.Api.Vault (NewPublicKey (..), VaultChange (..))
import Wiki.Api.Version (PageVersion (..))
import Wiki.Core.Assistant (Mode (..), ModelName (..))
import Wiki.Core.Assistant.Prompt (openAiRequest)
import Wiki.Core.Assistant.Reference (markdownReference)
import Wiki.Core.Encrypted (Encapsulated (..), PageKey (..), SealedContent (..), VersionTag (..), firstChain)
import Wiki.Core.Id (PageId (..), SpaceId (..), UserId (..))
import Wiki.Core.Page qualified as P
import Wiki.Core.PageKind (PageKind)
import Wiki.Core.PageKind qualified as Kind
import Wiki.Core.Path (parsePath)
import Wiki.Core.Search (Piece (..), Snippet (..))
import Wiki.Core.Slug (slugText)
import Wiki.Core.Space qualified as S
import Wiki.Core.Vault
import Wiki.Core.Version (Author (..), VersionInfo (..))
import Wiki.Server.Opaque (opaqueConfig)
import Wiki.Server.Options (ServeOptions (..))
import Wiki.Server.Vault (keyIdOf)
import Wiki.Server.WebAuthn (toBase64Url)

spec :: Backing -> Spec
spec backing = describe "the API for a page's Markdown" $ do
  signingIn backing
  pages backing
  creatingPages backing
  reference backing
  listing backing
  creatingSpaces backing
  search backing

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

editor :: Header
editor = basic "editor" "editor's password"

-- | The address of the API for a path after @api\/markdown\/@.
at :: Running -> Text -> Text
at r path = r.public <> "/api/markdown/" <> path

-- | A request to the API, as a program sends it: with these headers only.
request :: Running -> Method -> Text -> [Header] -> LByteString -> IO Reply
request r method path = sendBare r method (at r path)

-- | How many passwords the server has checked in full, with their
-- stretching, as its metrics on the admin listener count them.
fullChecks :: Running -> IO Int
fullChecks r = do
  reply <- getUrl r (r.admin <> "/metrics")
  let counted = [n | line <- T.lines (decodeUtf8 (BSL.toStrict reply.body)), Just rest <- [T.stripPrefix "wiki_api_password_checks_total " line], Just n <- [readMaybe (T.unpack rest)]]
  maybe (expectationFailure ("no count of password checks in the metrics: " <> show reply) >> pure 0) pure (head counted)

-- | The record of a password for a user, made with this stretching rather
-- than the one the server asks for: what a browser that chose its own
-- stretching would send. Registered on the admin listener.
stretchedRecord :: Running -> UserId -> Text -> Stretch -> IO PasswordRecord
stretchedRecord r u password stretch = do
  (st, req) <- either (\e -> panic ("no request: " <> show e)) pure =<< O.registrationRequest (encodeUtf8 password)
  reply <- send r methodPost (r.admin <> "/admin/" <> links.passwordRegistration) [(hContentType, "application/json")] (encode RegistrationStart {for = ForUser u, request = messageOf (O.encode req)})
  challenge :: RegistrationChallenge <- jsonAs 200 reply
  response <- maybe (panic "no response") pure (Opaque.bytesOf challenge.response >>= rightToMaybe . O.decode)
  (record, _) <- either (\e -> panic ("no record: " <> show e)) pure =<< O.finalizeRegistration (opaqueConfig stretch) st response O.defaultIdentities
  pure PasswordRecord {credential = challenge.credential, record = messageOf (O.encode record), stretch}

-- | The error a refusal names, from its JSON body.
errorName :: Reply -> Maybe Text
errorName reply = case decodeStrict (BSL.toStrict reply.body) of
  Just (Object o) | Just (String name) <- KeyMap.lookup "error" o -> Just name
  _ -> Nothing

signingIn :: Backing -> Spec
signingIn backing = describe "signing in" $ do
  it "reads a page with the right password, and checks it in full only once in a while" $ withPasswords backing $ \r -> do
    counted <- fullChecks r
    first' <- request r methodGet "s/blog/p/hello-world/" [editor] ""
    first'.status `shouldBe` 200
    fullChecks r `shouldReturn` (counted + 1)
    -- Remembered: the next requests run no stretching.
    replicateM_ 2 $ (.status) <$> request r methodGet "s/blog/p/hello-world/" [editor] "" `shouldReturn` 200
    fullChecks r `shouldReturn` (counted + 1)
    -- Another password for the same login is checked in full, and refused.
    (.status) <$> request r methodGet "s/blog/p/hello-world/" [basic "editor" "not the password"] "" `shouldReturn` 401
    fullChecks r `shouldReturn` (counted + 2)

  it "forgets a remembered password once the password changes" $ withPasswords backing $ \r -> do
    (.status) <$> request r methodGet "s/blog/p/hello-world/" [editor] "" `shouldReturn` 200
    givePassword r "user-editor" "a new password"
    (.status) <$> request r methodGet "s/blog/p/hello-world/" [editor] "" `shouldReturn` 401
    (.status) <$> request r methodGet "s/blog/p/hello-world/" [basic "editor" "a new password"] "" `shouldReturn` 200

  it "refuses a wrong password, an unknown login and a login without a password alike" $ withDemoRunning backing identity $ \r -> do
    givePassword r "user-editor" "editor's password"
    replies <- for [basic "editor" "wrong", basic "nobody" "editor's password", basic "reader" "reader's password"] $ \h -> request r methodGet "s/blog/p/hello-world/" [h] ""
    map (.status) replies `shouldBe` [401, 401, 401]
    map (.body) replies `shouldSatisfy` \case
      b : rest -> all (== b) rest
      [] -> False
    map (header "WWW-Authenticate") replies `shouldSatisfy` all (== Just "Basic realm=\"wiki\", charset=\"UTF-8\"")

  it "refuses an account with no password, as one with passkeys only has" $ withDemoRunning backing identity $ \r -> do
    reply <- request r methodGet "s/team/projects/p/the-wiki/" [basic "reader" ""] ""
    reply.status `shouldBe` 401
    decodeUtf8 (BSL.toStrict reply.body) `shouldSatisfy` T.isInfixOf "passkeys only"

  it "counts wrong passwords toward the form's limit, which then refuses both ways" $ withPasswords backing $ \r -> do
    replicateM_ 5 $ (.status) <$> request r methodGet "s/blog/p/hello-world/" [basic "editor" "guess"] "" `shouldReturn` 401
    limited <- request r methodGet "s/blog/p/hello-world/" [editor] ""
    limited.status `shouldBe` 429
    errorName limited `shouldBe` Just "TooManyRequests"
    header "Retry-After" limited `shouldSatisfy` isJust
    -- The form's own sign-in is held to the same count.
    (.status) <$> passwordSignIn r (\l -> r.public <> "/" <> l) [] "editor" "editor's password" `shouldReturn` 429
    -- Another login is not.
    (.status) <$> request r methodGet "s/team/projects/p/the-wiki/" [basic "reader" "reader's password"] "" `shouldReturn` 200

  it "refuses an account once it is disabled, though its password was remembered" $ withPasswords backing $ \r -> do
    (.status) <$> request r methodGet "s/blog/p/hello-world/" [editor] "" `shouldReturn` 200
    _ :: Account <- sendJson r methodPut (r.admin <> "/admin/" <> links.admin.setDisabled (UserId "user-editor")) AccountDisabled {disabled = True} >>= jsonAs 200
    (.status) <$> request r methodGet "s/blog/p/hello-world/" [editor] "" `shouldReturn` 401

  it "refuses, as a wrong password, an account whose record asks for more stretching than the budget" $ withDemoRunning backing (\o -> o {apiStretchBudget = Stretch {opsLimit = 1, memLimit = 8192}}) $ \r -> do
    -- The editor's record asks for twice the passes and memory the budget
    -- allows; the reader's is the tests' own, within it.
    record <- stretchedRecord r (UserId "user-editor") "editor's password" Stretch {opsLimit = 2, memLimit = 16384}
    _ :: Account <- sendJson r methodPut (r.admin <> "/admin/" <> links.admin.setPassword (UserId "user-editor")) record >>= jsonAs 200
    givePassword r "user-reader" "reader's password"
    counted <- fullChecks r
    right <- request r methodGet "s/blog/p/hello-world/" [editor] ""
    wrong <- request r methodGet "s/blog/p/hello-world/" [basic "editor" "not the password"] ""
    (right.status, right.body) `shouldBe` (401, wrong.body)
    -- Each ran one check, against the fake record, as a login without a
    -- record does.
    fullChecks r `shouldReturn` (counted + 2)
    (.status) <$> request r methodGet "s/team/projects/p/the-wiki/" [basic "reader" "reader's password"] "" `shouldReturn` 200

  it "refuses an account with a second factor, whose password alone signs nobody in" $ withPasswords backing $ \r -> do
    token <- signInAs r "editor" "editor's password"
    _ <- setUp r token
    refused <- request r methodGet "s/blog/p/hello-world/" [editor] ""
    refused.status `shouldBe` 403
    decodeUtf8 (BSL.toStrict refused.body) `shouldSatisfy` T.isInfixOf "second factor"

  it "refuses the session's cookie alone, and reads nothing of it beside a password" $ withPasswords backing $ \r -> do
    token <- signInAs r "admin" "admin's password"
    alone <- request r methodGet "s/blog/p/hello-world/" [cookie token] ""
    alone.status `shouldBe` 401
    -- The admin's cookie gives the editor nothing more: Team is private.
    (.status) <$> request r methodGet "s/team/p/team-handbook/" [cookie token, editor] "" `shouldReturn` 404

pages :: Backing -> Spec
pages backing = describe "a page's Markdown" $ do
  it "is read, written whole and read again, its ETag naming each version" $ withPasswords backing $ \r -> do
    read1 <- request r methodGet "s/blog/p/hello-world/" [editor] ""
    read1.status `shouldBe` 200
    header "Content-Type" read1 `shouldBe` Just "text/markdown; charset=utf-8"
    tag1 <- maybe (expectationFailure "no ETag" >> pure "") pure (header "ETag" read1)
    (page1, _) <- appPage r
    read1.body `shouldBe` toS (encodeUtf8 page1.body)
    history1 :: [VersionInfo] <- versionsOf r
    let new = toS (encodeUtf8 ("# Hello again\n\nWritten by a program: ünïcode too.\n" :: Text)) :: LByteString
    written <- request r methodPut "s/blog/p/hello-world/" [editor, ("If-Match", tag1)] new
    written.status `shouldBe` 204
    tag2 <- maybe (expectationFailure "no ETag" >> pure "") pure (header "ETag" written)
    tag2 `shouldNotBe` tag1
    read2 <- request r methodGet "s/blog/p/hello-world/" [editor] ""
    (read2.body, header "ETag" read2) `shouldBe` (new, Just tag2)
    -- The title stays; the history has a new version, by the editor.
    (page2, _) <- appPage r
    page2.title `shouldBe` page1.title
    history2 <- versionsOf r
    length history2 `shouldBe` length history1 + 1
    map (\v -> (toHeader (PageVersion v.updatedAt), (\a -> a.id) <$> v.author)) (take 1 history2) `shouldBe` [(tag2, Just (UserId "user-editor"))]

  it "is not written over a version saved since, with If-Match" $ withPasswords backing $ \r -> do
    old <- request r methodGet "s/blog/p/hello-world/" [editor] ""
    let tag = fromMaybe "" (header "ETag" old)
    (.status) <$> request r methodPut "s/blog/p/hello-world/" [editor, ("If-Match", tag)] "First." `shouldReturn` 204
    stale <- request r methodPut "s/blog/p/hello-world/" [editor, ("If-Match", tag)] "Second."
    stale.status `shouldBe` 412
    (.body) <$> request r methodGet "s/blog/p/hello-world/" [editor] "" `shouldReturn` "First."
    -- Without If-Match the write goes through.
    (.status) <$> request r methodPut "s/blog/p/hello-world/" [editor] "Third." `shouldReturn` 204
    (.body) <$> request r methodGet "s/blog/p/hello-world/" [editor] "" `shouldReturn` "Third."

  it "reads a page of a nested space, and a space's home page, at their reading addresses" $ withPasswords backing $ \r -> do
    (.status) <$> request r methodGet "s/team/projects/p/the-wiki/" [rita] "" `shouldReturn` 200
    (.status) <$> request r methodGet "s/team/projects/p/the-wiki" [rita] "" `shouldReturn` 200
    (.status) <$> request r methodGet "s/team/projects/" [rita] "" `shouldReturn` 200
    (.status) <$> request r methodGet "s/team/projects/p/no-such-page/" [rita] "" `shouldReturn` 404

  it "is refused to a user who may read but not edit, and hidden from one who may not read" $ withPasswords backing $ \r -> do
    tour <- request r methodGet "s/docs/p/tour/" [editor] ""
    tour.status `shouldBe` 200
    refused <- request r methodPut "s/docs/p/tour/" [editor] "Not mine to write."
    refused.status `shouldBe` 403
    (.body) <$> request r methodGet "s/docs/p/tour/" [editor] "" `shouldReturn` tour.body
    hidden <- request r methodGet "s/team/p/team-handbook/" [editor] ""
    nowhere <- request r methodGet "s/team/p/no-such-page/" [editor] ""
    (hidden.status, hidden.body) `shouldBe` (404, encode NotFound)
    (nowhere.status, nowhere.body) `shouldBe` (404, encode NotFound)
    (.status) <$> request r methodPut "s/team/p/team-handbook/" [editor] "Hidden." `shouldReturn` 404

  it "is refused both ways in an encrypted space, which the server cannot read" $ withPasswords backing $ \r -> do
    slug <- encryptedSpace r
    for_ [(methodGet, ""), (methodPut, "Plain text.")] $ \(method, body) -> do
      reply <- request r method ("s/" <> slug <> "/") [ada] body
      reply.status `shouldBe` 409
      errorName reply `shouldBe` Just "Stale"
      decodeUtf8 (BSL.toStrict reply.body) `shouldSatisfy` T.isInfixOf "the server cannot read them"
  where
    -- The editor's view of the page through the application's own API,
    -- with the cookie of the editor's session.
    appPage :: Running -> IO (P.Page, Header)
    appPage r = do
      session <- cookie <$> signInAs r "editor" "editor's password"
      blog :: SpaceId <- (\(s :: S.Space) -> s.id) <$> ok200 (getUrlWith r (r.public <> "/" <> links.spaces.atPath (slugs "blog")) [session])
      (,session) <$> ok200 (getUrlWith r (r.public <> "/" <> links.spaces.pageAt blog (slugs "hello-world")) [session])
    -- The page's history, newest first, as the application lists it.
    versionsOf :: Running -> IO [VersionInfo]
    versionsOf r = do
      (page, session) <- appPage r
      ok200 (getUrlWith r (r.public <> "/" <> links.pages.versions page.id Nothing Nothing) [session])
    slugs = either (panic . show) identity . parsePath

-- | How many pages each space has, as the administrator's session reads
-- the counts from the application's API: every space, encrypted ones too,
-- so that a page created anywhere changes them.
pageCounts :: Running -> IO (Map SpaceId Int)
pageCounts r = do
  token <- signInAs r "admin" "admin's password"
  ok200 (getUrlWith r (r.public <> "/" <> links.spaces.pageCounts) [cookie token])

-- | A page as the editor's session reads it from the application's API, by
-- its space's path and its own, with the page's history, newest first.
editorsPage :: Running -> Text -> Text -> IO (P.Page, [VersionInfo])
editorsPage r space path = do
  session <- cookie <$> signInAs r "editor" "editor's password"
  sid :: SpaceId <- (\(s :: S.Space) -> s.id) <$> ok200 (getUrlWith r (r.public <> "/" <> links.spaces.atPath (slugsOf space)) [session])
  page :: P.Page <- ok200 (getUrlWith r (r.public <> "/" <> links.spaces.pageAt sid (slugsOf path)) [session])
  (page,) <$> ok200 (getUrlWith r (r.public <> "/" <> links.pages.versions page.id Nothing Nothing) [session])
  where
    slugsOf = either (panic . show) identity . parsePath

creatingPages :: Backing -> Spec
creatingPages backing = describe "creating a page" $ do
  it "creates a page at a free address, titled from its slug, with one version by its author" $ withPasswords backing $ \r -> do
    (.status) <$> request r methodGet "s/blog/p/release-notes/" [editor] "" `shouldReturn` 404
    -- The counts see a page created: the other tests' "nothing was made".
    counts <- pageCounts r
    let text = toS (encodeUtf8 ("Written by a program: ünïcode too.\n" :: Text)) :: LByteString
    made <- request r methodPut "s/blog/p/release-notes/" [editor] text
    (made.status, header "Location" made) `shouldBe` (201, Just "/api/markdown/s/blog/p/release-notes/")
    pageCounts r >>= (`shouldNotBe` counts)
    tag <- maybe (expectationFailure "no ETag" >> pure "") pure (header "ETag" made)
    again <- follow r editor "/api/markdown/s/blog/p/release-notes/"
    (again.status, again.body, header "ETag" again) `shouldBe` (200, text, Just tag)
    blog :: Listed <- follow r editor "/api/list/s/blog/" >>= answerOf 200
    listed <- only "new page" (filter (\p -> p.markdown == "/api/markdown/s/blog/p/release-notes/") (fromMaybe [] blog.pages))
    (listed.title, listed.kind) `shouldBe` ("Release notes", Kind.Document)
    (page, history) <- editorsPage r "blog" "release-notes"
    page.title `shouldBe` "Release notes"
    map (\v -> (toHeader (PageVersion v.updatedAt), (\a -> a.id) <$> v.author)) history `shouldBe` [(tag, Just (UserId "user-editor"))]
    -- The page is there now: a PUT writes over it, as over any page.
    (.status) <$> request r methodPut "s/blog/p/release-notes/" [editor] "Second." `shouldReturn` 204

  it "takes the title and the kind from the query, and creates a page under a page" $ withPasswords backing $ \r -> do
    made <- request r methodPut "s/blog/p/q3/?title=Quarterly%20plan%20%E2%80%93%20Q3&kind=presentation" [editor] "# Plan\n"
    (made.status, header "Location" made) `shouldBe` (201, Just "/api/markdown/s/blog/p/q3/")
    nested <- request r methodPut "s/blog/p/q3/notes/" [editor] "Notes."
    (nested.status, header "Location" nested) `shouldBe` (201, Just "/api/markdown/s/blog/p/q3/notes/")
    blog :: Listed <- follow r editor "/api/list/s/blog/" >>= answerOf 200
    q3 <- only "q3" (filter (\p -> p.markdown == "/api/markdown/s/blog/p/q3/") (fromMaybe [] blog.pages))
    (q3.title, q3.kind) `shouldBe` ("Quarterly plan – Q3", Kind.Presentation)
    map (\c -> (c.title, c.kind, c.markdown)) <$> q3.children `shouldBe` Just [("Notes", Kind.Document, "/api/markdown/s/blog/p/q3/notes/")]
    (.body) <$> follow r editor "/api/markdown/s/blog/p/q3/notes/" `shouldReturn` "Notes."
    -- A slug that is one of the router's own words, written with its ~.
    escaped <- request r methodPut "s/blog/p/q3/~history/" [editor] "Its history."
    (escaped.status, header "Location" escaped) `shouldBe` (201, Just "/api/markdown/s/blog/p/q3/~history/")
    (.body) <$> follow r editor "/api/markdown/s/blog/p/q3/~history/" `shouldReturn` "Its history."

  it "creates only where there is no page, with If-None-Match: *" $ withPasswords backing $ \r -> do
    old <- request r methodGet "s/blog/p/hello-world/" [editor] ""
    refused <- request r methodPut "s/blog/p/hello-world/" [editor, ("If-None-Match", "*")] "Over it."
    (refused.status, errorName refused) `shouldBe` (412, Just "Conflict")
    (.body) <$> request r methodGet "s/blog/p/hello-world/" [editor] "" `shouldReturn` old.body
    (.status) <$> request r methodPut "s/blog/p/fresh/" [editor, ("If-None-Match", "*")] "New." `shouldReturn` 201
    (.status) <$> request r methodPut "s/blog/p/fresh/" [editor, ("If-None-Match", "*")] "Again." `shouldReturn` 412
    (.body) <$> request r methodGet "s/blog/p/fresh/" [editor] "" `shouldReturn` "New."

  it "creates nothing with If-Match, under a page or in a space not there, or with a bad slug, title, kind or If-None-Match" $ withPasswords backing $ \r -> do
    counts <- pageCounts r
    counts `shouldNotBe` mempty
    tag <- fromMaybe "" . header "ETag" <$> request r methodGet "s/blog/p/hello-world/" [editor] ""
    for_
      [ (404, "s/blog/p/not-yet/", [("If-Match", tag)])
      , (404, "s/blog/p/no-such-page/below/", [])
      , (404, "s/no-such-space/p/a-page/", [])
      , (404, "s/no-such-space/", [])
      , (400, "s/blog/p/Not_A_Slug/", [])
      , -- A slug that is one of the router's words is written ~history.
        (400, "s/blog/p/history/", [])
      , (400, "s/blog/p/blank/?title=", [])
      , (400, "s/blog/p/odd/?kind=spreadsheet", [])
      , (400, "s/blog/p/tagged/", [("If-None-Match", tag)])
      ]
      $ \(code, path, headers) -> do
        reply <- request r methodPut path (editor : headers) "Nothing."
        (path, reply.status) `shouldBe` (path, code)
    pageCounts r `shouldReturn` counts
    (.status) <$> request r methodGet "s/blog/p/not-yet/" [editor] "" `shouldReturn` 404

  it "is refused to a user who may not create a page there, and makes none" $ withPasswords backing $ \r -> do
    counts <- pageCounts r
    -- The editor reads Docs and may not edit it; the reader views Projects.
    (.status) <$> request r methodPut "s/docs/p/not-mine/" [editor] "Not mine." `shouldReturn` 403
    (.status) <$> request r methodPut "s/team/projects/p/not-mine/" [rita] "Not mine." `shouldReturn` 403
    -- Team is hidden from the editor, as a space that is not there.
    (.status) <$> request r methodPut "s/team/p/not-mine/" [editor] "Not mine." `shouldReturn` 404
    pageCounts r `shouldReturn` counts

  it "is refused in an encrypted space, and makes no page there" $ withPasswords backing $ \r -> do
    slug <- encryptedSpace r
    counts <- pageCounts r
    reply <- request r methodPut ("s/" <> slug <> "/p/plain/") [ada] "Plain text."
    (reply.status, errorName reply) `shouldBe` (409, Just "Stale")
    pageCounts r `shouldReturn` counts

reference :: Backing -> Spec
reference backing = describe "the Markdown reference" $
  it "is the text the writing assistant's prompt starts with, for anyone" $
    withDemoRunning backing identity $ \r -> do
      reply <- request r methodGet "reference" [] ""
      reply.status `shouldBe` 200
      header "Content-Type" reply `shouldBe` Just "text/markdown; charset=utf-8"
      reply.body `shouldBe` toS (encodeUtf8 markdownReference)
      case openAiRequest Agent (ModelName "a-model") "low" of
        Object o | Just (String instructions) <- KeyMap.lookup "instructions" o -> instructions `shouldSatisfy` T.isPrefixOf (decodeUtf8 (BSL.toStrict reply.body))
        other -> expectationFailure ("no instructions in the request: " <> show other)

-- | A space the admin has encrypted, with its home page, as the browser
-- encrypts one (the ciphertexts are bytes of no meaning): its slug.
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

-- | A space as the listing gives it ("Wiki.Server.ListApi"), read back. A
-- key left out of the JSON is 'Nothing'.
data Listed = Listed
  { title :: Text
  , path :: Text
  , list :: Maybe Text
  , home :: Maybe Text
  , encrypted :: Bool
  , subspaces :: Maybe [Listed]
  , pages :: Maybe [ListedPage]
  }
  deriving stock (Show, Generic)

instance FromJSON Listed

-- | A page as the listing gives it.
data ListedPage = ListedPage
  { title :: Text
  , kind :: PageKind
  , markdown :: Text
  , updated :: UTCTime
  , template :: Bool
  , children :: Maybe [ListedPage]
  }
  deriving stock (Show, Generic)

instance FromJSON ListedPage

-- | The listing of the top level.
newtype TopLevel = TopLevel {spaces :: [Listed]}
  deriving stock (Show, Generic)

instance FromJSON TopLevel

-- | A hit of the search, read back.
data Hit = Hit
  { title :: Text
  , titleMarked :: Snippet
  , space :: HitSpace
  , markdown :: Text
  , excerpt :: Text
  , excerptMarked :: Snippet
  }
  deriving stock (Show, Generic)

instance FromJSON Hit

-- | The space of a hit.
data HitSpace = HitSpace {path :: Text, title :: Text}
  deriving stock (Show, Generic)

instance FromJSON HitSpace

-- | The search's answer.
newtype Hits = Hits {hits :: [Hit]}
  deriving stock (Show, Generic)

instance FromJSON Hits

-- | A GET of an address the API gave (a path from the listener's root),
-- as a program sends it.
follow :: Running -> Header -> Text -> IO Reply
follow r who address = sendBare r methodGet (r.public <> address) [who] ""

-- | The JSON of a reply, of the type it should be, with this status.
answerOf :: (HasCallStack, FromJSON a) => Int -> Reply -> IO a
answerOf code reply = do
  when (reply.status /= code) $ expectationFailure ("expected status " <> show code <> ", got " <> show reply)
  either (\e -> expectationFailure ("not the JSON expected: " <> e <> ": " <> show reply.body) >> panic "unreachable") pure (eitherDecode reply.body)

-- | The one item of a list, or a failed test that shows the list.
only :: (HasCallStack, Show a) => Text -> [a] -> IO a
only what = \case
  [x] -> pure x
  other -> expectationFailure ("not one " <> toS what <> ": " <> show other) >> panic "unreachable"

-- | Every page of a listing, each before the pages under it.
everyPage :: [ListedPage] -> [ListedPage]
everyPage = concatMap (\p -> p : everyPage (fromMaybe [] p.children))

-- | Every space of a listing, each before its subspaces.
everySpace :: [Listed] -> [Listed]
everySpace = concatMap (\s -> s : everySpace (fromMaybe [] s.subspaces))

ada, rita :: Header
ada = basic "admin" "admin's password"
rita = basic "reader" "reader's password"

listing :: Backing -> Spec
listing backing = describe "the listing" $ do
  it "lists from the top level the spaces the user may read, and not a private one" $ withPasswords backing $ \r -> do
    let titles who = map (\s -> s.title) . (\t -> t.spaces) <$> (follow r who "/api/list/" >>= answerOf @TopLevel 200)
    edgar <- titles editor
    edgar `shouldSatisfy` \ts -> "Blog" `elem` ts && "Docs" `elem` ts
    edgar `shouldNotSatisfy` elem "Team"
    titles ada >>= (`shouldSatisfy` elem "Team")
    -- The rita may read Projects, under Team, and not Team: Team is
    -- there for its path, with no listing and no pages.
    top :: TopLevel <- follow r rita "/api/list" >>= answerOf 200
    team <- only "Team" (filter (\s -> s.title == "Team") top.spaces)
    (team.list, team.home, map (\p -> p.title) <$> team.pages) `shouldBe` (Nothing, Nothing, Nothing)
    map (\s -> (s.path, isJust s.list)) (fromMaybe [] team.subspaces) `shouldBe` [("team/projects", True)]

  it "lists a space as a tree, with addresses that answer" $ withPasswords backing $ \r -> do
    team :: Listed <- follow r ada "/api/list/s/team/" >>= answerOf 200
    (team.title, team.path, team.list, team.encrypted) `shouldBe` ("Team", "team", Just "/api/list/s/team/", False)
    team.home `shouldBe` Just "/api/markdown/s/team/"
    -- The handbook is the home page; the pages are the ones under it.
    let tops = fromMaybe [] team.pages
    map (\p -> p.title) tops `shouldSatisfy` elem "On-call"
    map (\p -> p.title) tops `shouldNotSatisfy` elem "Team handbook"
    map (\p -> p.template) (filter (\p -> p.title == "Weekly sync") tops) `shouldBe` [True]
    handbook <- follow r ada "/api/markdown/s/team/"
    handbook.status `shouldBe` 200
    decodeUtf8 (BSL.toStrict handbook.body) `shouldSatisfy` T.isPrefixOf "# Team handbook"
    for_ (everyPage tops) $ \p -> (p.title,) . (.status) <$> follow r ada p.markdown `shouldReturn` (p.title, 200)
    -- Projects, a subspace, with its own listing, which answers too.
    projects <- only "subspace" (fromMaybe [] team.subspaces)
    (projects.path, projects.home) `shouldBe` ("team/projects", Just "/api/markdown/s/team/projects/")
    again :: Listed <- maybe (expectationFailure "no listing of Projects" >> panic "unreachable") (follow r ada) projects.list >>= answerOf 200
    again.title `shouldBe` "Projects"
    -- Docs has pages under pages, each address answering.
    docs :: Listed <- follow r editor "/api/list/s/docs/" >>= answerOf 200
    let nested = [c | p <- fromMaybe [] docs.pages, c <- fromMaybe [] p.children]
    nested `shouldNotSatisfy` null
    for_ nested $ \p -> (p.title,) . (.status) <$> follow r editor p.markdown `shouldReturn` (p.title, 200)

  it "cuts the tree at a depth" $ withPasswords backing $ \r -> do
    whole :: Listed <- follow r editor "/api/list/s/docs/" >>= answerOf 200
    any (\p -> not (null (fromMaybe [] p.children))) (fromMaybe [] whole.pages) `shouldBe` True
    cut :: Listed <- follow r editor "/api/list/s/docs/?depth=1" >>= answerOf 200
    map (\p -> p.title) <$> cut.pages `shouldBe` map (\p -> p.title) <$> whole.pages
    map (\p -> map (\c -> c.title) <$> p.children) (fromMaybe [] cut.pages) `shouldSatisfy` all isNothing
    top :: TopLevel <- follow r ada "/api/list/?depth=1" >>= answerOf 200
    top.spaces `shouldNotSatisfy` null
    map (\s -> (isJust s.subspaces, isJust s.pages)) top.spaces `shouldSatisfy` all (== (False, False))
    teamTwo :: Listed <- follow r ada "/api/list/s/team/?depth=2" >>= answerOf 200
    map (\s -> isJust s.pages) (fromMaybe [] teamTwo.subspaces) `shouldBe` [True]
    (.status) <$> follow r ada "/api/list/?depth=0" `shouldReturn` 400
    (.status) <$> follow r ada "/api/list/?depth=all" `shouldReturn` 400

  it "lists an encrypted space, marked so, without the pages the server cannot read" $ withPasswords backing $ \r -> do
    slug <- encryptedSpace r
    top :: TopLevel <- follow r ada "/api/list/" >>= answerOf 200
    vault <- only "encrypted space" (filter (\s -> s.path == slug) (everySpace top.spaces))
    (vault.encrypted, vault.home, map (\p -> p.title) <$> vault.pages) `shouldBe` (True, Nothing, Just [])
    own :: Listed <- maybe (expectationFailure "no listing" >> panic "unreachable") (follow r ada) vault.list >>= answerOf 200
    (own.encrypted, map (\p -> p.title) <$> own.pages) `shouldBe` (True, Just [])

  it "answers for a space the user may not read as for one that is not there" $ withPasswords backing $ \r -> do
    hidden <- follow r editor "/api/list/s/team/"
    nowhere <- follow r editor "/api/list/s/no-such-space/"
    (hidden.status, hidden.body) `shouldBe` (404, encode NotFound)
    (nowhere.status, nowhere.body) `shouldBe` (404, encode NotFound)
    -- The rita sees Team above Projects, and may not read it.
    (.status) <$> follow r rita "/api/list/s/team/" `shouldReturn` 404
    (.status) <$> follow r rita "/api/list/s/team/projects/" `shouldReturn` 200
    (.status) <$> follow r ada "/api/list/s/team/" `shouldReturn` 200
    -- A page's address is not a space's listing.
    (.status) <$> follow r ada "/api/list/s/team/p/on-call/" `shouldReturn` 404

  it "signs in as the Markdown does, and takes a PUT only at a space" $ withPasswords backing $ \r -> do
    token <- signInAs r "editor" "editor's password"
    for_ ["/api/list/", "/api/list/s/blog/", "/api/search?q=wiki"] $ \address -> do
      bare <- sendBare r methodGet (r.public <> address) [] ""
      (address, bare.status, errorName bare) `shouldBe` (address, 401, Just "Unauthenticated")
      header "WWW-Authenticate" bare `shouldBe` Just "Basic realm=\"wiki\", charset=\"UTF-8\""
      (.status) <$> sendBare r methodGet (r.public <> address) [cookie token] "" `shouldReturn` 401
      (.status) <$> sendBare r methodGet (r.public <> address) [basic "editor" "wrong"] "" `shouldReturn` 401
      posted <- sendBare r methodPost (r.public <> address) [editor] ""
      -- A space's listing takes a PUT too, which creates the space.
      (posted.status, header "Allow" posted) `shouldBe` (405, Just (if address == "/api/list/s/blog/" then "GET, HEAD, PUT" else "GET, HEAD"))
      (.status) <$> follow r editor address `shouldReturn` 200
    -- The top level has no slug to create a space at.
    put' <- sendBare r methodPut (r.public <> "/api/list/") [editor] "{}"
    (put'.status, header "Allow" put') `shouldBe` (405, Just "GET, HEAD")

-- | A PUT to a space's listing, as a program sends it, with this body.
putSpace :: Running -> Header -> Text -> LByteString -> IO Reply
putSpace r who path = sendBare r methodPut (r.public <> "/api/list/s/" <> path) [who]

creatingSpaces :: Backing -> Spec
creatingSpaces backing = describe "creating a space" $ do
  it "creates a space at a free path, listed, with a home page that reads" $ withPasswords backing $ \r -> do
    made <- putSpace r editor "handbooks/" "{\"title\": \"Hand books\"}"
    header "Location" made `shouldBe` Just "/api/list/s/handbooks/"
    entry :: Listed <- answerOf 201 made
    (entry.title, entry.path, entry.list, entry.home, entry.encrypted) `shouldBe` ("Hand books", "handbooks", Just "/api/list/s/handbooks/", Just "/api/markdown/s/handbooks/", False)
    top :: TopLevel <- follow r editor "/api/list/" >>= answerOf 200
    map (\s -> s.path) top.spaces `shouldSatisfy` elem "handbooks"
    (.status) <$> follow r editor "/api/markdown/s/handbooks/" `shouldReturn` 200
    -- A subspace with an empty body, titled from its slug, and a page in it.
    sub :: Listed <- putSpace r editor "handbooks/on-call/" "" >>= answerOf 201
    (sub.title, sub.path) `shouldBe` ("On call", "handbooks/on-call")
    (.status) <$> request r methodPut "s/handbooks/on-call/p/rota/" [editor] "The rota." `shouldReturn` 201
    again :: Listed <- follow r editor "/api/list/s/handbooks/" >>= answerOf 200
    map (\s -> s.path) <$> again.subspaces `shouldBe` Just ["handbooks/on-call"]

  it "refuses a path taken, a parent not there, a user who may not, and a body it cannot read, and makes nothing" $ withPasswords backing $ \r -> do
    let paths = map (\s -> s.path) . everySpace . (\t -> t.spaces) <$> (follow r ada "/api/list/" >>= answerOf @TopLevel 200)
    listed <- paths
    for_
      [ (409, editor, "blog/", "{\"title\": \"Blog again\"}")
      , (404, editor, "no-such-space/child/", "")
      , -- Team is hidden from the editor, as a space that is not there.
        (404, editor, "team/child/", "")
      , -- The reader may not create a space at the top, nor under Projects,
        -- which they view and do not administer.
        (403, rita, "readers-room/", "")
      , (403, rita, "team/projects/below/", "")
      , (400, editor, "notes/", "not JSON")
      , (400, editor, "notes/", "[]")
      , (400, editor, "notes/", "{\"title\": 3}")
      , (400, editor, "notes/", "{\"name\": \"Notes\"}")
      , (400, editor, "notes/", "{\"title\": \"Notes\", \"encrypted\": true}")
      , (400, editor, "Not_A_Slug/", "")
      ]
      $ \(code, who, path, body) -> do
        reply <- putSpace r who path body
        (path, body, reply.status) `shouldBe` (path, body, code)
    paths `shouldReturn` listed
    blog :: Listed <- follow r editor "/api/list/s/blog/" >>= answerOf 200
    blog.title `shouldBe` "Blog"

  it "answers a path taken by a space the user may not view as a read of it does, and makes nothing" $ withPasswords backing $ \r -> do
    let paths = map (\s -> s.path) . everySpace . (\t -> t.spaces) <$> (follow r ada "/api/list/" >>= answerOf @TopLevel 200)
    listed <- paths
    -- The editor may create spaces at the top, and may not view Team.
    read' <- follow r editor "/api/list/s/team/"
    put' <- putSpace r editor "team/" "{\"title\": \"Team again\"}"
    (put'.status, put'.body) `shouldBe` (read'.status, read'.body)
    (put'.status, put'.body) `shouldBe` (404, encode NotFound)
    paths `shouldReturn` listed

  it "refuses a plain space inside an encrypted one" $ withPasswords backing $ \r -> do
    slug <- encryptedSpace r
    reply <- putSpace r ada (slug <> "/inside/") ""
    reply.status `shouldBe` 409
    (.status) <$> follow r ada ("/api/list/s/" <> slug <> "/inside/") `shouldReturn` 404

search :: Backing -> Spec
search backing = describe "the search" $ do
  it "finds a page by a word of its text, with an address that answers" $ withPasswords backing $ \r -> do
    found :: Hits <- follow r editor "/api/search?q=split" >>= answerOf 200
    hello <- only "hit of Hello, world" (filter (\h -> h.markdown == "/api/markdown/s/blog/p/hello-world/") found.hits)
    (hello.title, hello.space.path, hello.space.title) `shouldBe` ("Hello, world", "blog", "Blog")
    hello.excerpt `shouldSatisfy` T.isInfixOf "split view"
    let Snippet pieces = hello.excerptMarked
    [T.toLower p.text | p <- pieces, p.match] `shouldBe` ["split"]
    mconcat [p.text | p <- pieces] `shouldBe` hello.excerpt
    page <- follow r editor hello.markdown
    page.status `shouldBe` 200
    decodeUtf8 (BSL.toStrict page.body) `shouldSatisfy` T.isInfixOf "split view"

  it "keeps to a space and its subspaces" $ withPasswords backing $ \r -> do
    let pathsOf who address = map (\h -> h.space.path) . (\h -> h.hits) <$> (follow r who address >>= answerOf @Hits 200)
    everywhere <- pathsOf editor "/api/search?q=markdown"
    everywhere `shouldSatisfy` any (/= "docs")
    within <- pathsOf editor "/api/search?q=markdown&space=docs"
    within `shouldNotSatisfy` null
    within `shouldSatisfy` all (\p -> p == "docs" || "docs/" `T.isPrefixOf` p)
    -- Team, which the rita may not read, still has Projects under it.
    pathsOf rita "/api/search?q=milestones&space=team" `shouldReturn` ["team/projects"]
    pathsOf editor "/api/search?q=markdown&space=no-such-space" `shouldReturn` []
    length <$> pathsOf editor "/api/search?q=markdown&limit=1" `shouldReturn` 1

  it "does not find a page the user may not read" $ withPasswords backing $ \r -> do
    let pathsOf who = map (\h -> h.space.path) . (\h -> h.hits) <$> (follow r who "/api/search?q=handbook" >>= answerOf @Hits 200)
    pathsOf ada >>= (`shouldSatisfy` elem "team")
    pathsOf editor >>= (`shouldSatisfy` notElem "team")

  it "refuses a search without words" $ withPasswords backing $ \r ->
    for_ ["/api/search", "/api/search?q=", "/api/search?q=%20%20"] $ \address -> do
      reply <- follow r editor address
      (address, reply.status, errorName reply) `shouldBe` (address, 400, Just "Invalid")
