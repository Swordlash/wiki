-- | The API for a page's Markdown (WP5.138 of docs/PLAN.md), over real
-- HTTP, as @curl -u login:password@ drives it: signing in with HTTP Basic
-- alone, a page's Markdown read and written whole, and the Markdown
-- reference the writing assistant is given.
--
-- Every request goes through 'sendBare', without the application's marker
-- header, since a program's request has none. The demo's wiki is the
-- server's: Blog and Docs public, Team private; the editor edits Blog
-- through their group, the reader views Team's Projects; nobody has a
-- password until the administrator gives one.
module Test.Server.MarkdownApi (spec) where

import Data.Aeson (Value (..), decodeStrict, encode)
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
import Test.Server.Hygiene (setUp)
import Web.HttpApiData (toHeader)
import Wiki.Api (AdminApi (..), PagesApi (..), SpacesApi (..), WikiApi (..))
import Wiki.Api.Account (Account, AccountDisabled (..))
import Wiki.Api.Encrypted (EncryptSpace (..), NewGrant (..), NewSealedPage (..))
import Wiki.Api.Error (ApiError (..))
import Wiki.Api.Links (links)
import Wiki.Api.Types qualified as Api
import Wiki.Api.Vault (NewPublicKey (..), VaultChange (..))
import Wiki.Api.Version (PageVersion (..))
import Wiki.Core.Assistant (Mode (..), ModelName (..))
import Wiki.Core.Assistant.Prompt (openAiRequest)
import Wiki.Core.Assistant.Reference (markdownReference)
import Wiki.Core.Encrypted (Encapsulated (..), PageKey (..), SealedContent (..), VersionTag (..), firstChain)
import Wiki.Core.Id (PageId (..), SpaceId (..), UserId (..))
import Wiki.Core.Page qualified as P
import Wiki.Core.PageKind qualified as Kind
import Wiki.Core.Path (parsePath)
import Wiki.Core.Slug (slugText)
import Wiki.Core.Space qualified as S
import Wiki.Core.Vault
import Wiki.Core.Version (Author (..), VersionInfo (..))
import Wiki.Server.Vault (keyIdOf)
import Wiki.Server.WebAuthn (toBase64Url)

spec :: Backing -> Spec
spec backing = describe "the API for a page's Markdown" $ do
  signingIn backing
  pages backing
  reference backing

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
    let rita = basic "reader" "reader's password"
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
    let admin = basic "admin" "admin's password"
    for_ [(methodGet, ""), (methodPut, "Plain text.")] $ \(method, body) -> do
      reply <- request r method ("s/" <> slug <> "/") [admin] body
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
