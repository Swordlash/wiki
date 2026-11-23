-- | The API keys ("Wiki.Server.ApiKeys"): made, listed and revoked in a
-- signed-in session, and used by a program on the API for programs over
-- real HTTP, as @curl -H 'Authorization: Bearer …'@ sends them, each
-- request bare ('sendBare'); the sign-in itself against a test server's
-- database, at the times a test gives; and the request's log line, with
-- the listener's application called in process, where the line is
-- written before the call returns.
--
-- The demo's wiki is the server's: Blog and Docs public, Team private; the
-- editor edits Blog through their group, the reader views Team's
-- Projects, the admin is an administrator.
module Test.Server.ApiKeys (spec) where

import Data.Aeson (encode)
import Data.ByteArray.Encoding (Base (..), convertToBase)
import Data.ByteString.Lazy qualified as BSL
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, fromGregorian, getCurrentTime)
import Database.Beam (all_, runSelectReturningList, runUpdate, select, update, val_, (<-.), (==.))
import Network.HTTP.Types (Header, hContentType, methodGet, methodPost, methodPut, statusCode)
import Network.Wai (Request (..), defaultRequest, responseStatus)
import Network.Wai.Internal (ResponseReceived (..))
import Protolude
import Test.Database (Backing, placeOptionsOf)
import Test.Hspec
import Test.Server.Fixture
import Test.Server.Http (withApiEnv)
import Test.Server.Hygiene (halfSignedIn, pendingCookie, setUp)
import Test.Storage.Fixture (ok, withDemo, withPlaceIn)
import Wiki.Api (AccountApi (..), AdminApi (..), WikiApi (..))
import Wiki.Api.Account (Account, AccountDisabled (..))
import Wiki.Api.Admin (AdministratorFlag (..))
import Wiki.Api.ApiKeys (ApiKeyId (..), ApiKeyInfo (..), KeyRight (..), MadeApiKey (..), NewApiKey (..))
import Wiki.Api.Error (ApiError (..))
import Wiki.Api.Links (links)
import Wiki.Core.Id (UserId (..))
import Wiki.Core.User (Password (..))
import Wiki.Server.Accounts (createAccount, setPassword)
import Wiki.Server.Api (Env (..))
import Wiki.Server.ApiKeys
import Wiki.Server.App (Finding (..), Listener (..), listenerApp)
import Wiki.Server.Console (Keys (..), KeysCommand (..), runKeys)
import Wiki.Server.Limiter (defaultLimits, newLimiter)
import Wiki.Server.Log (silentLogger)
import Wiki.Server.Session (CookieSettings (..))
import Wiki.Server.Storage (Storage, defaultConfig, reading, withStorage, withStorageAside, writing)
import Wiki.Server.Storage.Rows (micro)
import Wiki.Server.Storage.Schema (ApiKeys (..), WikiDb (..), wikiDb)

spec :: Backing -> Spec
spec backing = describe "API keys" $ do
  making backing
  using backing
  refusing backing
  limiting backing
  atTimes backing
  logging backing
  console backing

-- | The demo with passwords for the editor, the reader and the admin.
withPasswords :: Backing -> (Running -> IO a) -> IO a
withPasswords backing test = withDemoRunning backing identity $ \r -> do
  givePassword r "user-editor" "editor's password"
  givePassword r "user-reader" "reader's password"
  givePassword r "user-admin" "admin's password"
  test r

-- | The address of the API for programs, for a path after @api\/@.
api :: Running -> Text -> Text
api r path = r.public <> "/api/" <> path

-- | A page every signed-in user of the demo may read, and the editor write.
hello :: Text
hello = "markdown/s/blog/p/hello-world/"

-- | The headers a program sends a key in.
bearer, xApiKey :: Text -> Header
bearer k = ("Authorization", "Bearer " <> encodeUtf8 k)
xApiKey k = ("X-API-Key", encodeUtf8 k)

-- | A request to the API for programs, as a program sends it: with these
-- headers only.
getAt :: Running -> Text -> [Header] -> IO Reply
getAt r path hs = sendBare r methodGet (api r path) hs ""

putAt :: Running -> Text -> [Header] -> LByteString -> IO Reply
putAt r path = sendBare r methodPut (api r path)

-- | Ask for a key in a session, with no proof of the password before it:
-- the answer.
postKey :: Running -> Text -> NewApiKey -> IO Reply
postKey r token new = send r methodPost (r.public <> "/" <> links.account.makeApiKey) [(hContentType, "application/json"), cookie token] (encode new)

-- | Make a key in a session, as the user's settings make one: the
-- session's password proved first (the demo's accounts have OPAQUE
-- records), then the key asked for. The answer.
makeKeyReply :: (HasCallStack) => Running -> Text -> Text -> NewApiKey -> IO Reply
makeKeyReply r token password new = do
  proof <- provePassword r (\l -> r.public <> "/" <> l) [cookie token] password
  proof.status `shouldBe` 204
  postKey r token new

-- | A key made in a session, with this right and expiry.
makeKey :: (HasCallStack) => Running -> Text -> Text -> KeyRight -> Maybe UTCTime -> IO MadeApiKey
makeKey r token password right expiresAt = makeKeyReply r token password NewApiKey {name = "A program", right, expiresAt, password = Nothing} >>= jsonAs 201

-- | The user's keys, as their list shows them.
keysOf :: (HasCallStack) => Running -> Text -> IO [ApiKeyInfo]
keysOf r token = send r methodGet (r.public <> "/" <> links.account.apiKeys) [cookie token] "" >>= jsonAs 200

revoke :: Running -> Text -> ApiKeyId -> IO Reply
revoke r token i = send r methodPost (r.public <> "/" <> links.account.revokeApiKey i) [cookie token] ""

-- | The secret of a key: what follows its id.
secretOf :: MadeApiKey -> Text
secretOf m = T.takeEnd 52 m.key

-- | The body of a reply as text.
bodyText :: Reply -> Text
bodyText reply = decodeUtf8 (BSL.toStrict reply.body)

-- | The status, the body and the challenge of a refusal: what a caller
-- could tell two refusals apart by.
refusalOf :: Reply -> (Int, LByteString, Maybe ByteString)
refusalOf reply = (reply.status, reply.body, header "WWW-Authenticate" reply)

-- | Change a running server's stored keys directly, as time passing would.
onKeys :: Running -> (Storage -> IO a) -> IO a
onKeys r = withStorageAside r.place

making :: Backing -> Spec
making backing = describe "made in a session" $ do
  it "is shown whole once, listed without its secret, and stored as its secret's hash" $ withPasswords backing $ \r -> do
    token <- signInAs r "editor" "editor's password"
    reply <- makeKeyReply r token "editor's password" NewApiKey {name = "Laptop assistant", right = WriteRight, expiresAt = Nothing, password = Nothing}
    made :: MadeApiKey <- jsonAs 201 reply
    made.key `shouldSatisfy` T.isPrefixOf "wiki_"
    T.length made.key `shouldBe` 74
    parseKey made.key `shouldBe` Just (made.info.id, secretOf made)
    (made.info.name, made.info.right, made.info.expiresAt, made.info.lastUsedAt, made.info.revokedAt)
      `shouldBe` ("Laptop assistant", WriteRight, Nothing, Nothing, Nothing)
    listed <- send r methodGet (r.public <> "/" <> links.account.apiKeys) [cookie token] ""
    bodyText listed `shouldNotSatisfy` T.isInfixOf (secretOf made)
    jsonAs 200 listed `shouldReturn` [made.info]
    rows <- onKeys r $ \st -> reading st (runSelectReturningList (select (all_ wikiDb.api_keys)))
    map (\k -> (k.id, k.user_id, k.secret_hash)) rows `shouldBe` [(made.info.id.text, "user-editor", secretHash (secretOf made))]
    (show rows :: Text) `shouldNotSatisfy` T.isInfixOf (secretOf made)
    -- Its Show hides it, so that a test or a log that shows the answer
    -- does not.
    (show made :: Text) `shouldNotSatisfy` T.isInfixOf (secretOf made)

  it "is made only with the user's password proved, as a passkey is" $ withPasswords backing $ \r -> do
    token <- signInAs r "editor" "editor's password"
    let new password = NewApiKey {name = "Laptop assistant", right = WriteRight, expiresAt = Nothing, password}
    -- Without a proof: the session alone does not make a key.
    (postKey r token (new Nothing) >>= jsonAs 401) `shouldReturn` Unauthenticated
    -- A wrong password, in the request's field or proved before it.
    (postKey r token (new (Just (Password "not the password"))) >>= jsonAs 401) `shouldReturn` Unauthenticated
    wrong <- provePassword r (\l -> r.public <> "/" <> l) [cookie token] "not the password"
    wrong.status `shouldBe` 401
    (postKey r token (new Nothing) >>= jsonAs 401) `shouldReturn` Unauthenticated
    keysOf r token `shouldReturn` []
    -- The right one makes the key, and is used up by it.
    made :: MadeApiKey <- makeKeyReply r token "editor's password" (new Nothing) >>= jsonAs 201
    (postKey r token (new Nothing) >>= jsonAs 401) `shouldReturn` Unauthenticated
    keysOf r token `shouldReturn` [made.info]

  it "names a key by the day it was made when it is given no name, and refuses an expiry in the past" $ withPasswords backing $ \r -> do
    token <- signInAs r "editor" "editor's password"
    made :: MadeApiKey <- makeKeyReply r token "editor's password" NewApiKey {name = "  ", right = ReadRight, expiresAt = Nothing, password = Nothing} >>= jsonAs 201
    made.info.name `shouldSatisfy` T.isPrefixOf "Made on "
    past <- addUTCTime (-60) <$> getCurrentTime
    (.status) <$> makeKeyReply r token "editor's password" NewApiKey {name = "Late", right = ReadRight, expiresAt = Just past, password = Nothing} `shouldReturn` 400
    (.status) <$> makeKeyReply r token "editor's password" NewApiKey {name = T.replicate 101 "n", right = ReadRight, expiresAt = Nothing, password = Nothing} `shouldReturn` 400

  it "gives the right admin to an administrator's key only" $ withPasswords backing $ \r -> do
    editor <- signInAs r "editor" "editor's password"
    refused <- makeKeyReply r editor "editor's password" NewApiKey {name = "Mine", right = AdminRight, expiresAt = Nothing, password = Nothing}
    refused.status `shouldBe` 403
    keysOf r editor `shouldReturn` []
    admin <- signInAs r "admin" "admin's password"
    made <- makeKey r admin "admin's password" AdminRight Nothing
    made.info.right `shouldBe` AdminRight

  it "keeps at most twenty keys that work, and makes room as one is revoked" $ withPasswords backing $ \r -> do
    token <- signInAs r "reader" "reader's password"
    made <- replicateM 20 (makeKey r token "reader's password" ReadRight Nothing)
    over <- makeKeyReply r token "reader's password" NewApiKey {name = "One more", right = ReadRight, expiresAt = Nothing, password = Nothing}
    over.status `shouldBe` 400
    bodyText over `shouldSatisfy` T.isInfixOf "20"
    case made of
      first' : _ -> (.status) <$> revoke r token first'.info.id `shouldReturn` 200
      [] -> expectationFailure "no keys made"
    (.status) <$> makeKeyReply r token "reader's password" NewApiKey {name = "One more", right = ReadRight, expiresAt = Nothing, password = Nothing} `shouldReturn` 201
    length <$> keysOf r token `shouldReturn` 21

  it "is not made by a sign-in still waiting for its second factor" $ withPasswords backing $ \r -> do
    token <- signInAs r "editor" "editor's password"
    _ <- setUp r token
    held <- halfSignedIn r
    refused <- send r methodPost (r.public <> "/" <> links.account.makeApiKey) [(hContentType, "application/json"), pendingCookie held] (encode NewApiKey {name = "Sneaky", right = WriteRight, expiresAt = Nothing, password = Nothing})
    refused.status `shouldBe` 401
    keysOf r token `shouldReturn` []

  it "is revoked, stays listed as revoked, and never signs in again" $ withPasswords backing $ \r -> do
    token <- signInAs r "editor" "editor's password"
    made <- makeKey r token "editor's password" WriteRight Nothing
    (.status) <$> getAt r hello [bearer made.key] `shouldReturn` 200
    revoked :: [ApiKeyInfo] <- revoke r token made.info.id >>= jsonAs 200
    map (\k -> (k.id, isJust k.revokedAt)) revoked `shouldBe` [(made.info.id, True)]
    (.status) <$> getAt r hello [bearer made.key] `shouldReturn` 401
    -- Revoking it again changes nothing, its time included.
    (revoke r token made.info.id >>= jsonAs 200) `shouldReturn` revoked
    -- Another user's key is not theirs to revoke, nor to know of.
    other <- signInAs r "reader" "reader's password"
    (.status) <$> revoke r other made.info.id `shouldReturn` 404
    (.status) <$> revoke r other (ApiKeyId "nosuchkeyatallxx") `shouldReturn` 404

using :: Backing -> Spec
using backing = describe "used by a program" $ do
  it "reads and writes as its user, through the Markdown, the listing and the search, by either header" $ withPasswords backing $ \r -> do
    token <- signInAs r "editor" "editor's password"
    made <- makeKey r token "editor's password" WriteRight Nothing
    (.status) <$> getAt r hello [bearer made.key] `shouldReturn` 200
    (.status) <$> putAt r hello [xApiKey made.key] "# Hello\n\nWritten with a key." `shouldReturn` 204
    (.body) <$> getAt r hello [bearer made.key] `shouldReturn` "# Hello\n\nWritten with a key."
    (.status) <$> getAt r "list/" [bearer made.key] `shouldReturn` 200
    (.status) <$> getAt r "search?q=hello" [xApiKey made.key] `shouldReturn` 200
    -- The scheme's name is read whatever its case, as HTTP's are.
    (.status) <$> getAt r hello [("Authorization", "bearer " <> encodeUtf8 made.key)] `shouldReturn` 200
    -- Its use is noted in the list.
    map (\k -> isJust k.lastUsedAt) <$> keysOf r token `shouldReturn` [True]

  it "never gives more than its user may: a space they may not read is not there" $ withPasswords backing $ \r -> do
    editor <- signInAs r "editor" "editor's password"
    made <- makeKey r editor "editor's password" WriteRight Nothing
    -- Team is private: as for the editor in the browser, it is not there.
    (.status) <$> getAt r "markdown/s/team/p/team-handbook/" [bearer made.key] `shouldReturn` 404
    (.status) <$> putAt r "markdown/s/team/p/team-handbook/" [bearer made.key] "Mine now." `shouldReturn` 404
    -- Docs the editor may read and not write.
    (.status) <$> putAt r "markdown/s/docs/p/tour/" [bearer made.key] "Mine now." `shouldReturn` 403
    -- The admin's session beside the key gives the key nothing more.
    admin <- signInAs r "admin" "admin's password"
    (.status) <$> getAt r "markdown/s/team/p/team-handbook/" [bearer made.key, cookie admin] `shouldReturn` 404

  it "with the right read, reads, and is refused every write, told which right it lacks" $ withPasswords backing $ \r -> do
    token <- signInAs r "editor" "editor's password"
    made <- makeKey r token "editor's password" ReadRight Nothing
    unchanged <- getAt r hello [bearer made.key]
    unchanged.status `shouldBe` 200
    (.status) <$> getAt r "list/" [bearer made.key] `shouldReturn` 200
    (.status) <$> getAt r "search?q=hello" [bearer made.key] `shouldReturn` 200
    refused <- putAt r hello [bearer made.key] "Not with this key."
    refused.status `shouldBe` 403
    bodyText refused `shouldSatisfy` T.isInfixOf "the right write"
    (.status) <$> putAt r "list/s/blog/later/" [bearer made.key] "" `shouldReturn` 403
    (.body) <$> getAt r hello [bearer made.key] `shouldReturn` unchanged.body

  it "with the right read, reads a page's contents and a section, and a write to a section is refused for the right first" $ withPasswords backing $ \r -> do
    token <- signInAs r "editor" "editor's password"
    writer <- makeKey r token "editor's password" WriteRight Nothing
    (.status) <$> putAt r hello [bearer writer.key] "# Hello\n\nText.\n" `shouldReturn` 204
    readOnly <- makeKey r token "editor's password" ReadRight Nothing
    contents <- getAt r "contents/s/blog/p/hello-world/" [bearer readOnly.key]
    contents.status `shouldBe` 200
    bodyText contents `shouldSatisfy` T.isInfixOf "?section=hello"
    (bodyText <$> getAt r (hello <> "?section=hello") [bearer readOnly.key]) `shouldReturn` "# Hello\n\nText.\n"
    -- A write to a section: refused for the key's right (403) before the
    -- section's own refusal of every write (405), which the write key gets.
    refused <- putAt r (hello <> "?section=hello") [bearer readOnly.key] "# Hello\n\nOther.\n"
    refused.status `shouldBe` 403
    bodyText refused `shouldSatisfy` T.isInfixOf "the right write"
    (.status) <$> putAt r (hello <> "?section=hello") [bearer writer.key] "# Hello\n\nOther.\n" `shouldReturn` 405

  it "with the right write, writes what its user may, and makes a space they may" $ withPasswords backing $ \r -> do
    token <- signInAs r "admin" "admin's password"
    made <- makeKey r token "admin's password" WriteRight Nothing
    (.status) <$> putAt r "list/s/written-by-key/" [bearer made.key] "" `shouldReturn` 201
    (.status) <$> putAt r "markdown/s/team/p/team-handbook/" [bearer made.key] "# Handbook\n\nBy key." `shouldReturn` 204

  it "with the right admin, writes while its user is an administrator, and stops when they are not" $ withPasswords backing $ \r -> do
    token <- signInAs r "admin" "admin's password"
    made <- makeKey r token "admin's password" AdminRight Nothing
    (.status) <$> putAt r hello [bearer made.key] "# Hello\n\nBy the admin's key." `shouldReturn` 204
    let administrator yes = sendJson r methodPut (r.admin <> "/admin/" <> links.admin.setAdministrator (UserId "user-admin")) AdministratorFlag {administrator = yes}
    (.status) <$> administrator False `shouldReturn` 204
    lapsed <- getAt r hello [bearer made.key]
    lapsed.status `shouldBe` 403
    bodyText lapsed `shouldSatisfy` T.isInfixOf "not an administrator"
    (.status) <$> administrator True `shouldReturn` 204
    (.status) <$> getAt r hello [bearer made.key] `shouldReturn` 200

  it "signs in an account with a second factor, which its password alone does not" $ withPasswords backing $ \r -> do
    token <- signInAs r "editor" "editor's password"
    made <- makeKey r token "editor's password" WriteRight Nothing
    _ <- setUp r token
    (.status) <$> getAt r hello [("Authorization", basicOf "editor" "editor's password")] `shouldReturn` 403
    (.status) <$> getAt r hello [bearer made.key] `shouldReturn` 200

  it "signs in an account with no password, as one with passkeys only has" $ withDemoRunning backing identity $ \r -> do
    -- The reader has no password: the key is made as the console makes
    -- one, since a session of theirs would need a passkey.
    now <- getCurrentTime
    made <- onKeys r $ \st -> makeApiKey st now (UserId "user-reader") False NewApiKey {name = "Passkeys only", right = ReadRight, expiresAt = Nothing, password = Nothing}
    case made of
      Left e -> expectationFailure ("no key: " <> show e)
      Right m -> (.status) <$> getAt r "markdown/s/team/projects/p/the-wiki/" [bearer m.key] `shouldReturn` 200

  it "is refused on the application's API, which answers the same request without it" $ withPasswords backing $ \r -> do
    token <- signInAs r "editor" "editor's password"
    made <- makeKey r token "editor's password" WriteRight Nothing
    let info = r.public <> "/" <> links.info
    (.status) <$> sendBare r methodGet info [] "" `shouldReturn` 200
    for_ [[bearer made.key], [xApiKey made.key], [bearer made.key, cookie token]] $ \hs -> do
      refused <- sendBare r methodGet info hs ""
      refused.status `shouldBe` 401
      bodyText refused `shouldSatisfy` T.isInfixOf "api/v1"
    -- A bearer token that is not a key of the wiki's form, as a proxy in
    -- front of the wiki may send, is left alone there.
    (.status) <$> sendBare r methodGet info [("Authorization", "Bearer something-of-a-proxy")] "" `shouldReturn` 200

  it "is the only way in besides the password: the session's cookie alone is refused" $ withPasswords backing $ \r -> do
    token <- signInAs r "editor" "editor's password"
    (.status) <$> getAt r hello [cookie token] `shouldReturn` 401

refusing :: Backing -> Spec
refusing backing = describe "refused" $ do
  it "alike for an unknown id, a wrong secret, an expired or revoked key and malformed keys of every shape" $ withPasswords backing $ \r -> do
    token <- signInAs r "editor" "editor's password"
    made <- makeKey r token "editor's password" WriteRight Nothing
    expiring <- addUTCTime 86400 <$> getCurrentTime
    expired <- makeKey r token "editor's password" WriteRight (Just expiring)
    past <- addUTCTime (-1) <$> getCurrentTime
    onKeys r $ \st -> writing st (runUpdate (update wikiDb.api_keys (\k -> k.expires_at <-. val_ (Just past)) (\k -> k.id ==. val_ expired.info.id.text)))
    revoked <- makeKey r token "editor's password" WriteRight Nothing
    _ <- revoke r token revoked.info.id
    let i = made.info.id.text
        shapes =
          [ "wiki_" <> T.replicate 16 "a" <> "_" <> secretOf made -- no key has the id
          , "wiki_" <> i <> "_" <> T.replicate 52 "a" -- the secret is wrong
          , expired.key
          , revoked.key
          , ""
          , "wiki_"
          , made.key <> "a"
          , T.dropEnd 1 made.key
          , T.toUpper made.key
          , T.replace "_" "-" made.key
          , "wiki_" <> i
          , "wiki_" <> i <> "_" <> T.replicate 51 "a" <> "1"
          , "not a key at all"
          , T.replicate 5000 "x"
          ]
    replies <- for shapes $ \k -> getAt r hello [bearer k]
    case map refusalOf replies of
      first'@(status, _, challenge) : rest -> do
        (status, challenge) `shouldBe` (401, Just "Bearer realm=\"wiki\", error=\"invalid_token\"")
        rest `shouldBe` map (const first') rest
      [] -> expectationFailure "no replies"
    -- The key itself still signs in.
    (.status) <$> getAt r hello [bearer made.key] `shouldReturn` 200

  it "for a key and a password together, two different keys, or a header twice" $ withPasswords backing $ \r -> do
    token <- signInAs r "editor" "editor's password"
    made <- makeKey r token "editor's password" WriteRight Nothing
    other <- makeKey r token "editor's password" WriteRight Nothing
    let password = ("Authorization", basicOf "editor" "editor's password")
    for_
      [ [password, xApiKey made.key]
      , [bearer made.key, xApiKey other.key]
      , [xApiKey made.key, xApiKey made.key]
      , [bearer made.key, bearer made.key]
      ]
      $ \hs -> (.status) <$> getAt r hello hs `shouldReturn` 400
    -- The same key in both headers is one key.
    (.status) <$> getAt r hello [bearer made.key, xApiKey made.key] `shouldReturn` 200

  it "for a disabled user's keys, which work again once the user is enabled" $ withPasswords backing $ \r -> do
    token <- signInAs r "editor" "editor's password"
    made <- makeKey r token "editor's password" WriteRight Nothing
    let disabled yes = sendJson r methodPut (r.admin <> "/admin/" <> links.admin.setDisabled (UserId "user-editor")) AccountDisabled {disabled = yes} >>= jsonAs @Account 200
    _ <- disabled True
    unknown <- getAt r hello [bearer ("wiki_" <> T.replicate 16 "b" <> "_" <> secretOf made)]
    refused <- getAt r hello [bearer made.key]
    refusalOf refused `shouldBe` refusalOf unknown
    _ <- disabled False
    (.status) <$> getAt r hello [bearer made.key] `shouldReturn` 200

  it "for a removed user, whose keys go with the account" $ withPasswords backing $ \r -> do
    token <- signInAs r "editor" "editor's password"
    made <- makeKey r token "editor's password" WriteRight Nothing
    _ <- sendJson r methodPut (r.admin <> "/admin/" <> links.admin.setDisabled (UserId "user-editor")) AccountDisabled {disabled = True} >>= jsonAs @Account 200
    (.status) <$> send r "DELETE" (r.admin <> "/admin/" <> links.admin.removeUser (UserId "user-editor")) [] "" `shouldReturn` 204
    (.status) <$> getAt r hello [bearer made.key] `shouldReturn` 401
    onKeys r (\st -> reading st (runSelectReturningList (select (all_ wikiDb.api_keys)))) `shouldReturn` []

limiting :: Backing -> Spec
limiting backing = describe "the limit on failed keys" $ do
  it "answers 429 once an address has failed thirty times, and still lets a right key in, and the password" $ withPasswords backing $ \r -> do
    token <- signInAs r "editor" "editor's password"
    made <- makeKey r token "editor's password" WriteRight Nothing
    -- Guesses at the key's secret, by someone who knows its public id.
    let guess n = "wiki_" <> made.info.id.text <> "_" <> T.justifyRight 52 'a' (T.pack (show (n :: Int)))
    for_ [1 .. 30] $ \n -> (.status) <$> getAt r hello [bearer (guess n)] `shouldReturn` 401
    limited <- getAt r hello [bearer (guess 31)]
    limited.status `shouldBe` 429
    header "Retry-After" limited `shouldSatisfy` isJust
    -- The owner's key is not locked out by the guesses at its id.
    (.status) <$> getAt r hello [bearer made.key] `shouldReturn` 200
    -- Nor is the address's sign-in with a password, counted apart.
    (.status) <$> getAt r hello [("Authorization", basicOf "reader" "reader's password")] `shouldReturn` 200
    (.status) <$> passwordSignIn r (\l -> r.public <> "/" <> l) [] "reader" "reader's password" `shouldReturn` 200

atTimes :: Backing -> Spec
atTimes backing = describe "signing in at a time" $ do
  it "works until its expiry and not from that moment" $ withDemo backing $ \st -> do
    limiter <- newLimiter defaultLimits
    now <- getCurrentTime
    let expiry = addUTCTime 3600 now
    made <- either (panic . show) identity <$> makeApiKey st now (UserId "user-editor") False NewApiKey {name = "An hour", right = ReadRight, expiresAt = Just expiry, password = Nothing}
    signInByKey st limiter (addUTCTime (-0.000001) expiry) "127.0.0.1" made.key
      `shouldReturn` KeySignedIn KeyHolder {user = UserId "user-editor", key = made.info.id, right = ReadRight}
    signInByKey st limiter expiry "127.0.0.1" made.key `shouldReturn` KeyRefused (Just made.info.id) Expired

  it "notes when it was used at most every ten minutes" $ withDemo backing $ \st -> do
    limiter <- newLimiter defaultLimits
    now <- getCurrentTime
    made <- either (panic . show) identity <$> makeApiKey st now (UserId "user-editor") False NewApiKey {name = "Busy", right = ReadRight, expiresAt = Nothing, password = Nothing}
    let usedAt at = do
          _ <- signInByKey st limiter at "127.0.0.1" made.key
          map (\k -> k.lastUsedAt) <$> apiKeysOf st (UserId "user-editor")
    usedAt (addUTCTime 1 now) `shouldReturn` [Just (micro (addUTCTime 1 now))]
    -- Nine minutes on, the time stays; eleven minutes on, it is written.
    usedAt (addUTCTime (9 * 60) now) `shouldReturn` [Just (micro (addUTCTime 1 now))]
    usedAt (addUTCTime (11 * 60) now) `shouldReturn` [Just (micro (addUTCTime (11 * 60) now))]

  it "writes the time once when requests that all found it due reach the writer one after another" $ withDemo backing $ \st -> do
    now <- getCurrentTime
    made <- either (panic . show) identity <$> makeApiKey st now (UserId "user-editor") False NewApiKey {name = "Burst", right = ReadRight, expiresAt = Nothing, password = Nothing}
    let lastUse = map (\k -> k.lastUsedAt) <$> apiKeysOf st (UserId "user-editor")
    -- Two requests read the key before either wrote its time, so both go
    -- on to the writer's step ('recordUse'). The first writes; the second,
    -- a second later and inside the interval, signs in and writes nothing.
    writing st (recordUse (addUTCTime 1 now) made.key) `shouldReturn` Right KeyHolder {user = UserId "user-editor", key = made.info.id, right = ReadRight}
    lastUse `shouldReturn` [Just (micro (addUTCTime 1 now))]
    writing st (recordUse (addUTCTime 2 now) made.key) `shouldReturn` Right KeyHolder {user = UserId "user-editor", key = made.info.id, right = ReadRight}
    lastUse `shouldReturn` [Just (micro (addUTCTime 1 now))]

  it "says why it refused, for the log: the id only of a key that was well formed" $ withDemo backing $ \st -> do
    limiter <- newLimiter defaultLimits
    now <- getCurrentTime
    made <- either (panic . show) identity <$> makeApiKey st now (UserId "user-editor") False NewApiKey {name = "Logged", right = ReadRight, expiresAt = Nothing, password = Nothing}
    signInByKey st limiter now "127.0.0.1" ("wiki_" <> made.info.id.text <> "_" <> T.replicate 52 "a") `shouldReturn` KeyRefused (Just made.info.id) WrongSecret
    signInByKey st limiter now "127.0.0.1" ("wiki_" <> T.replicate 16 "c" <> "_" <> secretOf made) `shouldReturn` KeyRefused (Just (ApiKeyId (T.replicate 16 "c"))) UnknownKey
    signInByKey st limiter now "127.0.0.1" (secretOf made) `shouldReturn` KeyRefused Nothing Malformed
    _ <- revokeApiKey st now (UserId "user-editor") made.info.id
    signInByKey st limiter now "127.0.0.1" made.key `shouldReturn` KeyRefused (Just made.info.id) Revoked

logging :: Backing -> Spec
logging backing = describe "the request's log line" $ do
  it "names the user and the key's id, or the password's user, and never holds the key" $ withDemo backing $ \st -> withApiEnv st $ \env0 -> do
    lines' <- newIORef []
    let logger = silentLogger (\line -> modifyIORef' lines' (line :))
        env = env0 {logger}
        listener = Listener {name = "public", mount = "/", finding = BySession CookieSettings {path = "/", secure = False}}
        app = listenerApp logger env Nothing listener
        -- The listener's application, called in process: the log line is
        -- written before the call returns.
        call path hs = do
          status <- newIORef 0
          let segments = filter (not . T.null) (T.splitOn "/" path)
          _ <- app defaultRequest {requestMethod = methodGet, rawPathInfo = encodeUtf8 path, pathInfo = segments, requestHeaders = hs} $ \response ->
            writeIORef status (statusCode (responseStatus response)) $> ResponseReceived
          readIORef status
    now <- getCurrentTime
    made <- either (panic . show) identity <$> makeApiKey st now (UserId "user-editor") False NewApiKey {name = "Logged", right = ReadRight, expiresAt = Nothing, password = Nothing}
    void (either (panic . show) identity <$> setPassword st (UserId "user-reader") (Password "reader's password"))
    call "/api/markdown/s/blog/p/hello-world/" [bearer made.key] `shouldReturn` 200
    call "/api/markdown/s/blog/p/hello-world/" [xApiKey ("wiki_" <> made.info.id.text <> "_" <> T.replicate 52 "a")] `shouldReturn` 401
    call "/api/markdown/s/blog/p/hello-world/" [bearer (secretOf made)] `shouldReturn` 401
    call "/api/markdown/s/blog/p/hello-world/" [("Authorization", basicOf "reader" "reader's password")] `shouldReturn` 200
    logged <- readIORef lines'
    filter (T.isInfixOf (secretOf made)) logged `shouldBe` []
    filter (T.isInfixOf ("principal=user:user-editor key=" <> made.info.id.text <> " ")) logged `shouldSatisfy` ((== 1) . length)
    filter (T.isInfixOf ("event=api-key-refused why=wrong-secret key=" <> made.info.id.text)) logged `shouldSatisfy` ((== 1) . length)
    filter (T.isInfixOf "event=api-key-refused why=malformed from=") logged `shouldSatisfy` ((== 1) . length)
    filter (T.isInfixOf "principal=user:user-reader from=") logged `shouldSatisfy` ((== 1) . length)

console :: Backing -> Spec
console backing = describe "the console's wiki-server keys" $ do
  it "makes a key printed once, lists it without its secret, and revokes it" $
    withPlaceIn backing $ \at -> do
      withStorage defaultConfig at $ \st -> void (ok =<< createAccount st "bea" "Bea" Nothing)
      let run command = runKeys Keys {place = placeOptionsOf at, login = "bea", command}
      key <-
        run (AddKey "Script" WriteRight (Just (fromGregorian 2099 12 31))) >>= \case
          Right [said, key] -> (said `shouldSatisfy` T.isInfixOf "shown this once") $> key
          other -> expectationFailure ("expected the key, got " <> show other) $> ""
      (i, secret) <- maybe (expectationFailure ("not a key: " <> toS key) $> (ApiKeyId "", "")) pure (parseKey key)
      run ListKeys >>= \case
        Right listed -> do
          T.unlines listed `shouldNotSatisfy` T.isInfixOf secret
          -- The key works until the end of the day given, in UTC.
          T.unlines listed `shouldSatisfy` T.isInfixOf (i.text <> "  write  works  2100-01-01 00:00  never      Script")
        Left why -> expectationFailure (toS why)
      run (AddKey "" AdminRight Nothing) `shouldReturn` Left "Only an administrator may have a key with the right admin."
      run (RevokeKey i) `shouldReturn` Right ["Revoked the key " <> i.text <> " of bea (Bea)."]
      run (RevokeKey (ApiKeyId "nosuchkeyatallxx")) `shouldReturn` Left "There is no such key among the user's."
      run ListKeys >>= \case
        Right listed -> T.unlines listed `shouldSatisfy` T.isInfixOf (i.text <> "  write  revoked")
        Left why -> expectationFailure (toS why)

-- | The @Authorization@ header of a login and a password, as @curl -u@
-- sends it.
basicOf :: Text -> Text -> ByteString
basicOf login password = "Basic " <> convertToBase Base64 (encodeUtf8 (login <> ":" <> password))
