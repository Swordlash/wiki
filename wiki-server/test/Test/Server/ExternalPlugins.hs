-- | External plugins ("Wiki.Api.Plugins.External", "Wiki.Server.PluginFrame"),
-- over real HTTP: the policy, which only an administrator sets; a plugin
-- installed for everyone and replaced by its hash; the rules an upload is
-- refused by; a user's own plugins, while the policy lets them; what
-- @GET api\/v1\/external-plugins@ answers each asker; the frame, its
-- headers, and everyone it is refused to; a bundle changed on the disk;
-- and removal, by the administrator, by the user, and with a removed user.
--
-- The demo's wiki is the server's, open to anyone: the editor, the reader
-- and the admin, who is an administrator, sign in with the passwords the
-- operator gives them.
module Test.Server.ExternalPlugins (spec) where

import Data.Aeson (encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Database.Beam (all_, runSelectReturningList, select)
import Network.HTTP.Types (Header, hContentType, methodDelete, methodGet, methodPost, methodPut)
import Protolude
import System.Directory (removeFile, renameFile)
import Test.Database (Backing)
import Test.HUnit (assertFailure)
import Test.Hspec
import Test.Server.Fixture
import Wiki.Api (AdminApi (..), ExternalPluginsApi (..), WikiApi (..))
import Wiki.Api.Account (Account, AccountDisabled (..))
import Wiki.Api.Activity (Activity (..))
import Wiki.Api.ContentPolicy (pluginFramePolicy)
import Wiki.Api.Error (ApiError (..))
import Wiki.Api.Links (links)
import Wiki.Api.Plugins (PluginId (..), WikiPlugins (..))
import Wiki.Api.Plugins.External
import Wiki.Core.Audit (AuditEntry (..))
import Wiki.Core.Id (BlobId (..), UserId (..))
import Wiki.Server.PluginFrame (codeScript, frameScripts, scriptHash)
import Wiki.Server.Storage (reading, withStorageAside)
import Wiki.Server.Storage.Blobs (blobFile, blobOf)
import Wiki.Server.Storage.Layout (attachmentsDir)
import Wiki.Server.Storage.Schema (InstalledPlugins (..), WikiDb (..), wikiDb)

spec :: Backing -> Spec
spec backing = describe "external plugins" $ do
  policies backing
  installing backing
  ownPlugins backing
  frames backing
  removing backing

-- | The demo with passwords for the editor, the reader and the admin.
withUsers :: Backing -> (Running -> IO a) -> IO a
withUsers backing test = withDemoRunning backing identity $ \r -> do
  givePassword r "user-editor" "editor's password"
  givePassword r "user-reader" "reader's password"
  givePassword r "user-admin" "admin's password"
  test r

-- | The headers of a signed-in session of a user, by login.
sessionOf :: (HasCallStack) => Running -> Text -> IO [Header]
sessionOf r login = pure . cookie <$> signInAs r login (login <> "'s password")

jsonType :: Header
jsonType = (hContentType, "application/json")

onPublic, onAdmin :: Running -> Text -> Text
onPublic r address = r.public <> "/" <> address
onAdmin r address = r.admin <> "/admin/" <> address

-- | What the list answers a session (or nobody).
listAs :: (HasCallStack) => Running -> [Header] -> IO ExternalPlugins
listAs r hs = send r methodGet (onPublic r links.externalPlugins.list) hs "" >>= jsonAs 200

-- | Set the policy, as the operator on the admin listener.
setPolicy :: (HasCallStack) => Running -> ExternalPolicy -> IO ()
setPolicy r p = void (sendJson r methodPut (onAdmin r links.externalPlugins.setPolicy) ExternalPolicyChange {policy = p} >>= jsonAs @ExternalPlugins 200)

-- | A manifest's JSON, with an id and hosts.
manifestOf :: Text -> [Text] -> Text
manifestOf pid hosts =
  decodeUtf8 . BSL.toStrict . encode $
    object
      [ "manifest" .= (1 :: Int)
      , "id" .= pid
      , "name" .= ("Plugin " <> pid)
      , "version" .= ("1.0.0" :: Text)
      , "api" .= (1 :: Int)
      , "contributes" .= object ["diagnostics" .= object []]
      , "permissions" .= ["document.read" :: Text]
      , "hosts" .= hosts
      ]

-- | Upload a plugin to an address with a session: its manifest, its
-- bundle's bytes, and the hash it replaces, if any.
uploadTo :: Running -> Text -> [Header] -> Text -> ByteString -> Maybe Text -> IO Reply
uploadTo r address hs manifest bundle replaces =
  multipartWith r methodPost hs address ([("manifest", manifest)] <> [("replaces", h) | Just h <- [replaces]]) [("bundle", "plugin.js", "text/javascript", BSL.fromStrict bundle)]

-- | Install one for everyone, as the operator.
installForWiki :: Running -> Text -> ByteString -> Maybe Text -> IO Reply
installForWiki r manifest = uploadTo r (onAdmin r links.externalPlugins.installForWiki) [] manifest

-- | Add one's own, in a session.
installMine :: Running -> [Header] -> Text -> ByteString -> Maybe Text -> IO Reply
installMine r hs = uploadTo r (onPublic r links.externalPlugins.installMine) hs

-- | A bundle's code.
codeA, codeB :: ByteString
codeA = "self.onmessage = () => postMessage({type: \"problems\", problems: []});"
codeB = "self.onmessage = () => postMessage({type: \"problems\", problems: [\"b\"]});"

hashOf :: ByteString -> Text
hashOf b = (blobOf b).text

-- | The 400 a request is refused with, and its sentence.
invalidAs :: (HasCallStack) => Reply -> IO Text
invalidAs reply =
  jsonAs 400 reply >>= \case
    Invalid why -> pure why
    other -> assertFailure ("expected Invalid, got " <> show other)

conflictAs :: (HasCallStack) => Reply -> IO Text
conflictAs reply =
  jsonAs 409 reply >>= \case
    Conflict why -> pure why
    other -> assertFailure ("expected Conflict, got " <> show other)

ids :: ExternalPlugins -> [(Text, Bool)]
ids ps = [(p.manifest.id.text, p.mine) | p <- ps.external]

policies :: Backing -> Spec
policies backing = describe "who may add them" $ do
  it "is nobody at first, so nothing is listed, to a visitor, a user or an administrator" $ withUsers backing $ \r -> do
    editor <- sessionOf r "editor"
    admin <- sessionOf r "admin"
    for_ [[], editor, admin] $ \hs -> listAs r hs `shouldReturn` ExternalPlugins {policy = ExternalOff, external = []}

  it "is set by an administrator only, and logged" $ withUsers backing $ \r -> do
    editor <- sessionOf r "editor"
    admin <- sessionOf r "admin"
    let putPolicy hs p = send r methodPut (onPublic r links.externalPlugins.setPolicy) (jsonType : hs) (encode ExternalPolicyChange {policy = p})
    (putPolicy editor ExternalUsers >>= jsonAs 403) `shouldReturn` Forbidden
    (putPolicy [] ExternalUsers >>= jsonAs 403) `shouldReturn` Forbidden
    (putPolicy admin ExternalUsers >>= jsonAs 200) `shouldReturn` ExternalPlugins {policy = ExternalUsers, external = []}
    listAs r [] `shouldReturn` ExternalPlugins {policy = ExternalUsers, external = []}
    entries <- (\a -> a.entries) <$> (getUrl r (onAdmin r (links.admin.activity Nothing Nothing Nothing)) >>= jsonAs @Activity 200)
    map (\e -> (e.action, Map.toList e.detail)) (take 1 entries) `shouldBe` [("settings.external-plugins", [("policy", "users")])]
    (send r methodPut (onPublic r links.externalPlugins.setPolicy) (jsonType : admin) "{\"policy\": \"everyone\"}" >>= \x -> pure x.status) `shouldReturn` 400

installing :: Backing -> Spec
installing backing = describe "a plugin for everyone" $ do
  it "is installed by an administrator whatever the policy, listed to users once the policy is on, and replaced by its bundle's hash" $ withUsers backing $ \r -> do
    editor <- sessionOf r "editor"
    admin <- sessionOf r "admin"
    made <- installForWiki r (manifestOf "doubled-words" ["https://api.languagetool.org"]) codeA Nothing >>= jsonAs @ExternalPlugin 200
    made.bundle `shouldBe` hashOf codeA
    made.size `shouldBe` BS.length codeA
    made.mine `shouldBe` False
    made.manifest.hosts `shouldBe` ["https://api.languagetool.org"]
    -- Off: the administration sees it, a user does not.
    ids <$> listAs r admin `shouldReturn` [("doubled-words", False)]
    ids <$> listAs r editor `shouldReturn` []
    setPolicy r ExternalWiki
    ids <$> listAs r editor `shouldReturn` [("doubled-words", False)]
    ids <$> listAs r [] `shouldReturn` []
    -- A second upload of the id replaces it only with the hash it knew.
    why <- installForWiki r (manifestOf "doubled-words" []) codeB Nothing >>= conflictAs
    why `shouldSatisfy` T.isInfixOf (hashOf codeA)
    _ <- installForWiki r (manifestOf "doubled-words" []) codeB (Just (hashOf codeB)) >>= conflictAs
    _ <- installForWiki r (manifestOf "other" []) codeB (Just (hashOf codeA)) >>= conflictAs
    replaced <- installForWiki r (manifestOf "doubled-words" []) codeB (Just (hashOf codeA)) >>= jsonAs @ExternalPlugin 200
    replaced.bundle `shouldBe` hashOf codeB
    (\ps -> map (\p -> (p.bundle, p.manifest.hosts)) ps.external) <$> listAs r editor `shouldReturn` [(hashOf codeB, [])]
    -- The activity: the install and the replacement, with the bundles.
    entries <- (\a -> a.entries) <$> (getUrl r (onAdmin r (links.admin.activity Nothing Nothing Nothing)) >>= jsonAs @Activity 200)
    map (\e -> (e.action, Map.lookup "bundle" e.detail)) (take 3 entries)
      `shouldBe` [("plugin.replace", Just (hashOf codeB)), ("settings.external-plugins", Nothing), ("plugin.install", Just (hashOf codeA))]

  it "is refused to a user who does not administer the wiki" $ withUsers backing $ \r -> do
    editor <- sessionOf r "editor"
    (uploadTo r (onPublic r links.externalPlugins.installForWiki) editor (manifestOf "doubled-words" []) codeA Nothing >>= jsonAs 403) `shouldReturn` Forbidden
    (uploadTo r (onPublic r links.externalPlugins.installForWiki) [] (manifestOf "doubled-words" []) codeA Nothing >>= jsonAs 403) `shouldReturn` Forbidden

  it "refuses a manifest or a bundle that breaks the rules, with the rule" $ withUsers backing $ \r -> do
    let agent = T.replace "\"diagnostics\":{}" "\"agent\":{}" (manifestOf "doubled-words" [])
    (installForWiki r agent codeA Nothing >>= invalidAs) >>= (`shouldSatisfy` T.isInfixOf "agent yet")
    (installForWiki r (manifestOf "errors" []) codeA Nothing >>= invalidAs) >>= (`shouldSatisfy` T.isInfixOf "built into the wiki")
    (installForWiki r (manifestOf "doubled-words" ["https://example.org; script-src *"]) codeA Nothing >>= invalidAs) >>= (`shouldSatisfy` T.isInfixOf "is not an origin")
    (installForWiki r "{}" codeA Nothing >>= invalidAs) >>= (`shouldSatisfy` T.isInfixOf "has the field")
    (installForWiki r (manifestOf "doubled-words" []) "\xff\xfe" Nothing >>= invalidAs) `shouldReturn` "A plugin's bundle is JavaScript in UTF-8."
    (installForWiki r (manifestOf "doubled-words" []) "   " Nothing >>= invalidAs) `shouldReturn` "A plugin's bundle is empty."
    (installForWiki r (manifestOf "doubled-words" []) (BS.replicate (maxBundleBytes + 1) 0x61) Nothing >>= invalidAs) >>= (`shouldSatisfy` T.isInfixOf "2 MiB")
    (installForWiki r (manifestOf "doubled-words" []) (BS.replicate maxBundleBytes 0x61) Nothing >>= jsonAs @ExternalPlugin 200) >>= \p -> p.size `shouldBe` maxBundleBytes
    -- Nothing was installed by the refusals.
    ids <$> (getUrl r (onAdmin r links.externalPlugins.list) >>= jsonAs 200) `shouldReturn` [("doubled-words", False)]

ownPlugins :: Backing -> Spec
ownPlugins backing = describe "a user's own plugin" $ do
  it "is added while the policy lets users add theirs, listed to its owner only, and never takes the id of one of the wiki's" $ withUsers backing $ \r -> do
    editor <- sessionOf r "editor"
    readerSession <- sessionOf r "reader"
    _ <- installForWiki r (manifestOf "doubled-words" []) codeA Nothing >>= jsonAs @ExternalPlugin 200
    for_ [ExternalOff, ExternalWiki] $ \p -> do
      setPolicy r p
      (installMine r editor (manifestOf "mine" []) codeB Nothing >>= jsonAs 403) `shouldReturn` Forbidden
    setPolicy r ExternalUsers
    (installMine r [] (manifestOf "mine" []) codeB Nothing >>= jsonAs 403) `shouldReturn` Forbidden
    made <- installMine r editor (manifestOf "mine" []) codeB Nothing >>= jsonAs @ExternalPlugin 200
    made.mine `shouldBe` True
    why <- installMine r editor (manifestOf "doubled-words" []) codeB Nothing >>= conflictAs
    why `shouldSatisfy` T.isInfixOf "installed for everyone"
    ids <$> listAs r editor `shouldReturn` [("doubled-words", False), ("mine", True)]
    ids <$> listAs r readerSession `shouldReturn` [("doubled-words", False)]
    -- The reader may use the same id for one of their own.
    _ <- installMine r readerSession (manifestOf "mine" []) codeA Nothing >>= jsonAs @ExternalPlugin 200
    ids <$> listAs r readerSession `shouldReturn` [("doubled-words", False), ("mine", True)]
    -- A user's own are not in the audit log.
    entries <- (\a -> a.entries) <$> (getUrl r (onAdmin r (links.admin.activity Nothing Nothing Nothing)) >>= jsonAs @Activity 200)
    map (\e -> e.action) (take 1 entries) `shouldBe` ["settings.external-plugins"]
    -- Kept, not listed, while users may not add theirs.
    setPolicy r ExternalWiki
    ids <$> listAs r editor `shouldReturn` [("doubled-words", False)]
    setPolicy r ExternalUsers
    ids <$> listAs r editor `shouldReturn` [("doubled-words", False), ("mine", True)]

  it "counts at most twenty for a user" $ withUsers backing $ \r -> do
    editor <- sessionOf r "editor"
    setPolicy r ExternalUsers
    for_ [1 .. maxOwnPlugins] $ \n -> installMine r editor (manifestOf ("p" <> show n) []) codeA Nothing >>= jsonAs @ExternalPlugin 200
    (installMine r editor (manifestOf "one-more" []) codeA Nothing >>= conflictAs) >>= (`shouldSatisfy` T.isInfixOf (show maxOwnPlugins))
    -- A replacement is not one more.
    _ <- installMine r editor (manifestOf "p1" []) codeB (Just (hashOf codeA)) >>= jsonAs @ExternalPlugin 200
    length . (\ps -> ps.external) <$> listAs r editor `shouldReturn` maxOwnPlugins

frames :: Backing -> Spec
frames backing = describe "a plugin's frame" $ do
  it "is the two scripts under the policy their hashes and the manifest's hosts make, to a user who may run the plugin" $ withUsers backing $ \r -> do
    editor <- sessionOf r "editor"
    made <- installForWiki r (manifestOf "doubled-words" ["https://api.languagetool.org"]) codeA Nothing >>= jsonAs @ExternalPlugin 200
    let at = onPublic r (framePath made)
        code = decodeUtf8 codeA
    -- Off: no frame for anyone.
    (\x -> x.status) <$> getUrlWith r at editor `shouldReturn` 404
    setPolicy r ExternalWiki
    reply <- getUrlWith r at editor
    reply.status `shouldBe` 200
    header "Content-Type" reply `shouldBe` Just "text/html; charset=utf-8"
    header "X-Content-Type-Options" reply `shouldBe` Just "nosniff"
    header "Content-Security-Policy" reply `shouldBe` Just (encodeUtf8 (pluginFramePolicy ["https://api.languagetool.org"] (map scriptHash (frameScripts code))))
    header "Cache-Control" reply `shouldBe` Just "private, no-cache"
    let body = decodeUtf8 (BSL.toStrict reply.body)
    body `shouldSatisfy` T.isInfixOf ("<script>" <> codeScript code <> "</script>")
    T.count "<script>" body `shouldBe` 2
    -- The code is a JSON string with every < escaped: nothing in it ends
    -- the script.
    codeScript "a</script><script>alert(1)" `shouldBe` "self.wikiPluginSource = \"a\\u003c/script>\\u003cscript>alert(1)\";"
    -- Revalidated by its tag.
    for_ (header "ETag" reply) $ \tag -> (\x -> x.status) <$> getUrlWith r at (("If-None-Match", tag) : editor) `shouldReturn` 304
    header "ETag" reply `shouldSatisfy` isJust
    -- A replacement that keeps the bundle and changes the hosts keeps the
    -- address, and makes a new tag: the old one is answered in full, with
    -- the new policy.
    _ <- installForWiki r (manifestOf "doubled-words" ["https://example.org"]) codeA (Just made.bundle) >>= jsonAs @ExternalPlugin 200
    for_ (header "ETag" reply) $ \tag -> do
      again <- getUrlWith r at (("If-None-Match", tag) : editor)
      again.status `shouldBe` 200
      header "Content-Security-Policy" again `shouldBe` Just (encodeUtf8 (pluginFramePolicy ["https://example.org"] (map scriptHash (frameScripts code))))
    _ <- installForWiki r (manifestOf "doubled-words" ["https://api.languagetool.org"]) codeA (Just made.bundle) >>= jsonAs @ExternalPlugin 200
    -- A frame of the wiki's page is answered; a tab, or another site, is not.
    (\x -> x.status) <$> getUrlWith r at ([("Sec-Fetch-Dest", "iframe"), ("Sec-Fetch-Site", "same-origin")] <> editor) `shouldReturn` 200
    (\x -> x.status) <$> getUrlWith r at (("Sec-Fetch-Dest", "document") : editor) `shouldReturn` 404
    (\x -> x.status) <$> getUrlWith r at ([("Sec-Fetch-Dest", "iframe"), ("Sec-Fetch-Site", "cross-site")] <> editor) `shouldReturn` 404
    -- Nobody signed in, a version replaced since, another owner, another
    -- contribution, and a method other than GET: none.
    (\x -> x.status) <$> getUrl r at `shouldReturn` 404
    (\x -> x.status) <$> getUrlWith r (T.replace made.bundle (hashOf codeB) at) editor `shouldReturn` 404
    (\x -> x.status) <$> getUrlWith r (T.replace "/wiki/" "/mine/" at) editor `shouldReturn` 404
    (\x -> x.status) <$> getUrlWith r (T.replace "/diagnostics" "/agent" at) editor `shouldReturn` 404
    (\x -> x.status) <$> send r methodPost at editor "" `shouldReturn` 405
    -- Off for the whole wiki: no frame.
    _ <- sendJson r methodPut (onAdmin r links.admin.setPlugins) WikiPlugins {off = Set.fromList [PluginId "doubled-words"]} >>= jsonAs @WikiPlugins 200
    (\x -> x.status) <$> getUrlWith r at editor `shouldReturn` 404
    _ <- sendJson r methodPut (onAdmin r links.admin.setPlugins) WikiPlugins {off = Set.empty} >>= jsonAs @WikiPlugins 200
    (\x -> x.status) <$> getUrlWith r at editor `shouldReturn` 200

  it "is a user's own plugin's to that user alone, while users may add theirs, and not when the wiki has the id" $ withUsers backing $ \r -> do
    editor <- sessionOf r "editor"
    readerSession <- sessionOf r "reader"
    setPolicy r ExternalUsers
    made <- installMine r editor (manifestOf "shared-id" []) codeB Nothing >>= jsonAs @ExternalPlugin 200
    let at = onPublic r (framePath made)
    (\x -> x.status) <$> getUrlWith r at editor `shouldReturn` 200
    (\x -> x.status) <$> getUrlWith r at readerSession `shouldReturn` 404
    setPolicy r ExternalWiki
    (\x -> x.status) <$> getUrlWith r at editor `shouldReturn` 404
    setPolicy r ExternalUsers
    -- The wiki's of the same id runs in its place.
    wikis <- installForWiki r (manifestOf "shared-id" []) codeA Nothing >>= jsonAs @ExternalPlugin 200
    (\x -> x.status) <$> getUrlWith r at editor `shouldReturn` 404
    (\x -> x.status) <$> getUrlWith r (onPublic r (framePath wikis)) editor `shouldReturn` 200

  it "runs nothing when the bundle's file was changed on the disk" $ withUsers backing $ \r -> do
    editor <- sessionOf r "editor"
    setPolicy r ExternalWiki
    made <- installForWiki r (manifestOf "doubled-words" []) codeA Nothing >>= jsonAs @ExternalPlugin 200
    let at = onPublic r (framePath made)
    file <- maybe (assertFailure "no file for the bundle") pure (blobFile (attachmentsDir r.dataDir) (BlobId made.bundle))
    renameFile file (file <> ".kept")
    BS.writeFile file codeB
    reply <- getUrlWith r at editor
    reply.status `shouldBe` 409
    header "Content-Security-Policy" reply `shouldBe` Nothing
    removeFile file
    (\x -> x.status) <$> getUrlWith r at editor `shouldReturn` 409
    renameFile (file <> ".kept") file
    (\x -> x.status) <$> getUrlWith r at editor `shouldReturn` 200

removing :: Backing -> Spec
removing backing = describe "removing" $ do
  it "a plugin for everyone, as an administrator, which ends its frame and is logged" $ withUsers backing $ \r -> do
    editor <- sessionOf r "editor"
    setPolicy r ExternalWiki
    made <- installForWiki r (manifestOf "doubled-words" []) codeA Nothing >>= jsonAs @ExternalPlugin 200
    let at = onPublic r (links.externalPlugins.removeForWiki (PluginId "doubled-words"))
    (send r methodDelete at editor "" >>= jsonAs 403) `shouldReturn` Forbidden
    (\x -> x.status) <$> send r methodDelete (onAdmin r (links.externalPlugins.removeForWiki (PluginId "doubled-words"))) [] "" `shouldReturn` 204
    (send r methodDelete (onAdmin r (links.externalPlugins.removeForWiki (PluginId "doubled-words"))) [] "" >>= jsonAs 404) `shouldReturn` NotFound
    (\x -> x.status) <$> getUrlWith r (onPublic r (framePath made)) editor `shouldReturn` 404
    ids <$> listAs r editor `shouldReturn` []
    entries <- (\a -> a.entries) <$> (getUrl r (onAdmin r (links.admin.activity Nothing Nothing Nothing)) >>= jsonAs @Activity 200)
    map (\e -> e.action) (take 1 entries) `shouldBe` ["plugin.remove"]

  it "a user's own, by that user whatever the policy, and with a removed user" $ withUsers backing $ \r -> do
    editor <- sessionOf r "editor"
    readerSession <- sessionOf r "reader"
    setPolicy r ExternalUsers
    _ <- installMine r editor (manifestOf "mine" []) codeA Nothing >>= jsonAs @ExternalPlugin 200
    _ <- installMine r readerSession (manifestOf "mine" []) codeB Nothing >>= jsonAs @ExternalPlugin 200
    _ <- installMine r readerSession (manifestOf "other" []) codeB Nothing >>= jsonAs @ExternalPlugin 200
    setPolicy r ExternalOff
    let removeAt = onPublic r (links.externalPlugins.removeMine (PluginId "other"))
    (\x -> x.status) <$> send r methodDelete removeAt readerSession "" `shouldReturn` 204
    (send r methodDelete removeAt readerSession "" >>= jsonAs 404) `shouldReturn` NotFound
    let ownersOf = withStorageAside r.place $ \st -> sort . map (\x -> (x.owner_id, x.plugin_id)) <$> reading st (runSelectReturningList (select (all_ wikiDb.external_plugins)))
    ownersOf `shouldReturn` [(Just "user-editor", "mine"), (Just "user-reader", "mine")]
    _ :: Account <- sendJson r methodPut (onAdmin r (links.admin.setDisabled (UserId "user-reader"))) AccountDisabled {disabled = True} >>= jsonAs 200
    (\x -> x.status) <$> send r methodDelete (onAdmin r (links.admin.removeUser (UserId "user-reader"))) [] "" `shouldReturn` 204
    ownersOf `shouldReturn` [(Just "user-editor", "mine")]
