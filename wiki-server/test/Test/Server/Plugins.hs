-- | Which plugins are on ("Wiki.Server.Plugins", "Wiki.Api.Plugins"), over
-- real HTTP: what @GET api\/v1\/plugins@ answers to a visitor, a user and
-- the operator; a user's state and options, changed apart; the bounds and
-- the refusals; the administrator's set, which leaves the users' choices
-- as they were; and what goes with a removed user.
--
-- The demo's wiki is the server's, open to anyone: the editor, the reader
-- and the admin, who is an administrator, sign in with the passwords the
-- operator gives them.
module Test.Server.Plugins (spec) where

import Data.Aeson (Value, encode, object, toJSON, (.=))
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Database.Beam (all_, runSelectReturningList, select)
import Network.HTTP.Types (Header, hContentType, methodDelete, methodGet, methodPut)
import Protolude
import Test.Database (Backing)
import Test.HUnit (assertFailure)
import Test.Hspec
import Test.Server.Fixture
import Wiki.Api (AccountApi (..), AdminApi (..), WikiApi (..))
import Wiki.Api.Account (Account, AccountDisabled (..))
import Wiki.Api.Activity (Activity (..))
import Wiki.Api.Admin (WikiSettings (..))
import Wiki.Api.Error (ApiError (..))
import Wiki.Api.Links (links)
import Wiki.Api.Plugins
import Wiki.Core.Access (Access (..))
import Wiki.Core.Audit (Actor (..), AuditEntry (..))
import Wiki.Core.Id (UserId (..))
import Wiki.Core.User (Login (..))
import Wiki.Server.Storage (reading, withStorageAside)
import Wiki.Server.Storage.Schema (UserPlugins (..), WikiDb (..), wikiDb)

spec :: Backing -> Spec
spec backing = describe "plugins" $ do
  nothingStored backing
  choosing backing
  refusing backing
  wikiWide backing
  apart backing

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

onPublic, onAdmin :: Running -> Text -> Text
onPublic r address = r.public <> "/" <> address
onAdmin r address = r.admin <> "/admin/" <> address

jsonType :: Header
jsonType = (hContentType, "application/json")

-- | What is on, as the public listener answers a session (or nobody).
pluginsAs :: (HasCallStack) => Running -> [Header] -> IO Plugins
pluginsAs r hs = send r methodGet (onPublic r links.plugins) hs "" >>= jsonAs 200

-- | Set a plugin's state in a session.
putState :: Running -> [Header] -> PluginId -> PluginState -> IO Reply
putState r hs p s = send r methodPut (onPublic r (links.account.setPluginState p)) (jsonType : hs) (encode PluginStateChange {state = s})

-- | Set a plugin's options in a session, with the body as sent.
putOptions :: Running -> [Header] -> PluginId -> LByteString -> IO Reply
putOptions r hs p = send r methodPut (onPublic r (links.account.setPluginOptions p)) (jsonType : hs)

clearOptions :: Running -> [Header] -> PluginId -> IO Reply
clearOptions r hs p = send r methodDelete (onPublic r (links.account.clearPluginOptions p)) hs ""

-- | Replace the wiki's set, on the listener the address is for, with
-- these headers.
putWikiSet :: Running -> Text -> [Header] -> Set PluginId -> IO Reply
putWikiSet r address hs off = send r methodPut address (jsonType : hs) (encode WikiPlugins {off})

assistant, errors :: PluginId
assistant = PluginId "assistant"
errors = PluginId "errors"

-- | A choice.
chose :: PluginState -> Maybe Value -> PluginChoice
chose s o = PluginChoice {state = s, options = o}

-- | Some options, and their JSON as a browser sends it.
someOptions :: Value
someOptions = object ["model" .= ("claude-opus" :: Text), "floating" .= True]

-- | The 400 a request is refused with, and its sentence.
invalidAs :: (HasCallStack) => Reply -> IO Text
invalidAs reply =
  jsonAs 400 reply >>= \case
    Invalid why -> pure why
    other -> assertFailure ("expected Invalid, got " <> show other)

nothingStored :: Backing -> Spec
nothingStored backing = describe "with nothing stored" $ do
  it "answers nothing off and no choices to a visitor, a user and the operator" $ withUsers backing $ \r -> do
    pluginsAs r [] `shouldReturn` noPlugins
    editor <- sessionOf r "editor"
    pluginsAs r editor `shouldReturn` noPlugins
    (getUrl r (onAdmin r links.plugins) >>= jsonAs 200) `shouldReturn` noPlugins
    (getUrl r (onAdmin r links.admin.plugins) >>= jsonAs 200) `shouldReturn` WikiPlugins {off = Set.empty}

  it "refuses a visitor of a closed wiki, as its other reads, and answers its users" $ withUsers backing $ \r -> do
    editor <- sessionOf r "editor"
    closed :: WikiSettings <- sendJson r methodPut (onAdmin r links.admin.setSettings) WikiSettings {access = Closed, newAccountsCreateSpaces = False, adminCreatesPasswordAccounts = True} >>= jsonAs 200
    closed.access `shouldBe` Closed
    (send r methodGet (onPublic r links.plugins) [] "" >>= jsonAs 401) `shouldReturn` Unauthenticated
    pluginsAs r editor `shouldReturn` noPlugins

choosing :: Backing -> Spec
choosing backing = describe "a user's choice" $ do
  it "sets the state and the options each apart, and keeps nothing for a plugin given back to its default with no options" $ withUsers backing $ \r -> do
    editor <- sessionOf r "editor"
    (putState r editor assistant PluginOn >>= jsonAs 200) `shouldReturn` chose PluginOn Nothing
    (putOptions r editor assistant (encode someOptions) >>= jsonAs 200) `shouldReturn` chose PluginOn (Just someOptions)
    (putState r editor errors PluginOff >>= jsonAs 200) `shouldReturn` chose PluginOff Nothing
    mine <- (\p -> p.mine) <$> pluginsAs r editor
    mine `shouldBe` Map.fromList [(assistant, chose PluginOn (Just someOptions)), (errors, chose PluginOff Nothing)]
    -- Back to the default: the options stay, until they are cleared too.
    (putState r editor assistant PluginDefault >>= jsonAs 200) `shouldReturn` chose PluginDefault (Just someOptions)
    (clearOptions r editor assistant >>= jsonAs 200) `shouldReturn` chose PluginDefault Nothing
    (putState r editor errors PluginDefault >>= jsonAs 200) `shouldReturn` chose PluginDefault Nothing
    pluginsAs r editor `shouldReturn` noPlugins
    -- Options alone, for a plugin left to its default, and options that
    -- are not an object.
    (putOptions r editor errors "[1, 2, 3]" >>= jsonAs 200) `shouldReturn` chose PluginDefault (Just (toJSON [1 :: Int, 2, 3]))
    (\p -> p.mine) <$> pluginsAs r editor `shouldReturn` Map.fromList [(errors, chose PluginDefault (Just (toJSON [1 :: Int, 2, 3])))]

  -- Two tabs of one user: each read what is on, then one flips the switch
  -- while the other saves the options. Neither request carries what the
  -- other writes, so neither undoes the other, in either order.
  it "keeps both of two tabs' changes, the switch from one and the options from the other, in either order" $ withUsers backing $ \r -> do
    editor <- sessionOf r "editor"
    _ <- putState r editor assistant PluginOn >>= jsonAs @PluginChoice 200
    _ <- putOptions r editor assistant (encode (object ["v" .= (1 :: Int)])) >>= jsonAs @PluginChoice 200
    opened <- pluginsAs r editor
    Map.lookup assistant opened.mine `shouldBe` Just (chose PluginOn (Just (object ["v" .= (1 :: Int)])))
    -- The switch first, then the options.
    _ <- putState r editor assistant PluginOff >>= jsonAs @PluginChoice 200
    _ <- putOptions r editor assistant (encode (object ["v" .= (2 :: Int)])) >>= jsonAs @PluginChoice 200
    Map.lookup assistant . (\p -> p.mine) <$> pluginsAs r editor `shouldReturn` Just (chose PluginOff (Just (object ["v" .= (2 :: Int)])))
    -- The options first, then the switch.
    _ <- putOptions r editor assistant (encode (object ["v" .= (3 :: Int)])) >>= jsonAs @PluginChoice 200
    _ <- putState r editor assistant PluginOn >>= jsonAs @PluginChoice 200
    Map.lookup assistant . (\p -> p.mine) <$> pluginsAs r editor `shouldReturn` Just (chose PluginOn (Just (object ["v" .= (3 :: Int)])))
    -- Clearing the options leaves the switch as the other tab set it.
    _ <- putState r editor assistant PluginOff >>= jsonAs @PluginChoice 200
    (clearOptions r editor assistant >>= jsonAs 200) `shouldReturn` chose PluginOff Nothing

refusing :: Backing -> Spec
refusing backing = describe "refuses" $ do
  it "an id that is not a plugin's, with what an id is" $ withUsers backing $ \r -> do
    editor <- sessionOf r "editor"
    let stateAt raw = send r methodPut (onPublic r ("api/v1/account/plugins/" <> raw <> "/state")) (jsonType : editor) (encode PluginStateChange {state = PluginOn})
    for_ ["Assistant", "9lives", "a_b", "-x", T.replicate 65 "a", "%C3%A9t%C3%A9"] $ \raw ->
      (stateAt raw >>= invalidAs) `shouldReturn` pluginIdRule
    (stateAt (T.replicate 64 "a") >>= jsonAs 200) `shouldReturn` chose PluginOn Nothing
    (stateAt "a-1-b" >>= jsonAs 200) `shouldReturn` chose PluginOn Nothing
    -- In the administrator's set too.
    bad <- send r methodPut (onAdmin r links.admin.setPlugins) [jsonType] "{\"off\": [\"errors\", \"Assistant\"]}"
    bad.status `shouldBe` 400
    (getUrl r (onAdmin r links.admin.plugins) >>= jsonAs 200) `shouldReturn` WikiPlugins {off = Set.empty}

  it "options over 16 kB as sent, or as stored, and null" $ withUsers backing $ \r -> do
    editor <- sessionOf r "editor"
    -- A string whose JSON, quotes and padding included, is the size given.
    let sized n = BSL.replicate (fromIntegral n - 4) 0x20 <> "\"ab\""
    (putOptions r editor assistant (sized maxOptionsBytes) >>= jsonAs 200) `shouldReturn` chose PluginDefault (Just "ab")
    (putOptions r editor assistant (sized (maxOptionsBytes + 1)) >>= invalidAs) `shouldReturn` optionsTooLarge
    -- Short as sent, but written out at length: each number has a
    -- thousand digits once stored.
    let numbers = "[" <> BSL.intercalate "," (replicate 20 "1e1000") <> "]"
    BSL.length numbers `shouldSatisfy` (< 200)
    (putOptions r editor assistant numbers >>= invalidAs) `shouldReturn` optionsTooLarge
    why <- putOptions r editor assistant "null" >>= invalidAs
    why `shouldSatisfy` T.isInfixOf "DELETE"
    (\p -> p.mine) <$> pluginsAs r editor `shouldReturn` Map.fromList [(assistant, chose PluginDefault (Just "ab"))]

  it "a user's 101st plugin, until one is given back to its default with no options" $ withUsers backing $ \r -> do
    readerSession <- sessionOf r "reader"
    let ids = [PluginId ("p" <> show n) | n <- [1 .. maxPluginsPerUser]]
    for_ ids $ \p -> putState r readerSession p PluginOff >>= jsonAs @PluginChoice 200
    why <- putState r readerSession (PluginId "one-more") PluginOn >>= invalidAs
    why `shouldSatisfy` T.isInfixOf (show maxPluginsPerUser)
    (putOptions r readerSession (PluginId "one-more") "{}" >>= invalidAs) `shouldReturn` why
    -- A plugin that has a choice already may change it.
    (putOptions r readerSession (PluginId "p1") "{}" >>= jsonAs 200) `shouldReturn` chose PluginOff (Just (object []))
    -- Back to the default, but with options: still kept, still counted.
    _ <- putState r readerSession (PluginId "p1") PluginDefault >>= jsonAs @PluginChoice 200
    _ <- putState r readerSession (PluginId "one-more") PluginOn >>= invalidAs
    _ <- clearOptions r readerSession (PluginId "p1") >>= jsonAs @PluginChoice 200
    (putState r readerSession (PluginId "one-more") PluginOn >>= jsonAs 200) `shouldReturn` chose PluginOn Nothing
    Map.size . (\p -> p.mine) <$> pluginsAs r readerSession `shouldReturn` maxPluginsPerUser

  it "a change by nobody signed in, and by the operator, who is no user" $ withUsers backing $ \r -> do
    (putState r [] assistant PluginOn >>= jsonAs 401) `shouldReturn` Unauthenticated
    (putOptions r [] assistant "{}" >>= jsonAs 401) `shouldReturn` Unauthenticated
    (clearOptions r [] assistant >>= jsonAs 401) `shouldReturn` Unauthenticated
    let operator = [jsonType]
    _ <- send r methodPut (onAdmin r (links.account.setPluginState assistant)) operator (encode PluginStateChange {state = PluginOn}) >>= invalidAs
    _ <- send r methodPut (onAdmin r (links.account.setPluginOptions assistant)) operator "{}" >>= invalidAs
    _ <- send r methodDelete (onAdmin r (links.account.clearPluginOptions assistant)) [] "" >>= invalidAs
    pluginsAs r [] `shouldReturn` noPlugins

wikiWide :: Backing -> Spec
wikiWide backing = describe "off for the whole wiki" $ do
  it "is off for everyone, keeps each user's choice, which applies again once it is back on, and is logged" $ withUsers backing $ \r -> do
    editor <- sessionOf r "editor"
    admin <- sessionOf r "admin"
    let activity = getUrl r (onAdmin r (links.admin.activity Nothing Nothing Nothing)) >>= jsonAs @Activity 200
    logged <- length . (\a -> a.entries) <$> activity
    _ <- putState r editor assistant PluginOn >>= jsonAs @PluginChoice 200
    _ <- putOptions r editor assistant (encode someOptions) >>= jsonAs @PluginChoice 200
    -- An administrator's session on the public listener.
    (putWikiSet r (onPublic r links.admin.setPlugins) admin (Set.fromList [assistant, errors]) >>= jsonAs 200)
      `shouldReturn` WikiPlugins {off = Set.fromList [assistant, errors]}
    pluginsAs r editor `shouldReturn` Plugins {offForWiki = Set.fromList [assistant, errors], mine = Map.fromList [(assistant, chose PluginOn (Just someOptions))]}
    pluginsAs r [] `shouldReturn` Plugins {offForWiki = Set.fromList [assistant, errors], mine = Map.empty}
    (getUrl r (onAdmin r links.plugins) >>= jsonAs 200) `shouldReturn` Plugins {offForWiki = Set.fromList [assistant, errors], mine = Map.empty}
    -- A user's own change while the wiki has it off is kept as well.
    _ <- putState r editor errors PluginOn >>= jsonAs @PluginChoice 200
    -- The operator turns it back on, on the admin listener.
    (putWikiSet r (onAdmin r links.admin.setPlugins) [] (Set.fromList [errors]) >>= jsonAs 200) `shouldReturn` WikiPlugins {off = Set.fromList [errors]}
    pluginsAs r editor `shouldReturn` Plugins {offForWiki = Set.fromList [errors], mine = Map.fromList [(assistant, chose PluginOn (Just someOptions)), (errors, chose PluginOn Nothing)]}
    (send r methodGet (onPublic r links.admin.plugins) admin "" >>= jsonAs 200) `shouldReturn` WikiPlugins {off = Set.fromList [errors]}
    -- The administrator's two changes are all the audit log gained, with
    -- what was off; the user's own changes, before and between them, are
    -- not in it.
    later <- (\a -> a.entries) <$> activity
    logged `shouldSatisfy` (< 40)
    length later `shouldBe` logged + 2
    map (\e -> (e.action, e.actor.login.text, Map.toList e.detail)) (take 2 later)
      `shouldBe` [ ("settings.plugins", "administrator", [("off", "errors")])
                 , ("settings.plugins", "admin", [("off", "assistant, errors")])
                 ]

  it "is set by an administrator only, and holds at most 100 plugins" $ withUsers backing $ \r -> do
    editor <- sessionOf r "editor"
    (putWikiSet r (onPublic r links.admin.setPlugins) editor (Set.fromList [assistant]) >>= jsonAs 403) `shouldReturn` Forbidden
    (putWikiSet r (onPublic r links.admin.setPlugins) [] (Set.fromList [assistant]) >>= jsonAs 403) `shouldReturn` Forbidden
    (send r methodGet (onPublic r links.admin.plugins) editor "" >>= jsonAs 403) `shouldReturn` Forbidden
    let upTo n = Set.fromList [PluginId ("p" <> show k) | k <- [1 .. n :: Int]]
    why <- putWikiSet r (onAdmin r links.admin.setPlugins) [] (upTo (maxPluginsOff + 1)) >>= invalidAs
    why `shouldSatisfy` T.isInfixOf (show maxPluginsOff)
    (putWikiSet r (onAdmin r links.admin.setPlugins) [] (upTo maxPluginsOff) >>= jsonAs 200) `shouldReturn` WikiPlugins {off = upTo maxPluginsOff}
    (\p -> p.offForWiki) <$> pluginsAs r editor `shouldReturn` upTo maxPluginsOff

apart :: Backing -> Spec
apart backing = describe "each user's own" $ do
  -- Two users choose differently for the same plugin, and a third chooses
  -- nothing: each reads their own only.
  it "is read by that user only" $ withUsers backing $ \r -> do
    editor <- sessionOf r "editor"
    readerSession <- sessionOf r "reader"
    admin <- sessionOf r "admin"
    _ <- putState r editor assistant PluginOn >>= jsonAs @PluginChoice 200
    _ <- putOptions r editor assistant "{\"by\": \"editor\"}" >>= jsonAs @PluginChoice 200
    _ <- putState r readerSession assistant PluginOff >>= jsonAs @PluginChoice 200
    _ <- putOptions r readerSession errors "{\"by\": \"reader\"}" >>= jsonAs @PluginChoice 200
    (\p -> p.mine) <$> pluginsAs r editor `shouldReturn` Map.fromList [(assistant, chose PluginOn (Just (object ["by" .= ("editor" :: Text)])))]
    (\p -> p.mine) <$> pluginsAs r readerSession
      `shouldReturn` Map.fromList [(assistant, chose PluginOff Nothing), (errors, chose PluginDefault (Just (object ["by" .= ("reader" :: Text)])))]
    (\p -> p.mine) <$> pluginsAs r admin `shouldReturn` Map.empty
    -- A change answers the user's own choice, not another's.
    (clearOptions r admin assistant >>= jsonAs 200) `shouldReturn` chose PluginDefault Nothing

  it "goes with a removed user, and the others' stay" $ withUsers backing $ \r -> do
    editor <- sessionOf r "editor"
    readerSession <- sessionOf r "reader"
    _ <- putState r editor assistant PluginOn >>= jsonAs @PluginChoice 200
    _ <- putOptions r readerSession assistant "{}" >>= jsonAs @PluginChoice 200
    _ <- putState r readerSession errors PluginOff >>= jsonAs @PluginChoice 200
    let ownersOf = withStorageAside r.place $ \st -> sort . map (\x -> (x.user_id, x.plugin_id)) <$> reading st (runSelectReturningList (select (all_ wikiDb.user_plugins)))
    ownersOf `shouldReturn` [("user-editor", "assistant"), ("user-reader", "assistant"), ("user-reader", "errors")]
    _ :: Account <- sendJson r methodPut (onAdmin r (links.admin.setDisabled (UserId "user-reader"))) AccountDisabled {disabled = True} >>= jsonAs 200
    (\x -> x.status) <$> send r methodDelete (onAdmin r (links.admin.removeUser (UserId "user-reader"))) [] "" `shouldReturn` 204
    ownersOf `shouldReturn` [("user-editor", "assistant")]
