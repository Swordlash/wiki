{-# LANGUAGE TemplateHaskell #-}

-- | The assistant's keys and the user's vault, in a browser (WP5.21,
-- WP5.74 and WP5.84 of docs/PLAN.md): a key typed in the assistant's
-- field kept sealed in this browser where there is no vault, or in the
-- vault the first sign-in made; and the settings stored as text before
-- the vault sealed, as a browser of an earlier version did.
--
-- Over the backend over HTTP, with the test's server in fetch's place
-- ("Test.Browser.Http") and the vault's server played beside it, and
-- passkeys made by a stub in the place of @navigator.credentials@
-- (@jsbits\/test-passkeys.js@): the harness has no virtual authenticator.
-- The stub's PRF output is the same on every device, as a synced
-- passkey's is. The real dialog of a browser is checked by hand.
module Test.Browser.Vault (spec) where

import Control.Monad.UUID (MonadUUID)
import Data.Aeson (Value, decodeStrict, encode)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Halogen.JSBits (Inert (..), JSText, Safety (..), browserJS, fromJSText, inBrowser, toJSText)
import Halogen.JSBits.Promise (Promise (..), wait)
import Halogen.VDom.DOM.Monad.Class (MonadBrowserDOM)
import Protolude hiding (find, wait)
import Test.Browser.Encrypted qualified as E
import Test.Browser.Http (Asked (..), answerWith, asked, holdAt, release, serveDemo, serverStartsWith, serverStops, startServer)
import Test.Browser.Mount (openSettings, showSection, signOut)
import Test.Hspec.Halogen
import Test.LegacyKeys (Keys (..), readKeys)
import Web.Storage.IndexedDB (MonadIndexedDB)
import Web.Storage.IndexedDB qualified as IDB
import Wiki.Api.Security (ResetToken (..))
import Wiki.Api.ServerSettings (defaultHomePage)
import Wiki.Api.Types (SessionInfo (..))
import Wiki.App.Assistant (MonadAssistant (..), assistantDatabase, dropSlotsIfIO, vaultGenerationIO)
import Wiki.App.Backend (MonadWiki (..), Session (..), session)
import Wiki.App.Backend.Encrypted (MonadEncryption (..))
import Wiki.App.Backend.Mem (newMemWiki, runMemWiki)
import Wiki.App.Backend.Mem.State qualified as M
import Wiki.App.Demo (demoNow, demoState)
import Wiki.App.Islands (MonadIslands)
import Wiki.App.Route (Route (..), SettingsSection (..))
import Wiki.App.Route qualified as R
import Wiki.App.Shell qualified as Shell
import Wiki.Core.Access (Access (..))
import Wiki.Core.Assistant (Provider (..), Settings, defaultSettings, withKey, withModel)
import Wiki.Core.Id (SpaceId (..), UserId (..))
import Wiki.Core.Page (Page (..))
import Wiki.Core.Permission (Grants (..))
import Wiki.Core.Space (Entry (..), Space (..))
import Wiki.Core.User (Standing (..))

$( browserJS
     ["jsbits/test-passkeys.js"]
     [ ("js_install", "wiki_test_passkeys_install", Safe, [t|Bool -> Bool -> IO ()|])
     , ("js_uninstall", "wiki_test_passkeys_uninstall", Safe, [t|IO ()|])
     , ("js_touches", "wiki_test_passkeys_touches", Safe, [t|IO Int|])
     , ("js_vault_server", "wiki_test_vault_server_start", Safe, [t|IO ()|])
     , ("js_vault_state", "wiki_test_vault_server_state", Safe, [t|IO JSText|])
     , ("js_new_devicePromise", "wiki_test_device_new", Safe, [t|IO (Promise ())|])
     , ("js_reload", "wiki_test_device_reload", Safe, [t|IO ()|])
     , ("js_stores_keyPromise", "wiki_test_device_stores_key", Safe, [t|JSText -> IO (Promise Bool)|])
     , ("js_recordPromise", "wiki_test_device_record", Safe, [t|JSText -> IO (Promise JSText)|])
     , ("js_scripted", "wiki_test_passkeys_scripted", Safe, [t|IO ()|])
     , ("js_cancel_next", "wiki_test_passkeys_cancel_next", Safe, [t|IO ()|])
     , ("js_add_unwrapped", "wiki_test_vault_server_add_unwrapped", Safe, [t|IO ()|])
     , ("js_other_tab_dropsPromise", "wiki_test_other_tab_drops", Safe, [t|JSText -> IO (Promise ())|])
     , ("js_stash", "wiki_test_vault_server_stash", Safe, [t|IO ()|])
     , ("js_exportablePromise", "wiki_test_exportable_keys_left", Safe, [t|IO (Promise Int)|])
     , ("js_keys_mark", "wiki_test_keys_mark", Safe, [t|IO ()|])
     , ("js_keys_checked", "wiki_test_keys_checked", Safe, [t|IO JSText|])
     , ("js_password_keys", "wiki_test_password_keys", Safe, [t|IO Int|])
     , ("js_loose_keys", "wiki_test_loose_keys", Safe, [t|IO Int|])
     , ("js_key_ownersPromise", "wiki_test_key_owners", Safe, [t|IO (Promise JSText)|])
     , ("js_existing", "wiki_test_passkeys_existing", Safe, [t|IO JSText|])
     , ("js_unregistered", "wiki_test_passkeys_unregistered", Safe, [t|IO JSText|])
     , ("js_offered", "wiki_test_passkeys_offered", Safe, [t|IO Bool|])
     , ("js_pick", "wiki_test_passkeys_pick", Safe, [t|IO ()|])
     , ("js_offers", "wiki_test_passkeys_offers", Safe, [t|IO Int|])
     , ("js_insecure", "wiki_test_passkeys_insecure", Safe, [t|IO ()|])
     , ("js_plain_http", "wiki_test_plain_http", Safe, [t|IO ()|])
     , ("js_plain_http_end", "wiki_test_plain_http_end", Safe, [t|IO ()|])
     , ("js_opaque_server", "wiki_test_opaque_server_start", Safe, [t|JSText -> IO ()|])
     , ("js_opaque_registerPromise", "wiki_test_opaque_register", Safe, [t|JSText -> IO (Promise ())|])
     , ("js_identity_exportPromise", "wiki_test_identity_export", Safe, [t|JSText -> IO (Promise JSText)|])
     , ("js_export_key_traces", "wiki_test_export_key_traces", Safe, [t|IO JSText|])
     , ("js_opaque_second_factor", "wiki_test_opaque_second_factor", Safe, [t|IO ()|])
     , ("js_assistant_keyPromise", "wiki_test_assistant_key", Safe, [t|JSText -> Bool -> IO (Promise ())|])
     , ("js_hear_drops", "wiki_test_hear_drops", Safe, [t|IO ()|])
     , ("js_drops_heard", "wiki_test_drops_heard", Safe, [t|IO JSText|])
     , ("js_page_key_slots", "wiki_test_page_key_slots", Safe, [t|IO JSText|])
     , ("js_page_key_holdPromise", "wiki_test_page_key_hold", Safe, [t|JSText -> IO (Promise ())|])
     , ("js_tell_dropped", "wiki_test_tell_dropped", Safe, [t|JSText -> IO ()|])
     , ("js_tell_signed_in", "wiki_test_tell_signed_in", Safe, [t|IO ()|])
     , ("js_heard_signed_in", "wiki_test_heard_signed_in", Safe, [t|IO Bool|])
     ]
 )

js_other_tab_drops :: JSText -> IO ()
js_other_tab_drops a1 = if inBrowser then wait =<< js_other_tab_dropsPromise a1 else pure inert

js_new_device :: IO ()
js_new_device = if inBrowser then wait =<< js_new_devicePromise else pure inert

js_stores_key :: JSText -> IO Bool
js_stores_key a1 = if inBrowser then wait =<< js_stores_keyPromise a1 else pure inert

js_record :: JSText -> IO JSText
js_record a1 = if inBrowser then wait =<< js_recordPromise a1 else pure inert

js_opaque_register :: JSText -> IO ()
js_opaque_register a1 = if inBrowser then wait =<< js_opaque_registerPromise a1 else pure inert

js_identity_export :: JSText -> IO JSText
js_identity_export a1 = if inBrowser then wait =<< js_identity_exportPromise a1 else pure inert

js_exportable :: IO Int
js_exportable = if inBrowser then wait =<< js_exportablePromise else pure inert

-- | 'runPage' for an example of this module, which fails it if it leaves a
-- key in the page that can be exported, counted from its own start: the
-- examples share the page and its registry of keys, so a key one example
-- leaves fails that example alone, and not every example after it. The
-- check is made once the example has ended, its views unmounted, whether
-- or not it checked at a point of its own (as 'closed' does, while its
-- passkey stub is still there for the vault's own count): an example that
-- forgot to check is checked all the same. An example that failed is not
-- checked again; the next counts from where it ended.
runOwn :: (HasCallStack) => (forall s. PageM s a) -> IO a
runOwn test = do
  js_keys_mark
  result <- runPage test `onException` js_keys_checked
  left <- fromJSText <$> js_keys_checked
  unless (T.null left) $
    panic ("the example left keys in the page that can be exported (handle:algorithm:usages): " <> left)
  pure result

-- | Whether the login field's offer of passkeys is open, as a count of
-- the waits the test holds on purpose ('serverStartsWith'): the offer
-- waits in the program until the test picks a passkey or the form ends
-- it, and a read in a 'Quiet' group would otherwise wait for it until the
-- deadline.
offerOpen :: PageM s Int
offerOpen = fromEnum <$> unsafeIOToPageM js_offered

js_assistant_key :: JSText -> Bool -> IO ()
js_assistant_key a1 a2 = if inBrowser then wait =<< js_assistant_keyPromise a1 a2 else pure inert

js_page_key_hold :: JSText -> IO ()
js_page_key_hold a1 = if inBrowser then wait =<< js_page_key_holdPromise a1 else pure inert

type Specs m = (Typeable m, MonadBrowserTest m, MonadBrowserDOM m, MonadIslands m, MonadAssistant m, MonadUUID m, MonadIndexedDB m)

-- The reads look once, with the program quiet: its requests to the test's
-- server, IndexedDB and WebCrypto answered. What the test holds on purpose
-- is counted as held ('offerOpen', and the server's own count).
spec :: forall m -> (Specs m) => Spec
spec m = readsOnce Quiet $ do
  vault m
  signing m
  userKeys m

-- | Signing in with a passkey, and the account's passkeys (WP5.30 of
-- docs/PLAN.md): the button and the login field's offer sign in with the
-- stub's passkey alone; the second factor's form takes one in place of the
-- code; the settings' section "Security" lists, adds and removes them;
-- over plain HTTP there is neither.
signing :: forall m -> (Specs m) => Spec
signing m = describe "signing in with a passkey" $ do
  it "signs in with the button, and with the passkey the login field offers" $ runOwn $ do
    (ui, id') <- signedOut m
    me <- signedInAs "user-editor"
    answerWith "PUT" "api/v1/session/passkey" 200 me
    find ui ".sign-in-toggle" >>= click
    -- The login field offers the passkey as the form opens; the button
    -- ends that offer and opens the dialog.
    offered `shouldReturn` True
    find ui ".passkey-sign-in" >>= click
    find ui ".account .user-name" >>= (`shouldHaveText` "Edgar Editor")
    offered `shouldReturn` False
    sent <- filter (\r -> r.method == "PUT" && r.url == "api/v1/session/passkey") <$> asked
    map (\r -> (field ["id"] =<< decodeStrict (encodeUtf8 r.body), field ["userHandle"] =<< decodeStrict (encodeUtf8 r.body))) sent
      `shouldBe` [(Just (Aeson.String id'), Just (Aeson.String "dXNlci1lZGl0b3I"))]
    unmount ui
    unsafeIOToPageM js_uninstall
    serverStops
    -- Picked in the login field instead.
    (again, _) <- signedOut m
    answerWith "PUT" "api/v1/session/passkey" 200 me
    find again ".sign-in-toggle" >>= click
    offered `shouldReturn` True
    unsafeIOToPageM js_pick
    find again ".account .user-name" >>= (`shouldHaveText` "Edgar Editor")
    closedSignedIn again

  it "says why a passkey did not sign in, and offers it again" $ runOwn $ do
    (ui, _) <- signedOut m
    answerWith "PUT" "api/v1/session/passkey" 400 (Aeson.object ["error" Aeson..= ("Invalid" :: Text), "message" Aeson..= ("This passkey is not one of this wiki's: it may have been removed. Sign in with your password." :: Text)])
    find ui ".sign-in-toggle" >>= click
    find ui ".passkey-sign-in" >>= click
    find ui ".sign-in-form .sign-in-error" >>= textContent >>= (`shouldSatisfy` T.isInfixOf "not one of this wiki's")
    offered `shouldReturn` True
    -- Closing the form ends the offer.
    find ui ".sign-in-toggle" >>= click
    offered `shouldReturn` False
    closedSignedIn ui

  it "starts no offer in the login field whose challenge arrives after the form closed" $ runOwn $ do
    (ui, _) <- signedOut m
    holdAt "POST" "api/v1/session/passkey"
    find ui ".sign-in-toggle" >>= click
    -- Quiet once the server holds the request, which it counts as held.
    waitQuiet
    length . filter (\r -> r.url == "api/v1/session/passkey") <$> asked `shouldReturn` 1
    find ui ".sign-in-toggle" >>= click
    release
    -- The challenge arrived after the form it was for closed. The form
    -- opened again starts its own offer, and only that one.
    find ui ".sign-in-toggle" >>= click
    offered `shouldReturn` True
    unsafeIOToPageM js_offers `shouldReturn` 1
    closedSignedIn ui

  it "opens no passkey dialog whose challenge arrives after the form closed" $ runOwn $ do
    (ui, _) <- signedOut m
    find ui ".sign-in-toggle" >>= click
    offered `shouldReturn` True
    holdAt "POST" "api/v1/session/passkey"
    find ui ".passkey-sign-in" >>= click
    -- Quiet once the server holds the request, which it counts as held.
    waitQuiet
    length . filter (\r -> r.url == "api/v1/session/passkey") <$> asked `shouldReturn` 2
    find ui ".sign-in-toggle" >>= click
    release
    -- The form opened again offers in the field; no dialog was opened, and
    -- nobody signed in.
    find ui ".sign-in-toggle" >>= click
    offered `shouldReturn` True
    touches `shouldReturn` 0
    length . filter (\r -> r.method == "PUT") <$> asked `shouldReturn` 0
    closedSignedIn ui

  it "completes a password's sign-in with a passkey in place of the code" $ runOwn $ do
    (ui, id') <- signedOut m
    me <- signedInAs "user-editor"
    answerWith "POST" "api/v1/session" 401 (Aeson.object ["error" Aeson..= ("SecondFactorRequired" :: Text)])
    answerWith "POST" "api/v1/session/second-factor/passkey" 200 (Aeson.object ["challenge" Aeson..= ("Y2hhbGxlbmdl" :: Text), "rpId" Aeson..= ("localhost" :: Text), "allow" Aeson..= [id']])
    answerWith "PUT" "api/v1/session/second-factor/passkey" 401 (Aeson.object ["error" Aeson..= ("Unauthenticated" :: Text)])
    find ui ".sign-in-toggle" >>= click
    find ui ".sign-in-form input[name=login]" >>= (`typeText` "editor")
    find ui ".sign-in-form input[name=password]" >>= (`typeText` "editor's password")
    find ui ".sign-in-form button[type=submit]" >>= click
    find ui ".sign-in-code-form .passkey-code" >>= click
    find ui ".sign-in-code-form .sign-in-error" >>= textContent >>= (`shouldSatisfy` T.isInfixOf "That passkey was not accepted")
    answerWith "PUT" "api/v1/session/second-factor/passkey" 200 me
    find ui ".sign-in-code-form .passkey-code" >>= click
    find ui ".account .user-name" >>= (`shouldHaveText` "Edgar Editor")
    closedSignedIn ui

  it "lists the account's passkeys, adds one with a name, and removes one" $ runOwn $ do
    serverStartsWith offerOpen
    serveDemo
    me <- signedInAs "user-editor"
    answerWith "GET" "api/v1/session" 200 me
    answerAccount
    unsafeIOToPageM js_vault_server
    unsafeIOToPageM (js_install True True)
    _ <- unsafeIOToPageM js_existing
    ui <- startServer m (UserSettings SecuritySection)
    length <$> findAll ui ".passkey-item" `shouldReturn` 1
    find ui ".passkey-name" >>= (`shouldHaveText` "My laptop")
    find ui ".passkey-add input[name=passkey-name]" >>= (`typeText` "My phone")
    find ui ".passkey-add input[name=passkey-password]" >>= (`typeText` "editor's password")
    find ui ".passkey-add button[type=submit]" >>= click
    length <$> findAll ui ".passkey-item" `shouldReturn` 2
    -- The password went with that one attempt: the field is empty again.
    (find ui ".passkey-add input[name=passkey-password]" >>= (`getProperty` "value")) `shouldReturn` ""
    map (T.strip) <$> (findAll ui ".passkey-name" >>= traverse textContent) `shouldReturn` ["My laptop", "My phone"]
    -- Made without the PRF extension: a passkey to sign in with.
    sent <- filter (\r -> r.method == "PUT" && r.url == "api/v1/account/passkeys/registration") <$> asked
    map (\r -> field ["name"] =<< decodeStrict (encodeUtf8 r.body)) sent `shouldBe` [Just (Aeson.String "My phone")]
    -- Making it asked for the password, since it signs the user in: proved
    -- once at proof/old, as an account from before OPAQUE proves it
    -- ('serveOpaque'), and not sent with the registration.
    begun <- filter (\r -> r.method == "POST" && r.url == "api/v1/account/passkeys/registration") <$> asked
    map (\r -> field ["password"] =<< decodeStrict (encodeUtf8 r.body)) begun `shouldBe` [Just Aeson.Null]
    proved <- filter (\r -> r.method == "PUT" && r.url == "api/v1/account/password/proof/old") <$> asked
    map (\r -> field ["password"] =<< decodeStrict (encodeUtf8 r.body)) proved `shouldBe` [Just (Aeson.String "editor's password")]
    findAll ui ".passkey-remove" >>= \case
      remove : _ -> click remove
      [] -> panic "no Remove"
    length <$> findAll ui ".passkey-item" `shouldReturn` 1
    find ui ".passkey-name" >>= (`shouldHaveText` "My phone")
    closedSignedIn ui

  -- A passkey is made on the authenticator first, then registered with the
  -- server. The settings' section "Security" can be left in between (the
  -- person opens another section or page, or signs out); the registration
  -- then still goes to the server, from the section's finalizer, so that
  -- the passkey the person made is the account's rather than one on their
  -- authenticator that signs nobody in.
  it "registers a passkey made as the security settings are left" $ runOwn $ do
    serverStartsWith offerOpen
    serveDemo
    me <- signedInAs "user-editor"
    answerWith "GET" "api/v1/session" 200 me
    answerAccount
    unsafeIOToPageM js_vault_server
    unsafeIOToPageM (js_install True True)
    _ <- unsafeIOToPageM js_existing
    ui <- startServer m (UserSettings SecuritySection)
    length <$> findAll ui ".passkey-item" `shouldReturn` 1
    find ui ".passkey-add input[name=passkey-name]" >>= (`typeText` "My phone")
    find ui ".passkey-add input[name=passkey-password]" >>= (`typeText` "editor's password")
    let finishes = length . filter (\r -> r.method == "PUT" && r.url == "api/v1/account/passkeys/registration") <$> asked
    holdAt "PUT" "api/v1/account/passkeys/registration"
    find ui ".passkey-add button[type=submit]" >>= click
    -- Quiet once the server holds the request, which it counts as held.
    waitQuiet
    finishes `shouldReturn` 1
    showSection ui ProfileSection
    length <$> findAll ui ".passkey-add" `shouldReturn` 0
    release
    -- The finalizer of the section left sends the registration on the
    -- shell's tree, which is still mounted: 'waitQuiet' counts its wait.
    waitQuiet
    unsafeIOToPageM (fromJSText <$> js_unregistered) `shouldReturn` "[]"
    closedSignedIn ui

  -- The section opened again, and another passkey begun, while the first
  -- one's registration, sent again from the section left, waits: the
  -- server refuses that late answer to a challenge the new one replaced,
  -- and leaves the new one's challenge for its own answer. The new passkey
  -- is the account's; the first stays on the authenticator unknown to the
  -- wiki, as one did before the section sent it again.
  it "registers a passkey begun in the security settings opened again while the registration sent as they were left waits" $ runOwn $ do
    serverStartsWith offerOpen
    serveDemo
    me <- signedInAs "user-editor"
    answerWith "GET" "api/v1/session" 200 me
    answerAccount
    unsafeIOToPageM js_vault_server
    unsafeIOToPageM (js_install True True)
    _ <- unsafeIOToPageM js_existing
    ui <- startServer m (UserSettings SecuritySection)
    let finishes = length . filter (\r -> r.method == "PUT" && r.url == "api/v1/account/passkeys/registration") <$> asked
        addPasskey name = do
          find ui ".passkey-add input[name=passkey-name]" >>= (`typeText` name)
          find ui ".passkey-add input[name=passkey-password]" >>= (`typeText` "editor's password")
          find ui ".passkey-add button[type=submit]" >>= click
    length <$> findAll ui ".passkey-item" `shouldReturn` 1
    holdAt "PUT" "api/v1/account/passkeys/registration"
    addPasskey "My phone"
    -- Quiet once the server holds the request, which it counts as held.
    waitQuiet
    finishes `shouldReturn` 1
    -- Left: its finalizer sends the registration again, held too, in
    -- place of the section's own, which leaving it ended.
    showSection ui ProfileSection
    waitQuiet
    finishes `shouldReturn` 2
    showSection ui SecuritySection
    length <$> findAll ui ".passkey-item" `shouldReturn` 1
    addPasskey "My tablet"
    waitQuiet
    finishes `shouldReturn` 3
    release
    map T.strip <$> (findAll ui ".passkey-name" >>= traverse textContent) `shouldReturn` ["My laptop", "My tablet"]
    closedSignedIn ui

  it "offers no passkey on a page served over plain HTTP, and says why" $ runOwn $ do
    serverStartsWith offerOpen
    serveDemo
    answerWith "GET" "api/v1/session" 200 nobody
    unsafeIOToPageM js_vault_server
    unsafeIOToPageM (js_install True True)
    unsafeIOToPageM js_insecure
    ui <- startServer m Home
    find ui ".sign-in-toggle" >>= click
    _ <- find ui ".sign-in-form input[name=login]"
    length <$> findAll ui ".passkey-sign-in" `shouldReturn` 0
    offered `shouldReturn` False
    unmount ui
    me <- signedInAs "user-editor"
    answerWith "GET" "api/v1/session" 200 me
    answerAccount
    signedIn <- startServer m (UserSettings SecuritySection)
    find signedIn ".passkeys-unavailable" >>= textContent >>= (`shouldSatisfy` T.isInfixOf "served over HTTPS")
    -- No key is made on such a page, and the count of keys left needs a
    -- passkey, which it cannot have: the page is closed without it.
    unmount signedIn
    unsafeIOToPageM js_uninstall
    serverStops
  where
    -- The wiki over the test's server with nobody signed in, the vault's
    -- server keeping one passkey of the editor's, which the stub has too:
    -- the page, and the passkey's id.
    signedOut :: forall m -> (Specs m) => PageM s (Mounted s Shell.Query Void m, Text)
    signedOut m' = do
      serverStartsWith offerOpen
      serveDemo
      answerWith "GET" "api/v1/session" 200 nobody
      answerWith "POST" "api/v1/session/passkey" 200 (Aeson.object ["challenge" Aeson..= ("Y2hhbGxlbmdl" :: Text), "rpId" Aeson..= ("localhost" :: Text), "allow" Aeson..= ([] :: [Text])])
      unsafeIOToPageM js_vault_server
      unsafeIOToPageM (js_install True True)
      id' <- fromJSText <$> unsafeIOToPageM js_existing
      ui <- startServer m' Home
      pure (ui, id')
    touches :: PageM s Int
    touches = waitQuiet *> unsafeIOToPageM js_touches
    -- Whether the login field offers a passkey now.
    offered :: PageM s Bool
    offered = waitQuiet *> unsafeIOToPageM js_offered
    -- What the security settings fetch besides the passkeys.
    answerAccount = do
      answerWith "GET" "api/v1/account/identities" 200 (Aeson.object ["password" Aeson..= True, "identities" Aeson..= ([] :: [Text])])
      answerWith "GET" "api/v1/account/sessions" 200 ([] :: [Text])
      answerWith "GET" "api/v1/account/second-factor" 200 (Aeson.object ["enabled" Aeson..= False, "recoveryCodesLeft" Aeson..= (0 :: Int), "asksPassword" Aeson..= True])
      answerWith "GET" "api/v1/account/email" 200 (Aeson.object ["email" Aeson..= Aeson.Null, "verified" Aeson..= False, "mailing" Aeson..= False, "mailError" Aeson..= Aeson.Null, "asksPassword" Aeson..= True])
    closedSignedIn :: Mounted s Shell.Query Void m -> PageM s ()
    closedSignedIn ui = do
      -- What the sign-in set going (the keys brought up to date, a vault
      -- made) ends first: a step stopped halfway by the unmount would leave
      -- its key in the page for the next test.
      waitQuiet
      unsafeIOToPageM (evaluate =<< js_exportable) `shouldReturn` 0
      unmount ui
      unsafeIOToPageM js_uninstall
      serverStops

vault :: forall m -> (Specs m) => Spec
vault m = do
  -- Syncing is gone (WP5.84): a key is the assistant's secret, in the
  -- vault where the user has one, and in this browser, sealed, where they
  -- have none yet. Unlocking, passkeys and "ask each time" are the
  -- Security section's, tested below and in "Keeping an account safe";
  -- the move of what was synced before is "The move of the assistant's
  -- legacy settings", and the field's unlock the field's own tests.
  it "keeps a key typed in the assistant's field sealed in this browser, where the user has no vault yet, and sends the server nothing of it" $ runOwn $ do
    ui <- opened m True True
    anthropic <- find ui ".assistant-provider-settings[data-provider='anthropic']"
    findIn anthropic ".secret-field-set" >>= (`shouldHaveText` "Set")
    sentKey `shouldReturn` False
    record <- unsafeIOToPageM (evaluate . fromJSText =<< js_record (toJSText "secrets:user-editor"))
    record `shouldSatisfy` (\r -> "sealed" `T.isInfixOf` r && not (secretKey `T.isInfixOf` r))
    closed ui

  it "seals the settings stored as text before the vault, and keeps no key as text" $ runOwn $ do
    unsafeIOToPageM (evaluate =<< js_new_device)
    let old = withModel Anthropic "claude-test" (withKey Anthropic (Just secretKey) defaultSettings)
    unsafeIOToPageM (runTest (IDB.put assistantDatabase "settings" "user-editor" (BSL.toStrict (encode old)) :: m ()))
    settingsHere m `shouldReturn` old
    record <- unsafeIOToPageM (evaluate . fromJSText =<< js_record (toJSText "user-editor"))
    record `shouldSatisfy` (\r -> "\"device\"" `T.isInfixOf` r && not (secretKey `T.isInfixOf` r))
    unsafeIOToPageM (evaluate =<< js_stores_key (toJSText "user-editor")) `shouldReturn` True
    unsafeIOToPageM (evaluate =<< js_new_device)
  where
    secretKey :: Text
    secretKey = "test-key-anthropic-one"
    -- The reads of what the program leaves in the page, in IndexedDB and on
    -- the vault's server wait first for the program to be quiet.
    exportable :: PageM s Int
    exportable = waitQuiet *> unsafeIOToPageM (evaluate =<< js_exportable)
    -- Whether anything sent to the server holds a key in the clear.
    sentKey :: PageM s Bool
    sentKey = any (\r -> any (`T.isInfixOf` r.body) [secretKey, "test-key-openai", "test-key-anthropic-changed"]) <$> asked
    closed :: Mounted s Shell.Query Void m -> PageM s ()
    -- As for the user's keys below: no test leaves a key in the page that
    -- can be exported.
    closed ui = do
      exportable `shouldReturn` 0
      unmount ui
      unsafeIOToPageM js_uninstall
      serverStops

-- | The assistant's settings page over the test's server, signed in as the
-- demo's editor, on a new device, with the vault's server empty and
-- passkeys with the PRF extension or without, whose output comes as they
-- are made or only when used; and a key kept for Anthropic.
opened :: forall m -> (Specs m) => Bool -> Bool -> PageM s (Mounted s Shell.Query Void m)
opened m prf atCreate = do
  serverStartsWith offerOpen
  serveDemo
  unsafeIOToPageM js_scripted
  me <- signedInAs "user-editor"
  answerWith "GET" "api/v1/session" 200 me
  unsafeIOToPageM js_vault_server
  unsafeIOToPageM (js_install prf atCreate)
  unsafeIOToPageM (evaluate =<< js_new_device)
  ui <- startServer m (UserSettings AssistantSection)
  anthropic <- find ui ".assistant-provider-settings[data-provider='anthropic']"
  findIn anthropic ".secret-field-value" >>= (`typeText` "test-key-anthropic-one")
  findIn anthropic ".secret-field-keep" >>= click
  findIn anthropic ".secret-field-set" >>= (`shouldHaveText` "Set")
  pure ui

-- | The editor's settings as this device keeps them now.
settingsHere :: forall m -> (Specs m) => PageM s Settings
settingsHere m = unsafeIOToPageM (either (const defaultSettings) (\k -> k.settings) <$> runTest (readKeys (UserId "user-editor") :: m (Either Text Keys)))

-- | Nobody signed in, in an open wiki.
nobody :: SessionInfo
nobody = SessionInfo {user = Nothing, groups = mempty, grants = Grants {global = [], spaces = [], access = Open}, standing = NotSignedIn, providers = [], exports = [], boxes = [], homePage = defaultHomePage, mathMacros = Nothing, assistantServices = [], whatsNew = Nothing}

-- | One of the demo's users, signed in.
signedInAs :: Text -> PageM s SessionInfo
signedInAs uid = unsafeIOToPageM $ do
  wiki <- newMemWiki ((demoState demoNow) {M.current = Just (UserId uid)})
  me <- runMemWiki wiki session
  pure SessionInfo {user = me.user, groups = me.groups, grants = me.grants, standing = SignedIn, providers = [], exports = [], boxes = [], homePage = defaultHomePage, mathMacros = Nothing, assistantServices = [], whatsNew = Nothing}

-- | The user's keys (WP5.74b1 of docs/PLAN.md, "Wiki.App.Vault"): made at
-- the first sign-in with a password (once its second factor is given, if
-- it has one), the vault key wrapped for the password, the private key kept on the device as a key that cannot be
-- exported and no export key left in the page; unlocked with the password
-- on a second device, the same key pair; carried through a change of
-- password; made anew after a reset of a password that alone unlocked
-- them, after the reset page's warning; and unlocked with a passkey. Over
-- plain HTTP, where the browser has no WebCrypto, the sign-in and a change
-- of password still go through, and make no keys. Over
-- a real OPAQUE exchange, with serenity-kit's server functions playing the
-- server's side (@jsbits\/test-passkeys.js@), so that the export key is
-- the one each sign-in with the same password gives.
userKeys :: forall m -> (Specs m) => Spec
userKeys m = describe "the user's keys" $ do
  it "makes the key pair at the first sign-in with a password, and keeps neither its private key exportable nor the export key in the page" $ runOwn $ do
    ui <- signedOut "editor's password"
    signIn ui "editor's password"
    made <- madeKeys
    -- The vault holds the key pair and the vault key wrapped for the
    -- password, and the directory the public key; nothing in the clear.
    (field ["vault", "password", "key"] made, isJust (field ["publicKey", "publicKey"] made)) `shouldSatisfy` (\(w, k) -> isJust w && k)
    identityExport `shouldReturn` "refused"
    unsafeIOToPageM (fromJSText <$> js_export_key_traces) `shouldReturn` "[]"
    closed ui

  it "makes the key pair once a sign-in with a password is completed by its second factor" $ runOwn $ do
    ui <- signedOut "editor's password"
    unsafeIOToPageM js_opaque_second_factor
    me <- signedInAs "user-editor"
    answerWith "POST" "api/v1/session/second-factor" 200 me
    find ui ".sign-in-toggle" >>= click
    find ui ".sign-in-form input[name=login]" >>= (`typeText` "editor")
    find ui ".sign-in-form input[name=password]" >>= (`typeText` "editor's password")
    find ui ".sign-in-form button[type=submit]" >>= click
    find ui ".sign-in-code-form input[name=code]" >>= (`typeText` "287082")
    find ui ".sign-in-code-form button[type=submit]" >>= click
    find ui ".account .user-name" >>= (`shouldHaveText` "Edgar Editor")
    made <- madeKeys
    isJust (field ["vault", "password", "key"] made) `shouldBe` True
    closed ui

  it "gives a passkey's sign-in no key of a password sign-in left at its second factor" $ runOwn $ do
    ui <- signedOut "editor's password"
    unsafeIOToPageM js_opaque_second_factor
    -- A password sign-in reaches its second factor, and is left there.
    find ui ".sign-in-toggle" >>= click
    find ui ".sign-in-form input[name=login]" >>= (`typeText` "editor")
    find ui ".sign-in-form input[name=password]" >>= (`typeText` "editor's password")
    find ui ".sign-in-form button[type=submit]" >>= click
    _ <- find ui ".sign-in-code-form input[name=code]"
    find ui ".sign-in-toggle" >>= click
    -- A passkey signs in instead: the password's key is not this
    -- sign-in's, so no vault is made with it.
    me <- signedInAs "user-editor"
    answerWith "POST" "api/v1/session/passkey" 200 (Aeson.object ["challenge" Aeson..= ("Y2hhbGxlbmdl" :: Text), "rpId" Aeson..= ("localhost" :: Text), "allow" Aeson..= ([] :: [Text])])
    answerWith "PUT" "api/v1/session/passkey" 200 me
    _ <- unsafeIOToPageM js_existing
    find ui ".sign-in-toggle" >>= click
    find ui ".passkey-sign-in" >>= click
    find ui ".account .user-name" >>= (`shouldHaveText` "Edgar Editor")
    openSettings ui SecuritySection
    _ <- find ui ".keys-none"
    field ["vault"] <$> vaultState `shouldReturn` Just Aeson.Null
    closed ui

  it "keeps the assistant's key, sealed, in the vault the first sign-in made" $ runOwn $ do
    ui <- signedOutAt (UserSettings AssistantSection) "editor's password"
    unsafeIOToPageM js_scripted
    signIn ui "editor's password"
    made <- madeKeys
    anthropic <- find ui ".assistant-provider-settings[data-provider='anthropic']"
    findIn anthropic ".secret-field-value" >>= (`typeText` "test-key-anthropic-in-the-vault")
    findIn anthropic ".secret-field-keep" >>= click
    findIn anthropic ".secret-field-set" >>= (`shouldHaveText` "Set")
    kept <- vaultState
    ( isJust (field ["vault", "items", "plugin-assistant"] kept)
      , isJust (field ["vault", "items", "assistant"] kept)
      , field ["publicKey", "keyId"] kept == field ["publicKey", "keyId"] made
      )
      `shouldBe` (True, False, True)
    any (\r -> "test-key-anthropic-in-the-vault" `T.isInfixOf` r.body) <$> asked `shouldReturn` False
    closed ui

  it "unlocks the key pair on a second device with the password: the same key pair, and nothing stored again" $ runOwn $ do
    ui <- signedOut "editor's password"
    signIn ui "editor's password"
    made <- madeKeys
    unmount ui
    unsafeIOToPageM (evaluate =<< js_new_device)
    identityExport `shouldReturn` "none"
    again <- restarted
    signIn again "editor's password"
    identityExport `shouldReturn` "refused"
    now <- vaultState
    (field ["publicKey", "keyId"] now, field ["revision"] now) `shouldBe` (field ["publicKey", "keyId"] made, field ["revision"] made)
    closed again

  it "keeps the key pair through a change of password: the new password unlocks it on another device" $ runOwn $ do
    ui <- signedOut "editor's password"
    signIn ui "editor's password"
    made <- madeKeys
    openSettings ui SecuritySection
    find ui ".password-change-toggle" >>= click
    find ui ".password-change input[name=current]" >>= (`typeText` "editor's password")
    find ui ".password-change input[name=new]" >>= (`typeText` "a password of the editor's")
    find ui ".password-change input[name=repeat]" >>= (`typeText` "a password of the editor's")
    find ui ".password-change button[type=submit]" >>= click
    find ui ".account-told" >>= textContent >>= (`shouldSatisfy` T.isInfixOf "Your password is changed")
    changed <- vaultState
    -- The new wrap, with the new password, in one request.
    (field ["vault", "password"] changed /= field ["vault", "password"] made, field ["publicKey", "keyId"] changed == field ["publicKey", "keyId"] made)
      `shouldBe` (True, True)
    unmount ui
    unsafeIOToPageM (evaluate =<< js_new_device)
    again <- restarted
    signIn again "a password of the editor's"
    identityExport `shouldReturn` "refused"
    field ["publicKey", "keyId"] <$> vaultState `shouldReturn` field ["publicKey", "keyId"] made
    closed again

  -- The change of password holds the vault key, unwrapped so that it can
  -- be exported, until the server has answered. The section that asks for
  -- the change is unmounted when the reader leaves it, which kills its
  -- workers. The change runs in the shell instead, so it ends: the
  -- request is not aborted, the page lets go of the key, and the server
  -- has the new password with the new wrap.
  it "ends a change of password asked for in the settings left before the server's answer: no key left, and the new password unlocks the keys" $ runOwn $ do
    ui <- signedOut "editor's password"
    signIn ui "editor's password"
    made <- madeKeys
    openSettings ui SecuritySection
    find ui ".password-change-toggle" >>= click
    find ui ".password-change input[name=current]" >>= (`typeText` "editor's password")
    find ui ".password-change input[name=new]" >>= (`typeText` "a password of the editor's")
    find ui ".password-change input[name=repeat]" >>= (`typeText` "a password of the editor's")
    holdAt "PUT" "api/v1/account/password"
    find ui ".password-change button[type=submit]" >>= click
    -- Quiet once the server holds the request, which it counts as held.
    waitQuiet
    showSection ui ProfileSection
    _ <- find ui ".account-avatar"
    release
    waitQuiet
    unsafeIOToPageM (evaluate =<< js_exportable) `shouldReturn` 0
    map (\r -> r.aborted) . filter (\r -> r.method == "PUT" && r.url == "api/v1/account/password") <$> asked `shouldReturn` [False]
    changed <- vaultState
    (field ["vault", "password"] changed /= field ["vault", "password"] made, field ["publicKey", "keyId"] changed == field ["publicKey", "keyId"] made)
      `shouldBe` (True, True)
    unmount ui
    unsafeIOToPageM (evaluate =<< js_new_device)
    again <- restarted
    signIn again "a password of the editor's"
    identityExport `shouldReturn` "refused"
    closed again

  -- The operations of the settings that hold keys while they wait for the
  -- server. Each is begun, the server's answer held, the section left for
  -- another (which unmounts it, and kills its workers), and the answer let
  -- go: no key the operation registered stays in the page ('cutOff').
  -- The mechanism the examples below rest on, in the page's registry
  -- itself (jsbits/vault.js): what an owner of keys holds goes with it,
  -- even a key made after it went, and nothing is kept under it then.
  it "lets an owner's keys go with it, registers no key made after, and keeps none under it" $ runOwn $ do
    found <- unsafeIOToPageM (if inBrowser then fromJSText <$> (wait =<< js_key_ownersPromise) else pure "")
    decodeStrict (encodeUtf8 found)
      `shouldBe` Just
        ( Aeson.object
            [ "heldWhileOpen" Aeson..= True
            , "goneWithOwner" Aeson..= True
            , "lateHandle" Aeson..= (0 :: Int)
            , "lateRegistered" Aeson..= False
            , "keepRefused" Aeson..= True
            , "storedAfter" Aeson..= (0 :: Int)
            , "allGone" Aeson..= True
            , "keptByOtherDrop" Aeson..= True
            , "goneBySignOut" Aeson..= True
            ]
        )

  describe "an operation of the settings left before the server's answer leaves no key in the page" $ do
    it "making the keys with the password" $ runOwn $ do
      ui <- withoutKeys pass
      find ui ".account-keys input[name=keys-password]" >>= (`typeText` "editor's password")
      cutOff ui "PUT" "api/v1/assistant/vault" (find ui ".keys-make" >>= click)
      closed ui

    it "making the keys with a passkey" $ runOwn $ do
      -- A user the second factor asks no password of has none to give:
      -- the keys are made with a passkey.
      ui <- withoutKeys (answerWith "GET" "api/v1/account/second-factor" 200 (Aeson.object ["enabled" Aeson..= False, "recoveryCodesLeft" Aeson..= (0 :: Int), "asksPassword" Aeson..= False]))
      cutOff ui "PUT" "api/v1/assistant/vault" (find ui ".keys-make-passkey" >>= click)
      closed ui

    it "unlocking with the password" $ runOwn $ do
      ui <- signedOut "editor's password"
      signIn ui "editor's password"
      _ <- madeKeys
      -- Another device's page: the vault, and no key here.
      unsafeIOToPageM (evaluate =<< js_new_device)
      openSettings ui SecuritySection
      find ui ".account-keys input[name=keys-password]" >>= (`typeText` "editor's password")
      cutOff ui "PUT" "api/v1/account/password/proof" (find ui ".keys-unlock-password-button" >>= click)
      closed ui

    it "unlocking with a passkey" $ runOwn $ do
      ui <- withPasskeyUnlock
      unsafeIOToPageM (evaluate =<< js_new_device)
      showSection ui ProfileSection
      showSection ui SecuritySection
      cutOff ui "PUT" "api/v1/assistant/passkeys/assertion" (find ui ".keys-unlock-passkey-button" >>= click)
      closed ui

    it "unlocking with a passkey too" $ runOwn $ do
      ui <- signedOut "editor's password"
      signIn ui "editor's password"
      _ <- madeKeys
      openSettings ui SecuritySection
      find ui ".account-keys input[name=keys-password]" >>= (`typeText` "editor's password")
      cutOff ui "PUT" "api/v1/assistant/vault" (find ui ".keys-passkey-add" >>= click)
      closed ui

    it "unlocking with the password too" $ runOwn $ do
      ui <- withPasskeyUnlock
      find ui ".keys-password-remove" >>= click
      length <$> findAll ui ".keys-unlock-password" `shouldReturn` 0
      find ui ".account-keys input[name=keys-password]" >>= (`typeText` "editor's password")
      cutOff ui "PUT" "api/v1/assistant/vault" (find ui ".keys-password-add" >>= click)
      closed ui

    it "adding a passkey to sign in with" $ runOwn $ do
      ui <- signedOut "editor's password"
      signIn ui "editor's password"
      _ <- madeKeys
      openSettings ui SecuritySection
      find ui ".passkey-add input[name=passkey-password]" >>= (`typeText` "editor's password")
      cutOff ui "PUT" "api/v1/account/password/proof" (find ui ".passkey-add button[type=submit]" >>= click)
      closed ui

    it "setting up a second factor" $ runOwn $ do
      ui <- signedOut "editor's password"
      signIn ui "editor's password"
      _ <- madeKeys
      openSettings ui SecuritySection
      find ui ".second-factor-start" >>= click
      find ui ".totp-password-form input[name=password]" >>= (`typeText` "editor's password")
      cutOff ui "PUT" "api/v1/account/password/proof" (find ui ".totp-password-form button[type=submit]" >>= click)
      closed ui

    it "giving an e-mail address" $ runOwn $ do
      ui <- signedOut "editor's password"
      signIn ui "editor's password"
      _ <- madeKeys
      -- A server that sends mail, where an address can be given.
      answerWith "GET" "api/v1/account/email" 200 (Aeson.object ["email" Aeson..= Aeson.Null, "verified" Aeson..= False, "mailing" Aeson..= True, "mailError" Aeson..= Aeson.Null, "asksPassword" Aeson..= True])
      openSettings ui SecuritySection
      find ui ".email-edit" >>= click
      find ui ".email-form input[name=email]" >>= (`typeText` "editor@example.org")
      find ui ".email-form input[name=password]" >>= (`typeText` "editor's password")
      cutOff ui "PUT" "api/v1/account/password/proof" (find ui ".email-form button[type=submit]" >>= click)
      closed ui

    -- Left by signing out: the operation's keys go, and none of the
    -- user's is kept on this device after the sign-out let go of them.
    it "unlocking with a passkey too, left by signing out" $ runOwn $ do
      ui <- signedOut "editor's password"
      signIn ui "editor's password"
      _ <- madeKeys
      openSettings ui SecuritySection
      find ui ".account-keys input[name=keys-password]" >>= (`typeText` "editor's password")
      before <- looseKeys
      holdAt "PUT" "api/v1/assistant/vault"
      find ui ".keys-passkey-add" >>= click
      -- Quiet once the server holds the request, which it counts as held.
      waitQuiet
      answerWith "DELETE" "api/v1/session" 200 nobody
      signOut ui
      _ <- find ui ".sign-in-toggle"
      release
      looseKeys `shouldReturn` before
      stored "vault:user-editor" `shouldReturn` False
      closed ui

    it "making an API key" $ runOwn $ do
      ui <- signedOut "editor's password"
      signIn ui "editor's password"
      _ <- madeKeys
      answerWith "GET" "api/v1/account/api-keys" 200 ([] :: [Text])
      openSettings ui ApiKeysSection
      find ui ".api-key-make input[name=api-key-name]" >>= (`typeText` "laptop")
      find ui ".api-key-make input[name=api-key-password]" >>= (`typeText` "editor's password")
      cutOff ui "PUT" "api/v1/account/password/proof" (find ui ".api-key-make button[type=submit]" >>= click)
      closed ui

  it "makes a new key pair after a reset of the password that alone unlocked the keys, after saying what is lost" $ runOwn $ do
    ui <- signedOut "editor's password"
    signIn ui "editor's password"
    _ <- madeKeys
    before' <- fingerprint ui
    unmount ui
    resetPage <- restartedAt (ResetPassword (ResetToken "t1"))
    find resetPage ".reset-warning" >>= textContent >>= (`shouldSatisfy` \t -> "the writing assistant's API keys" `T.isInfixOf` t && "key pair" `T.isInfixOf` t)
    find resetPage ".reset-form input[name=new]" >>= (`typeText` "a password set again")
    find resetPage ".reset-form input[name=repeat]" >>= (`typeText` "a password set again")
    find resetPage ".reset-form button[type=submit]" >>= click
    _ <- find resetPage ".reset-sign-in"
    field ["vault"] <$> vaultState `shouldReturn` Just Aeson.Null
    unmount resetPage
    again <- restarted
    signIn again "a password set again"
    _ <- madeKeys
    after' <- fingerprint again
    (T.length before', before' /= after') `shouldBe` (79, True)
    closed again

  -- Every way to unlock lost (the password forgotten, no passkey here):
  -- the locked field offers nothing that empties the vault, says what a
  -- reset loses and leads to Security; once a reset of the password made
  -- the vault anew, the field takes a new key.
  it "leads a locked field to Security, and takes a new key once a reset of the password made the vault anew" $ runOwn $ do
    ui <- signedOutAt (UserSettings AssistantSection) "editor's password"
    unsafeIOToPageM js_scripted
    signIn ui "editor's password"
    _ <- madeKeys
    anthropic <- find ui ".assistant-provider-settings[data-provider='anthropic']"
    findIn anthropic ".secret-field-value" >>= (`typeText` "test-key-anthropic-before")
    findIn anthropic ".secret-field-keep" >>= click
    findIn anthropic ".secret-field-set" >>= (`shouldHaveText` "Set")
    unmount ui
    unsafeIOToPageM (evaluate =<< js_new_device)
    me <- signedInAs "user-editor"
    answerWith "GET" "api/v1/session" 200 me
    there <- startServer m (UserSettings AssistantSection)
    locked <- find there ".assistant-provider-settings[data-provider='anthropic']"
    findIn locked ".secret-field-lost" >>= textContent >>= (`shouldSatisfy` T.isInfixOf "everything in the vault is lost")
    findIn locked ".secret-field-security" >>= click
    _ <- find there ".account-keys"
    unmount there
    resetPage <- restartedAt (ResetPassword (ResetToken "t1"))
    find resetPage ".reset-form input[name=new]" >>= (`typeText` "a password set again")
    find resetPage ".reset-form input[name=repeat]" >>= (`typeText` "a password set again")
    find resetPage ".reset-form button[type=submit]" >>= click
    _ <- find resetPage ".reset-sign-in"
    field ["vault"] <$> vaultState `shouldReturn` Just Aeson.Null
    unmount resetPage
    again <- restartedAt (UserSettings AssistantSection)
    signIn again "a password set again"
    _ <- madeKeys
    anew <- find again ".assistant-provider-settings[data-provider='anthropic']"
    findIn anew ".secret-field-state" >>= (`shouldHaveText` "Not set")
    findIn anew ".secret-field-value" >>= (`typeText` "test-key-anthropic-after")
    findIn anew ".secret-field-keep" >>= click
    findIn anew ".secret-field-set" >>= (`shouldHaveText` "Set")
    closed again

  it "offers to unlock the keys with the password after a sign-in, and withdraws the offer at the sign-out" $ runOwn $ do
    ui <- signedOut "editor's password"
    signIn ui "editor's password"
    _ <- madeKeys
    -- The vault comes to open with a passkey alone: one added, the
    -- password's wrap taken away.
    openSettings ui SecuritySection
    find ui ".account-keys input[name=keys-password]" >>= (`typeText` "editor's password")
    find ui ".keys-passkey-add" >>= click
    length <$> findAll ui ".keys-unlock-passkey" `shouldReturn` 1
    find ui ".keys-password-remove" >>= click
    length <$> findAll ui ".keys-unlock-password" `shouldReturn` 0
    leaveSettings ui
    -- Signed out and in again with the password: the offer.
    answerWith "DELETE" "api/v1/session" 200 nobody
    signOut ui
    signIn ui "editor's password"
    _ <- find ui ".keys-offer"
    -- Signed out: the offer was the editor's, and goes.
    signOut ui
    _ <- find ui ".sign-in-toggle"
    length <$> findAll ui ".keys-offer" `shouldReturn` 0
    closed ui

  -- The sign-in's view can go while the keys are brought up to date after
  -- a sign-in with the password: the shell takes it off the page when the
  -- wiki cannot be read again just after (the network gone), and with the
  -- whole page. The key the password gave is then held by no one but the
  -- view's state: the view lets go of it as it closes, and the answer that
  -- comes after offers nothing. Here the offer would be made (the vault
  -- opens with a passkey alone), the vault's answer is held, and the view
  -- goes meanwhile.
  it "lets go of the password's key when the sign-in's view goes while the keys are brought up to date" $ runOwn $ do
    ui <- signedOut "editor's password"
    signIn ui "editor's password"
    _ <- madeKeys
    openSettings ui SecuritySection
    find ui ".account-keys input[name=keys-password]" >>= (`typeText` "editor's password")
    find ui ".keys-passkey-add" >>= click
    length <$> findAll ui ".keys-unlock-passkey" `shouldReturn` 1
    find ui ".keys-password-remove" >>= click
    length <$> findAll ui ".keys-unlock-password" `shouldReturn` 0
    leaveSettings ui
    answerWith "DELETE" "api/v1/session" 200 nobody
    signOut ui
    _ <- find ui ".sign-in-toggle"
    settle
    let passwordKeys = waitQuiet *> unsafeIOToPageM js_password_keys
        vaultReads = length . filter (\r -> r.method == "GET" && r.url == "api/v1/assistant/vault") <$> asked
    before <- passwordKeys
    readsBefore <- vaultReads
    holdAt "GET" "api/v1/assistant/vault"
    signIn ui "editor's password"
    -- Quiet once the server holds the request, which it counts as held.
    waitQuiet
    vaultReads >>= (`shouldSatisfy` (> readsBefore))
    -- The unmount disposes of the tree: the sign-in view's finalizer lets
    -- go of the key synchronously, and the view's fiber waiting for the
    -- held answer is cancelled. A disposed tree is not in the harness's
    -- count, so nothing of it is left to wait for.
    unmount ui
    release
    passwordKeys `shouldReturn` before
    unsafeIOToPageM js_uninstall
    serverStops

  -- #213: signing out lets go of the vault key and the private
  -- key, in the page, in IndexedDB and in the wiki's other tabs. The
  -- assistant's own key stays; one that is the vault key goes. A sign-in
  -- that types no password, as one through a provider, finds the keys
  -- locked, until a passkey or the password unlocks them.
  for_ [False, True] $ \shared ->
    it ("lets go of the vault key and the private key at the sign-out, in every tab, and " <> (if shared then "of the assistant's key that is the vault key" else "keeps the assistant's own key")) $ runOwn $ do
      ui <- signedOut "editor's password"
      signIn ui "editor's password"
      _ <- madeKeys
      stored "vault:user-editor" `shouldReturn` True
      stored "identity:user-editor" `shouldReturn` True
      unsafeIOToPageM (js_assistant_key (toJSText ("user-editor" :: Text)) shared)
      unsafeIOToPageM js_hear_drops
      answerWith "DELETE" "api/v1/session" 200 nobody
      signOut ui
      _ <- find ui ".sign-in-toggle"
      stored "vault:user-editor" `shouldReturn` False
      stored "identity:user-editor" `shouldReturn` False
      stored "user-editor" `shouldReturn` not shared
      -- The test hears the messages as another tab does, each in a task
      -- of its own, outside the program.
      eventually (sort <$> heard `shouldReturn` sort (["vault:user-editor", "identity:user-editor"] <> ["user-editor" | shared]))
      filter (`elem` ["vault:user-editor", "identity:user-editor"]) <$> pageKeySlots `shouldReturn` []
      -- What another tab is told, this one does too: its copy goes.
      unsafeIOToPageM (js_page_key_hold (toJSText ("identity:user-other" :: Text)))
      unsafeIOToPageM (js_tell_dropped (toJSText ("identity:user-other" :: Text)))
      -- The message from the other tab arrives in a task of its own,
      -- which the program does not know of until then.
      eventually (filter (== "identity:user-other") <$> pageKeySlots `shouldReturn` [])
      unmount ui
      -- Signed in again, with no password typed: the keys are locked.
      me <- signedInAs "user-editor"
      answerWith "GET" "api/v1/session" 200 me
      again <- startServer m (UserSettings SecuritySection)
      find again ".keys-device" >>= textContent >>= (`shouldSatisfy` T.isInfixOf "does not hold your keys")
      closed again

  -- A session can end with no tab open. A page that starts and hears from
  -- the server that nobody is signed in in this browser lets go of every
  -- user's keys this device keeps, as a sign-out would have.
  it "lets go of every user's keys when a page starts with nobody signed in" $ runOwn $ do
    ui <- signedOut "editor's password"
    signIn ui "editor's password"
    _ <- madeKeys
    stored "vault:user-editor" `shouldReturn` True
    stored "identity:user-editor" `shouldReturn` True
    unmount ui
    -- A sign-in completed in another tab before this page looked: it lets
    -- go of nothing.
    unsafeIOToPageM js_reload
    unsafeIOToPageM js_tell_signed_in
    -- The other tab's message arrives in a task of its own.
    eventually (unsafeIOToPageM js_heard_signed_in `shouldReturn` True)
    kept <- restarted
    _ <- find kept ".sign-in-toggle"
    unmount kept
    stored "vault:user-editor" `shouldReturn` True
    -- A page started again, with nobody signed in and no sign-in heard of.
    unsafeIOToPageM js_reload
    again <- restarted
    _ <- find again ".sign-in-toggle"
    stored "vault:user-editor" `shouldReturn` False
    stored "identity:user-editor" `shouldReturn` False
    closed again

  -- The drop of every user's keys reads the store's generation, then
  -- looks the keys up, then deletes them: a key kept in between (a
  -- sign-in completed meanwhile, here or in another tab) stops it, decided
  -- in the same transaction as the deletion.
  it "lets go of no key when one was kept between the drop's look and its deletion" $ runOwn $ do
    ui <- signedOut "editor's password"
    signIn ui "editor's password"
    _ <- madeKeys
    stored "vault:user-editor" `shouldReturn` True
    let doomed = [UserId "vault:user-editor", UserId "identity:user-editor"]
    -- A page started afresh, which heard of no sign-in: only the store's
    -- generation decides.
    unsafeIOToPageM js_reload
    before <- unsafeIOToPageM vaultGenerationIO
    unsafeIOToPageM (runTest (newVaultKey False >>= \k -> void (keepDeviceKey (UserId "user-other") k True) >> forgetKey k :: m ()))
    unsafeIOToPageM (dropSlotsIfIO before doomed) `shouldReturn` False
    stored "vault:user-editor" `shouldReturn` True
    stored "identity:user-editor" `shouldReturn` True
    now <- unsafeIOToPageM vaultGenerationIO
    now `shouldSatisfy` (> before)
    unsafeIOToPageM (dropSlotsIfIO now doomed) `shouldReturn` True
    stored "vault:user-editor" `shouldReturn` False
    stored "identity:user-editor" `shouldReturn` False
    closed ui

  -- The view of a space that asks each time, pressed as a reader does: a
  -- wrong password leaves it shut, with nothing of it decrypted; the right
  -- one opens it; and, once a passkey unlocks the keys, the passkey.
  it "opens a space that asks each time with the password typed in its view, not with a wrong one, and with a passkey" $ runOwn $ do
    ui <- signedOut "editor's password"
    signIn ui "editor's password"
    made <- madeKeys
    let text' path = case field path made of
          Just (Aeson.String t) -> t
          _ -> panic "no public key"
    env <- E.memberSpace "user-editor" (text' ["publicKey", "keyId"]) (text' ["publicKey", "publicKey"])
    void (either (panic . show) pure =<< E.run env (encryptSpace E.secret.id))
    page <- either (panic . show) pure =<< E.run env (createPage E.secret.id Nothing "Behind the password" "Opened with the password.")
    void (either (panic . show) pure =<< E.run env (setEntry E.secret.id ConfirmEach))
    unmount ui
    E.recordDecryptions
    gated <- startServer m (R.Page [E.secretSlug] [page.slug])
    _ <- find gated ".encrypted-confirm-password"
    length <$> findAll gated ".encrypted-confirm-passkey" `shouldReturn` 0
    find gated "input[name=encrypted-password]" >>= (`typeText` "not the password")
    find gated ".encrypted-confirm-password" >>= click
    _ <- find gated ".encrypted-error"
    length <$> findAll gated ".page-view" `shouldReturn` 0
    filter (T.isInfixOf E.secret.id.text) <$> E.decryptedSince `shouldReturn` []
    find gated "input[name=encrypted-password]" >>= (`typeText` "editor's password")
    find gated ".encrypted-confirm-password" >>= click
    find gated ".page-view .page-body" >>= textContent >>= (`shouldSatisfy` T.isInfixOf "Opened with the password.")
    -- A passkey added to the keys: the view offers it, and it opens the
    -- space in another tab.
    openSettings gated SecuritySection
    find gated ".account-keys input[name=keys-password]" >>= (`typeText` "editor's password")
    find gated ".keys-passkey-add" >>= click
    length <$> findAll gated ".keys-unlock-passkey" `shouldReturn` 1
    unmount gated
    -- Both are offered: a passkey that fails here (one not on this
    -- device, its dialog closed) leaves the password.
    failing <- startServer m (R.Page [E.secretSlug] [page.slug])
    _ <- find failing ".encrypted-confirm-passkey"
    _ <- find failing ".encrypted-confirm-password"
    unsafeIOToPageM js_cancel_next
    find failing ".encrypted-confirm-passkey" >>= click
    _ <- find failing ".encrypted-error"
    length <$> findAll failing ".page-view" `shouldReturn` 0
    find failing "input[name=encrypted-password]" >>= (`typeText` "editor's password")
    find failing ".encrypted-confirm-password" >>= click
    find failing ".page-view .page-body" >>= textContent >>= (`shouldSatisfy` T.isInfixOf "Opened with the password.")
    unmount failing
    other <- startServer m (R.Page [E.secretSlug] [page.slug])
    find other ".encrypted-confirm-passkey" >>= click
    find other ".page-view .page-body" >>= textContent >>= (`shouldSatisfy` T.isInfixOf "Opened with the password.")
    closed other

  it "unlocks the key pair with a passkey added to it, on a device where the password was not typed" $ runOwn $ do
    ui <- signedOut "editor's password"
    signIn ui "editor's password"
    made <- madeKeys
    openSettings ui SecuritySection
    find ui ".account-keys input[name=keys-password]" >>= (`typeText` "editor's password")
    find ui ".keys-passkey-add" >>= click
    length <$> findAll ui ".keys-unlock-passkey" `shouldReturn` 1
    unmount ui
    unsafeIOToPageM (evaluate =<< js_new_device)
    me <- signedInAs "user-editor"
    answerWith "GET" "api/v1/session" 200 me
    again <- startServer m (UserSettings SecuritySection)
    find again ".keys-device" >>= textContent >>= (`shouldSatisfy` T.isInfixOf "does not hold your keys")
    find again ".keys-unlock-passkey-button" >>= click
    find again ".keys-device" >>= textContent >>= (`shouldSatisfy` T.isInfixOf "holds your private key")
    identityExport `shouldReturn` "refused"
    field ["publicKey", "keyId"] <$> vaultState `shouldReturn` field ["publicKey", "keyId"] made
    closed again

  it "has the assistant's key on a second device once a passkey added in Security unlocks the vault there" $ runOwn $ do
    ui <- signedOutAt (UserSettings AssistantSection) "editor's password"
    unsafeIOToPageM js_scripted
    signIn ui "editor's password"
    _ <- madeKeys
    anthropic <- find ui ".assistant-provider-settings[data-provider='anthropic']"
    findIn anthropic ".secret-field-value" >>= (`typeText` "test-key-anthropic-first-device")
    findIn anthropic ".secret-field-keep" >>= click
    findIn anthropic ".secret-field-set" >>= (`shouldHaveText` "Set")
    openSettings ui SecuritySection
    find ui ".account-keys input[name=keys-password]" >>= (`typeText` "editor's password")
    find ui ".keys-passkey-add" >>= click
    length <$> findAll ui ".keys-unlock-passkey" `shouldReturn` 1
    unmount ui
    -- A second device: nothing of the first's keys or records.
    unsafeIOToPageM (evaluate =<< js_new_device)
    me <- signedInAs "user-editor"
    answerWith "GET" "api/v1/session" 200 me
    again <- startServer m (UserSettings AssistantSection)
    there <- find again ".assistant-provider-settings[data-provider='anthropic']"
    findIn there ".secret-field-locked" >>= textContent >>= (`shouldSatisfy` T.isInfixOf "locked in this browser")
    length <$> findAllIn there ".secret-field-set" `shouldReturn` 0
    findIn there ".secret-field-unlock-passkey" >>= click
    findIn there ".secret-field-set" >>= (`shouldHaveText` "Set")
    any (\r -> "test-key-anthropic-first-device" `T.isInfixOf` r.body) <$> asked `shouldReturn` False
    closed again

  -- The passkeys of the vault, made and used in Security's "Also unlock
  -- with a passkey" and the assistant's field (ported from the examples of
  -- the assistant's syncing, which made and used them before WP5.84).
  describe "a passkey for the vault" $ do
    it "makes a passkey whose PRF output comes only when it is used, with one more touch" $ runOwn $ do
      ui <- securityAfterSignIn
      unsafeIOToPageM (js_install True False)
      before <- touches
      addPasskey ui
      length <$> findAll ui ".keys-unlock-passkey" `shouldReturn` 1
      touches `shouldReturn` (before + 2)
      closed ui

    it "refuses a passkey without the PRF extension for the vault, says why, and keeps it to sign in with" $ runOwn $ do
      ui <- securityAfterSignIn
      unsafeIOToPageM (js_install False False)
      addPasskey ui
      find ui ".keys-error" >>= textContent >>= (`shouldSatisfy` T.isInfixOf "It was kept to sign in with")
      kept <- vaultState
      (isJust (field ["passkeys", "0", "id"] kept), field ["passkeys", "0", "wrapped", "key"] kept, isJust (field ["passkeys", "1"] kept))
        `shouldBe` (True, Nothing, False)
      length <$> findAll ui ".keys-unlock-passkey" `shouldReturn` 0
      closed ui

    it "makes a new passkey for the vault when the one the user has is not used" $ runOwn $ do
      ui <- securityAfterSignIn
      existing <- fromJSText <$> unsafeIOToPageM js_existing
      -- The dialog for the passkey the user has is closed: a new one is made.
      unsafeIOToPageM js_cancel_next
      addPasskey ui
      length <$> findAll ui ".keys-unlock-passkey" `shouldReturn` 1
      kept <- vaultState
      (field ["passkeys", "0", "id"] kept, isJust (field ["passkeys", "1", "wrapped", "key"] kept), isJust (field ["passkeys", "2"] kept))
        `shouldBe` (Just (Aeson.String existing), True, False)
      closed ui

    it "unlocks the vault with a passkey the user has already, making none" $ runOwn $ do
      ui <- securityAfterSignIn
      existing <- fromJSText <$> unsafeIOToPageM js_existing
      before <- touches
      addPasskey ui
      length <$> findAll ui ".keys-unlock-passkey" `shouldReturn` 1
      -- One touch: the passkey used, none made.
      touches `shouldReturn` (before + 1)
      kept <- vaultState
      (field ["passkeys", "0", "id"] kept, isJust (field ["passkeys", "0", "wrapped", "key"] kept), isJust (field ["passkeys", "1"] kept))
        `shouldBe` (Just (Aeson.String existing), True, False)
      closed ui

    it "offers for unlocking only the passkeys that hold a copy of the vault's key" $ runOwn $ do
      ui <- securityAfterSignIn
      addPasskey ui
      length <$> findAll ui ".keys-unlock-passkey" `shouldReturn` 1
      unmount ui
      -- Another device, and a passkey of the user's that holds no copy,
      -- which the stub would use first if it were offered.
      unsafeIOToPageM (evaluate =<< js_new_device)
      unsafeIOToPageM js_add_unwrapped
      me <- signedInAs "user-editor"
      answerWith "GET" "api/v1/session" 200 me
      again <- startServer m (UserSettings AssistantSection)
      there <- find again ".assistant-provider-settings[data-provider='anthropic']"
      before <- touches
      findIn there ".secret-field-unlock-passkey" >>= click
      _ <- findIn there ".secret-field-value"
      length <$> findAllIn there ".secret-field-locked" `shouldReturn` 0
      touches `shouldReturn` (before + 1)
      closed again

    it "lets go of the vault's key another tab let go of, and shows the vault locked" $ runOwn $ do
      ui <- signedOutAt (UserSettings AssistantSection) "editor's password"
      signIn ui "editor's password"
      _ <- madeKeys
      there <- find ui ".assistant-provider-settings[data-provider='anthropic']"
      -- Unlocked here by the sign-in with the password.
      _ <- findIn there ".secret-field-value"
      unsafeIOToPageM (evaluate =<< js_stores_key (toJSText "vault:user-editor")) `shouldReturn` True
      -- The page asks for the session again once another tab lets go of
      -- the keys: the editor is still signed in.
      me <- signedInAs "user-editor"
      answerWith "GET" "api/v1/session" 200 me
      unsafeIOToPageM (evaluate =<< js_other_tab_drops (toJSText "vault:user-editor"))
      again <- find ui ".assistant-provider-settings[data-provider='anthropic']"
      findIn again ".secret-field-locked" >>= textContent >>= (`shouldSatisfy` T.isInfixOf "locked in this browser")
      unsafeIOToPageM (evaluate =<< js_stores_key (toJSText "vault:user-editor")) `shouldReturn` False
      closed ui

  -- The owner's wiki at work is served over plain HTTP, where the browser
  -- gives no crypto.subtle (WP5.79 of docs/PLAN.md). OPAQUE's client needs
  -- none: the sign-in and the change of password go through, and give no
  -- key. What needs WebCrypto, the user's keys, says why it is not there.
  -- The page's crypto is put back even when the test fails, since the
  -- tests after it share the page.
  it "signs in and changes the password on a page served over plain HTTP, without crypto.subtle, and makes no keys there" $ (`finally` js_plain_http_end) $ runOwn $ do
    -- A device that holds no key of an earlier test's, so that none is
    -- found on it at the end unless this test made one.
    unsafeIOToPageM (evaluate =<< js_new_device)
    unsafeIOToPageM js_plain_http
    ui <- signedOut "editor's password"
    signIn ui "editor's password"
    openSettings ui SecuritySection
    find ui ".keys-unavailable" >>= textContent >>= (`shouldSatisfy` T.isInfixOf "plain HTTP")
    find ui ".password-change-toggle" >>= click
    find ui ".password-change input[name=current]" >>= (`typeText` "editor's password")
    find ui ".password-change input[name=new]" >>= (`typeText` "a password of the editor's")
    find ui ".password-change input[name=repeat]" >>= (`typeText` "a password of the editor's")
    find ui ".password-change button[type=submit]" >>= click
    find ui ".account-told" >>= textContent >>= (`shouldSatisfy` T.isInfixOf "Your password is changed")
    leaveSettings ui
    -- Signed out and in again with the new password: the record made
    -- without WebCrypto is the one the server checks.
    answerWith "DELETE" "api/v1/session" 200 nobody
    signOut ui
    signIn ui "a password of the editor's"
    field ["vault"] <$> vaultState `shouldReturn` Just Aeson.Null
    identityExport `shouldReturn` "none"
    -- The count of keys left in the page needs WebCrypto back.
    unsafeIOToPageM js_plain_http_end
    closed ui
  where
    -- The wiki with nobody signed in, the editor's password registered
    -- with the OPAQUE server, and the vault's server empty.
    signedOut :: Text -> PageM s (Mounted s Shell.Query Void m)
    signedOut = signedOutAt Home
    signedOutAt :: Route -> Text -> PageM s (Mounted s Shell.Query Void m)
    signedOutAt at password = do
      serverStartsWith offerOpen
      serveDemo
      answerAccount
      unsafeIOToPageM js_vault_server
      me <- signedInAs "user-editor"
      unsafeIOToPageM (js_opaque_server (toJSText (decodeUtf8 (BSL.toStrict (encode me)))))
      unsafeIOToPageM (evaluate =<< js_opaque_register (toJSText password))
      unsafeIOToPageM (js_install True True)
      restartedAt at
    -- The wiki again, with nobody signed in, over the same servers.
    restarted :: PageM s (Mounted s Shell.Query Void m)
    restarted = restartedAt Home
    restartedAt :: Route -> PageM s (Mounted s Shell.Query Void m)
    restartedAt at = do
      answerWith "GET" "api/v1/session" 200 nobody
      startServer m at
    signIn ui password = do
      find ui ".sign-in-toggle" >>= click
      find ui ".sign-in-form input[name=login]" >>= (`typeText` "editor")
      find ui ".sign-in-form input[name=password]" >>= (`typeText` password)
      find ui ".sign-in-form button[type=submit]" >>= click
      find ui ".account .user-name" >>= (`shouldHaveText` "Edgar Editor")
    -- The vault's server once the key pair is stored.
    madeKeys = do
      (isJust . field ["publicKey", "keyId"]) <$> vaultState `shouldReturn` True
      identityExport `shouldReturn` "refused"
      vaultState
    -- The reads of what the program leaves in the page, in IndexedDB and on
    -- the vault's server wait first for the program to be quiet.
    identityExport = waitQuiet *> unsafeIOToPageM (fromJSText <$> (evaluate =<< js_identity_export (toJSText "user-editor")))
    stored :: Text -> PageM s Bool
    stored slot = waitQuiet *> unsafeIOToPageM (evaluate =<< js_stores_key (toJSText slot))
    heard = unsafeIOToPageM (fromMaybe [] . decodeStrict . encodeUtf8 . fromJSText <$> js_drops_heard) :: PageM s [Text]
    pageKeySlots = unsafeIOToPageM (fromMaybe [] . decodeStrict . encodeUtf8 . fromJSText <$> js_page_key_slots) :: PageM s [Text]
    fingerprint ui = do
      openSettings ui SecuritySection
      printed <- find ui ".keys-fingerprint code" >>= textContent
      leaveSettings ui
      pure printed
    -- Leave the settings for the front page, as the panel they were once
    -- in was closed: their views are unmounted.
    leaveSettings ui = find ui ".wiki-brand" >>= click
    -- An operation begun ('start') with the server's answer to this
    -- request held, the section left for another meanwhile (it is
    -- unmounted, and its workers killed), and the answer let go: the keys
    -- the page holds loose (no slot of the device's tracks them) are as
    -- many as before.
    cutOff :: Mounted s Shell.Query Void m -> Text -> Text -> PageM s () -> PageM s ()
    cutOff ui method url start = do
      before <- looseKeys
      holdAt method url
      start
      -- Quiet once the server holds the request, which it counts as held.
      waitQuiet
      showSection ui ProfileSection
      _ <- find ui ".account-avatar"
      release
      looseKeys `shouldReturn` before
    looseKeys :: PageM s Int
    looseKeys = waitQuiet *> unsafeIOToPageM js_loose_keys
    -- Signed in with the password, with no vault anywhere and no key on
    -- this device: the section "Security" offers to make the keys, once
    -- the server is told what else to answer ('answering').
    withoutKeys :: PageM s () -> PageM s (Mounted s Shell.Query Void m)
    withoutKeys answering = do
      ui <- signedOut "editor's password"
      signIn ui "editor's password"
      _ <- madeKeys
      unsafeIOToPageM js_stash
      unsafeIOToPageM (evaluate =<< js_new_device)
      answering
      openSettings ui SecuritySection
      _ <- find ui ".keys-none"
      pure ui
    -- Signed in with the password, the vault unlocked with a passkey too,
    -- in the section "Security".
    withPasskeyUnlock :: PageM s (Mounted s Shell.Query Void m)
    withPasskeyUnlock = do
      ui <- signedOut "editor's password"
      signIn ui "editor's password"
      _ <- madeKeys
      openSettings ui SecuritySection
      find ui ".account-keys input[name=keys-password]" >>= (`typeText` "editor's password")
      find ui ".keys-passkey-add" >>= click
      length <$> findAll ui ".keys-unlock-passkey" `shouldReturn` 1
      pure ui
    -- Every test ends with no key of its own in the page that can be
    -- exported ('runOwn' counts from the test's start). A step still
    -- running (a vault being made after a sign-in) is waited for: stopped
    -- halfway by the unmount, it would leave its key behind.
    closed ui = do
      waitQuiet
      unsafeIOToPageM (evaluate =<< js_exportable) `shouldReturn` 0
      unmount ui
      unsafeIOToPageM js_uninstall
      serverStops
    vaultState :: PageM s Value
    vaultState = waitQuiet *> unsafeIOToPageM (fromMaybe Aeson.Null . decodeStrict . encodeUtf8 . fromJSText <$> js_vault_state)
    -- The stub's touches of a passkey so far.
    touches :: PageM s Int
    touches = waitQuiet *> unsafeIOToPageM js_touches
    -- Signed in with the password, which makes the vault, at Security.
    securityAfterSignIn :: PageM s (Mounted s Shell.Query Void m)
    securityAfterSignIn = do
      ui <- signedOut "editor's password"
      signIn ui "editor's password"
      _ <- madeKeys
      openSettings ui SecuritySection
      pure ui
    -- "Also unlock with a passkey", with the password.
    addPasskey :: Mounted s Shell.Query Void m -> PageM s ()
    addPasskey ui = do
      find ui ".account-keys input[name=keys-password]" >>= (`typeText` "editor's password")
      find ui ".keys-passkey-add" >>= click
    answerAccount = do
      answerWith "GET" "api/v1/account/identities" 200 (Aeson.object ["password" Aeson..= True, "identities" Aeson..= ([] :: [Text])])
      answerWith "GET" "api/v1/account/sessions" 200 ([] :: [Text])
      answerWith "GET" "api/v1/account/second-factor" 200 (Aeson.object ["enabled" Aeson..= False, "recoveryCodesLeft" Aeson..= (0 :: Int), "asksPassword" Aeson..= True])
      answerWith "GET" "api/v1/account/email" 200 (Aeson.object ["email" Aeson..= Aeson.Null, "verified" Aeson..= False, "mailing" Aeson..= False, "mailError" Aeson..= Aeson.Null, "asksPassword" Aeson..= True])

-- | A field of JSON, by its path of keys (an array's items by number).
field :: [Text] -> Value -> Maybe Value
field path value = foldM step value path
  where
    step (Aeson.Object o) k = KeyMap.lookup (Key.fromText k) o
    step (Aeson.Array a) k = readMaybe k >>= \i -> toList a `atMay` i
    step _ _ = Nothing
