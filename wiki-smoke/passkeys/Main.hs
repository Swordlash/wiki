-- | The user's vault and the writing assistant's keys in it, end to end
-- (WP5.21, WP5.74b1 and WP5.84 of docs/PLAN.md): the vault the first
-- sign-in with a password makes, a passkey that unlocks it, added under
-- Security, and the assistant's key kept as the plugin's secret, which the
-- assistant's field unlocks with the passkey: the server's build of the wiki, served by a
-- real wiki-server, in headless Chromium with a virtual authenticator
-- (Chromium's own WebAuthn, through DevTools' WebAuthn domain), which makes
-- and uses real passkeys with the PRF extension. So the browser's
-- @navigator.credentials@, its PRF output, WebCrypto and the server's
-- checks of real attestation objects and ES256 signatures
-- ("Wiki.Server.WebAuthn") all take part, which the browser tests' stub of
-- @navigator.credentials@ leaves out. What a person sees in the browser's
-- own passkey dialog is not tested here: the virtual authenticator answers
-- in its place.
--
-- > sh toolchain/smoke-suite.sh passkeys    # after npm run build-wasm; part of npm run smoke-test
--
-- WebAuthn takes no IP address as the relying party, so the wiki is
-- reached at localhost, which is a secure context over plain HTTP.
--
-- One walk on one page ('onWalkPage'): each example goes on from the one
-- before, and once one has failed the ones after it are pending. Every
-- action of the driver waits until the page's program is idle; after one
-- that asks the server or the authenticator, 'waitQuiet' waits until the
-- program has nothing left waiting, and the page is read once. Without
-- the builds the suite is one pending example ("Wiki.Smoke.Server").
module Main (main) where

import Control.Monad (when)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Halogen.Playwright
import Halogen.Playwright.Hspec
import System.FilePath ((</>))
import Wiki.Smoke.Api
import Wiki.Smoke.Browser
import Wiki.Smoke.Demo (nothingWrong)
import Wiki.Smoke.Server
import Wiki.Smoke.Walk

-- | Ada's password: she is the administrator the console makes.
password :: Text
password = "the passkeys test's password"

main :: IO ()
main = withServerSuite "passkeys, against a real wiki-server" $ \built run ->
  withServerBuild built "wiki-passkeys-" $ \build -> do
    let dataDir = build.work </> "data"
    addUser built SQLite dataDir NewUser {login = "ada", displayName = "Ada", password, admin = True}
    withServer build (serverOptions dataDir SQLite) {adminListener = False} $ \server -> do
      -- localhost, not 127.0.0.1: WebAuthn takes no IP address.
      let origin = Text.replace "://127.0.0.1:" "://localhost:" server.origin
      wiki <- newWiki origin
      chain <- newChain
      session <- newIORef Nothing
      authenticator <- newIORef Nothing
      run . playwright $ spec Passkeys {origin, wiki, chain, session, authenticator}

-- | What the walk's examples share.
data Passkeys = Passkeys
  { origin :: Text
  , wiki :: Wiki
  , chain :: Chain
  , session :: IORef (Maybe Session)
  -- ^ Ada's, once she has signed in.
  , authenticator :: IORef (Maybe Text)
  -- ^ The first virtual authenticator's id.
  }

spec :: Passkeys -> SpecWith Playwright
spec p = describe "the vault, its passkeys and the assistant's keys in it" $ onWalkPage serverContext setup $ do
  let step' = step p.chain

  -- Ada's session, signed in through the header's form in a browser of
  -- its own; the first sign-in with a password makes her vault
  -- (WP5.74b1): her key pair, the vault's key wrapped for her password,
  -- and her public key. The sign-in's wait ends when that is done.
  step' "makes the key pair at the first sign-in with a password, wrapped for the password, and the public key" $ \w -> do
    (outcome, signedIn) <- signInThroughTheForm w.browser p.origin "ada" password
    outcome `shouldBe` SignedIn
    s <- maybe (expectationFailure "no session cookie" >> fail "no session") pure signedIn
    writeIORef p.session (Just s)
    first' <- api p.wiki (Just s) "GET" "assistant/vault" Nothing [] >>= expectStatus 200 "the vault" >>= apiJson
    expect (not (isNull (first' .! "vault" .! "items" .! "identity")) && not (isNull (first' .! "vault" .! "wraps" .! "password"))) "the first sign-in made no vault with the key pair and the password's wrap"
    me <- api p.wiki (Just s) "GET" "session" Nothing [] >>= apiJson >>= \v -> text (v .! "user" .! "id")
    own <- api p.wiki (Just s) "GET" ("account/public-keys/" <> escape me) Nothing [] >>= apiJson
    own .! "keyId" `shouldBe` first' .! "vault" .! "items" .! "identity" .! "keyId"
    addCookies (pageContext w.page) [sessionCookie p.origin s]

  step' "keeps the assistant's key sealed in the vault, and unlocks the vault with a real passkey with the PRF extension, under Security" $ \w -> do
    let page = w.page
    _ :: Value <- cdp page "WebAuthn.enable" (object [])
    added :: Value <- cdp page "WebAuthn.addVirtualAuthenticator" (object ["options" .= authenticatorOptions "internal"])
    text (added .! "authenticatorId") >>= writeIORef p.authenticator . Just
    visit page (p.origin <> "/settings/assistant/")
    let anthropic = locator page ".assistant-provider-settings[data-provider='anthropic']"
    -- The host's field of the key. Ada signed in in a browser of her own,
    -- so this one does not hold her vault's key yet: the field unlocks the
    -- vault with her password first.
    waitQuiet page
    locked <- count (locator anthropic ".secret-field-locked")
    when (locked > 0) $ do
      fill (locator anthropic ".secret-field-password") password
      click (locator anthropic ".secret-field-unlock-password")
      waitQuiet page
      noProblem page ".secret-field-problem" "unlocking with the password"
    fill (locator anthropic ".secret-field-value") "a-test-key-for-anthropic"
    click (locator anthropic ".secret-field-keep")
    -- Keeping a key fetches the provider's models, from the scripted
    -- provider here, and stores the vault.
    waitQuiet page
    noProblem page ".secret-field-problem" "keeping the key"
    count (locator anthropic ".secret-field-set") `shouldReturn` 1
    kept <- vaultNow p
    expect (not (isNull (kept .! "vault" .! "items" .! "plugin-assistant")) && not (isNull (kept .! "vault" .! "items" .! "identity"))) "the vault does not hold the assistant's secrets and the key pair"
    raw <- readIORef p.session >>= \s -> api p.wiki s "GET" "assistant/vault" Nothing []
    expect (not ("a-test-key-for-anthropic" `Text.isInfixOf` bodyText raw)) "the server holds the key in the clear"
    -- A passkey too, under Security: making it asks for the password (a
    -- passkey signs Ada in), which unwraps the vault's key for the
    -- passkey to wrap.
    visit page (p.origin <> "/settings/security/")
    fill (locator page "input[name='keys-password']") password
    click (locator page ".keys-passkey-add")
    waitQuiet page
    noProblem page ".keys-error" "adding a passkey to the vault"
    vault <- vaultNow p
    length (passkeyWraps vault) `shouldBe` 1
    expect (not (isNull (vault .! "vault" .! "wraps" .! "password"))) "the password's wrap is gone"
    nothingWrong page

  -- Asked each time: after a reload the vault is locked, and the
  -- assistant's field unlocks it with one use of the passkey, whose
  -- assertion the server checks.
  step' "asked each time, locks the vault after a reload, and the assistant's field unlocks it with the passkey, checked by the server" $ \w -> do
    let page = w.page
        anthropic = locator page ".assistant-provider-settings[data-provider='anthropic']"
    click (locator page ".keys-ask-each-time")
    waitQuiet page
    isChecked (locator page ".keys-ask-each-time") `shouldReturn` True
    visit page (p.origin <> "/settings/assistant/")
    -- The secrets are read from the server's vault and this browser.
    waitQuiet page
    count (locator anthropic ".secret-field-locked") `shouldReturn` 1
    click (locator anthropic ".secret-field-unlock-passkey")
    waitQuiet page
    noProblem page ".secret-field-problem" "unlocking with the passkey"
    count (locator anthropic ".secret-field-set") `shouldReturn` 1
    passkeys <- passkeysNow p
    case passkeys of
      [used] -> expect (not (isNull (used .! "lastUsedAt"))) "the server checked no use of the passkey"
      other -> expectationFailure ("the passkeys: " <> show other)
    nothingWrong page

  -- Signed out, the passkey left on the authenticator signs Ada in alone:
  -- a discoverable credential, whose user handle names her, and no code.
  step' "signs its owner in from the sign-in form, signed out, with a real passkey and no password" $ \w -> do
    let page = w.page
    clearCookies (pageContext page)
    visit page (p.origin <> "/")
    click (locator page ".sign-in-toggle")
    click (locator page ".passkey-sign-in")
    waitQuiet page
    Text.strip <$> textContent (locator page ".account .user-name") `shouldReturn` "Ada"
    me :: Value <- evaluate page "(async () => (await fetch('/api/v1/session', { headers: { 'X-Wiki-Request': '1' } })).json())()"
    me .! "user" .! "login" `shouldBe` String "ada"
    nothingWrong page
  where
    -- The scripted provider answers for every provider: keeping a key
    -- fetches the provider's models, and this test reaches no provider.
    setup w = addInitScript (pageContext w.page) "globalThis.wikiAssistantScript = { turns: [] };"

-- | A virtual authenticator with resident keys, user verification and the
-- PRF extension, on a transport: @internal@ (the device's own) or @usb@ (a
-- security key).
authenticatorOptions :: Text -> Value
authenticatorOptions transport =
  object
    [ "protocol" .= ("ctap2" :: Text)
    , "transport" .= transport
    , "hasResidentKey" .= True
    , "hasUserVerification" .= True
    , "isUserVerified" .= True
    , "hasPrf" .= True
    , "automaticPresenceSimulation" .= True
    ]

-- | Fail with what the page says, if it shows a problem in the elements
-- this selects.
noProblem :: Page -> Text -> String -> IO ()
noProblem page selector what = do
  problems <- count (locator page selector)
  when (problems > 0) $ do
    said <- textContent (locator page selector)
    expectationFailure (what <> ": " <> Text.unpack said)

-- | Ada's vault, as the server has it.
vaultNow :: Passkeys -> IO Value
vaultNow p = readIORef p.session >>= \s -> api p.wiki s "GET" "assistant/vault" Nothing [] >>= expectStatus 200 "the vault" >>= apiJson

-- | Ada's passkeys, as the server lists them.
passkeysNow :: Passkeys -> IO [Value]
passkeysNow p = readIORef p.session >>= \s -> api p.wiki s "GET" "account/passkeys" Nothing [] >>= expectStatus 200 "the passkeys" >>= apiJson >>= list

-- | The ways the vault's key is wrapped for a passkey: @passkey:@ and the
-- passkey's id.
passkeyWraps :: Value -> [Text]
passkeyWraps v = case v .! "vault" .! "wraps" of
  Object o -> filter ("passkey:" `Text.isPrefixOf`) (map Key.toText (KeyMap.keys o))
  _ -> []
