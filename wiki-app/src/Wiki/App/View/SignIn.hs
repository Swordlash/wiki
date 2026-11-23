{-# LANGUAGE QuasiQuotes #-}

-- | The sign-in control in the header of a wiki on a server: who is
-- signed in, and the way to sign in and out ("Wiki.App.Account"). Someone
-- not signed in sees "Sign in", which opens a small form under it (login,
-- password, and why the last attempt failed). Someone signed in sees their
-- name, a button that opens a short menu: "Settings", which leads to their
-- settings ('Wiki.App.Route.UserSettings'), and "Sign out". The server's
-- operator, who is the administrator without signing in, sees only the
-- name.
--
-- In an open wiki ("Wiki.Core.Access") there is "Create account" beside
-- "Sign in", which opens its own form (login, display name, password) and
-- signs the new user in as the account it creates (WP3.5 of docs/PLAN.md).
-- A closed wiki accepts accounts by invitation only, and has no such
-- button.
--
-- Where the server offers sign-in with providers (OpenID Connect, WP3.6 of
-- docs/PLAN.md: Google, GitHub and the others the session names), both
-- forms offer them too, as links. Signing in with one is a round trip
-- through the provider's pages, driven by the server ("Wiki.Api.Auth"),
-- which ends back at this page's address, signed in.
--
-- A user with a second factor (WP4.11 of docs/PLAN.md) is asked for its
-- code after their password, in the same form, which then has only the
-- code's field; a sign-in left waiting too long starts over at the
-- password. Under the sign-in form, "Forgot your password?" opens a form
-- that requests a reset by mail with a login or an e-mail address; the
-- answer is the same whether or not that matches anyone.
--
-- The menu closes on a press outside it and on Escape
-- ("Wiki.App.View.Popup"), as the header's other menus do. Until issue 340
-- the name opened a long panel instead: how the user signs in, the
-- account's security, the avatar, the personal space, the notifications
-- and a link to the writing assistant's settings. Those are the sections
-- of the settings page now ("Wiki.App.Shell"; "Wiki.App.View.SignInMethods"
-- holds how the user signs in). The header shows the user's avatar beside
-- their name.
--
-- Signing in or out tells the shell the new session ('AccountChanged'),
-- which reloads everything as that user. The shell can also open the form
-- with a message of its own ('Prompt'), when a session has ended by
-- itself.
--
-- Where the browser offers passkeys (a secure page), the sign-in form
-- has "Sign in with a passkey" (WP5.30 of docs/PLAN.md), which opens the
-- browser's dialog of the passkeys it has for the wiki, and signs in with
-- the one picked, without a second factor. Where the browser can, it also
-- offers those passkeys in the login field as it is focused (conditional
-- mediation): the form asks for that as it opens ('Offer'), and ends it
-- when it closes or the password is sent. The second factor's form has
-- "Use a passkey instead", which asks for one of the user's passkeys in
-- place of the code.
--
-- The second factor's form and the reset form each sit in their own
-- container, which is always there, so that neither takes the place of
-- the sign-in form. Otherwise the sign-in form's login field (with what
-- was typed in it) would become their first field.
--
-- The inputs are uncontrolled (the component never writes their value),
-- so that a render arriving while someone types never loses a key; the
-- state records what is typed as it is typed. Messages in the form (the
-- shell's message, why an attempt failed) appear and disappear inside
-- containers that are always there, so that the inputs keep their
-- positions among the form's children and are never recreated, which
-- would lose what was typed in them.
module Wiki.App.View.SignIn
  ( Input (..)
  , Query (..)
  , Output (..)
  , component
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.UUID (MonadUUID)
import Data.Row (Empty)
import Halogen qualified as H
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Router (Follow, Router)
import Halogen.Router qualified as Router
import Halogen.TH (hh)
import Halogen.VDom.DOM.Monad.Class (MonadBrowserDOM)
import Protolude hiding (State)
import Wiki.Api.Account (NewAccount (..))
import Wiki.Api.Auth (Purpose (..), startUrl)
import Wiki.Api.Security (resetLifetime)
import Wiki.App.Account (Accounts (..), SignInError (..), accountErrorText, codeErrorText, passkeyErrorText, signInErrorText)
import Wiki.App.Assistant (KeyRef, MonadAssistant (..), PasskeyRefusal (..))
import Wiki.App.Assistant.Sync (MonadVault (takePasswordKey))
import Wiki.App.Backend (Session (..), WikiError (..))
import Wiki.App.Editor.Browser (focusElement)
import Wiki.App.Route (Route (UserSettings), SettingsSection (..))
import Wiki.App.Vault (Settled (..), addPasswordUnlock, afterSignIn)
import Wiki.App.View.Common (AvatarSize (..), ariaBool, avatar)
import Wiki.App.View.Popup (watchPopup)
import Wiki.Core.Access (takesNewAccounts)
import Wiki.Core.Id (UserId)
import Wiki.Core.Permission (Grants (..))
import Wiki.Core.User (Password (..), SignInProvider (..), Standing (..), User (..), minimumPasswordLength)

data Input m = Input
  { accounts :: Accounts m
  , session :: Session
  , avatar :: Maybe Text
  -- ^ The URL of the signed-in user's avatar, if they have one, which the
  -- header shows beside their name.
  , here :: Text
  -- ^ The page's address, as the router writes it: where signing in with
  -- a provider returns to.
  , router :: Router Route
  -- ^ For the menu's link to the settings.
  , changingPassword :: Bool
  -- ^ Whether the shell runs a change of the user's password. Signing
  -- out waits for it to end: a change still under way after a sign-out
  -- would go on in the session that comes after.
  }

-- | Open the form, with the reason.
data Query a = Prompt Text a

-- | What the sign-in control tells the shell.
data Output
  = -- | Someone signed in or out: the new session.
    AccountChanged Session
  | -- | The user asked to sign out, before the request is sent: what is
    -- shown of encrypted spaces goes at once, whatever its answer.
    SigningOut

-- | Which step of signing in the form is at.
data Step
  = -- | The login and the password.
    AtPassword
  | -- | The second factor's code, after a correct password.
    AtCode
  | -- | Requesting a password reset by mail.
    AtReset
  deriving stock (Eq)

data State m = State
  { input :: Input m
  , open :: Bool
  , step :: Step
  , typedCode :: Text
  , codeField :: H.RefLabel
  , resetField :: H.RefLabel
  , resetTold :: Maybe Text
  -- ^ Whether a reset was requested.
  , typedLogin :: Text
  , typedPassword :: Text
  , notice :: Maybe Text
  -- ^ Why the form was opened, if the shell gave a reason.
  , problem :: Maybe Text
  -- ^ Why the last attempt failed.
  , busy :: Bool
  , passkeys :: Bool
  -- ^ Whether the browser offers passkeys here, known once the form has
  -- opened.
  , loginField :: H.RefLabel
  , registering :: Bool
  -- ^ Whether the form to create an account is open (and the sign-in form
  -- closed: only one at a time).
  , typedName :: Text
  , registerField :: H.RefLabel
  , menuOpen :: Bool
  -- ^ Whether the menu under the signed-in user's name is open.
  , menuHolder :: H.RefLabel
  -- ^ The element that holds the name and the menu: a press outside it
  -- closes the menu.
  , menuWatch :: Maybe H.SubscriptionId
  -- ^ The watch for a press outside the menu or Escape, while it is open.
  , keysOffer :: Maybe (UserId, KeyRef)
  -- ^ The offer, after a sign-in with a password, to unlock the user's
  -- vault with the password too ("Wiki.App.Vault"), with the key the
  -- password gave, which the page forgets when it is answered.
  , keysProblem :: Maybe Text
  -- ^ Why the offer could not be taken.
  , keysInUse :: [KeyRef]
  -- ^ The keys a password gave that a step waiting on the vault holds
  -- ('settleAfter', 'AcceptKeysOffer'): each named here in the step that
  -- takes it, and let go of in the step that takes the answer, or when
  -- the view closes before it ('Finalize'). The view's program is
  -- cancelled when it closes, and a key only that program knew would
  -- stay in the page's registry until the tab closed.
  }

data Action
  = Receive Session (Maybe Text) Text Bool
  | Toggle
  | Close
  | TypedLogin Text
  | TypedPassword Text
  | Submit
  | TypedCode Text
  | SubmitCode
  | Offer
  | PasskeySignIn
  | PasskeyCode
  | ForgotPassword
  | SubmitReset
  | BackToPassword
  | SignOut
  | ToggleRegister
  | TypedName Text
  | Register
  | ToggleMenu
  | CloseMenu
  | Followed (Follow Route)
  | AcceptKeysOffer
  | DeclineKeysOffer
  | Finalize

component :: forall m. (Typeable m, MonadUUID m, MonadUnliftIO m, MonadBrowserDOM m, MonadAssistant m, MonadVault m) => H.Component Query (Input m) Output m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = \input -> do
          loginField <- H.newRefLabel "sign-in-login"
          registerField <- H.newRefLabel "register-login"
          menuHolder <- H.newRefLabel "account-menu"
          codeField <- H.newRefLabel "sign-in-code"
          resetField <- H.newRefLabel "reset-login"
          pure
            State
              { input
              , open = False
              , step = AtPassword
              , typedCode = ""
              , codeField
              , resetField
              , resetTold = Nothing
              , typedLogin = ""
              , typedPassword = ""
              , notice = Nothing
              , problem = Nothing
              , busy = False
              , passkeys = False
              , loginField
              , registering = False
              , typedName = ""
              , registerField
              , menuOpen = False
              , menuHolder
              , menuWatch = Nothing
              , keysOffer = Nothing
              , keysProblem = Nothing
              , keysInUse = []
              }
      , render
      , eval =
          H.mkEval
            H.defaultEval
              { H.handleAction = handleAction
              , H.handleQuery = handleQuery
              , H.receive = \input -> Just (receive input)
              , H.finalize = Just Finalize
              }
      }
  where
    -- The input changes when the shell's session does; the form stays as
    -- it is.
    receive :: Input m -> Action
    receive input = Receive input.session input.avatar input.here input.changingPassword

    render :: State m -> H.ComponentHTML Action Empty m
    render st = case st.input.session of
      Session {user = Just u, standing = SignedIn} ->
        [hh|
          <div class="account">
            <div class="account-holder" {{ HP.ref st.menuHolder }}>
              <button type="button" class="user-name account-toggle" aria-expanded={{ ariaBool st.menuOpen }} onClick={{ const ToggleMenu }}>{{ avatar SmallAvatar st.input.avatar u.displayName }}{{ u.displayName }}</button>
              {% if st.menuOpen %}
                <ul class="user-menu account-menu">
                  <li>
                    <a class="settings-entry" {{ Router.linkProps st.input.router Followed (UserSettings ProfileSection) }}>Settings</a>
                  </li>
                  <li>
                    <button type="button" class="sign-out" disabled={{ st.busy || st.input.changingPassword }} onClick={{ const SignOut }}>Sign out</button>
                    {% if st.input.changingPassword %}
                      <p class="sign-out-note">Your password is being changed: you can sign out once that is done.</p>
                    {% end %}
                  </li>
                </ul>
              {% end %}
            </div>
            {{ keysNotice st }}
          </div>
        |]
      Session {user = Just u} -> [hh|<span class="user-name">{{ u.displayName }}</span>|]
      Session {user = Nothing} ->
        [hh|
          <div class="account">
            <button type="button" class="sign-in-toggle" aria-expanded={{ ariaBool st.open }} onClick={{ const Toggle }}>Sign in</button>
            {% if takesNewAccounts st.input.session.grants.access %}
              <button type="button" class="register-toggle" aria-expanded={{ ariaBool st.registering }} onClick={{ const ToggleRegister }}>Create account</button>
            {% end %}
            {% if st.registering %}
              {{ registerForm st }}
            {% end %}
            <div class="sign-in-step">
              {% if st.open && st.step == AtCode %}
                {{ codeForm st }}
              {% end %}
            </div>
            <div class="sign-in-step">
              {% if st.open && st.step == AtReset %}
                {{ resetForm st }}
              {% end %}
            </div>
            {% if st.open && st.step == AtPassword %}
              <form class="sign-in-form" aria-label="Sign in" {{ HE.preventingDefault (HE.onSubmit (const Submit)) }}>
                <div class="sign-in-told">
                  {% for n in maybeToList st.notice %}
                    <p class="sign-in-notice" role="status">{{ n }}</p>
                  {% end %}
                </div>
                <label class="field">
                  <span>Login</span>
                  <input type="text" name="login" autocomplete="username webauthn" autocapitalize="none" spellcheck="false" required onValueInput={{ TypedLogin }} {{ HP.ref st.loginField }}>
                </label>
                <label class="field">
                  <span>Password</span>
                  <input type="password" name="password" autocomplete="current-password" required onValueInput={{ TypedPassword }}>
                </label>
                <div class="sign-in-told">
                  {% for p in maybeToList st.problem %}
                    <p class="error sign-in-error" role="alert">{{ p }}</p>
                  {% end %}
                </div>
                <div class="actions">
                  <button type="submit" class="primary" disabled={{ st.busy }}>Sign in</button>
                  <button type="button" onClick={{ const Close }}>Cancel</button>
                </div>
                <button type="button" class="link-button forgot-password" onClick={{ const ForgotPassword }}>Forgot your password?</button>
                {% if st.passkeys %}
                  <button type="button" class="passkey-sign-in" disabled={{ st.busy }} onClick={{ const PasskeySignIn }}>Sign in with a passkey</button>
                {% end %}
                {{ providerLinks st "Sign in with " }}
              </form>
            {% end %}
          </div>
        |]

    -- The second factor's code, once the password was right.
    codeForm :: State m -> H.ComponentHTML Action Empty m
    codeForm st =
      [hh|
        <form class="sign-in-code-form" aria-label="Second factor" {{ HE.preventingDefault (HE.onSubmit (const SubmitCode)) }}>
          <p class="sign-in-notice" role="status">Your account has a second factor: type the code your authenticator application shows, or one of your recovery codes.</p>
          <label class="field">
            <span>Code</span>
            <input type="text" name="code" autocomplete="one-time-code" autocapitalize="none" spellcheck="false" required onValueInput={{ TypedCode }} {{ HP.ref st.codeField }}>
          </label>
          <div class="sign-in-told">
            {% for p in maybeToList st.problem %}
              <p class="error sign-in-error" role="alert">{{ p }}</p>
            {% end %}
          </div>
          <div class="actions">
            <button type="submit" class="primary" disabled={{ st.busy }}>Sign in</button>
            <button type="button" onClick={{ const Close }}>Cancel</button>
          </div>
          {% if st.passkeys %}
            <button type="button" class="link-button passkey-code" disabled={{ st.busy }} onClick={{ const PasskeyCode }}>Use a passkey instead</button>
          {% end %}
        </form>
      |]

    -- A password reset by mail, requested with a login or an address.
    resetForm :: State m -> H.ComponentHTML Action Empty m
    resetForm st =
      [hh|
        <form class="reset-request-form" aria-label="Forgot your password?" {{ HE.preventingDefault (HE.onSubmit (const SubmitReset)) }}>
          <p>Give your login or your e-mail address. If your account has a verified address, a link to choose a new password is mailed there; it works once, for {{ show (round (resetLifetime / 60) :: Int) <> " minutes" :: Text }}.</p>
          <label class="field">
            <span>Login or e-mail address</span>
            <input type="text" name="login" autocomplete="username" autocapitalize="none" spellcheck="false" required onValueInput={{ TypedLogin }} {{ HP.ref st.resetField }}>
          </label>
          <div class="sign-in-told">
            {% for t in maybeToList st.resetTold %}
              <p class="reset-told" role="status">{{ t }}</p>
            {% end %}
            {% for p in maybeToList st.problem %}
              <p class="error reset-request-error" role="alert">{{ p }}</p>
            {% end %}
          </div>
          <div class="actions">
            <button type="submit" class="primary" disabled={{ st.busy }}>Mail me a link</button>
            <button type="button" onClick={{ const BackToPassword }}>Back to sign in</button>
          </div>
        </form>
      |]

    -- Creating one's own account, in an open wiki: like the sign-in form,
    -- with a display name, and a new password.
    registerForm :: State m -> H.ComponentHTML Action Empty m
    registerForm st =
      [hh|
        <form class="register-form" aria-label="Create account" {{ HE.preventingDefault (HE.onSubmit (const Register)) }}>
          <label class="field">
            <span>Login</span>
            <input type="text" name="login" autocomplete="username" autocapitalize="none" spellcheck="false" required onValueInput={{ TypedLogin }} {{ HP.ref st.registerField }}>
          </label>
          <label class="field">
            <span>Display name</span>
            <input type="text" name="name" autocomplete="name" placeholder="Your login if blank" onValueInput={{ TypedName }}>
          </label>
          <label class="field">
            <span>Password</span>
            <input type="password" name="password" autocomplete="new-password" required minlength={{ minimumPasswordLength }} onValueInput={{ TypedPassword }}>
          </label>
          <div class="sign-in-told">
            {% for p in maybeToList st.problem %}
              <p class="error register-error" role="alert">{{ p }}</p>
            {% end %}
          </div>
          <div class="actions">
            <button type="submit" class="primary" disabled={{ st.busy }}>Create account</button>
            <button type="button" onClick={{ const Close }}>Cancel</button>
          </div>
          {{ providerLinks st "Create account with " }}
        </form>
      |]

    -- The providers to sign in with, if the server has any: links to the
    -- server's round trip, which returns here.
    providerLinks :: State m -> Text -> H.ComponentHTML Action Empty m
    providerLinks st verb =
      [hh|
        <div class="sign-in-providers">
          {% for p in st.input.session.providers %}
            <a class="provider-link" href={{ startUrl p.name SignInWith st.input.here }}>{{ verb <> p.label }}</a>
          {% end %}
        </div>
      |]

    handleAction :: Action -> H.HalogenM (State m) Action Empty Output m ()
    handleAction = \case
      Receive session face here changingPassword -> do
        -- The offer to unlock the keys with the password is the signed-in
        -- user's: another session (a sign-out, another user) drops it.
        offered <- gets (\st -> fst <$> st.keysOffer)
        when (isJust offered && offered /= ((\u -> u.id) <$> session.user)) dropKeysOffer
        -- The menu is the signed-in user's too: another session closes it.
        before <- gets (\st -> (\u -> u.id) <$> st.input.session.user)
        shut <- if before /= ((\u -> u.id) <$> session.user) then shutMenu else pure identity
        modify (shut . \st -> st {input = Input {accounts = st.input.accounts, session, avatar = face, here, router = st.input.router, changingPassword}})
      Followed f -> do
        shutMenu >>= modify
        gets (\st -> st.input.router) >>= (`Router.follow` f)
      ToggleMenu ->
        gets (\st -> st.menuOpen) >>= \case
          True -> shutMenu >>= modify
          False -> do
            modify (\st -> st {menuOpen = True})
            watch <- gets (\st -> st.menuHolder) >>= (`watchPopup` CloseMenu)
            modify (\st -> st {menuWatch = watch})
      CloseMenu -> whenM (gets (\st -> st.menuOpen)) (shutMenu >>= modify)
      AcceptKeysOffer -> do
        signedIn <- gets (\st -> (\u -> u.id) <$> st.input.session.user)
        gets (\st -> st.keysOffer)
          >>= traverse_
            ( \(u, key) ->
                if Just u /= signedIn
                  then dropKeysOffer
                  else do
                    modify (\st -> st {keysOffer = Nothing, keysProblem = Nothing, keysInUse = key : st.keysInUse})
                    result <- H.async (addPasswordUnlock u (Just key) Nothing)
                    -- Let go of in any case: 'addPasswordUnlock' does once
                    -- it has the vault, and a refusal before it does not.
                    modify (\st -> st {keysInUse = filter (/= key) st.keysInUse})
                    H.liftEffect (forgetKey key)
                    case result of
                      Left why -> modify (\st -> st {keysProblem = Just why})
                      Right _ -> pass
            )
      DeclineKeysOffer -> dropKeysOffer
      -- The keys the view still names: the offer's, and those of a step
      -- that waits for the vault, whose answer will not be taken now.
      Finalize -> do
        held <- state (\st -> (map snd (toList st.keysOffer) <> st.keysInUse, st {keysOffer = Nothing, keysInUse = []}))
        traverse_ (H.liftEffect . forgetKey) held
      Toggle ->
        gets (\st -> st.open) >>= \case
          True -> close
          False -> openForm Nothing
      Close -> close
      TypedLogin typedLogin -> modify (\st -> st {typedLogin})
      TypedPassword typedPassword -> modify (\st -> st {typedPassword})
      Submit -> do
        st <- get
        unless st.busy $ do
          modify (\s -> s {busy = True, problem = Nothing})
          H.liftEffect abortSignInPasskey
          result <- H.async (st.input.accounts.signIn st.typedLogin (Password st.typedPassword))
          case result of
            Left SecondFactorNeeded -> do
              modify (\s -> s {busy = False, step = AtCode, typedPassword = "", typedCode = "", notice = Nothing, problem = Nothing})
              gets (\s -> s.codeField) >>= H.getRef >>= traverse_ (H.liftEffect . focusElement)
            Left e -> do
              modify (\s -> s {busy = False, problem = Just (signInErrorText e)})
              offer
            Right s -> do
              modify (\x -> x {busy = False, open = False, typedPassword = "", notice = Nothing, problem = Nothing})
              H.raise (AccountChanged s)
              settleAfter s
      Offer -> do
        -- The login field's offer of saved passkeys. It waits until a
        -- passkey is picked there, which signs in as the button does, or
        -- until it is ended ('Cancelled'), which says nothing.
        -- The ticket is taken before the challenge is asked for: closing
        -- the form or using the button meanwhile keeps the offer from
        -- starting when the challenge arrives.
        ticket <- H.liftEffect passkeyTicket
        here <- H.async conditionalHere
        when here $
          gets (\st -> st.input.accounts) >>= \a ->
            H.async a.beginPasskeySignIn >>= \case
              Left _ -> pass
              Right options ->
                H.async (signInPasskey options True ticket) >>= \case
                  Right used -> finishPasskey used
                  Left _ -> pass
      PasskeySignIn -> do
        st <- get
        unless st.busy $ do
          -- The field's offer ends now, not only once the dialog opens.
          -- Closing the form while the challenge is fetched keeps the
          -- dialog from opening ('passkeyTicket').
          H.liftEffect abortSignInPasskey
          ticket <- H.liftEffect passkeyTicket
          modify (\s -> s {busy = True, problem = Nothing})
          H.async st.input.accounts.beginPasskeySignIn >>= \case
            Left e -> modify (\s -> s {busy = False, problem = Just (passkeyErrorText e)})
            Right options ->
              H.async (signInPasskey options False ticket) >>= \case
                Right used -> finishPasskey used
                Left Cancelled -> modify (\s -> s {busy = False}) *> offer
                Left (PasskeyFailed why) -> modify (\s -> s {busy = False, problem = Just why}) *> offer
      PasskeyCode -> do
        st <- get
        unless st.busy $ do
          ticket <- H.liftEffect passkeyTicket
          modify (\s -> s {busy = True, problem = Nothing})
          let refused = \case
                -- Waited too long, or too many wrong answers: back to the
                -- password.
                SignInFailed (Invalid why) -> modify (\s -> s {busy = False, step = AtPassword, typedCode = "", problem = Just why}) *> offer
                WrongCredentials -> modify (\s -> s {busy = False, problem = Just "That passkey was not accepted: use another one, or type a code."})
                e -> modify (\s -> s {busy = False, problem = Just (passkeyErrorText e)})
          H.async st.input.accounts.beginSecondFactorPasskey >>= \case
            Left e -> refused e
            Right options ->
              H.async (signInPasskey options False ticket) >>= \case
                Left Cancelled -> modify (\s -> s {busy = False})
                Left (PasskeyFailed why) -> modify (\s -> s {busy = False, problem = Just why})
                Right used ->
                  H.async (st.input.accounts.finishSecondFactorPasskey used) >>= \case
                    Left e -> refused e
                    Right s -> do
                      modify (\x -> x {busy = False, open = False, step = AtPassword, typedCode = "", notice = Nothing, problem = Nothing})
                      H.raise (AccountChanged s)
                      settleAfter s
      TypedCode typedCode -> modify (\st -> st {typedCode})
      SubmitCode -> do
        st <- get
        unless st.busy $ do
          modify (\s -> s {busy = True, problem = Nothing})
          H.async (st.input.accounts.confirmSecondFactor st.typedCode) >>= \case
            -- Waited too long, or too many wrong codes: back to the
            -- password.
            Left (SignInFailed (Invalid why)) -> modify (\s -> s {busy = False, step = AtPassword, typedCode = "", problem = Just why})
            Left e -> modify (\s -> s {busy = False, problem = Just (codeErrorText e)})
            Right s -> do
              modify (\x -> x {busy = False, open = False, step = AtPassword, typedCode = "", notice = Nothing, problem = Nothing})
              H.raise (AccountChanged s)
              settleAfter s
      ForgotPassword -> do
        modify (\st -> st {step = AtReset, typedLogin = "", typedPassword = "", notice = Nothing, problem = Nothing, resetTold = Nothing})
        gets (\st -> st.resetField) >>= H.getRef >>= traverse_ (H.liftEffect . focusElement)
      BackToPassword -> do
        modify (\st -> st {step = AtPassword, typedLogin = "", problem = Nothing, resetTold = Nothing})
        gets (\st -> st.loginField) >>= H.getRef >>= traverse_ (H.liftEffect . focusElement)
      SubmitReset -> do
        st <- get
        unless st.busy $ do
          modify (\s -> s {busy = True, problem = Nothing, resetTold = Nothing})
          H.async (st.input.accounts.requestPasswordReset st.typedLogin) >>= \case
            Left e -> modify (\s -> s {busy = False, problem = Just (signInErrorText e)})
            Right () ->
              modify $ \s ->
                s
                  { busy = False
                  , resetTold = Just "If that is the login or the verified address of an account, a link to choose a new password has been mailed to its address. Look in your mail, and in its spam folder too."
                  }
      ToggleRegister ->
        gets (\st -> st.registering) >>= \case
          True -> close
          False -> do
            H.liftEffect abortSignInPasskey
            -- The other form's inputs are removed, together with what was
            -- typed in them.
            modify (\st -> st {registering = True, open = False, notice = Nothing, problem = Nothing, typedLogin = "", typedName = "", typedPassword = ""})
            gets (\st -> st.registerField) >>= H.getRef >>= traverse_ (H.liftEffect . focusElement)
      TypedName typedName -> modify (\st -> st {typedName})
      Register -> do
        st <- get
        unless st.busy $ do
          modify (\s -> s {busy = True, problem = Nothing})
          result <- H.async (st.input.accounts.register NewAccount {login = st.typedLogin, displayName = st.typedName, password = Password st.typedPassword, email = Nothing})
          case result of
            Left e -> modify (\s -> s {busy = False, problem = Just (accountErrorText e)})
            Right s -> do
              modify (\x -> x {busy = False, registering = False, typedPassword = "", typedName = "", problem = Nothing})
              H.raise (AccountChanged s)
              settleAfter s
      SignOut -> do
        st <- get
        unless (st.busy || st.input.changingPassword) $ do
          shut <- shutMenu
          modify (shut . \s -> s {busy = True})
          H.raise SigningOut
          result <- H.async st.input.accounts.signOut
          modify (\s -> s {busy = False})
          when (isRight result) dropKeysOffer
          -- A sign-out that failed leaves the session as it was; the next
          -- request shows whether it still is.
          traverse_ (H.raise . AccountChanged) (rightToMaybe result)

    -- After a sign-in: the user's keys brought up to date on this device
    -- with the key the password gave, if any, and the offer to unlock the
    -- vault with the password too, when there is one to make.
    settleAfter :: Session -> H.HalogenM (State m) Action Empty Output m ()
    settleAfter s = do
      -- An earlier sign-in's offer is not this one's, even when bringing
      -- this one's keys up to date fails.
      dropKeysOffer
      for_ s.user $ \u -> do
        -- The key the password gave is taken from the backend and named
        -- in the state in one step ('H.liftEffect': taking it does not
        -- wait), so that the view lets go of it if it closes while the
        -- vault is read.
        key <- H.liftEffect takePasswordKey
        modify (\st -> st {keysInUse = toList key <> st.keysInUse})
        result <- H.async (afterSignIn u.id key)
        -- Nothing waits on the keys: the settings' section "Security"
        -- shows them, and why they could not be brought up to date, when it
        -- is opened.
        -- The answer is taken in one step: the key is no longer the
        -- step's, and the offer it makes replaces the one before, whose
        -- key goes ('afterSignIn' let go of the key it does not offer).
        replaced <- state $ \st ->
          let st' = st {keysInUse = filter (`notElem` toList key) st.keysInUse}
           in case result of
                Left _ -> ([], st')
                Right settled -> (map snd (toList st'.keysOffer), st' {keysOffer = (u.id,) <$> settled.offer, keysProblem = Nothing})
        traverse_ (H.liftEffect . forgetKey) replaced

    -- Withdraw the offer to unlock the keys with the password, and forget
    -- the key it holds: taken out of the state and let go of in one step
    -- ('forgetKey' does not wait). Forgotten through 'H.async' and taken out
    -- a step later, an offer made in between was taken out unforgotten.
    dropKeysOffer :: H.HalogenM (State m) Action Empty Output m ()
    dropKeysOffer = do
      withdrawn <- state (\st -> (st.keysOffer, st {keysOffer = Nothing, keysProblem = Nothing}))
      traverse_ (H.liftEffect . forgetKey . snd) withdrawn

    handleQuery :: Query a -> H.HalogenM (State m) Action Empty Output m (Maybe a)
    handleQuery (Prompt why next) = openForm (Just why) $> Just next

    openForm :: Maybe Text -> H.HalogenM (State m) Action Empty Output m ()
    openForm notice = do
      opened <- gets (\st -> st.open && st.step == AtPassword)
      modify $ \st ->
        if opened
          then st {notice, problem = Nothing}
          else st {open = True, step = AtPassword, registering = False, notice, problem = Nothing, typedLogin = "", typedPassword = "", typedCode = ""}
      gets (\st -> st.loginField) >>= H.getRef >>= traverse_ (H.liftEffect . focusElement)
      unless opened $ do
        passkeys <- H.sync passkeysHere
        modify (\st -> st {passkeys})
        offer

    -- Ask for the login field's offer of passkeys, in a fork of its own,
    -- since it waits for as long as the form is open.
    offer :: H.HalogenM (State m) Action Empty Output m ()
    offer = do
      st <- get
      when (st.passkeys && st.open && st.step == AtPassword) $
        void (H.fork (handleAction Offer))

    -- Sign in with the browser's answer for a passkey, from the button or
    -- the login field's offer.
    finishPasskey used = do
      modify (\s -> s {busy = True, problem = Nothing})
      accounts <- gets (\st -> st.input.accounts)
      H.async (accounts.finishPasskeySignIn used) >>= \case
        Left e -> do
          modify (\s -> s {busy = False, problem = Just (passkeyErrorText e)})
          offer
        Right s -> do
          modify (\x -> x {busy = False, open = False, typedPassword = "", notice = Nothing, problem = Nothing})
          H.raise (AccountChanged s)

    -- The menu closed: its watch ended now, and the change to the state,
    -- for the caller to make together with its own.
    shutMenu :: H.HalogenM (State m) Action Empty Output m (State m -> State m)
    shutMenu = do
      gets (\st -> st.menuWatch) >>= traverse_ H.unsubscribe
      pure (\st -> st {menuOpen = False, menuWatch = Nothing})

    close :: H.HalogenM (State m) Action Empty Output m ()
    close = H.liftEffect abortSignInPasskey *> modify (\st -> st {open = False, step = AtPassword, registering = False, notice = Nothing, problem = Nothing, typedPassword = "", typedCode = "", resetTold = Nothing})

-- | The offer, after a sign-in with a password, to unlock the user's vault
-- with the password too, and why the keys could not be brought up to date,
-- if they could not.
keysNotice :: State m -> H.ComponentHTML Action Empty m
keysNotice st =
  [hh|
    <div class="keys-notice">
    {% if isJust st.keysOffer %}
      <div class="keys-offer" role="dialog" aria-label="Unlock your keys with your password">
        <p>Also unlock your keys with your password? Then signing in with your password opens them on any device, without a passkey; they are then as safe as your password. It asks for one of your passkeys once.</p>
        <div class="actions">
          <button type="button" class="primary keys-offer-accept" onClick={{ const AcceptKeysOffer }}>Also with my password</button>
          <button type="button" class="keys-offer-decline" onClick={{ const DeclineKeysOffer }}>Not now</button>
        </div>
      </div>
    {% end %}
    {% for p in maybeToList st.keysProblem %}
      <p class="error keys-problem" role="alert">{{ p }}</p>
    {% end %}
    </div>
  |]
