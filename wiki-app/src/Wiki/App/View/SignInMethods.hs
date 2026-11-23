{-# LANGUAGE QuasiQuotes #-}

-- | How a signed-in user signs in, on a wiki on a server: the first part
-- of the settings' section "Security" ("Wiki.App.Shell"). Until issue 340
-- it was the top of the panel under the user's name in the header
-- ("Wiki.App.View.SignIn").
--
-- It shows the user's password, if any, with a form to change it (or to
-- set one, for a user who has none and whose providers are gone: WP3.7 of
-- docs/PLAN.md); the form asks for the current password, the new one, and
-- the new one again. A user who has no password and signs in with one of
-- the server's providers has none to set and gets no form (WP5.6,
-- 'passwordOffer'). It also shows the provider accounts they sign in with,
-- each of which they may disconnect unless it is their last way to sign
-- in, and a link to connect each provider they have no account with yet.
-- Connecting is a round trip through the provider's pages, which ends
-- back at the address given ('here').
--
-- The sign-in methods are fetched in the initializer, in the browser only:
-- the shell mounts the settings' sections only there.
--
-- The inputs are uncontrolled, as the sign-in form's are: the state
-- records what is typed as it is typed, and the inputs are removed with
-- the form, together with what was typed in them.
module Wiki.App.View.SignInMethods
  ( Input (..)
  , component
  , passwordOffer
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.UUID (MonadUUID)
import Data.Row (Empty)
import Halogen qualified as H
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.TH (hh)
import Protolude hiding (State)
import Wiki.Api.Account (LinkedIdentity (..), PasswordChange (..), SignInMethods (..), mayDisconnect, providerOnly)
import Wiki.Api.Auth (Purpose (..), startUrl)
import Wiki.App.Account (Accounts (..), passwordErrorText)
import Wiki.App.Assistant (MonadAssistant)
import Wiki.App.Assistant.Sync (MonadVault)
import Wiki.App.Backend (Session (..), WikiError (..))
import Wiki.App.Editor.Browser (focusElement)
import Wiki.App.Vault (changeOwnPassword)
import Wiki.App.View.Common (errorMessage)
import Wiki.Core.User (Email (..), Password (..), SignInProvider (..), minimumPasswordLength)

data Input m = Input
  { accounts :: Accounts m
  , session :: Session
  -- ^ The signed-in user's session: the providers the server signs in
  -- with.
  , here :: Text
  -- ^ The page's address, as the router writes it: where connecting a
  -- provider's account returns to.
  }

data State m = State
  { input :: Input m
  , methods :: Maybe (Either WikiError SignInMethods)
  -- ^ The user's sign-in methods, once fetched ('Nothing' while they are
  -- being fetched).
  , changingPassword :: Bool
  -- ^ Whether the form to change their password is open.
  , typedCurrent :: Text
  , typedNew :: Text
  , typedRepeat :: Text
  , passwordProblem :: Maybe Text
  -- ^ Why the last change of password failed.
  , passwordTold :: Maybe Text
  -- ^ Whether the last change was made.
  , currentField :: H.RefLabel
  , newField :: H.RefLabel
  , problem :: Maybe Text
  -- ^ Why the last disconnection failed.
  , busy :: Bool
  }

data Action m
  = Initialize
  | Receive (Input m)
  | Disconnect Text
  | AskPassword
  | CancelPassword
  | TypedCurrent Text
  | TypedNew Text
  | TypedRepeat Text
  | SavePassword

component :: forall query output m. (Typeable m, MonadUUID m, MonadUnliftIO m, MonadAssistant m, MonadVault m) => H.Component query (Input m) output m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = \input -> do
          currentField <- H.newRefLabel "password-current"
          newField <- H.newRefLabel "password-new"
          pure
            State
              { input
              , methods = Nothing
              , changingPassword = False
              , typedCurrent = ""
              , typedNew = ""
              , typedRepeat = ""
              , passwordProblem = Nothing
              , passwordTold = Nothing
              , currentField
              , newField
              , problem = Nothing
              , busy = False
              }
      , render
      , eval =
          H.mkEval
            H.defaultEval
              { H.handleAction = handleAction
              , H.initialize = Just Initialize
              , H.receive = Just . Receive
              }
      }
  where
    -- The signed-in user's sign-in methods, and the ways to change them.
    render :: State m -> H.ComponentHTML (Action m) Empty m
    render st =
      [hh|
        <section class="account-methods-section" aria-label="How you sign in">
          <h2>How you sign in</h2>
          {{ body }}
        </section>
      |]
      where
        providers = st.input.session.providers
        labelOf name = maybe name (\p -> p.label) (find (\p -> p.name == name) providers)
        body = case st.methods of
          Nothing -> [hh|<p class="loading">Loading…</p>|]
          Just (Left e) -> [hh|<p class="error" role="alert">{{ errorMessage e }}</p>|]
          Just (Right m) ->
            [hh|
              <div class="account-methods">
                <p class="account-password">{{ if m.password then "With your password" else "No password: you sign in with the accounts below." :: Text }}</p>
                {% if st.changingPassword %}
                  {{ passwordForm st m }}
                {% else %}
                  {% for offered in maybeToList (passwordOffer providers m) %}
                    <button type="button" class="password-change-toggle" disabled={{ st.busy }} onClick={{ const AskPassword }}>
                      {{ offered }}
                    </button>
                  {% end %}
                {% end %}
                <div class="sign-in-told">
                  {% for t in maybeToList st.passwordTold %}
                    <p class="account-told" role="status">{{ t }}</p>
                  {% end %}
                </div>
                <ul class="identities">
                  {% for i in m.identities %}
                    <li class="identity">
                      <span class="identity-provider">{{ labelOf i.provider }}</span>
                      {% for e in maybeToList i.email %}
                        <span class="identity-email">{{ e.text }}</span>
                      {% end %}
                      <button
                        type="button"
                        class="identity-disconnect"
                        disabled={{ st.busy || not (mayDisconnect i.provider m) }}
                        title={{ if mayDisconnect i.provider m then "" else "Your only way to sign in" :: Text }}
                        onClick={{ const (Disconnect i.provider) }}
                      >
                        Disconnect
                      </button>
                    </li>
                  {% end %}
                </ul>
                <div class="identity-connects">
                  {% for p in filter (\p -> p.name `notElem` map (\i -> i.provider) m.identities) providers %}
                    <a class="identity-connect" href={{ startUrl p.name Connect st.input.here }}>{{ "Connect " <> p.label }}</a>
                  {% end %}
                </div>
                <div class="sign-in-told">
                  {% for p in maybeToList st.problem %}
                    <p class="error account-error" role="alert">{{ p }}</p>
                  {% end %}
                </div>
              </div>
            |]

    -- A new password, typed twice, and the current one where there is
    -- one. The inputs are removed with the form when it closes, together
    -- with what was typed in them.
    passwordForm :: State m -> SignInMethods -> H.ComponentHTML (Action m) Empty m
    passwordForm st m =
      [hh|
        <form class="password-change" aria-label={{ verb }} {{ HE.preventingDefault (HE.onSubmit (const SavePassword)) }}>
          {% if m.password %}
            <label class="field">
              <span>Current password</span>
              <input type="password" name="current" autocomplete="current-password" required onValueInput={{ TypedCurrent }} {{ HP.ref st.currentField }}>
            </label>
          {% end %}
          <label class="field">
            <span>New password</span>
            <input type="password" name="new" autocomplete="new-password" required minlength={{ minimumPasswordLength }} onValueInput={{ TypedNew }} {{ HP.ref st.newField }}>
          </label>
          <label class="field">
            <span>New password again</span>
            <input type="password" name="repeat" autocomplete="new-password" required onValueInput={{ TypedRepeat }}>
          </label>
          <div class="sign-in-told">
            {% for p in maybeToList st.passwordProblem %}
              <p class="error password-error" role="alert">{{ p }}</p>
            {% end %}
          </div>
          <div class="actions">
            <button type="submit" class="primary" disabled={{ st.busy }}>{{ verb }}</button>
            <button type="button" onClick={{ const CancelPassword }}>Cancel</button>
          </div>
        </form>
      |]
      where
        verb :: Text
        verb = if m.password then "Change password" else "Set password"

    handleAction :: Action m -> H.HalogenM (State m) (Action m) Empty output m ()
    handleAction = \case
      Initialize -> do
        accounts <- gets (\st -> st.input.accounts)
        result <- H.async accounts.identities
        modify (\x -> x {methods = Just result})
      -- The session changes with the shell's (an avatar changed). The
      -- shell mounts the section again for another user.
      Receive input -> modify (\st -> st {input})
      Disconnect provider -> do
        st <- get
        unless st.busy $ do
          modify (\s -> s {busy = True, problem = Nothing})
          H.async (st.input.accounts.unlinkIdentity provider) >>= \case
            Left e -> modify (\s -> s {busy = False, problem = Just (errorMessage e)})
            Right m -> modify (\s -> s {busy = False, methods = Just (Right m)})
      AskPassword -> do
        modify (\st -> (forgetPassword st) {changingPassword = True, passwordTold = Nothing})
        st <- get
        -- The current password's field, or the new one's where there is
        -- no current one.
        current <- H.getRef st.currentField
        new <- H.getRef st.newField
        traverse_ (H.liftEffect . focusElement) (current <|> new)
      CancelPassword -> modify forgetPassword
      TypedCurrent typedCurrent -> modify (\st -> st {typedCurrent})
      TypedNew typedNew -> modify (\st -> st {typedNew})
      TypedRepeat typedRepeat -> modify (\st -> st {typedRepeat})
      SavePassword -> do
        st <- get
        case st.methods of
          Just (Right m)
            | st.busy -> pass
            | st.typedNew /= st.typedRepeat ->
                modify (\s -> s {passwordProblem = Just "The new password and its repetition are not the same."})
            | otherwise -> do
                modify (\s -> s {busy = True, passwordProblem = Nothing})
                let change = PasswordChange {current = if m.password then Just (Password st.typedCurrent) else Nothing, new = Password st.typedNew}
                H.async (changeOwnPassword st.input.accounts.changePassword change) >>= \case
                  Left e -> modify (\s -> s {busy = False, passwordProblem = Just (passwordErrorText e)})
                  Right after ->
                    modify $ \s ->
                      (forgetPassword s)
                        { busy = False
                        , methods = Just (Right after)
                        , passwordTold =
                            Just
                              ( if m.password
                                  then "Your password is changed. You stay signed in here, and anywhere else you were signed in is signed out."
                                  else "Your password is set: you may sign in with your login and it too."
                              )
                        }
          _ -> pass

    -- Close the form to change the password, and clear what was typed in
    -- it.
    forgetPassword :: State m -> State m
    forgetPassword st = st {changingPassword = False, typedCurrent = "", typedNew = "", typedRepeat = "", passwordProblem = Nothing}

-- | The button that opens the form for one's own password, if any:
-- "Change password…" for a user who has a password; "Set a password…" for
-- one who has none and no server provider to sign in with (theirs was
-- disconnected, or removed from the server); and none for one who signs in
-- with a server provider, who has no password to set (WP5.6 of
-- docs/PLAN.md). The server refuses that case the same way.
passwordOffer :: [SignInProvider] -> SignInMethods -> Maybe Text
passwordOffer providers m
  | m.password = Just "Change password…"
  | providerOnly (map (\p -> p.name) providers) m.password (map (\i -> i.provider) m.identities) = Nothing
  | otherwise = Just "Set a password…"
