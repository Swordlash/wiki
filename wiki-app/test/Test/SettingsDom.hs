-- | The way to the user's settings (issue 340), the real components on the
-- in-memory DOM ("Test.Harness.MemDom"): the menus under the user's name
-- hold "Settings", the demo's switching of users and the server's
-- "Sign out", and nothing else; a visitor is offered to sign in; and
-- "Settings" leads to the settings' first section, beside the list of
-- every section.
--
-- The demo's menu is the shell's own ("Wiki.App.Shell"), mounted over the
-- demo. The server's is the sign-in view's ("Wiki.App.View.SignIn"),
-- mounted alone with a backend that signs nobody in: the menu asks the
-- server nothing. Natively the settings' sections are not mounted (they
-- read what they show in the browser only), so the page says that it is
-- reading them; the sections themselves are the browser suite's. Only
-- the section "How you sign in" ("Wiki.App.View.SignInMethods") is
-- mounted here, alone: it asks the shell to change the password, and
-- shows the shell's answer.
module Test.SettingsDom (spec) where

import Data.Map.Strict qualified as Map
import Data.Time (UTCTime (..), fromGregorian)
import Halogen.Router qualified as Router
import Halogen.Subscription qualified as HS
import Halogen.VDom.DOM.Monad (MemDOM, runMemDOM)
import Halogen.VDom.DOM.Monad.Native qualified as N
import Protolude hiding (find)
import Test.Harness.MemDom (View, click, clickOutside, find, findAll, outputs, setInput, settle, tell, textOf, typeInto, withView)
import Test.Hspec
import Wiki.Api.Account (LinkedIdentity (..), SignInMethods (..))
import Wiki.Api.Account qualified as Account (PasswordChange (..))
import Wiki.App (component, input)
import Wiki.App.Account (Accounts (..), SignInError (..))
import Wiki.App.Backend (Session, WikiError (..), anonymousIn, session)
import Wiki.App.Backend.Mem (MemWiki, newMemWiki, runMemWiki)
import Wiki.App.Backend.Mem.State (MemState (..))
import Wiki.App.Demo (demoNow, demoState)
import Wiki.App.Route (Route (..), SettingsSection (..), route)
import Wiki.App.Shell qualified as Shell
import Wiki.App.View.SignIn qualified as SignIn
import Wiki.App.View.SignInMethods qualified as SignInMethods
import Wiki.Core.Access (Access (..))
import Wiki.Core.Id (UserId (..))
import Wiki.Core.User (Password (..))

spec :: Spec
spec = describe "Wiki.App.View.SignIn and Wiki.App.Shell, the way to the settings" $ do
  describe "the demo's menu of users" $ do
    it "holds the demo's users and, for a user, Settings, and nothing else" $
      withShell (Just "user-editor") Home $ \view -> do
        click view ".user-switcher-toggle"
        settle view
        entries view `shouldReturn` replicate (1 + length demoUsers) (Just "user-option") <> [Just "settings-entry"]
        textOf view ".user-menu .settings-entry" `shouldReturn` "Settings"
        hrefOf view ".user-menu .settings-entry" `shouldReturn` Just "/settings/profile/"

    it "holds only the demo's users for nobody signed in" $
      withShell Nothing Home $ \view -> do
        click view ".user-switcher-toggle"
        settle view
        entries view `shouldReturn` replicate (1 + length demoUsers) (Just "user-option")

    it "leads to the settings' first section, beside the list of every section, the menu closed" $
      withShell (Just "user-editor") Home $ \view -> do
        click view ".user-switcher-toggle"
        settle view
        click view ".user-menu .settings-entry"
        settle view
        textOf view ".settings-view h1" `shouldReturn` "Profile"
        (findAll view ".settings-nav-link" >>= traverse N.textContent) `shouldReturn` ["Profile", "Security", "API keys", "Plugins", "Notifications", "Writing assistant"]
        (findAll view ".settings-nav-link" >>= traverse (N.attribute "href")) `shouldReturn` map Just ["/settings/profile/", "/settings/security/", "/settings/api-keys/", "/settings/plugins/", "/settings/notifications/", "/settings/assistant/"]
        textOf view ".settings-nav-link[aria-current=page]" `shouldReturn` "Profile"
        textOf view ".settings-reading" `shouldReturn` "Reading your settings…"
        (length <$> findAll view ".user-menu") `shouldReturn` 0

    it "shows the section chosen, and asks nobody signed in to choose who they are" $ do
      withShell (Just "user-editor") (UserSettings NotificationsSection) $ \view -> do
        textOf view ".settings-view h1" `shouldReturn` "Notifications"
        textOf view ".settings-nav-link[aria-current=page]" `shouldReturn` "Notifications"
      withShell Nothing (UserSettings SecuritySection) $ \view -> do
        textOf view ".settings-view h1" `shouldReturn` "Settings"
        textOf view ".settings-view .sign-in-hint" `shouldReturn` "Your settings are a user's own, and you are not signed in. Choose who you are at the top right."
        (length <$> findAll view ".settings-nav-link") `shouldReturn` 0

  describe "the server's menu under the user's name" $ do
    it "holds Settings and Sign out for a user signed in, and closes on a press outside" $ do
      editor <- signedIn "user-editor"
      withSignIn editor False $ \view -> do
        (length <$> findAll view ".account-menu") `shouldReturn` 0
        click view ".account-toggle"
        settle view
        (findAll view ".account-menu > li > *" >>= traverse N.textContent) `shouldReturn` ["Settings", "Sign out"]
        hrefOf view ".account-menu .settings-entry" `shouldReturn` Just "/settings/profile/"
        clickOutside view
        settle view
        (length <$> findAll view ".account-menu") `shouldReturn` 0

    it "signs nobody out while the shell changes the user's password, and says why" $ do
      editor <- signedIn "user-editor"
      withSignIn editor True $ \view -> do
        click view ".account-toggle"
        settle view
        textOf view ".account-menu .sign-out-note" `shouldReturn` "Your password is being changed: you can sign out once that is done."
        click view ".account-menu .sign-out"
        settle view
        length <$> outputs view `shouldReturn` 0

    it "offers a visitor only to sign in, and to create an account where the wiki takes new ones" $ do
      withSignIn (anonymousIn Open) False $ \view -> do
        (findAll view ".account > button" >>= traverse N.textContent) `shouldReturn` ["Sign in", "Create account"]
        (length <$> findAll view ".account-toggle, .settings-entry, .sign-out") `shouldReturn` 0
      withSignIn (anonymousIn Closed) False $ \view ->
        (findAll view ".account > button" >>= traverse N.textContent) `shouldReturn` ["Sign in"]

  describe "the section \"How you sign in\", whose change of password the shell runs" $ do
    it "asks the shell for the change, and shows the shell's answer to it" $ do
      editor <- signedIn "user-editor"
      withMethods editor False $ \view -> do
        asked <- askChange view
        case asked of
          [SignInMethods.ChangePassword ticket change] -> do
            (change.current, change.new) `shouldBe` (Just (Password "the old one"), Password "a new password")
            -- Asked once: a second submit while the answer is awaited
            -- asks nothing more.
            click view ".password-change button[type=submit]"
            settle view
            length <$> outputs view `shouldReturn` 1
            tell view (SignInMethods.Answered ticket (Right withPassword))
            settle view
            textOf view ".account-told" `shouldReturn` "Your password is changed. You stay signed in here, and anywhere else you were signed in is signed out."
            length <$> findAll view ".password-change" `shouldReturn` 0
          _ -> expectationFailure ("one change asked for, not " <> show (length asked))

    -- The methods shown come from one ask at a time. A section mounted
    -- again while the shell runs another mount's change reads nothing
    -- (its read could be answered before or after the change), offers
    -- nothing (a disconnection would overlap the change), and does not
    -- show that change's answer, which may be older than a read asked
    -- after it. Once the change has ended, it reads the methods.
    it "reads and offers nothing while the shell runs a change asked for by a section left since, takes nothing from that change's answer, and reads the methods once it has ended" $ do
      editor <- signedIn "user-editor"
      -- The ticket of a mount left since: the section mounted before.
      left <- withMethods editor False askChange
      withMethods editor True $ \view -> do
        textOf view ".account-methods-section .loading" `shouldReturn` "Your password is being changed. How you sign in is shown once that is done."
        length <$> findAll view ".password-change-toggle, .identity-disconnect" `shouldReturn` 0
        case left of
          [SignInMethods.ChangePassword ticket _] -> do
            tell view (SignInMethods.Answered ticket (Right withPassword))
            settle view
            length <$> findAll view ".account-password, .identity" `shouldReturn` 0
            setInput view (methodsInput editor False)
            settle view
            textOf view ".identity-provider" `shouldReturn` "github"
            length <$> findAll view ".identity-disconnect:not([disabled])" `shouldReturn` 1
            length <$> outputs view `shouldReturn` 0
          _ -> expectationFailure ("one change asked for, not " <> show (length left))
  where
    withPassword = SignInMethods {password = True, identities = []}
    -- Open the form, type a change, and submit it: what the section asked
    -- the shell for.
    askChange view = do
      click view ".password-change-toggle"
      settle view
      typeInto view ".password-change input[name=current]" "the old one"
      typeInto view ".password-change input[name=new]" "a new password"
      typeInto view ".password-change input[name=repeat]" "a new password"
      click view ".password-change button[type=submit]"
      settle view
      outputs view
    demoUsers = Map.elems (demoState demoNow).users
    -- The class of each entry of the menu of users, in order.
    entries view = findAll view ".user-menu > li > *" >>= traverse (N.attribute "class")
    hrefOf view selector = find view selector >>= fmap join . traverse (N.attribute "href")

-- | The shell over the demo, as someone (by user id, or nobody), at a
-- route.
withShell :: Maybe Text -> Route -> (View Shell.Query (Shell.Input (MemWiki MemDOM)) Void -> IO a) -> IO a
withShell who at body = do
  wiki <- newMemWiki (demoState demoNow) {current = UserId <$> who}
  let run :: MemWiki MemDOM a -> MemDOM a
      run = runMemWiki wiki
  router <- runMemDOM (run (Router.newRouter "/" Unknown route))
  Router.redirect router at
  withView run (component @Void) (input router) $ \view -> do
    settle view
    body view

-- | The demo's session of one of its users.
signedIn :: Text -> IO Session
signedIn user = do
  wiki <- newMemWiki (demoState demoNow) {current = Just (UserId user)}
  runMemDOM (runMemWiki wiki session)

-- | The sign-in view alone, for this session, over a backend that signs
-- nobody in and says nothing.
-- And whether the shell runs a change of the user's password.
withSignIn :: Session -> Bool -> (View SignIn.Query (SignIn.Input (MemWiki MemDOM)) SignIn.Output -> IO a) -> IO a
withSignIn s changingPassword body = do
  wiki <- newMemWiki (demoState demoNow)
  let run :: MemWiki MemDOM a -> MemDOM a
      run = runMemWiki wiki
  router <- runMemDOM (run (Router.newRouter "/" Unknown route))
  withView run SignIn.component SignIn.Input {accounts = refusing, session = s, avatar = Nothing, here = "/", router, changingPassword} $ \view -> do
    settle view
    body view

-- | The section "How you sign in" alone ('methodsInput').
withMethods :: Session -> Bool -> (View SignInMethods.Query (SignInMethods.Input MemDOM) SignInMethods.Output -> IO a) -> IO a
withMethods s changing body =
  withView identity SignInMethods.component (methodsInput s changing) $ \view -> do
    settle view
    body view

-- | The input of the section "How you sign in", for this session, and
-- whether the shell runs a change of password for the user already. The
-- user has a password and a provider's account.
methodsInput :: Session -> Bool -> SignInMethods.Input MemDOM
methodsInput s changing = SignInMethods.Input {accounts, session = s, here = "/", changing}
  where
    accounts = answering (Right SignInMethods {password = True, identities = [github]})
    github = LinkedIdentity {provider = "github", email = Nothing, name = Nothing, linkedAt = UTCTime (fromGregorian 2026 1 1) 0}

-- | Sign-in methods that refuse everything: the menu needs none of them.
refusing :: (Applicative m) => Accounts m
refusing = answering (Left (Invalid "This test signs nobody in."))

-- | Sign-in methods that refuse everything but the user's sign-in
-- methods, which are these.
answering :: (Applicative m) => Either WikiError SignInMethods -> Accounts m
answering methods =
  Accounts
    { signIn = \_ _ -> pure (Left refused)
    , signOut = pure (Left no)
    , sessionChanges = pure (HS.makeEmitter (\_ -> pure pass))
    , register = \_ -> pure (Left refused)
    , invitationFor = \_ -> pure (Left no)
    , acceptInvitation = \_ _ -> pure (Left refused)
    , identities = pure methods
    , unlinkIdentity = \_ -> pure (Left no)
    , changePassword = \_ _ -> pure (Left refused)
    , confirmSecondFactor = \_ -> pure (Left refused)
    , beginPasskeySignIn = pure (Left refused)
    , finishPasskeySignIn = \_ -> pure (Left refused)
    , beginSecondFactorPasskey = pure (Left refused)
    , finishSecondFactorPasskey = \_ -> pure (Left refused)
    , requestPasswordReset = \_ -> pure (Left refused)
    , resetPassword = \_ _ -> pure (Left no)
    , verifyEmail = \_ -> pure (Left no)
    }
  where
    no = Invalid "This test signs nobody in."
    refused = SignInFailed no
