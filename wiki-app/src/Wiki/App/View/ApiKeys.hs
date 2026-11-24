{-# LANGUAGE QuasiQuotes #-}

-- | The user's API keys (WP5.138 of docs/PLAN.md, "Wiki.Api.ApiKeys"): the
-- keys a program on their own machine signs in to the wiki's API with, as
-- them.
--
-- * __The list.__ Each key, the newest first: its name, its right, when it
--   was made, when it expires, when it was last used (to within ten
--   minutes, as the server writes it), and whether it works. A key that
--   works has Revoke, asked once more before it is sent. Revoked and
--   expired keys are shown dimmed, after the ones that work.
-- * __Make a key.__ A name, a right (the right to administer offered only
--   to an administrator), an expiry, and the current password, which the
--   server asks for as it does to add a passkey ("Wiki.App.View.Security":
--   the field shows unless the user is known to have no password). The
--   server's refusals are shown beside the form in its words.
-- * __The key, once.__ The key just made, in a field with Copy, with three
--   examples of its use filled in with the wiki's address, until the box
--   is closed or the panel goes away. It is never stored.
--
-- What the panel knows is its machine's ("Wiki.App.View.ApiKeys.Machine"):
-- this module renders it, turns the user's acts into its events and
-- carries out its effects. In the demo the keys are the in-memory
-- backend's, which keeps them by the server's rules and signs nothing in
-- with them, as the panel says.
module Wiki.App.View.ApiKeys
  ( Input (..)
  , component
  , rightText
  , examples
  , programAddress
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.UUID (MonadUUID)
import Data.Row (Empty)
import Data.Text qualified as T
import Data.Time (UTCTime (..), addUTCTime, defaultTimeLocale, formatTime, fromGregorian, getCurrentTime)
import Halogen qualified as H
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Machine qualified as Machine
import Halogen.Router (Router)
import Halogen.Router qualified as Router
import Halogen.Sync qualified as Sync
import Halogen.TH (hh)
import Protolude hiding (State)
import Wiki.Api.ApiKeys (ApiKeyId (..), ApiKeyInfo (..), KeyRight (..), MadeApiKey (..), NewApiKey (..))
import Wiki.Api.Security (SecondFactorState (..))
import Wiki.App.Account (SignInError (..), signInErrorText)
import Wiki.App.Backend (WikiError (..))
import Wiki.App.Browser (copyText, endOfDay)
import Wiki.App.Route (Route (Home))
import Wiki.App.Security (MonadSecurity (..))
import Wiki.App.View.ApiKeys.Machine
  ( Choice (..)
  , Draft (..)
  , Effect (..)
  , Event (..)
  , Expiry (..)
  , Form (..)
  , KeyState (..)
  , Model (..)
  , busy
  , choiceOf
  , choiceValue
  , initial
  , keyState
  , ordered
  , step
  )
import Wiki.App.View.Common (errorMessage)
import Wiki.Core.User (Password (..))

-- | 'Input' is what the panel is given.
data Input = Input
  { session :: Int
  -- ^ The number of the session signed in ("Wiki.App.View.Common"'s
  -- @sessionNumber@): another number is another user, whose keys the panel
  -- reads afresh.
  , administers :: Bool
  -- ^ Whether the user is an administrator: only then is the right to
  -- administer offered.
  , address :: Text
  -- ^ The wiki's address as a program reaches it, such as
  -- @https:\/\/wiki.example@ or @https:\/\/example.org\/wiki@, without a
  -- final slash: the examples are written with it.
  , demo :: Bool
  -- ^ Whether the backend is the demo's, whose keys sign in nowhere.
  }

data State = State
  { input :: Input
  , machine :: Machine.Machine () Event Model
  , keyField :: H.RefLabel
  -- ^ The field the key just made is shown in, which Copy selects where
  -- the browser has no clipboard for the page.
  }

data Action
  = Initialize
  | Finalize
  | Receive Input
  | Happen Event
  | ChooseExpiry Text

component :: forall query output m. (MonadSecurity m, MonadUUID m, MonadUnliftIO m) => H.Component query Input output m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = \input -> do
          keyField <- H.newRefLabel "api-key-made"
          pure State {input, machine = Machine.new (initial input.session input.administers), keyField}
      , render
      , eval =
          H.mkEval
            H.defaultEval
              { H.handleAction = handleAction
              , H.initialize = Just Initialize
              , H.finalize = Just Finalize
              , H.receive = Just . Receive
              }
      }
  where
    render :: State -> H.ComponentHTML Action Empty m
    render st =
      [hh|
        <div class="api-keys">
          <p class="account-note api-keys-about">An API key lets a program, a script or an assistant on your own machine sign in to this wiki's API as you, doing what you may do, narrowed by the key's right. These are not the keys of an AI provider, which are under Writing assistant.</p>
          {% if st.input.demo %}
            <p class="account-note api-keys-demo">This demo has no server: the keys made here sign in nowhere.</p>
          {% end %}
          {% for made in maybeToList m.shown %}
            {{ madeView st made }}
          {% end %}
          {{ listView m }}
          {{ formView m }}
        </div>
      |]
      where
        m = Machine.current st.machine

    listView :: Model -> H.ComponentHTML Action Empty m
    listView m = case m.keys of
      Nothing -> [hh|<p class="loading">Loading…</p>|]
      Just (Left e) -> [hh|<p class="error api-keys-error" role="alert">{{ errorMessage e }}</p>|]
      Just (Right [])
        | isNothing m.problem -> [hh|<p class="api-keys-empty">You have no keys yet.</p>|]
      Just (Right _) ->
        [hh|
          <div>
            <ul class="api-key-list">
              {% for k in ordered m %}
                {{ keyView m k }}
              {% end %}
            </ul>
            {% for e in maybeToList m.problem %}
              <p class="error api-keys-error" role="alert">{{ errorMessage e }}</p>
            {% end %}
          </div>
        |]

    keyView :: Model -> ApiKeyInfo -> H.ComponentHTML Action Empty m
    keyView m k =
      [hh|
        <li class={{ "api-key api-key-" <> stateClass }} data-key={{ k.id.text }}>
          <span class="api-key-name">{{ k.name }}</span>
          <span class="api-key-right">{{ rightText k.right }}</span>
          <span class="api-key-times">{{ "Made " <> stamped k.createdAt <> ". " <> expiresText <> " " <> usedText }}</span>
          <span class="api-key-state">{{ stateText }}</span>
          {% if kState == Works %}
            {% if m.confirming == Just k.id %}
              <span class="api-key-confirm" role="alert">
                <span>{{ "Revoke '" <> k.name <> "'? Programs using it stop at once." }}</span>
                <button type="button" class="api-key-revoke-yes" onClick={{ const (Happen RevokeConfirmed) }}>Revoke</button>
                <button type="button" class="api-key-revoke-no" onClick={{ const (Happen RevokeCancelled) }}>Cancel</button>
              </span>
            {% else %}
              <button type="button" class="api-key-revoke" disabled={{ k.id `elem` m.revoking }} onClick={{ const (Happen (RevokeClicked k.id)) }}>Revoke</button>
            {% end %}
          {% end %}
        </li>
      |]
      where
        kState = keyState m.now k
        stateClass :: Text
        stateClass = case kState of
          Works -> "works"
          Expired -> "expired"
          RevokedKey -> "revoked"
        stateText :: Text
        stateText = case kState of
          Works -> "Works"
          Expired -> "Expired"
          RevokedKey -> maybe "Revoked" (\t -> "Revoked " <> stamped t) k.revokedAt
        expiresText = case k.expiresAt of
          Nothing -> "Expires never."
          Just e
            | kState == Expired -> "Expired " <> stamped e <> "."
            | otherwise -> "Expires " <> stamped e <> "."
        usedText = case k.lastUsedAt of
          Nothing -> "Never used."
          Just t -> "Last used around " <> stamped t <> "."

    formView :: Model -> H.ComponentHTML Action Empty m
    formView m =
      [hh|
        <form class="api-key-make" aria-label="Make a key" {{ HE.preventingDefault (HE.onSubmit (const (Happen MakeClicked))) }}>
          <h4>Make a key</h4>
          <label class="field">
            <span>Name</span>
            <input type="text" name="api-key-name" maxlength="100" placeholder="Named by the day it is made" value={{ m.form.keyName }} onValueInput={{ Happen . NameTyped }}>
          </label>
          <fieldset class="api-key-rights">
            <legend>What it may do</legend>
            {% for r in offered %}
              <label class="api-key-right-choice">
                <input type="radio" name="api-key-right" value={{ rightValue r }} checked={{ m.form.keyRight == r }} onChange={{ const (Happen (RightChosen r)) }}>
                <span>{{ rightText r }}</span>
                <span class="account-note">{{ rightNote r }}</span>
              </label>
            {% end %}
          </fieldset>
          <label class="field">
            <span>Expires</span>
            <select name="api-key-expiry" onValueChange={{ ChooseExpiry }}>
              {% for c in [minBound .. maxBound :: Choice] %}
                <option value={{ choiceValue c }} selected={{ m.form.choice == c }}>{{ choiceText c }}</option>
              {% end %}
            </select>
          </label>
          {% if m.form.choice == OnDay %}
            <label class="field">
              <span>The last day it works</span>
              <input type="date" name="api-key-day" value={{ m.form.day }} onValueInput={{ Happen . DayTyped }}>
            </label>
          {% end %}
          {% if m.asksPassword /= Just False %}
            <label class="field">
              <span>Current password</span>
              <input type="password" name="api-key-password" autocomplete="current-password" required value={{ m.form.typed.text }} onValueInput={{ Happen . PasswordTyped . Password }}>
            </label>
          {% end %}
          <button type="submit" class="primary" disabled={{ busy m }}>Make a key</button>
          <div class="sign-in-told">
            {% if m.badDay %}
              <p class="error api-key-refusal" role="alert">Choose the last day the key works.</p>
            {% end %}
            {% for e in maybeToList m.refusal %}
              <p class="error api-key-refusal" role="alert">{{ refusalText e }}</p>
            {% end %}
          </div>
        </form>
      |]
      where
        offered = [ReadRight, WriteRight] <> [AdminRight | m.administers]

    madeView :: State -> MadeApiKey -> H.ComponentHTML Action Empty m
    madeView st made =
      [hh|
        <div class="api-key-made" role="status">
          <p>{{ "Your key '" <> made.info.name <> "'. Copy it now: it is not shown again." }}</p>
          <div class="api-key-made-row">
            <input type="text" class="api-key-text" readonly spellcheck="false" value={{ made.key }} aria-label="The key" {{ HP.ref st.keyField }}>
            <button type="button" class="api-key-copy" onClick={{ const (Happen CopyClicked) }}>{{ copyLabel }}</button>
          </div>
          <p class="account-note">Use it like this:</p>
          {% for (label, e) in examples st.input.address made.key %}
            {% for l in maybeToList label %}
              <p class="account-note api-key-example-label">{{ l }}</p>
            {% end %}
            <pre class="api-key-example"><code>{{ e }}</code></pre>
          {% end %}
          <button type="button" class="api-key-made-close" onClick={{ const (Happen KeyClosed) }}>I have copied it</button>
        </div>
      |]
      where
        copyLabel :: Text
        copyLabel = case (Machine.current st.machine).copied of
          Just True -> "Copied"
          Just False -> "Select and copy"
          Nothing -> "Copy"

    handleAction :: Action -> H.HalogenM State Action Empty output m ()
    handleAction = \case
      Initialize -> happen Started
      Finalize -> Machine.finish machineSpec Finished
      Receive input -> do
        modify (\st -> st {input})
        happen (Given input.session input.administers)
      Happen e -> happen e
      ChooseExpiry v -> traverse_ (happen . ChoiceMade) (choiceOf v)

    happen :: Event -> H.HalogenM State Action Empty output m ()
    happen = Machine.happen machineSpec

    machineSpec :: Machine.Spec () State Action Empty output m Event Effect Model
    machineSpec = Machine.Spec {Machine.step = step, Machine.carry = carry, Machine.get = \st -> st.machine, Machine.set = \machine st -> st {machine}}

    carry :: Effect -> Machine.Carry () State Action Empty output m Event
    carry = \case
      Load asked -> Machine.Ask (Machine.run (timed ownApiKeys)) (\r -> let (at, a) = timedAnswer r in Loaded asked at a)
      LoadProof s -> Machine.Ask (Machine.run (fmap (\f -> f.asksPassword) <$> secondFactorState)) (ProofKnown s . answerOf)
      Make asked draft -> Machine.Ask (Machine.run (timed (makeFrom draft))) (\r -> let (at, a) = timedSignIn r in Made asked at a)
      Revoke asked i -> Machine.Ask (Machine.run (timed (revokeApiKey i))) (\r -> let (at, a) = timedAnswer r in Revoked asked i at a)
      -- The key is read from the model in place, with the field it is shown
      -- in; then the copy waits for the browser's clipboard.
      Copy i ->
        Machine.AskWith
          ( do
              field <- gets (\st -> st.keyField) >>= Sync.getRef
              shown <- gets (\st -> (Machine.current st.machine).shown)
              pure ((,) <$> field <*> shown)
          )
          (\found -> Machine.run (maybe (pure False) (\(el, k) -> copyText el k.key) found))
          (Copied i . fromRight False)

-- | A key made from a draft: its expiry turned into a moment from the
-- clock and the reader's time zone.
makeFrom :: (MonadSecurity m, MonadIO m) => Draft -> m (Either SignInError MadeApiKey)
makeFrom draft = do
  now <- liftIO getCurrentTime
  expiresAt <- case draft.expiry of
    Never -> pure Nothing
    AfterDays n -> pure (Just (addUTCTime (fromIntegral n * 86400) now))
    EndOf day -> Just <$> endOfDay day
  makeApiKey NewApiKey {name = draft.name, right = draft.right, expiresAt, password = draft.password}

-- | An answer with the moment it came.
timed :: (MonadIO m) => m a -> m (UTCTime, a)
timed act = do
  a <- act
  at <- liftIO getCurrentTime
  pure (at, a)

-- | The answer to a read or a revoke, as the machine takes it. The backend
-- gives what went wrong as a 'Left' and does not throw for it; what it
-- throws all the same is taken as the failure it is, or as 'Unreachable'.
-- An answer that never came is dated with the first moment there is: it
-- moves no clock.
timedAnswer :: Either SomeException (UTCTime, Either WikiError a) -> (UTCTime, Either WikiError a)
timedAnswer = \case
  Right r -> r
  Left e -> (epoch, Left (failure e))

timedSignIn :: Either SomeException (UTCTime, Either SignInError a) -> (UTCTime, Either SignInError a)
timedSignIn = \case
  Right r -> r
  Left e -> (epoch, Left (SignInFailed (failure e)))

answerOf :: Either SomeException (Either WikiError a) -> Either WikiError a
answerOf = either (Left . failure) identity

failure :: SomeException -> WikiError
failure e = fromMaybe (Unreachable (toS (displayException e))) (fromException e)

epoch :: UTCTime
epoch = UTCTime (fromGregorian 1970 1 1) 0

-- | A right, in words.
rightText :: KeyRight -> Text
rightText = \case
  ReadRight -> "Read"
  WriteRight -> "Read and write"
  AdminRight -> "Administer"

-- | What a right allows, in a line.
rightNote :: KeyRight -> Text
rightNote = \case
  ReadRight -> "Reads pages, lists and searches the spaces you may read."
  WriteRight -> "Also writes pages and creates spaces where you may."
  AdminRight -> "For the administration through the API, which is to come: today it does what Read and write does. It stops working if you stop being an administrator."

rightValue :: KeyRight -> Text
rightValue = \case
  ReadRight -> "read"
  WriteRight -> "write"
  AdminRight -> "admin"

choiceText :: Choice -> Text
choiceText = \case
  In30Days -> "In 30 days"
  In90Days -> "In 90 days"
  InAYear -> "In a year"
  NeverExpires -> "Never"
  OnDay -> "On a day…"

-- | Why a make was refused, in the server's words where it has some.
refusalText :: SignInError -> Text
refusalText = \case
  WrongCredentials -> "Your current password is not right."
  SignInFailed Forbidden -> "Only an administrator may make a key that administers."
  SignInFailed (Invalid why) -> why
  SignInFailed (Conflict why) -> why
  e -> signInErrorText e

-- | The wiki's address as a program reaches it: the page's origin (its
-- scheme and host, @https:\/\/wiki.example@) and the path the wiki is
-- served under, which the router's front page names (@\/@, or
-- @\/wiki\/@ for a wiki served with @--base-path \/wiki\/@), without a
-- final slash. The API and the MCP server are served under the same path,
-- so the examples written with it reach them whatever the wiki's base path
-- is.
programAddress :: Text -> Router Route -> Text
programAddress origin router = T.dropWhileEnd (== '/') (T.dropWhileEnd (== '/') origin <> Router.href router Home)

-- | The examples of a key's use shown with it, each with a label when the
-- command alone does not say what it is for: a request with @curl@, the
-- wiki's shell client (@clients\/wiki.sh@), and the line that adds the
-- wiki's MCP server (@\/mcp@) to Claude Code. All three are at the wiki's
-- address. Every value taken from the address or the key goes through
-- 'shellWord', so a pasted example stays one command whatever characters
-- the wiki's base path holds (@&@, @;@, a single quote).
examples :: Text -> Text -> [(Maybe Text, Text)]
examples given key =
  [ (Nothing, "curl -H " <> shellWord bearer <> " " <> shellWord (address <> "/api/list/"))
  , (Nothing, "WIKI_TOKEN=" <> shellWord key <> " WIKI_URL=" <> shellWord address <> " sh wiki.sh list")
  ,
    ( Just "For an assistant such as Claude Code, through this wiki's MCP server:"
    , "claude mcp add --transport http wiki " <> shellWord (address <> "/mcp") <> " --header " <> shellWord bearer
    )
  ]
  where
    address = T.dropWhileEnd (== '/') given
    bearer = "Authorization: Bearer " <> key

-- | A text as one word of a POSIX shell's command line: in single quotes,
-- where nothing is special, and each single quote of the text written as
-- @'\\''@ (the quoted part closed, a quote escaped, a new quoted part
-- opened).
shellWord :: Text -> Text
shellWord t = "'" <> T.replace "'" "'\\''" t <> "'"

-- | A time, in words for a person to read, in UTC, as the security panel
-- writes it.
stamped :: UTCTime -> Text
stamped = toS . formatTime defaultTimeLocale "%Y-%m-%d %H:%M UTC"
