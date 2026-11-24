{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The assistant panel: the writing assistant's panel in the editor
-- (WP4.19 of docs/PLAN.md), a chat with a model that edits the page, as
-- in code editors. It is the assistant plugin's surface
-- ("Wiki.App.Plugin.Assistant"), mounted by the plugin's agent
-- ("Wiki.App.Plugin.Assistant.Agent") the first time the editor opens it.
--
-- The panel shows, under its header, the assistant's mode, Ask or Agent,
-- and the provider's model and effort; then a list of messages, the
-- user's and the model's (the model's streamed as they arrive). Below it
-- is a row of shortcuts that fill in the message (improve, fix grammar,
-- shorten, expand, summarise, continue, translate), then the message box,
-- and Send, or Stop while the model is working. Enter sends,
-- Shift+Enter starts a new line, and Escape closes the panel, as does
-- "Close" in its header. "New" in the header starts a new conversation
-- ('NewConversation'). The editor's buttons open the panel with a
-- provider ('UseProvider'); a provider other than the conversation's
-- starts a new conversation too. A message that is a command
-- ('Wiki.Core.Assistant.Prompt.command') is the panel's, never sent:
-- @/new@ or @/clear@ starts a new conversation, as "New" does, and
-- @/help@ lists the commands; a word that is no command is said to be one,
-- and stays in the box to be corrected.
--
-- The mode (WP5.43, issue #120): in Agent mode the model edits the page,
-- as described below; in Ask mode it only answers. Its tools are then
-- turned off in the request ("Wiki.Core.Assistant.Prompt"), and an edit
-- it asks for anyway is refused, so nothing in the page changes. The mode
-- is kept in the user's settings, like the model, and applies from the
-- next message; a conversation may go on in the other mode.
--
-- The model picker changes the model stored in the user's settings,
-- effective from the next message. The effort picker starts at the effort
-- the settings page chose for the provider ('Wiki.Core.Assistant.effortOf')
-- and changes the effort for the current conversation only. The settings
-- keep their default.
--
-- The model's message shows that the model is working (WP5.11). From Send
-- until something arrives, and again after each tool call until the model
-- goes on, it shows a "Thinking" line with three animated dots. After a
-- few seconds of silence ('quietAfter') a count of the seconds waited
-- appears beside the dots. Under the line is the summary of the model's
-- thinking as the provider streams it, collapsed until clicked. Once the
-- model goes on, the summary stays at the top of the message as
-- "Thought". While the model writes a tool call, a status line says that
-- it is editing the page; the edit's note replaces the line once the edit
-- is made. Streaming text ends in a blinking cursor. Each message shows
-- the time it was sent, by the browser's clock, and a finished response
-- shows how long it took. Once the model's message ends, it also shows
-- the tokens its requests took, as the service reported them
-- ('Wiki.Core.Assistant.usageText'): the input, the part of it read from
-- the provider's cache, the output. A service that reports none shows
-- nothing.
--
-- What is sent with a message: when the editor has a selection, the
-- selection is attached, shown as a chip ("lines 12–20") with a button to
-- detach it, and the model receives it with a paragraph around it.
-- Otherwise the model receives the page, its first 'bodyCap' characters:
-- whole with the first message of a conversation, and with each later one
-- only what changed since the copy last sent, so that the conversation is
-- never changed and holds about one copy of the page ('sentCopy',
-- "Wiki.Core.Assistant.Prompt"). The panel says under the message box what
-- goes with the next message, and under each message of the user's what
-- went with it. The conversation lasts until "New"
-- starts another, or the editor closes (it is not stored anywhere), and is
-- sent whole with each message.
--
-- The model edits the page itself, through its tools, and the author
-- decides. Each edit is made in the editor immediately as a tracked change
-- ("Wiki.App.Editor.Browser", "Wiki.Core.Assistant.Edit"), shown with the
-- text it replaced. The tool tells the model that the edit was made, or
-- why not (its text was not found, or found twice), and the model goes
-- on. The author accepts or rejects the changes one by one on the change
-- itself, or all at once in the panel. Nothing is written into the page
-- that the author cannot reject, and the assistant never saves the page's
-- Markdown: only the author saves (a save with changes still pending
-- accepts them, as the editor says next to Save).
--
-- Stop ends the model's turn, and its request; the changes made so far
-- stay, for the author to decide. Closing the panel also stops the turn,
-- and puts the focus back in the page's text. The panel stays mounted,
-- with its conversation, until the editor is closed.
--
-- The user's settings belong to the editor, which reads them, stores them
-- when the panel changes the model ('ChangeSettings'), and fetches the
-- model list of each provider with a key when it opens. Without a key for
-- the chosen provider, the panel says where to add one. The models offered
-- are the provider's own list, grouped into the recommended models and
-- all the others ('Wiki.Core.Assistant.modelChoices'), or the wiki's
-- short list until a list is fetched. A provider that could not be
-- reached on the last attempt (because a network blocks it) is marked as
-- unreachable, with "Try again" ('Retry'). A message that gets a response
-- marks the provider as reachable again, and one that gets none marks it
-- as unreachable ('Reached').
module Wiki.App.Plugin.Assistant.Panel
  ( Input (..)
  , Query (..)
  , Output (..)
  , component
  )
where

import Control.Monad.UUID (MonadUUID)
import Data.Aeson (Value)
import Data.Map.Strict qualified as Map
import Data.Row (Empty)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Halogen qualified as H
import Halogen.HTML.Events qualified as HE
import Halogen.JSBits (JSText, fromJSText, inBrowser)
import Halogen.JSBits.Fields (inlineJS)
import Halogen.Subscription qualified as HS
import Halogen.TH (hh)
import Protolude hiding (State)
import Web.Event.Event (Event, EventType (..))
import Web.UIEvent.KeyboardEvent (KeyboardEvent (..))
import Wiki.App.Plugin (Document (..), LinkClick, OptionsLink, attr, focusElement, followOptionsLink, optionsLinkProps)
import Wiki.App.Plugin.Capability.Assistant (Heard (..), Models (..), Opening (..), Talk)
import Wiki.Core.Assistant
  ( AssistantError (kind)
  , ErrorKind (BadKey, Unreachable)
  , Mode (..)
  , Model (..)
  , ModelChoices (..)
  , ModelName (..)
  , Provider (..)
  , ProviderSettings (..)
  , Ready (..)
  , Settings (..)
  , Usage
  , baseUrlOf
  , chosenModel
  , conversationText
  , defaultBodyCap
  , effortOf
  , efforts
  , errorDetail
  , errorMeaning
  , errorText
  , modeLabel
  , modelChoices
  , modelLabel
  , pageCapOf
  , providerLabel
  , ready
  , serviceName
  , serviceOf
  , settingsOf
  , turnUsage
  , usageText
  , withMode
  , withModel
  )
import Wiki.Core.Assistant.Edit (Change (..), Decision (..), Refusal (..), answer, describeEdit, parseEdit)
import Wiki.Core.Assistant.Prompt
  ( Command (Help)
  , Outcome (..)
  , PageContext (..)
  , Shortcut (..)
  , Span (..)
  , anthropicRequest
  , chipLabel
  , command
  , commandHelp
  , copyAfter
  , lineSpan
  , openAiRequest
  , pageContext
  , scopeText
  , sentNote
  , shortcutLabel
  , shortcutText
  , shortcuts
  , unknownCommand
  , userTurn
  )
import Wiki.Core.Assistant.Prompt qualified as Prompt

-- The time of day as the browser's language writes a short time: the one
-- call with an options object, which no field takes.
$(inlineJS "js_clock" "new Date($1).toLocaleTimeString(undefined, { timeStyle: \"short\" })" [t|Double -> IO JSText|])

-- | 'Input': what the agent passes to the panel. The link to the
-- assistant's settings, the providers, where the page's text is, its
-- current title and Markdown, the selection in it, and the number of the
-- assistant's changes still to decide.
data Input m = Input
  { optionsLink :: OptionsLink
  , models :: Models m
  , settings :: Maybe (Either Text Settings)
  -- ^ The user's settings, once the editor has read them.
  , problem :: Maybe Text
  -- ^ Why storing a change of the settings failed, if it did.
  , checking :: Set Provider
  -- ^ The providers whose model lists the editor is fetching.
  , document :: Maybe Document
  -- ^ The editor's text, once CodeMirror is in place.
  , title :: Text
  , body :: Text
  , selection :: Maybe Span
  , pending :: Int
  , decisions :: Map Int Decision
  -- ^ The author's decision on each of the assistant's changes, by id.
  , encrypted :: Bool
  -- ^ Whether the page is in an encrypted space: the panel then says,
  -- before anything is sent, that what the assistant is asked about leaves
  -- this browser decrypted, to the provider named.
  }

-- | 'Query': the panel was closed ('Hide'), and the model's running turn,
-- if any, stops. Or it was opened with a provider ('UseProvider', from
-- the editor's button for each provider), which the editor makes the
-- assistant's provider. If the conversation was with another provider, it
-- ends, and a new one starts with the next message (a conversation cannot
-- move between providers: Anthropic's thinking blocks mean nothing to
-- OpenAI). The changes made so far stay, for the author to decide: they
-- belong to the editor, not to the conversation.
data Query a = Hide a | UseProvider Provider a

-- | 'Output': the panel's own close button, or Escape; "Accept all" or
-- "Reject all", which the editor carries out, as it does a decision on one
-- change; a change of the settings (a model), which the editor stores;
-- "Try again" for a provider that could not be reached; and whether a
-- message reached its provider.
data Output
  = Closed
  | DecideAll Decision
  | ChangeSettings (Settings -> Settings)
  | Retry Provider
  | Reached Provider Bool

-- | 'Entry': a message of the conversation.
data Entry
  = -- | The user's message.
    FromUser Said
  | -- | The model's message.
    FromModel Reply
  | -- | An error.
    Trouble Text
  | -- | A key the provider refused (a 401): what it means, which says to
    -- paste a new key, and the provider's own message, shown smaller
    -- under a link to the settings.
    KeyRefused Text Text
  | -- | A note from the panel about the conversation itself.
    Notice Text

-- | A message of the user's: the message, the lines of the selection
-- attached, if one was, what of the page went with it ('sentNote'), and
-- the time it was sent, as the chat shows it ('clockTime').
data Said = Said
  { asked :: Text
  , chip :: Maybe Text
  , sent :: Maybe Text
  , askedAt :: Text
  }

-- | A message of the model's, one per message of the user's.
data Reply = Reply
  { said :: Text
  -- ^ The text the model wrote, as it streams.
  , notes :: [Note]
  -- ^ The edits it made or tried.
  , stage :: Stage
  , thinking :: Text
  -- ^ The summary of the thinking the model is doing now, as it streams:
  -- shown under the "Thinking" line while the stage is 'Waiting'.
  , thought :: Text
  -- ^ The summaries of the thinking it did before, shown as "Thought" at
  -- the top of the message. 'settle' moves 'thinking' here once the model
  -- goes on.
  , clock :: Text
  -- ^ When the user's message was sent, as the chat shows it.
  , began :: UTCTime
  -- ^ When the user's message was sent: the start of the answer's
  -- duration.
  , quietSince :: UTCTime
  -- ^ When the current wait began: the user's message sent, or a tool
  -- call answered. The seconds counter counts from here.
  , took :: Maybe NominalDiffTime
  -- ^ How long the answer took, once it ended.
  , used :: [Usage]
  -- ^ The tokens of each request of the answer that the service reported,
  -- one per round of its tool loop. Their sum ('turnUsage') shows once
  -- the answer ended, was stopped or failed.
  }

-- | Where a model's message is.
data Stage
  = -- | Nothing has come since the message was sent or a tool call was
    -- answered: the model is thinking.
    Waiting
  | -- | Text is streaming.
    Writing
  | -- | The model is writing a tool call: an edit of the page.
    Editing
  | -- | The answer ended, or failed.
    Ended
  | -- | The user stopped it.
    Stopped
  deriving stock (Eq)

data Note = Made Text | NotMade Text Text

-- | Whether the model is still at work on a message.
working :: Stage -> Bool
working = (`elem` [Waiting, Writing, Editing])

-- | How long a wait goes before the "Thinking" line counts its seconds:
-- long enough that a quick answer shows none, short enough that a slow
-- one shows that the panel has not hung.
quietAfter :: NominalDiffTime
quietAfter = 3

data State m = State
  { input :: Input m
  , message :: Text
  , entries :: [Entry]
  , talk :: Maybe Talk
  -- ^ The chat, once the first message has opened it.
  , using :: Maybe Ready
  -- ^ The settings the chat uses, kept for its error messages, from which
  -- the key is removed.
  , talking :: Bool
  , detached :: Maybe Span
  -- ^ A selection the user detached, which is not sent until another
  -- selection is made.
  , nextChange :: Int
  , made :: Map Int Text
  -- ^ The model's changes whose outcome the model has not been told yet,
  -- by id, with their descriptions.
  , absorbed :: [Int]
  -- ^ Changes absorbed by a later change of the model, which need no mention.
  , spent :: [Usage]
  -- ^ The tokens of each request of this conversation that the service
  -- reported, summed under the header ('conversationText'). It is emptied
  -- with the conversation ('endConversation').
  , sentCopy :: Maybe Text
  -- ^ The copy of the page last sent in this conversation, cut to
  -- 'Wiki.Core.Assistant.Prompt.bodyCap'. The next message without a
  -- selection carries what changed since it. It is set in the step that
  -- sends a message, and kept when the turn is stopped or fails, since the
  -- chat keeps the message. It is taken back when no chat took the message
  -- ('HeardUnsent'); a new conversation starts without one.
  , listener :: Maybe (HS.Listener IO Heard)
  , otherModel :: Bool
  -- ^ Whether "Other…" is chosen for the model, which shows its text field.
  , turnMode :: Mode
  -- ^ The mode the last message was sent in, by which the model's tool
  -- calls in its answer are made or refused.
  , effortChoice :: Maybe Text
  -- ^ The effort picked in the panel for the current conversation, if
  -- any. 'Nothing' means the settings' default ('effortNow'). A new
  -- conversation with another provider starts with 'Nothing'.
  , now :: Maybe UTCTime
  -- ^ The time the ticker last read while the model is working. The
  -- seconds counter counts up to it.
  , ticker :: Maybe H.ForkId
  -- ^ The ticker, while the model is working: a fork that reads the time
  -- every second.
  , ended :: Maybe (MVar ())
  -- ^ The turn's end, while the model is working: an empty variable that
  -- 'finish' fills. A fork waits on it ('countTurn'), so that the turn
  -- counts as the program's work for as long as the model works.
  , field :: H.RefLabel
  }

data Action m
  = Initialize
  | Finalize
  | Receive (Input m)
  | Typed Text
  | Shortcut Shortcut
  | Send
  | Stop
  | Detach
  | FromTalk Heard
  | Decided Decision
  | Close
  | Followed LinkClick
  | ChooseMode Mode
  | ChooseModel Text
  | TypedModel Text
  | ChooseEffort Text
  | TryAgain Provider
  | NewConversation
  | Tick

component :: forall m. (Typeable m, MonadUUID m, MonadIO m) => H.Component Query (Input m) Output m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = \input -> do
          field <- H.newRefLabel "assistant-message"
          pure
            State
              { input
              , message = ""
              , entries = []
              , talk = Nothing
              , using = Nothing
              , talking = False
              , detached = Nothing
              , nextChange = 1
              , made = mempty
              , sentCopy = Nothing
              , spent = []
              , absorbed = []
              , listener = Nothing
              , otherModel = False
              , turnMode = Agent
              , effortChoice = Nothing
              , now = Nothing
              , ticker = Nothing
              , ended = Nothing
              , field
              }
      , render
      , eval =
          H.mkEval
            H.defaultEval
              { H.handleAction = handleAction
              , H.handleQuery = handleQuery
              , H.initialize = Just Initialize
              , H.finalize = Just Finalize
              , H.receive = Just . Receive
              }
      }
  where
    render :: State m -> H.ComponentHTML (Action m) Empty m
    render st =
      [hh|
        <section class="assistant" aria-label="Writing assistant">
          <header class="assistant-head">
            <h2>Assistant</h2>
            <span class="assistant-using">{{ usingLine st }}</span>
            <a class="assistant-settings-link" {{ optionsLinkProps st.input.optionsLink Followed }}>Settings</a>
            <button type="button" class="assistant-new" title="Start a new conversation" disabled={{ st.talking }} onClick={{ const NewConversation }}>New</button>
            <button type="button" class="assistant-close" title="Close the assistant (Escape)" onClick={{ const Close }}>Close</button>
          </header>
          {% for spentLine in maybeToList (conversationText st.spent) %}
            <p class="assistant-spent" title="The tokens of this conversation's answers so far, as the service reported them">{{ spentLine }}</p>
          {% end %}
          {% for service in maybeToList (leavesFor st) %}
            <p class="assistant-leaves" role="note">This page is encrypted. What you ask the assistant about leaves this browser decrypted, to {{ service }}.</p>
          {% end %}
          {{ choicesView st }}
          {{ setupNote st }}
          {% if st.input.pending > 0 %}
            <div class="assistant-changes" role="status">
              <span class="assistant-pending">{{ changesWord st.input.pending }} to accept or reject</span>
              <button type="button" class="assistant-accept-all" onClick={{ const (Decided Accept) }}>Accept all</button>
              <button type="button" class="assistant-reject-all" onClick={{ const (Decided Reject) }}>Reject all</button>
            </div>
          {% end %}
          <div class="assistant-log">
            <div class="assistant-messages" aria-live="polite">
              {% if null st.entries %}
                {% if modeNow st == Ask %}
                  <p class="assistant-empty">Ask about the page, or choose a request below: in Ask mode the assistant answers and changes nothing. Select text first to ask about it alone. Type /help for the commands.</p>
                {% else %}
                  <p class="assistant-empty">Ask for a change to the page, or choose one below. Select text first to ask about it alone. Each change the assistant makes is shown in the page, for you to accept or reject. Type /help for the commands.</p>
                {% end %}
              {% end %}
              {% for e in st.entries %}
                {{ entryView st e }}
              {% end %}
            </div>
          </div>
          <div class="assistant-shortcuts" role="group" aria-label="Shortcuts">
            {% for s in shortcuts %}
              <button type="button" class="assistant-shortcut" {{ attr "data-shortcut" (shortcutLabel s) }} disabled={{ st.talking }} onClick={{ const (Shortcut s) }}>{{ shortcutLabel s }}</button>
            {% end %}
          </div>
          <div class="assistant-scope">{{ scopeView st }}</div>
          <div class="assistant-compose">
            <textarea
              class="assistant-input"
              rows="3"
              placeholder={{ if modeNow st == Ask then "Ask a question about the page" else "Ask for a change, or a question about the page" :: Text }}
              aria-label="Message to the assistant"
              value={{ st.message }}
              onValueInput={{ Typed }}
              {{ HE.preventingDefault (HE.handler' (EventType "keydown") sendKey) }}
              ref={{ st.field }}
            ></textarea>
            <div class="assistant-send-row">
              {% if st.talking %}
                <button type="button" class="assistant-stop" onClick={{ const Stop }}>Stop</button>
              {% else %}
                <button type="button" class="primary assistant-send" disabled={{ T.null (T.strip st.message) }} onClick={{ const Send }}>Send</button>
              {% end %}
            </div>
          </div>
        </section>
      |]

    -- The mode, and the model and effort of the assistant's provider,
    -- under the panel's header, where they are seen before writing, each
    -- effective from the next message. The mode is two buttons, the one
    -- in force pressed, and is stored in the settings, as the model is in
    -- the provider's settings (the settings page shows the same model).
    -- The effort starts at the settings' default and is changed for this
    -- conversation only ('effortNow'). The models are grouped: the
    -- recommended ones, all the provider's other chat models, and
    -- "Other…", for any id typed in.
    choicesView :: State m -> Maybe (H.ComponentHTML (Action m) Empty m)
    choicesView st = case st.input.settings of
      Just (Right s) ->
        let ps = settingsOf s.provider s
            choices = modelChoices s.provider ps
            chosen = chosenModel s.provider ps
            listed = chosen `elem` map (\m -> m.id) (choices.recommended <> choices.others)
            other = st.otherModel || not listed
            option m = [hh|<option value={{ m.id.text }} selected={{ not other && chosen == m.id }}>{{ m.label }}</option>|]
            modeButton md =
              [hh|
                <button
                  type="button"
                  class="assistant-mode-choice"
                  {{ attr "data-mode" (T.toLower (modeLabel md)) }}
                  aria-pressed={{ if s.chatMode == md then "true" else "false" :: Text }}
                  title={{ modeTitle md }}
                  disabled={{ st.talking }}
                  onClick={{ const (ChooseMode md) }}
                >{{ modeLabel md }}</button>
              |]
         in Just
              [hh|
                <div class="assistant-choices">
                  <div class="assistant-mode" role="group" aria-label="Mode">
                    {% for md in [minBound .. maxBound] %}
                      {{ modeButton md }}
                    {% end %}
                  </div>
                  <label class="assistant-choice">
                    <span>Model</span>
                    <select class="assistant-model-choice" disabled={{ st.talking }} onValueChange={{ ChooseModel }}>
                      {% if not (null choices.recommended) %}
                        <optgroup label="Recommended">
                          {% for m in choices.recommended %}
                            {{ option m }}
                          {% end %}
                        </optgroup>
                      {% end %}
                      {% if not (null choices.others) %}
                        <optgroup label="All models">
                          {% for m in choices.others %}
                            {{ option m }}
                          {% end %}
                        </optgroup>
                      {% end %}
                      <option value="" selected={{ other }}>Other…</option>
                    </select>
                  </label>
                  {% if other %}
                    <input type="text" class="assistant-model-other" spellcheck="false" aria-label="The model's id" placeholder="A model's id" value={{ if listed then "" else chosen.text }} disabled={{ st.talking }} onValueChange={{ TypedModel }}>
                  {% end %}
                  <label class="assistant-choice">
                    <span>Effort</span>
                    <select class="assistant-effort" disabled={{ st.talking }} onValueChange={{ ChooseEffort }}>
                      {% for e in efforts s.provider %}
                        <option value={{ e }} selected={{ effortNow st s == e }}>{{ e }}</option>
                      {% end %}
                    </select>
                  </label>
                </div>
              |]
      _ -> Nothing

    -- Enter sends and Escape closes the panel; any other key goes to the
    -- message box.
    sendKey :: Event -> Maybe (Action m)
    sendKey ev
      | sendsMessage ev = Just Send
      | closesPanel ev = Just Close
      | otherwise = Nothing

    -- The service the text goes to, named as the line above names it,
    -- for a page of an encrypted space.
    leavesFor :: State m -> Maybe Text
    leavesFor st = case st.input.settings of
      Just (Right s)
        | st.input.encrypted ->
            let ps = settingsOf s.provider s
             in Just (if isJust (baseUrlOf s.provider ps) then serviceName s.provider ps else providerLabel s.provider)
      _ -> Nothing

    usingLine :: State m -> Text
    usingLine st = case st.input.settings of
      Just (Right s) ->
        let ps = settingsOf s.provider s
            -- A provider at another address (a gateway, a server on this
            -- machine) is named by its URL's host.
            service = if isJust (baseUrlOf s.provider ps) then serviceName s.provider ps else providerLabel s.provider
         in modelLabel s.provider ps (chosenModel s.provider ps) <> " \183 " <> service
      _ -> ""

    setupNote :: State m -> Maybe (H.ComponentHTML (Action m) Empty m)
    setupNote st = case st.input.settings of
      Nothing -> Just [hh|<p class="loading">Loading your settings…</p>|]
      Just (Left why) -> Just [hh|<p class="error assistant-setup" role="alert">{{ why }}</p>|]
      Just (Right s)
        | (settingsOf s.provider s).unreachable ->
            let ps = settingsOf s.provider s
             in Just
                  [hh|
                    <p class="assistant-setup assistant-unreachable" role="alert">
                      Cannot reach {{ serviceName s.provider ps }} from this network.
                      <button type="button" class="assistant-try-again" disabled={{ Set.member s.provider st.input.checking }} onClick={{ const (TryAgain s.provider) }}>Try again</button>
                    </p>
                  |]
        | Just why <- st.input.problem -> Just [hh|<p class="error assistant-setup" role="alert">{{ why }}</p>|]
      Just (Right s) -> case ready s of
        Left why ->
          Just
            [hh|
              <p class="assistant-setup">
                {{ why <> " " }}
                <a {{ optionsLinkProps st.input.optionsLink Followed }}>Open the assistant's settings</a>
              </p>
            |]
        Right _ -> Nothing

    -- What is sent with the next message: the selection, with a button to
    -- detach it, or the page.
    scopeView :: State m -> H.ComponentHTML (Action m) Empty m
    scopeView st = case attached st of
      Just sp ->
        [hh|
          <span class="assistant-chip">
            {{ chipLabel (lineSpan st.input.body sp) }}
            <button type="button" class="assistant-detach" title="Send without the selection" aria-label="Send without the selection" onClick={{ const Detach }}>×</button>
          </span>
        |]
      Nothing -> [hh|<span class="assistant-whole">{{ scopeText (capNow st) (T.length st.input.body) (isJust st.sentCopy) }}</span>|]

    entryView :: State m -> Entry -> H.ComponentHTML (Action m) Empty m
    entryView st = \case
      FromUser m ->
        [hh|
          <div class="assistant-message from-user">
            <p class="assistant-text">{{ m.asked }}</p>
            {% for c in maybeToList m.chip %}
              <span class="assistant-chip">{{ c }}</span>
            {% end %}
            {% for n in maybeToList m.sent %}
              <p class="assistant-sent">{{ n }}</p>
            {% end %}
            <p class="assistant-meta"><span class="assistant-time">{{ m.askedAt }}</span></p>
          </div>
        |]
      FromModel r ->
        [hh|
          <div class="assistant-message from-model{% if working r.stage %} streaming{% end %}">
            {% if not (T.null r.thought) %}
              <details class="assistant-thought">
                <summary>Thought</summary>
                <p class="assistant-thought-text">{{ r.thought }}</p>
              </details>
            {% end %}
            {% if not (T.null r.said) %}
              <p class="assistant-text">{{ r.said }}{% if r.stage == Writing %}<span class="assistant-cursor" aria-hidden="true"></span>{% end %}</p>
            {% end %}
            {% if not (null r.notes) %}
              <ul class="assistant-notes">
                {% for n in r.notes %}
                  {{ noteView n }}
                {% end %}
              </ul>
            {% end %}
            {% if r.stage == Waiting %}
              {{ thinkingView r }}
            {% end %}
            {% if r.stage == Editing %}
              <p class="assistant-status">Editing the page{{ dots }}</p>
            {% end %}
            {% if r.stage == Stopped %}
              <p class="assistant-stopped">Stopped.</p>
            {% end %}
            <p class="assistant-meta">
              <span class="assistant-time">{{ r.clock }}</span>
              {% for d in maybeToList r.took %}
                {% if r.stage == Ended %}
                  <span class="assistant-took" title="How long the answer took">{{ duration d }}</span>
                {% end %}
              {% end %}
              {% if not (working r.stage) %}
                {% for u in maybeToList (turnUsage r.used) %}
                  <span class="assistant-tokens" title="The tokens of this answer, as the service reported them">{{ usageText u }}</span>
                {% end %}
              {% end %}
            </p>
          </div>
        |]
      Trouble why -> [hh|<p class="error assistant-error" role="alert">{{ why }}</p>|]
      KeyRefused meaning detail ->
        [hh|
          <div class="error assistant-error assistant-key-refused" role="alert">
            <p>
              {{ meaning <> " " }}
              <a class="assistant-settings-open" {{ optionsLinkProps st.input.optionsLink Followed }}>Open the assistant's settings</a>
            </p>
            {% if not (T.null detail) %}
              <p class="assistant-error-detail">{{ detail }}</p>
            {% end %}
          </div>
        |]
      Notice what -> [hh|<p class="assistant-notice" role="status">{{ what }}</p>|]
      where
        -- The line that says the model is at work while nothing arrives,
        -- with the seconds waited once there are a few, and the summary of
        -- the model's thinking, if the provider sends one, collapsed under
        -- it.
        thinkingView :: Reply -> H.ComponentHTML (Action m) Empty m
        thinkingView r
          | T.null r.thinking = [hh|<p class="assistant-thinking">Thinking{{ dots }}{{ waited r }}</p>|]
          | otherwise =
              [hh|
                <details class="assistant-thinking">
                  <summary>Thinking{{ dots }}{{ waited r }}</summary>
                  <p class="assistant-thinking-text">{{ r.thinking }}</p>
                </details>
              |]
        waited :: Reply -> Maybe (H.ComponentHTML (Action m) Empty m)
        waited r = case st.now of
          Just t
            | diffUTCTime t r.quietSince >= quietAfter ->
                Just [hh|<span class="assistant-waited" aria-hidden="true">{{ duration (diffUTCTime t r.quietSince) }}</span>|]
          _ -> Nothing
        -- Three dots that blink in turn; hidden from screen readers, which
        -- would otherwise read them out.
        dots :: H.ComponentHTML (Action m) Empty m
        dots = [hh|<span class="assistant-dots" aria-hidden="true"><span>.</span><span>.</span><span>.</span></span>|]

    noteView :: Note -> H.ComponentHTML (Action m) Empty m
    noteView = \case
      Made what -> [hh|<li class="assistant-made">{{ what }}</li>|]
      NotMade what why -> [hh|<li class="assistant-not-made">{{ what }}: not made, {{ why }}</li>|]

    handleQuery :: Query a -> H.HalogenM (State m) (Action m) Empty Output m (Maybe a)
    handleQuery = \case
      Hide next -> stop $> Just next
      UseProvider p next -> useProvider p $> Just next

    handleAction :: Action m -> H.HalogenM (State m) (Action m) Empty Output m ()
    handleAction = \case
      Initialize -> do
        gets (\st -> st.input.models) >>= \ms -> H.liftEffect ms.prepare
        channel <- H.sync (liftIO HS.create)
        void (H.subscribe (FromTalk <$> channel.emitter))
        modify (\st -> st {listener = Just channel.listener})
      Finalize -> do
        gets (\st -> (st.input.models, st.talk)) >>= \(ms, talk) -> traverse_ (H.liftEffect . ms.close) talk
        endTurn
      Receive input -> modify $ \st ->
        -- A detached selection stays detached until another is made.
        st {input, detached = if input.selection == st.input.selection then st.detached else Nothing}
      Typed t -> modify (\st -> st {message = t})
      Shortcut s -> do
        modify (\st -> st {message = shortcutText s})
        gets (\st -> st.field) >>= H.getRef >>= traverse_ (H.liftEffect . focusElement)
      Send -> send
      Stop -> stop
      Detach -> modify (\st -> st {detached = st.input.selection})
      FromTalk heard -> hear heard
      Decided d -> H.raise (DecideAll d)
      Close -> do
        stop
        endTurn
        H.raise Closed
        -- Back to the page's text, where the user was before the panel.
        gets (\st -> st.input.document) >>= traverse_ (\d -> H.liftEffect (liftIO d.focus))
      Followed f -> gets (\st -> st.input.optionsLink) >>= (`followOptionsLink` f)
      ChooseMode md -> changeSettings (withMode md)
      ChooseModel "" -> modify (\st -> st {otherModel = True})
      ChooseModel m -> do
        modify (\st -> st {otherModel = False})
        changeSettings (\s -> withModel s.provider m s)
      TypedModel m -> unless (T.null (T.strip m)) $ changeSettings (\s -> withModel s.provider m s)
      ChooseEffort e -> modify (\st -> st {effortChoice = Just e})
      TryAgain p -> H.raise (Retry p)
      NewConversation -> do
        -- "New" is disabled while the model works; the check here is the
        -- one that holds, read in the same step as the chat is closed.
        busy <- gets (\st -> st.talking)
        unless busy $ do
          endConversation
          -- The messages go, errors among them. The message box keeps its
          -- draft and its selection, the pickers what they show, and the
          -- changes still to decide stay the editor's.
          modify (\st -> st {entries = []})
      Tick -> do
        t <- H.sync (liftIO getCurrentTime)
        modify (\st -> st {now = Just t})

    -- The provider the editor's button asked for (which the editor makes
    -- the assistant's provider). If the conversation was with another
    -- provider, it ends, and a new one starts with the next message.
    useProvider :: Provider -> H.HalogenM (State m) (Action m) Empty Output m ()
    useProvider p = do
      st <- get
      -- The effort picked was for the other provider's conversation.
      when (((\s -> s.provider) <$> (rightToMaybe =<< st.input.settings)) /= Just p) $
        modify (\s' -> s' {effortChoice = Nothing})
      for_ st.using $ \r -> when (r.provider /= p) $ do
        endConversation
        modify (\s' -> s' {otherModel = False})
        addEntry (Notice ("The conversation with " <> providerLabel r.provider <> " ended. The next message starts a new one with " <> providerLabel p <> "."))

    -- End the conversation: the chat is closed and dropped, so that the
    -- next message opens a new one ('send'), as the editor's first message
    -- does. Closing the chat stops its turn, and no more of its events
    -- arrive. The chat is closed and dropped in one step, so no action
    -- between finds a closed chat in the state. A turn under way is then
    -- marked as stopped. What the panel shows is the caller's to change.
    -- The changes the model made stay the editor's, to accept or reject
    -- there or with the panel's "Accept all" and "Reject all". The new chat
    -- is told nothing of them: its model did not make them, so 'made' and
    -- 'absorbed' are emptied. 'nextChange' stays, since a change's id must
    -- stay unique in the editor. The copy of the page last sent goes too,
    -- so that the new chat's first message carries the whole page.
    endConversation :: H.HalogenM (State m) (Action m) Empty Output m ()
    endConversation = do
      st <- get
      for_ st.talk (H.liftEffect . st.input.models.close)
      modify (\s' -> s' {talk = Nothing, using = Nothing, made = mempty, absorbed = [], sentCopy = Nothing, spent = []})
      when st.talking (finish Stopped)

    -- Change the user's settings (the model); the editor stores them.
    changeSettings :: (Settings -> Settings) -> H.HalogenM (State m) (Action m) Empty Output m ()
    changeSettings = H.raise . ChangeSettings

    stop :: H.HalogenM (State m) (Action m) Empty Output m ()
    stop = do
      st <- get
      when st.talking $ traverse_ (H.liftEffect . st.input.models.stop) st.talk

    send :: H.HalogenM (State m) (Action m) Empty Output m ()
    send = do
      st <- get
      let text = T.strip st.message
      unless (st.talking || T.null text) $ case command text of
        -- A command is the panel's, before anything of a message is sent:
        -- no selection attached, no copy of the page sent, no chat made.
        Just (Right Prompt.New) -> do
          handleAction NewConversation
          modify (\s' -> s' {message = ""})
        Just (Right Help) -> do
          traverse_ (addEntry . Notice) commandHelp
          modify (\s' -> s' {message = ""})
        -- An unknown command stays in the box, to be corrected.
        Just (Left word) -> addEntry (Notice (unknownCommand word))
        Nothing -> sendMessage st text

    -- Send the message to the model: the chat opened if none is, the
    -- page's context worked out, the turn begun.
    sendMessage :: State m -> Text -> H.HalogenM (State m) (Action m) Empty Output m ()
    sendMessage st text = case (st.input.settings, st.input.document) of
      (Just (Right s), Just document) -> case ready s of
        Left why -> addEntry (Trouble why)
        Right kept -> do
          -- The effort of this conversation, which the settings do not
          -- keep.
          let r = Ready {provider = kept.provider, model = kept.model, key = kept.key, effort = effortNow st s, baseUrl = kept.baseUrl}
              md = s.chatMode
          body <- H.liftEffect (liftIO document.text)
          decided <- outcomes document
          -- The clock read on the loop, without a wait ('H.liftEffect'):
          -- from the check above to the turn begun below is one step.
          -- Read through 'H.async', the second click of a double click
          -- came between, found the panel idle as well, and sent the
          -- message again in a conversation of its own.
          sentAt <- H.liftEffect (liftIO getCurrentTime)
          shown <- H.liftEffect (clockTime sentAt)
          let cap = pageCapOf s
              context = pageContext cap st.input.title body st.sentCopy (attached st)
              chip = case context of
                Selected {lineRange} -> Just (chipLabel lineRange)
                _ -> Nothing
              reply = Reply {said = "", notes = [], stage = Waiting, thinking = "", thought = "", clock = shown, began = sentAt, quietSince = sentAt, took = Nothing, used = []}
          talk <- maybe (open md r) pure st.talk
          modify $ \s' ->
            s'
              { entries = s'.entries <> [FromUser Said {asked = text, chip, sent = sentNote context, askedAt = shown}, FromModel reply]
              , message = ""
              , talking = True
              , talk = Just talk
              , using = Just r
              , detached = Nothing
              , sentCopy = copyAfter cap body context st.sentCopy
              , now = Just sentAt
              , turnMode = md
              }
          startTicker
          countTurn
          H.liftEffect (st.input.models.say talk (requestFor md r) (userTurn md decided context text))
      (Just (Left why), _) -> addEntry (Trouble why)
      _ -> pass

    -- The chat, opened with the settings' provider, model and key, its
    -- turn events handled by the panel.
    open :: Mode -> Ready -> H.HalogenM (State m) (Action m) Empty Output m Talk
    open md r = do
      (tell, ms) <- gets (\st -> (maybe (const pass) HS.notify st.listener, st.input.models))
      H.liftEffect (ms.open Opening {ready = r, request = requestFor md r} tell)

    hear :: Heard -> H.HalogenM (State m) (Action m) Empty Output m ()
    hear = \case
      HeardText t -> modifyReply (\r -> (settle r) {said = r.said <> t, stage = Writing})
      HeardThinking t -> modifyReply (\r -> r {thinking = r.thinking <> t})
      HeardToolStart _ -> modifyReply (\r -> (settle r) {stage = Editing})
      HeardUsage u -> do
        modifyReply (\r -> r {used = r.used <> [u]})
        modify (\st -> st {spent = st.spent <> [u]})
      HeardCall call name input -> do
        (edited, told, ok) <- edit name input
        -- The model goes on once it has the answer: a new wait.
        t <- H.sync (liftIO getCurrentTime)
        modifyReply (\r -> (settle r) {notes = r.notes <> [edited], stage = Waiting, quietSince = t})
        modify (\st -> st {now = Just t})
        gets (\st -> (st.input.models, st.talk)) >>= \(ms, talk) -> traverse_ (\chat -> H.liftEffect (ms.answer chat call told ok)) talk
      HeardDone -> do
        finish Ended
        reached True
      -- The chat that would have taken the message could not be made, and
      -- the next message goes to a new one, which has no copy of the page.
      -- The event comes before the turn's end, while no message can be
      -- sent, so no message between reads the copy now taken back.
      HeardUnsent -> modify (\st -> st {sentCopy = Nothing})
      HeardStopped -> finish Stopped
      HeardFailed e -> do
        finish Ended
        using <- gets (\st -> st.using)
        let key = (\r -> r.key) <$> using
            service = maybe "The provider" serviceOf using
        addEntry $
          if e.kind == BadKey
            then KeyRefused (errorMeaning service e) (errorDetail key e)
            else Trouble (errorText service key e)
        -- Any response, including a refusal, comes from the provider; no
        -- response means it cannot be reached.
        reached (e.kind /= Unreachable)

    -- Report to the editor whether the conversation's provider was
    -- reached, when that changes what is known.
    reached :: Bool -> H.HalogenM (State m) (Action m) Empty Output m ()
    reached yes = do
      st <- get
      for_ st.using $ \r -> case st.input.settings of
        Just (Right s) | (settingsOf r.provider s).unreachable == yes -> H.raise (Reached r.provider yes)
        _ -> pass

    -- A tool call's edit, made in the editor as a tracked change. The
    -- result is the conversation's note on it, the tool result for the
    -- model, and whether the edit was made.
    edit :: Text -> Value -> H.HalogenM (State m) (Action m) Empty Output m (Note, Text, Bool)
    edit name input = case parseEdit name input of
      Left why -> pure (NotMade ("A call of " <> name) (refusalWords why), answer (Left why :: Either Refusal ()), False)
      Right e -> do
        st <- get
        case st.input.document of
          -- In Ask mode the tools are off; a model that calls one anyway
          -- is told so, and the page is left as it is.
          _
            | st.turnMode == Ask ->
                pure (NotMade (describeEdit e) "edits are off in Ask mode", "Not applied: the user is in Ask mode, where you do not change the page. Answer in words, and quote any change you suggest.", False)
          Nothing -> pure (NotMade (describeEdit e) "the editor is not ready", "Not applied: the editor is not ready. Try again later.", False)
          Just document ->
            H.liftEffect (liftIO (document.propose st.nextChange e)) >>= \case
              Left why -> pure (NotMade (describeEdit e) (refusalWords why), answer (Left why :: Either Refusal ()), False)
              Right (r, taken) -> do
                modify $ \s ->
                  s
                    { nextChange = s.nextChange + 1
                    , made = Map.insert s.nextChange (describeEdit e) s.made
                    , absorbed = taken <> s.absorbed
                    }
                pure (Made (describeEdit e), answer (Right r), True)

    -- The outcomes of the model's changes since it was last told: changes
    -- decided, or removed without a decision (undone, or edited away),
    -- each reported once. Pending changes wait, and changes absorbed by a
    -- later change need no mention.
    outcomes :: Document -> H.HalogenM (State m) (Action m) Empty Output m [(Text, Outcome)]
    outcomes document = do
      pending <- map (\c -> c.id) <$> H.liftEffect (liftIO document.tracked)
      st <- get
      let gone = Map.filterWithKey (\i _ -> i `notElem` pending) st.made
          outcome i = case Map.lookup i st.input.decisions of
            Just Accept -> Just Accepted
            Just Reject -> Just Rejected
            Nothing
              | i `elem` st.absorbed -> Nothing
              | otherwise -> Just Undone
      modify (\s -> s {made = Map.difference s.made gone})
      pure [(what, o) | (i, what) <- Map.toList gone, Just o <- [outcome i]]

    -- The model's message ended (or was stopped): how long it took, and
    -- the ticker stopped.
    finish :: Stage -> H.HalogenM (State m) (Action m) Empty Output m ()
    finish stage = do
      t <- H.sync (liftIO getCurrentTime)
      modifyReply (\r -> (settle r) {stage, took = Just (diffUTCTime t r.began)})
      gets (\st -> st.ticker) >>= traverse_ H.kill
      modify (\st -> st {talking = False, ticker = Nothing, now = Nothing})
      endTurn

    -- The model's turn counted as the program's work. The turn's events
    -- arrive through an emitter, which no fiber waits on, so without this
    -- fork the page is idle the moment Send has rendered, and a test that
    -- waits for the program to be quiet (haskell-halogen-playwright's
    -- waitQuiet) would read the panel before the answer. The fork waits in
    -- an H.async until 'endTurn' fills the turn's variable: when the turn
    -- ends ('finish': done, stopped or failed), the panel closes, or the
    -- component goes. Making the variable and filling it do not wait, and
    -- run through 'H.liftEffect': 'send' begins the turn and sends the
    -- message in one step, so a Stop clicked right after finds the message
    -- sent, and stops its turn. Through 'H.async' the message went out
    -- after the wait, even when Stop had come in it.
    countTurn :: H.HalogenM (State m) (Action m) Empty Output m ()
    countTurn = do
      endTurn
      ended <- H.liftEffect (liftIO newEmptyMVar)
      modify (\st -> st {ended = Just ended})
      void (H.fork (H.async (liftIO (takeMVar ended))))

    -- Let the fork of 'countTurn' end, if a turn is counted.
    endTurn :: H.HalogenM (State m) (Action m) Empty Output m ()
    endTurn = do
      gets (\st -> st.ended) >>= traverse_ (\ended -> H.liftEffect (liftIO (void (tryPutMVar ended ()))))
      modify (\st -> st {ended = Nothing})

    -- Read the time every second while the model is at work, for the
    -- seconds counter.
    startTicker :: H.HalogenM (State m) (Action m) Empty Output m ()
    startTicker = do
      gets (\st -> st.ticker) >>= traverse_ H.kill
      ticker <- H.fork . forever $ do
        H.delay 1
        handleAction Tick
      modify (\st -> st {ticker = Just ticker})

    -- Modify the model's last message, if the last entry is one.
    modifyReply :: (Reply -> Reply) -> H.HalogenM (State m) (Action m) Empty Output m ()
    modifyReply f = modify $ \st -> case unsnoc st.entries of
      Just (before, FromModel r) -> st {entries = before <> [FromModel (f r)]}
      _ -> st

    addEntry :: Entry -> H.HalogenM (State m) (Action m) Empty Output m ()
    addEntry e = modify (\st -> st {entries = st.entries <> [e]})

-- | Whether a keydown in the message box sends the message: Enter, but not
-- with Shift (a new line) nor while a character is being composed. It is
-- pure, so that the keydown listener can decide synchronously, and prevent
-- Enter from inserting a new line.
sendsMessage :: Event -> Bool
sendsMessage event =
  let key = KeyboardEvent event
   in key.key == "Enter" && not key.shiftKey && not key.isComposing && not (key.ctrlKey || key.metaKey || key.altKey)

-- | Whether a keydown in the message box closes the panel: Escape, but not
-- in the middle of composing a character, where Escape cancels the
-- composition.
closesPanel :: Event -> Bool
closesPanel event =
  let key = KeyboardEvent event
   in key.key == "Escape" && not key.isComposing

-- | The time of day of a moment, as the chat shows it beside a message:
-- in the browser's time zone, written the way the browser's language
-- writes a short time ("14:32", or "2:32 PM"). Natively, where there is
-- no browser, hours and minutes in UTC.
clockTime :: (MonadIO m) => UTCTime -> m Text
clockTime t
  | inBrowser = liftIO (fromJSText <$> js_clock (realToFrac (utcTimeToPOSIXSeconds t) * 1000))
  | otherwise = pure (toS (formatTime defaultTimeLocale "%H:%M" t))

-- | The model's message once the model has gone on after a wait. The
-- summary of the thinking done in the wait is added to the summaries
-- before it, which the message shows as "Thought".
settle :: Reply -> Reply
settle r
  | T.null (T.strip r.thinking) = r {thinking = ""}
  | T.null r.thought = r {thought = T.strip r.thinking, thinking = ""}
  | otherwise = r {thought = r.thought <> "\n\n" <> T.strip r.thinking, thinking = ""}

-- | The effort the next message is sent with: the one picked in the
-- panel for this conversation, if the provider offers it, or else
-- the provider's default from the settings.
effortNow :: State m -> Settings -> Text
effortNow st s = case st.effortChoice of
  Just e | e `elem` efforts s.provider -> e
  _ -> effortOf (settingsOf s.provider s)

-- | A duration as the chat shows it: "8 s", "1 min 12 s".
duration :: NominalDiffTime -> Text
duration d
  | seconds < 60 = show seconds <> " s"
  | otherwise = show (seconds `div` 60) <> " min " <> show (seconds `mod` 60) <> " s"
  where
    seconds = floor d :: Int

-- | The selection sent with the next message, unless it was detached.
attached :: State m -> Maybe Span
attached st = mfilter (\sp -> Just sp /= st.detached && sp.to > sp.from) st.input.selection

-- | The request a message is sent with, by provider, in a mode. The
-- scripted provider receives Anthropic's request, which its tests read.
requestFor :: Mode -> Ready -> Value
requestFor md r = case r.provider of
  OpenAI -> openAiRequest md r.model r.effort
  Anthropic -> anthropicRequest md r.model r.effort
  Scripted -> anthropicRequest md r.model r.effort

-- | The page's cap in force: the user's setting, once the settings are
-- read.
capNow :: State m -> Int
capNow st = maybe defaultBodyCap pageCapOf (rightToMaybe =<< st.input.settings)

-- | The mode the next message is sent in: the settings', once they are
-- read.
modeNow :: State m -> Mode
modeNow st = maybe Agent (\s -> s.chatMode) (rightToMaybe =<< st.input.settings)

-- | What a mode's button says on hover.
modeTitle :: Mode -> Text
modeTitle = \case
  Ask -> "Ask: the assistant answers, and changes nothing in the page"
  Agent -> "Agent: the assistant changes the page, each change for you to accept or reject"

-- | Why an edit was not made, as text for the conversation.
refusalWords :: Refusal -> Text
refusalWords = \case
  NotFound _ -> "the page has no such text"
  FoundMany _ n -> "the text is there " <> show n <> " times"
  EmptyQuote -> "it quoted nothing"
  UnknownTool _ -> "there is no such tool"
  Malformed _ -> "its input was not what the tool takes"

changesWord :: Int -> Text
changesWord 1 = "1 suggested change"
changesWord n = show n <> " suggested changes"
