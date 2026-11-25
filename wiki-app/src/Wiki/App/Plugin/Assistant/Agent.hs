-- | The writing assistant's agent: the assistant plugin's 'Agent'
-- ("Wiki.App.Plugin"), which the editor mounts for its whole life.
--
-- The agent holds the user's assistant settings for the editor
-- ("Wiki.App.Plugin.Assistant.Settings"): its options, as the host gives
-- them; its keys, the plugin's secrets, read when the editor opens and
-- again each time the secrets change; and what this browser learnt from
-- the providers. Once they are read, it asks each provider with a key for
-- its list of models: that list is what the panel offers, and also proof
-- that the provider can be reached from here. A provider that cannot be
-- reached (a network that blocks it) keeps its button, dimmed, with a
-- note, and the panel offers to try again. A change the panel makes is
-- split again: the options go to the host to keep, what was learnt to the
-- cache.
--
-- Its buttons are the providers' ('buttonsOf'), which the editor draws as
-- round bubbles at the top right of the text: a provider gets one once the
-- user has a key for it, and a user with no key has one button,
-- "Assistant", which opens the panel to say where keys go. Where the
-- user's vault is locked in this browser, the keys cannot be read: the
-- panel says to unlock it. A provider's button opens the panel with that
-- provider, or closes it if it is open with that provider. The panel
-- ("Wiki.App.Plugin.Assistant.Panel") is mounted the first time a button
-- opens it, and kept with its conversation while the editor shows the
-- page.
module Wiki.App.Plugin.Assistant.Agent
  ( component
  , buttonsOf
  , shownSettings
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.UUID (MonadUUID)
import Data.Row (type (.==))
import Data.Time (getCurrentTime)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.Machine qualified as Machine
import Halogen.Sync qualified as Sync
import Protolude hiding (State)
import Wiki.App.Plugin (AgentButton (..), AgentInput (..), AgentOutput (..), AgentQuery (..), Capabilities (..), Editing (..), Face (..), Local (..), SecretField (..), Secrets (..), Unavailable (..))
import Wiki.App.Plugin.Assistant.Icons (claudeColour, claudePath, openAiPath)
import Wiki.App.Plugin.Assistant.Panel qualified as Panel
import Wiki.App.Plugin.Assistant.Settings qualified as Settings
import Wiki.App.Plugin.Capability.Assistant qualified as Capability (AssistantCapabilities (..), Models (..))
import Wiki.Core.Assistant
  ( Provider (..)
  , ProviderSettings (..)
  , Settings (..)
  , baseUrlOf
  , chooseProvider
  , listStale
  , providerKey
  , providers
  , serviceName
  , settingsOf
  , usable
  , withReached
  )

type Slots = ("panel" .== H.Slot Panel.Query Panel.Output ())

data State m = State
  { input :: AgentInput m
  , machine :: Machine.Machine () Settings.Event Settings.Model
  -- ^ The user's assistant settings, as read and changed
  -- ("Wiki.App.Plugin.Assistant.Settings").
  , scripted :: Bool
  -- ^ Whether a test installed the scripted provider, which gets a button
  -- of its own.
  , panelMounted :: Bool
  -- ^ Whether a button has opened the panel yet: it is then mounted, and
  -- kept while the editor shows the page.
  , buttons :: [AgentButton]
  -- ^ The buttons last told to the editor.
  }

data Action m
  = Initialize
  | Receive (AgentInput m)
  | FromPanel Panel.Output
  | SecretsMoved

component :: forall m. (Typeable m, MonadUUID m, MonadUnliftIO m) => H.Component AgentQuery (AgentInput m) AgentOutput m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = \input -> pure State {input, machine = Machine.new (Settings.initial (readerOf input) input.options), scripted = False, panelMounted = False, buttons = []}
      , render
      , eval =
          H.mkEval
            H.defaultEval
              { H.handleAction = handleAction
              , H.handleQuery = handleQuery
              , H.initialize = Just Initialize
              , H.receive = Just . Receive
              }
      }
  where
    -- Nothing until a button opens the panel; then the panel, while the
    -- editor shows the page.
    render :: State m -> H.ComponentHTML (Action m) Slots m
    render st = case st.input.editing of
      Just editing | st.panelMounted -> HH.slot "panel" () Panel.component (panelInput st editing) FromPanel
      _ -> HH.text ""

    panelInput :: State m -> Editing -> Panel.Input m
    panelInput st editing =
      Panel.Input
        { optionsLink = st.input.can.optionsLink
        , models = st.input.can.assistant.models
        , settings = shownSettings m
        , problem = m.problem
        , checking = m.checking
        , document = editing.document
        , title = editing.title
        , body = editing.body
        , selection = editing.selection
        , pending = editing.pending
        , decisions = editing.decisions
        , encrypted = editing.encrypted
        }
      where
        m = Machine.current st.machine

    handleQuery :: AgentQuery a -> H.HalogenM (State m) (Action m) Slots AgentOutput m (Maybe a)
    handleQuery = \case
      Press key opening next -> do
        case (find (\p -> providerKey p == key) [minBound .. maxBound], opening) of
          (_, False) -> H.tell "panel" () Panel.Hide
          (Nothing, True) -> modify (\st -> st {panelMounted = True})
          (Just p, True) -> openWith p
        pure (Just next)

    handleAction :: Action m -> H.HalogenM (State m) (Action m) Slots AgentOutput m ()
    handleAction = \case
      -- After the first render, which a prerendered page shows.
      Initialize -> do
        models <- gets (\st -> st.input.can.assistant.models)
        scripted <- H.liftEffect models.scripted
        when scripted $ do
          modify (\st -> st {scripted})
          publish
        -- The assistant's settings, read for the reader: the page loads
        -- meanwhile. The secrets' changes are heard from before the first
        -- read, so that none goes unheard.
        here <- gets (\st -> st.input.can.here)
        whenM (H.liftEffect here) $ do
          changes <- gets (\st -> st.input.can.secrets.changes) >>= H.liftEffect
          void (H.subscribe (SecretsMoved <$ changes))
          gets (\st -> Settings.Opened (readerOf st.input) st.input.options (Just st.scripted)) >>= happen
      -- The input, and the reader and the options in it, in one step.
      Receive input -> do
        modify (\st -> st {input})
        whenM (H.liftEffect input.can.here) (happen (Settings.Given (readerOf input) input.options))
      SecretsMoved -> happen Settings.SecretsChanged
      FromPanel Panel.Closed -> H.raise Closed
      FromPanel (Panel.ChangeSettings f) -> happen (Settings.Changed f)
      FromPanel (Panel.Retry p) -> happen (Settings.Check p)
      FromPanel (Panel.Reached p reached) -> happen (Settings.Changed (withReached p reached))
      FromPanel (Panel.DecideAll d) -> H.raise (DecideAll d)

    happen :: Settings.Event -> H.HalogenM (State m) (Action m) Slots AgentOutput m ()
    happen = Machine.happen machineSpec

    machineSpec :: Machine.Spec () (State m) (Action m) Slots AgentOutput m Settings.Event Settings.Effect Settings.Model
    machineSpec = Machine.Spec {Machine.step = Settings.step, Machine.carry = carry, Machine.get = \st -> st.machine, Machine.set = \machine st -> st {machine}}

    -- Each ask made with the capabilities the input last gave: those of
    -- the reader the step asked for.
    carry :: Settings.Effect -> Machine.Carry () (State m) (Action m) Slots AgentOutput m Settings.Event
    carry = \case
      Settings.ReadKeys n -> Machine.AskWith (gets (\st -> st.input.can.secrets.get)) Machine.run (Settings.KeysRead n . either (Left . Failed . toS . displayException) identity)
      Settings.ReadCache n -> Machine.AskWith (gets (\st -> st.input.can.local.load)) Machine.run (Settings.CacheRead n . join . rightToMaybe)
      Settings.SaveCache n v -> Machine.AskWith (gets (\st -> st.input.can.local.save v)) Machine.run (Settings.CacheSaved n . either (Left . toS . displayException) identity)
      Settings.SetOptions o -> Machine.Now (Sync.raise (SetOptions o))
      Settings.ListModels p r -> Machine.AskWith (gets (\st -> st.input.can.assistant.models.list r)) (\listing -> Machine.run ((,) <$> liftIO getCurrentTime <*> listing)) (Settings.listedAnswer p r)
      Settings.Shown -> Machine.Now publish

    -- A provider's button pressed to open the panel with it: the panel
    -- mounted, if it was not, before it is told the provider, and the
    -- provider made the assistant's. A model list more than a day old, or
    -- a provider that could not be reached, is asked again.
    openWith :: Provider -> H.HalogenM (State m) (Action m) Slots AgentOutput m ()
    openWith p = do
      current <- gets (\st -> (\s -> s.provider) <$> Settings.settings (Machine.current st.machine))
      modify (\st -> st {panelMounted = True})
      H.tell "panel" () (Panel.UseProvider p)
      unless (current == Just p) $ happen (Settings.Changed (chooseProvider p))
      now <- H.sync (liftIO getCurrentTime)
      gets (\st -> Settings.settings (Machine.current st.machine)) >>= \case
        Just s | listStale now (settingsOf p s) || (settingsOf p s).unreachable -> happen (Settings.Check p)
        _ -> pass

-- | Tell the editor the buttons, when they changed.
publish :: (Sync.MonadSync (State m) (Action m) Slots AgentOutput m h) => h ()
publish = do
  st <- get
  let now = buttonsOf st.scripted (shownSettings (Machine.current st.machine))
  when (now /= st.buttons) $ do
    modify (\s -> s {buttons = now})
    Sync.raise (Buttons now)

-- | The reader a surface's input is for: the session's number and the
-- user, as the host made its secrets for.
readerOf :: AgentInput m -> Settings.Reader
readerOf input = input.can.secrets.field.reader

-- | The settings as the panel and the buttons show them: 'Nothing' until
-- they are read; where the vault is locked in this browser and no
-- provider can be asked without a key, why the assistant cannot be used.
shownSettings :: Settings.Model -> Maybe (Either Text Settings)
shownSettings m = case (Settings.settings m, Settings.lockedKeys m) of
  (Nothing, _) -> Nothing
  (Just s, Just why)
    | not (any (\p -> usable p (settingsOf p s)) [minBound .. maxBound]) -> Just (Left (lockedText why))
  (Just s, _) -> Just (Right s)
  where
    lockedText = \case
      Locked -> "Your vault is locked in this browser, and your API keys are sealed in it: unlock it in the assistant's settings, or under Settings, Security."
      NoUser -> "Sign in to use the assistant: its settings are kept for each user."
      Failed why -> "Your API keys could not be read: " <> why

-- | The buttons the editor shows for the assistant: one per provider the
-- user has a key for (OpenAI's API at another address needs none; the
-- scripted provider's only where a test installed it), dimmed, with the
-- reason, when the provider could not be reached the last time it was
-- asked; with no provider, one button, "Assistant", which opens the panel
-- to say where the keys go; and before the settings are read (and in a
-- prerendered page, which has none), no buttons.
buttonsOf :: Bool -> Maybe (Either Text Settings) -> [AgentButton]
buttonsOf scripted = \case
  Nothing -> []
  Just (Left _) -> [neutral]
  Just (Right s) -> case [p | p <- providers scripted, usable p (settingsOf p s)] of
    [] -> [neutral]
    shown -> map (providerButton s) shown
  where
    neutral = AgentButton {key = "assistant", label = "Assistant", title = "Assistant", ariaLabel = "Assistant", selected = True, unreachable = False, face = ChatGlyph}
    providerButton s p =
      let ps = settingsOf p s
          blocked = ps.unreachable
          cannotReach = "Cannot reach " <> serviceName p ps <> " from this network"
          -- A service other than OpenAI's own is named by its address, and
          -- does not get OpenAI's logo: it may serve any model. A gateway
          -- in front of Anthropic's API serves Claude, so its button stays
          -- Claude's; the panel names the gateway's host.
          (label, face) = case p of
            Anthropic -> ("Chat with Claude", Logo claudePath (Just claudeColour))
            OpenAI
              | isJust (baseUrlOf p ps) -> ("Chat through " <> serviceName p ps, ChatGlyph)
              | otherwise -> ("Chat with ChatGPT", Logo openAiPath Nothing)
            Scripted -> ("Chat with the scripted provider", Letter "S")
       in AgentButton
            { key = providerKey p
            , label
            , title = if blocked then cannotReach else label
            , ariaLabel = if blocked then label <> " (" <> cannotReach <> ")" else label
            , selected = s.provider == p
            , unreachable = blocked
            , face
            }
