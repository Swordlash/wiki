-- | The writing assistant's agent: the assistant plugin's 'Agent'
-- ("Wiki.App.Plugin"), which the editor mounts for its whole life.
--
-- The agent holds the user's assistant settings for the editor. It reads
-- them when the editor opens, and asks each provider with a key for its
-- list of models: that list is what the panel offers, and also proof that
-- the provider can be reached from here. A provider that cannot be reached
-- (a network that blocks it) keeps its button, dimmed, with a note, and
-- the panel offers to try again. The agent stores the changes the panel
-- makes to the settings.
--
-- Its buttons are the providers' ('buttonsOf'), which the editor draws as
-- round bubbles at the top right of the text: a provider gets one once the
-- user has a key for it, and a user with no key has one button,
-- "Assistant", which opens the panel to say where keys go. A provider's
-- button opens the panel with that provider, or closes it if it is open
-- with that provider. The panel ("Wiki.App.Plugin.Assistant.Panel") is
-- mounted the first time a button opens it, and kept with its
-- conversation while the editor shows the page.
module Wiki.App.Plugin.Assistant.Agent
  ( component
  , buttonsOf
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.UUID (MonadUUID)
import Data.Row (type (.==))
import Data.Set qualified as Set
import Data.Time (getCurrentTime)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Protolude hiding (State)
import Wiki.App.Plugin (AgentButton (..), AgentInput (..), AgentOutput (..), AgentQuery (..), Capabilities (..), Editing (..), Face (..))
import Wiki.App.Plugin.Assistant.Icons (claudeColour, claudePath, openAiPath)
import Wiki.App.Plugin.Assistant.Panel qualified as Panel
import Wiki.App.Plugin.Capability.Assistant (Keys)
import Wiki.App.Plugin.Capability.Assistant qualified as Capability (AssistantCapabilities (..), Keys (..), Models (..), Secret (..))
import Wiki.Core.Assistant
  ( AssistantError (kind)
  , ErrorKind (Unreachable)
  , Provider (..)
  , ProviderSettings (..)
  , Ready (baseUrl, key)
  , Settings (..)
  , baseUrlOf
  , chooseProvider
  , listStale
  , providerKey
  , providers
  , readyFor
  , serviceName
  , settingsOf
  , usable
  , withModels
  , withReached
  )

type Slots = ("panel" .== H.Slot Panel.Query Panel.Output ())

data State m = State
  { input :: AgentInput m
  , settings :: Maybe (Either Text Settings)
  -- ^ The user's assistant settings, once read (or why they could not
  -- be): which providers have a key and which cannot be reached, for the
  -- buttons, and the panel's settings.
  , problem :: Maybe Text
  -- ^ Why a change of the settings could not be saved, if it could not.
  , checking :: Set Provider
  -- ^ The providers being asked for their list of models.
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

component :: forall m. (Typeable m, MonadUUID m, MonadUnliftIO m) => H.Component AgentQuery (AgentInput m) AgentOutput m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = \input -> pure State {input, settings = Nothing, problem = Nothing, checking = mempty, scripted = False, panelMounted = False, buttons = []}
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
        , settings = st.settings
        , problem = st.problem
        , checking = st.checking
        , document = editing.document
        , title = editing.title
        , body = editing.body
        , selection = editing.selection
        , pending = editing.pending
        , decisions = editing.decisions
        , encrypted = editing.encrypted
        }

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
        -- The assistant's settings, and whether each provider with a key
        -- can be reached, in a fork: the page loads meanwhile.
        here <- gets (\st -> st.input.can.here)
        whenM (H.liftEffect here) $ void (H.fork loadAssistant)
      Receive input -> modify (\st -> st {input})
      FromPanel Panel.Closed -> H.raise Closed
      FromPanel (Panel.ChangeSettings f) -> changeAssistant f
      FromPanel (Panel.Retry p) -> checkProvider p
      FromPanel (Panel.Reached p reached) -> changeAssistant (withReached p reached)
      FromPanel (Panel.DecideAll d) -> H.raise (DecideAll d)

    -- A provider's button pressed to open the panel with it: the panel
    -- mounted, if it was not, before it is told the provider, and the
    -- provider made the assistant's. A model list more than a day old, or
    -- a provider that could not be reached, is asked again.
    openWith :: Provider -> H.HalogenM (State m) (Action m) Slots AgentOutput m ()
    openWith p = do
      current <- gets (\st -> (\s -> s.provider) <$> (rightToMaybe =<< st.settings))
      modify (\st -> st {panelMounted = True})
      H.tell "panel" () (Panel.UseProvider p)
      unless (current == Just p) $ changeAssistant (chooseProvider p)
      now <- H.sync (liftIO getCurrentTime)
      gets (\st -> st.settings) >>= \case
        Just (Right s) | listStale now (settingsOf p s) || (settingsOf p s).unreachable -> checkProvider p
        _ -> pass

    -- Tell the editor the buttons, when they changed.
    publish :: H.HalogenM (State m) (Action m) Slots AgentOutput m ()
    publish = do
      st <- get
      let now = buttonsOf st.scripted st.settings
      when (now /= st.buttons) $ do
        modify (\s -> s {buttons = now})
        H.raise (Buttons now)

    -- Read the user's assistant settings, then ask each provider that can
    -- be asked for its list of models. Settings that sync from the user's
    -- other devices and are still locked on this one, with nothing typed
    -- here meanwhile, say where to unlock them.
    loadAssistant :: H.HalogenM (State m) (Action m) Slots AgentOutput m ()
    loadAssistant =
      gets (\st -> st.input.user) >>= \case
        Nothing -> do
          modify (\st -> st {settings = Just (Left "Sign in to use the assistant: its settings are kept for each user.")})
          publish
        Just user -> do
          secret <- gets (\st -> st.input.can.assistant.secret)
          settings <- H.async (secret.followed user) <&> (>>= unlockedSettings)
          modify (\st -> st {settings = Just settings})
          publish
          scripted <- gets (\st -> st.scripted)
          for_ (rightToMaybe settings) $ \s ->
            traverse_ checkProvider [p | p <- providers scripted, usable p (settingsOf p s)]

    unlockedSettings :: Keys -> Either Text Settings
    unlockedSettings k
      | k.locked && not (any (\p -> usable p (settingsOf p k.settings)) [minBound .. maxBound]) =
          Left "Your keys sync from your other devices and are locked in this browser: unlock them with a passkey on the assistant's settings page."
      | otherwise = Right k.settings

    -- Change the assistant's settings: at once in what the buttons and the
    -- panel show, then in the stored settings, applying the change to them
    -- as they are now (a provider check may have changed them meanwhile).
    changeAssistant :: (Settings -> Settings) -> H.HalogenM (State m) (Action m) Slots AgentOutput m ()
    changeAssistant f = do
      modify (\st -> st {settings = fmap f <$> st.settings})
      publish
      gets (\st -> st.input.user)
        >>= traverse_
          ( \user -> do
              secret <- gets (\st -> st.input.can.assistant.secret)
              H.async (secret.keep user f) >>= \case
                Right k -> modify (\st -> st {settings = Just (Right k.settings), problem = Nothing}) >> publish
                Left why -> modify (\st -> st {problem = Just why})
          )

    -- Ask a provider for its list of models, in a fork: that is the list
    -- the panel offers, and whether the provider can be reached from here
    -- at all. Any response, even a refusal of the key, means it can be
    -- reached; no response (a network that blocks it) means it cannot. The
    -- result is kept only if the key and the address are still those it
    -- was asked with.
    checkProvider :: Provider -> H.HalogenM (State m) (Action m) Slots AgentOutput m ()
    checkProvider p = do
      st <- get
      case rightToMaybe =<< st.settings of
        Just s
          | Right r <- readyFor p s
          , not (Set.member p st.checking) -> do
              modify (\st' -> st' {checking = Set.insert p st'.checking})
              void . H.fork $ do
                now <- H.sync (liftIO getCurrentTime)
                result <- H.async (st.input.can.assistant.models.list r)
                modify (\st' -> st' {checking = Set.delete p st'.checking})
                changeAssistant $ \s' ->
                  if either (const False) (\r' -> r'.key == r.key && r'.baseUrl == r.baseUrl) (readyFor p s')
                    then case result of
                      Right listed -> withModels p listed now s'
                      Left e -> withReached p (e.kind /= Unreachable) s'
                    else s'
        _ -> pass

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
