{-# LANGUAGE QuasiQuotes #-}

-- | The external plugins of the editor (WP5.84 of docs/PLAN.md, decision
-- 69): a component the editor mounts for its whole life, which runs each
-- external plugin that is on in a hidden frame of its own, sends it the
-- text as last checked, and tells the editor what each plugin answered
-- ('Heard'), which the editor counts and marks with its other problems
-- ("Wiki.App.Plugin.External"'s @diagnosticsOf@).
--
-- Its logic is a machine ("Wiki.App.Plugin.External.Machine"); this module
-- carries out the machine's effects. What it draws is a line for each
-- plugin stopped, with why, and a line in an encrypted space, where no
-- external plugin runs. The frames go into an element Halogen draws empty
-- and does not look inside ("Wiki.App.Plugin.External.Frame").
--
-- A frame, the callback its messages come through, and the wait for its
-- answer are made in place and kept in the state in the same step, and
-- the finalizer closes every frame the state holds.
module Wiki.App.Plugin.External.Frames
  ( component
  , Input (..)
  , Output (..)
  )
where

import Control.Monad.UUID (MonadUUID)
import Data.Map.Strict qualified as Map
import Data.Row (Empty)
import Halogen qualified as H
import Halogen.HTML.Properties qualified as HP
import Halogen.Machine qualified as Machine
import Halogen.Subscription qualified as HS
import Halogen.Sync qualified as Sync
import Halogen.TH (hh)
import Protolude hiding (State)
import System.IO.Error (userError)
import Wiki.Api.Plugins (PluginId (..))
import Wiki.Api.Plugins.External (ExternalManifest (..), ExternalPlugin (..))
import Wiki.App.Backend (MonadLoad (..), MonadWiki (..), WikiError (..))
import Wiki.App.Islands.Monad (MonadIslands (..))
import Wiki.App.Plugin.External (Heard)
import Wiki.App.Plugin.External.Frame (FrameSaid, PluginFrame (..))
import Wiki.App.Plugin.External.Machine (Effect (..), Event (..), Input (..), Model (..), Stopped (..), answerWithin, initialRead, restInEncrypted, startWithin, step, stoppedPlugins)

-- | What the component tells the editor: what each external plugin that
-- runs last answered, whenever that changes.
newtype Output = Heard (Map PluginId Heard)

data State = State
  { machine :: Machine.Machine Int Event Model
  , container :: H.RefLabel
  -- ^ The element the frames go in.
  , opened :: Map Int PluginFrame
  -- ^ The frames open, by number.
  , listener :: Maybe (HS.Listener IO (Int, FrameSaid))
  -- ^ Where the frames' messages go, by frame number; made as the
  -- component starts.
  }

data Action
  = Initialize
  | Receive Input
  | FromFrame Int FrameSaid
  | Finalize

-- | The component, given the editor's 'Input' with each render.
component :: forall query m. (MonadLoad m, MonadIslands m, MonadUUID m, MonadIO m) => H.Component query Input Output m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = \input -> do
          container <- H.newRefLabel "editor-plugin-frames"
          -- Read at once when the backend can ('atOnce'), as the editor
          -- reads its page: on the server's prerender the read is then
          -- part of the render, and its answer in the page's preload, not
          -- a race with the render's end; in a browser it answers from
          -- that preload, or is read when the machine starts.
          answered <- atOnce externalPluginsNow
          pure State {machine = Machine.new (initialRead input answered), container, opened = mempty, listener = Nothing}
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
      let m = Machine.current st.machine
       in [hh|
            <div class="editor-plugins">
              <div class="editor-plugin-frames" hidden={{ True }} {{ HP.ref st.container }}></div>
              {% for s in stoppedPlugins m %}
                <p class="editor-plugin-stopped" role="status" data-plugin={{ s.plugin.manifest.id.text }}>The plugin {{ s.plugin.manifest.name }} was stopped: {{ s.reason }}. It runs again when the editor opens again.</p>
              {% end %}
              {% if restInEncrypted m %}
                <p class="editor-plugin-note" role="note">External plugins do not run in an encrypted space: the page's text stays in this browser.</p>
              {% end %}
            </div>
          |]

    handleAction :: Action -> H.HalogenM State Action Empty Output m ()
    handleAction = \case
      -- The frames' messages come through one channel, subscribed before
      -- the first frame is made.
      Initialize -> do
        channel <- H.liftEffect (liftIO HS.create)
        void (H.subscribe ((\(n, said) -> FromFrame n said) <$> channel.emitter))
        modify (\st -> st {listener = Just channel.listener})
        gets (\st -> (Machine.current st.machine).input) >>= happen . Given
      Receive input -> happen (Given input)
      FromFrame n said -> happen (Said n said)
      Finalize -> Machine.finish spec Finished

    happen :: Event -> H.HalogenM State Action Empty Output m ()
    happen = Machine.happen spec

    spec :: Machine.Spec Int State Action Empty Output m Event Effect Model
    spec = Machine.Spec {Machine.step = step, Machine.carry = carry, Machine.get = \st -> st.machine, Machine.set = \machine st -> st {machine}}

    carry :: Effect -> Machine.Carry Int State Action Empty Output m Event
    carry = \case
      ReadInstalled ticket -> Machine.Ask (Machine.run externalPluginsNow) (Read ticket . either (Left . Unreachable . toS . displayException) identity)
      -- The frame is made and kept in one step, then its wait starts.
      Open n ticket address ->
        Machine.AskUnderWith
          n
          ( do
              container <- gets (\st -> st.container) >>= Sync.getRef
              listener <- gets (\st -> st.listener)
              case (container, listener) of
                (Just element, Just heard) -> do
                  frame <- Sync.sync (openPluginFrame element address (\said -> HS.notify heard (n, said)))
                  modify (\st -> st {opened = Map.insert n frame st.opened})
                _ -> Sync.sync (liftIO (throwIO (userError "the editor has no place for the plugin's frame")))
          )
          (\_ -> Machine.delayCounted startWithin)
          (Waited n ticket . first (toS . displayException))
      Send n ticket message ->
        Machine.AskUnderWith
          n
          ( gets (\st -> Map.lookup n st.opened) >>= \case
              Just frame -> Sync.sync (liftIO (frame.post message))
              Nothing -> Sync.sync (liftIO (throwIO (userError "the plugin's frame is closed")))
          )
          (\_ -> Machine.delayCounted answerWithin)
          (Waited n ticket . first (toS . displayException))
      StopWaiting n -> Machine.Cancel n
      Close n -> Machine.Now $ do
        frame <- state (\st -> (Map.lookup n st.opened, st {opened = Map.delete n st.opened}))
        traverse_ (\f -> Sync.sync (liftIO f.close)) frame
      Tell heard -> Machine.Now (Sync.raise (Heard heard))
