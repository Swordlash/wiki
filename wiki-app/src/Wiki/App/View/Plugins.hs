{-# LANGUAGE QuasiQuotes #-}

-- | The settings' section "Plugins" (WP5.84 of docs/PLAN.md, decision 69):
-- the plugins built into the wiki, in the registry's order
-- ("Wiki.App.Plugin.Registry"), each turned on or off for the user.
--
-- Each plugin's row has its name, a line on what it does, and a switch.
-- A plugin the user made no choice for is as its manifest says
-- ('Wiki.App.Plugin.onByDefault'), and the row says "Default"; once the
-- user chose, the row offers the way back to the default. A plugin the
-- administrator turned off for the whole wiki is off whatever the user
-- chose: its switch is disabled, and the row says why. The user's choice
-- is kept all the same, and applies again once the administrator turns
-- the plugin back on. The writing assistant's row links to its options,
-- the section "Writing assistant", which stays a section of its own.
--
-- What the section shows is the plugins' state
-- ("Wiki.App.View.Plugins.Machine"), which the shell keeps and runs, so
-- that a write sent goes on after the section is gone ("Wiki.App.Shell"):
-- this module draws the state it is given and raises the reader's acts as
-- its events.
module Wiki.App.View.Plugins
  ( Input (..)
  , component
  )
where

import Data.Map.Strict qualified as Map
import Data.Row (Empty)
import Data.Set qualified as Set
import Halogen qualified as H
import Halogen.Router (Follow, Router)
import Halogen.Router qualified as Router
import Halogen.TH (hh)
import Protolude hiding (State)
import Wiki.Api.Plugins (PluginChoice (..), PluginId (..), PluginState (..), Plugins (..))
import Wiki.App.Backend (WikiError (..))
import Wiki.App.Plugin (Manifest (..))
import Wiki.App.Plugin.Registry (assistantId, isOn, manifests)
import Wiki.App.Route (Route (UserSettings), SettingsSection (AssistantSection))
import Wiki.App.View.Common (errorMessage)
import Wiki.App.View.Plugins.Machine (Event (..), Model (..), shownState)

-- | 'Input' is what the section is given.
data Input = Input
  { model :: Model
  -- ^ The plugins' state, as the shell holds it now: the one value of
  -- what is on, and the user's changes not done yet.
  , router :: Router Route
  -- ^ For the link to a plugin's options.
  }

newtype State = State {input :: Input}

data Action
  = Initialize
  | Receive Input
  | Happen Event
  | Followed (Follow Route)

-- | The section. Its output is the machine's events, which the shell
-- steps.
component :: forall query m. (MonadIO m) => H.Component query Input Event m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = \input -> pure State {input}
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
    render :: State -> H.ComponentHTML Action Empty m
    render st =
      [hh|
        <div class="plugins">
          <p class="account-note plugins-about">Plugins add to what the wiki does. Each is on or off for you alone; one you have not chosen for is as the wiki ships it.</p>
          {{ listView }}
        </div>
      |]
      where
        m = st.input.model
        listView = case m.known of
          Nothing -> [hh|<p class="loading">Loading…</p>|]
          Just (Left e) -> [hh|<p class="error plugins-error" role="alert">{{ "Your plugins could not be read. " <> errorMessage e }}</p>|]
          Just (Right known) ->
            [hh|
              <ul class="plugin-list">
                {% for manifest in manifests %}
                  {{ row st m known manifest }}
                {% end %}
              </ul>
            |]

    row :: State -> Model -> Plugins -> Manifest -> H.ComponentHTML Action Empty m
    row st m known manifest =
      [hh|
        <li class={{ "plugin plugin-" <> if isOnNow then "on" else "off" :: Text }} data-plugin={{ manifest.id.text }}>
          <div class="plugin-head">
            <span class="plugin-name">{{ manifest.name }}</span>
            <label class="plugin-switch">
              <input
                type="checkbox"
                role="switch"
                class="plugin-toggle"
                aria-label={{ manifest.name }}
                checked={{ isOnNow }}
                disabled={{ offForAll }}
                onChange={{ const (Happen (Chosen manifest.id (if isOnNow then PluginOff else PluginOn))) }}
              >
              <span class="plugin-state">{{ if isOnNow then "On" else "Off" :: Text }}</span>
            </label>
          </div>
          <p class="plugin-description account-note">{{ manifest.description }}</p>
          {% if offForAll %}
            <p class="plugin-off-for-wiki account-note" role="note">An administrator has turned this plugin off for everyone on this wiki. Your choice is kept, for when it is turned on again.</p>
          {% else %}
            {% if shown == PluginDefault %}
              <p class="plugin-default account-note">{{ "Default: " <> onOff manifest.onByDefault <> "." }}</p>
            {% else %}
              <button type="button" class="plugin-to-default" onClick={{ const (Happen (Chosen manifest.id PluginDefault)) }}>{{ "Back to the default (" <> onOff manifest.onByDefault <> ")" }}</button>
            {% end %}
          {% end %}
          {% if manifest.id == assistantId %}
            <a class="plugin-options" {{ Router.linkProps st.input.router Followed (UserSettings AssistantSection) }}>Its options are under Writing assistant</a>
          {% end %}
          {% for e in maybeToList (Map.lookup manifest.id m.refused) %}
            <p class="error plugin-refused" role="alert">{{ "Not changed: " <> refusalText e }}</p>
          {% end %}
        </li>
      |]
      where
        shown = shownState m manifest.id
        offForAll = Set.member manifest.id known.offForWiki
        -- The switch shows the state chosen while its write is out.
        isOnNow = isOn known {mine = Map.insert manifest.id PluginChoice {state = shown, options = Nothing} known.mine} manifest

    handleAction :: Action -> H.HalogenM State Action Empty Event m ()
    handleAction = \case
      -- Opened, the section asks for a read, which the machine sends only
      -- when nothing was read in this session; and again, once, when it is
      -- given a new session.
      Initialize -> H.raise Opened
      Receive input -> do
        before <- gets (\st -> st.input.model.session)
        modify (\st -> st {input})
        when (input.model.session /= before) (H.raise Opened)
      Happen e -> H.raise e
      Followed f -> gets (\st -> st.input.router) >>= \r -> Router.follow r f

-- | On or off, in a word.
onOff :: Bool -> Text
onOff = bool "off" "on"

-- | Why a write was refused, in the server's words where it has some.
refusalText :: WikiError -> Text
refusalText = \case
  Invalid why -> why
  e -> errorMessage e
