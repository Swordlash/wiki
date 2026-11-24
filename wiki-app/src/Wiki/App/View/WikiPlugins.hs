{-# LANGUAGE QuasiQuotes #-}

-- | The administration's switch of plugins (WP5.84 of docs/PLAN.md,
-- decision 69): the plugins built into the wiki, in the registry's order
-- ("Wiki.App.Plugin.Registry"), each with a box "Off for everyone". A
-- plugin ticked is off for every user of the wiki, whatever each chose;
-- each user's choice is kept, and applies again once the box is cleared.
-- The server writes each change to the audit log (@settings.plugins@).
--
-- It is shown in the section "Permissions", under whom the wiki lets in,
-- beside the wiki's other settings. What it shows is the shell's machine
-- ("Wiki.App.View.WikiPlugins.Machine"), which the shell keeps and runs,
-- so that a save sent goes on after the view is gone and the shell hears
-- what it changed ("Wiki.App.Shell"); other readers see it at their next
-- read of the session. This module draws the model it is given and raises
-- the administrator's acts as the machine's events.
module Wiki.App.View.WikiPlugins
  ( component
  )
where

import Data.Row (Empty)
import Data.Set qualified as Set
import Halogen qualified as H
import Halogen.TH (hh)
import Protolude hiding (State)
import Wiki.Api.Plugins (PluginId (..))
import Wiki.App.Backend (WikiError (..))
import Wiki.App.Plugin (Manifest (..))
import Wiki.App.Plugin.Registry (manifests)
import Wiki.App.View.Common (errorMessage)
import Wiki.App.View.WikiPlugins.Machine (Event (..), Model (..), shownOff)

newtype State = State {model :: Model}

data Action
  = Initialize
  | Receive Model
  | Happen Event

-- | The view, given the shell's machine's model as it is now. Its output
-- is the machine's events, which the shell steps.
component :: forall query m. (Monad m) => H.Component query Model Event m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = \model -> pure State {model}
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
        <section class="admin-section admin-plugins" aria-label="Plugins">
          <h2>Plugins</h2>
          <p class="admin-intro">The plugins built into the wiki. A plugin turned off here is off for everyone, whatever each user chose in their settings; their choices are kept for when it is turned on again.</p>
          {{ body }}
        </section>
      |]
      where
        m = st.model
        body = case (m.off, shownOff m) of
          (Just (Left e), _) -> [hh|<p class="error admin-plugins-error" role="alert">{{ errorMessage e }}</p>|]
          (_, Just off) ->
            [hh|
              <div>
                <ul class="admin-plugin-list">
                  {% for manifest in manifests %}
                    <li class="admin-plugin" data-plugin={{ manifest.id.text }}>
                      <label class="admin-plugin-choice">
                        <input type="checkbox" class="admin-plugin-off" checked={{ Set.member manifest.id off }} onChange={{ const (Happen (Ticked manifest.id (not (Set.member manifest.id off)))) }}>
                        <span class="admin-plugin-name">{{ manifest.name }}</span>
                        <span class="admin-plugin-note">Off for everyone</span>
                      </label>
                      <p class="account-note">{{ manifest.description }}</p>
                    </li>
                  {% end %}
                </ul>
                {% for e in maybeToList m.problem %}
                  <p class="error admin-plugins-error" role="alert">{{ "Not saved: " <> refusalText e }}</p>
                {% end %}
              </div>
            |]
          _ -> [hh|<p class="loading">Loading…</p>|]

    handleAction :: Action -> H.HalogenM State Action Empty Event m ()
    handleAction = \case
      -- Opened, the view asks for a read, which the machine sends only when
      -- nothing was read in this session; and again, once, when it is given
      -- a new session.
      Initialize -> H.raise Opened
      Receive model -> do
        before <- gets (\st -> st.model.session)
        modify (\st -> st {model})
        when (model.session /= before) (H.raise Opened)
      Happen e -> H.raise e

refusalText :: WikiError -> Text
refusalText = \case
  Invalid why -> why
  e -> errorMessage e
