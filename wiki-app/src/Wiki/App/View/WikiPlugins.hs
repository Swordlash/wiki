{-# LANGUAGE QuasiQuotes #-}

-- | The administration's switch of plugins (WP5.84 of docs/PLAN.md,
-- decision 69): the plugins built into the wiki, in the registry's order
-- ("Wiki.App.Plugin.Registry"), each with a box "Off for everyone". A
-- plugin ticked is off for every user of the wiki, whatever each chose;
-- each user's choice is kept, and applies again once the box is cleared.
-- The server writes each change to the audit log (@settings.plugins@).
--
-- It is shown in the section "Permissions", under whom the wiki lets in,
-- beside the wiki's other settings. What it knows is its machine's
-- ("Wiki.App.View.WikiPlugins.Machine"). A change the server took is told
-- to the shell ('PluginsChanged'), which reads again what is on, so that
-- the administrator's own editor follows at once; other readers see it at
-- their next read of the session.
module Wiki.App.View.WikiPlugins
  ( component
  )
where

import Data.Row (Empty)
import Data.Set qualified as Set
import Halogen qualified as H
import Halogen.Machine qualified as Machine
import Halogen.Sync qualified as Sync
import Halogen.TH (hh)
import Protolude hiding (State)
import Wiki.Api.Plugins (PluginId (..), WikiPlugins (..))
import Wiki.App.Backend (WikiError (..))
import Wiki.App.Backend.Plugins (MonadPlugins (..))
import Wiki.App.Plugin (Manifest (..))
import Wiki.App.Plugin.Registry (manifests)
import Wiki.App.View.Common (ViewOutput (PluginsChanged), errorMessage)
import Wiki.App.View.WikiPlugins.Machine (Effect (..), Event (..), Model (..), initial, shownOff, step)

newtype State = State {machine :: Machine.Machine () Event Model}

data Action
  = Initialize
  | Finalize
  | Happen Event

component :: forall query input m. (MonadPlugins m) => H.Component query input ViewOutput m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure State {machine = Machine.new initial}
      , render
      , eval =
          H.mkEval
            H.defaultEval
              { H.handleAction = handleAction
              , H.initialize = Just Initialize
              , H.finalize = Just Finalize
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
        m = Machine.current st.machine
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

    handleAction :: Action -> H.HalogenM State Action Empty ViewOutput m ()
    handleAction = \case
      Initialize -> happen Started
      Finalize -> Machine.finish machineSpec Finished
      Happen e -> happen e

    happen :: Event -> H.HalogenM State Action Empty ViewOutput m ()
    happen = Machine.happen machineSpec

    machineSpec :: Machine.Spec () State Action Empty ViewOutput m Event Effect Model
    machineSpec = Machine.Spec {Machine.step = step, Machine.carry = carry, Machine.get = \st -> st.machine, Machine.set = \machine st -> st {machine}}

    carry :: Effect -> Machine.Carry () State Action Empty ViewOutput m Event
    carry = \case
      Load asked -> Machine.Ask (Machine.run wikiPluginsOff) (Loaded asked . answerOf)
      Save asked off -> Machine.Ask (Machine.run (setWikiPluginsOff WikiPlugins {off})) (Saved asked . answerOf)
      Tell -> Machine.Now (Sync.raise PluginsChanged)

refusalText :: WikiError -> Text
refusalText = \case
  Invalid why -> why
  e -> errorMessage e

answerOf :: Either SomeException (Either WikiError a) -> Either WikiError a
answerOf = either (Left . failure) identity

failure :: SomeException -> WikiError
failure e = fromMaybe (Unreachable (toS (displayException e))) (fromException e)
