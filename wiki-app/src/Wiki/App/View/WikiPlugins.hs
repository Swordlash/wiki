{-# LANGUAGE QuasiQuotes #-}

-- | The administration's switch of plugins (WP5.84 of docs/PLAN.md,
-- decision 69): the plugins built into the wiki, in the registry's order
-- ("Wiki.App.Plugin.Registry"), each with a box "Off for everyone"; and
-- the external plugins: who may add them, and the wiki's own, installed,
-- replaced and removed for everyone.
--
-- A built-in plugin ticked is off for every user of the wiki, whatever
-- each chose; each user's choice is kept, and applies again once the box
-- is cleared. The server writes each change to the audit log
-- (@settings.plugins@).
--
-- External plugins ("Wiki.Api.Plugins.External") are off at first. The
-- administrator lets the wiki's own run ('ExternalWiki'), or those and
-- each user's own ('ExternalUsers'). The wiki's are listed here whatever
-- the policy, with their version, the hosts they may send what they read
-- to and their permissions; each upload comes after the same warning a
-- user sees, saying it is for everyone ("Wiki.App.View.PluginUpload").
-- The server writes the policy, each install, replacement and removal to
-- the audit log. The demo has no server to run them, and offers none.
--
-- It is shown in the section "Permissions", under whom the wiki lets in,
-- beside the wiki's other settings. What it shows is the plugins' state
-- ("Wiki.App.View.Plugins.Machine"), which the shell keeps and runs, so
-- that a save sent goes on after the view is gone, and is in the one value
-- of what is on as soon as it is done ("Wiki.App.Shell"); other readers
-- see it at their next read of the session. This module draws the state
-- it is given and raises the administrator's acts as its events; it reads
-- the files picked for an upload, off its loop.
module Wiki.App.View.WikiPlugins
  ( Input (..)
  , component
  )
where

import Data.Map.Strict qualified as Map
import Data.Row (Empty)
import Data.Set qualified as Set
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.TH (hh)
import Protolude hiding (State)
import Web.Event.Event qualified as Web
import Wiki.Api.Plugins (PluginId (..))
import Wiki.Api.Plugins.External (ExternalManifest (..), ExternalPlugin (..), ExternalPlugins (..), ExternalPolicy (..), Owner (..))
import Wiki.App.Islands.Monad (MonadIslands (..))
import Wiki.App.Plugin (Manifest (..))
import Wiki.App.Plugin.Registry (manifests)
import Wiki.App.View.Common (attr, errorMessage)
import Wiki.App.View.PluginUpload (changeText, hostsText, permissionText, readPluginFiles, refusalText, warning)
import Wiki.App.View.Plugins.Machine (Event (..), Model (..), Picked (..), shownOff, shownPolicy)

-- | 'Input' is what the view is given.
data Input = Input
  { model :: Model
  -- ^ The shell's machine's model as it is now.
  , demo :: Bool
  -- ^ Whether this is the demo, which has no external plugins.
  }

data State = State
  { input :: Input
  , picks :: Int
  -- ^ How many times files were picked: a read of files raises its result
  -- only while no later pick was made.
  , reading :: Bool
  }

data Action
  = Initialize
  | Receive Input
  | Happen Event
  | FilesChosen (Maybe (PluginId, Text)) Web.Event

-- | The view. Its output is the machine's events, which the shell steps.
component :: forall query m. (MonadIslands m, MonadIO m) => H.Component query Input Event m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = \input -> pure State {input, picks = 0, reading = False}
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
          {{ externalView }}
        </section>
      |]
      where
        m = st.input.model
        body = case (m.known, shownOff m) of
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
        externalView
          | st.input.demo =
              [hh|
                <div class="admin-external" aria-label="External plugins">
                  <h3>External plugins</h3>
                  <p class="account-note">The demo has no server to run external plugins.</p>
                </div>
              |]
          | otherwise = case (m.installed, shownPolicy m) of
              (Just (Left e), _) -> [hh|<p class="error admin-external-error" role="alert">{{ "The external plugins could not be read. " <> errorMessage e }}</p>|]
              (Just (Right installed), Just policy) ->
                [hh|
                  <div class="admin-external" aria-label="External plugins">
                    <h3>External plugins</h3>
                    <p class="admin-intro">Plugins someone else wrote, uploaded here and run in a sandbox beside the editor. Each one reads the page being edited and may send it to the sites its manifest names.</p>
                    <fieldset class="admin-external-policy">
                      <legend>Who may add them</legend>
                      {% for p in [minBound .. maxBound] %}
                        <label class="check">
                          <input type="radio" name="external-plugins" class={{ "admin-external-policy-" <> policyValue p }} value={{ policyValue p }} checked={{ policy == p }} onChange={{ const (Happen (PolicyChosen p)) }}>
                          <span>{{ policyText p }}</span>
                        </label>
                      {% end %}
                    </fieldset>
                    {% for e in maybeToList m.policyProblem %}
                      <p class="error admin-external-policy-error" role="alert">{{ "Not saved: " <> refusalText e }}</p>
                    {% end %}
                    {% if policy == ExternalOff %}
                      <p class="account-note admin-external-off" role="note">External plugins are off: the wiki's below are kept, and none runs.</p>
                    {% end %}
                    <ul class="admin-external-list">
                      {% for p in wikis installed %}
                        {{ wikiRow m p }}
                      {% end %}
                    </ul>
                    <div class="plugin-add">
                      <label class="button plugin-add-files">
                        Install a plugin for everyone…
                        <input type="file" multiple aria-label="Install a plugin for everyone" {{ [attr "accept" ".json,.js,.mjs"] }} onChange={{ FilesChosen Nothing }}>
                      </label>
                      <p class="field-hint">Choose its two files together: its manifest, a .json file, and its code, one JavaScript file.</p>
                      {% if st.reading %}
                        <p class="plugin-reading" role="status">Reading the files…</p>
                      {% end %}
                      {% for c in notListed installed m.changing %}
                        <p class="plugin-changing" role="status">{{ changeText c }}</p>
                      {% end %}
                      {% for e in notListed installed m.externalRefused %}
                        <p class="error plugin-refused" role="alert">{{ "Not done: " <> refusalText e }}</p>
                      {% end %}
                      {% for picked in maybeToList (pickFor m Nothing) %}
                        {{ warning picked Happen }}
                      {% end %}
                    </div>
                  </div>
                |]
              _ -> [hh|<p class="loading">Loading…</p>|]

    wikiRow :: Model -> ExternalPlugin -> H.ComponentHTML Action Empty m
    wikiRow m p =
      [hh|
        <li class="admin-external-plugin" data-plugin={{ p.manifest.id.text }}>
          <p><span class="admin-plugin-name">{{ p.manifest.name }}</span> {{ " · version " <> p.manifest.version }}</p>
          {% if not (T.null p.manifest.description) %}
            <p class="account-note">{{ p.manifest.description }}</p>
          {% end %}
          <ul class="plugin-permissions">
            {% for perm in Set.toList p.manifest.permissions %}
              <li>{{ permissionText perm }}</li>
            {% end %}
          </ul>
          <p class="plugin-hosts">{{ hostsText p.manifest.hosts }}</p>
          {% for c in maybeToList (Map.lookup key m.changing) %}
            <p class="plugin-changing" role="status">{{ changeText c }}</p>
          {% end %}
          <div class="plugin-own-actions">
            <label class="button plugin-replace">
              Replace…
              <input type="file" multiple aria-label={{ "Replace " <> p.manifest.name }} {{ [attr "accept" ".json,.js,.mjs"] }} onChange={{ FilesChosen (Just (p.manifest.id, p.bundle)) }}>
            </label>
            <button type="button" class="plugin-remove" onClick={{ const (Happen (RemoveChosen TheWiki p.manifest.id p.bundle)) }}>Remove for everyone</button>
          </div>
          {% for e in maybeToList (Map.lookup key m.externalRefused) %}
            <p class="error plugin-refused" role="alert">{{ "Not done: " <> refusalText e }}</p>
          {% end %}
          {% for picked in maybeToList (pickFor m (Just p.manifest.id)) %}
            {{ warning picked Happen }}
          {% end %}
        </li>
      |]
      where
        key = (TheWiki, p.manifest.id)

    handleAction :: Action -> H.HalogenM State Action Empty Event m ()
    handleAction = \case
      -- Opened, the view asks for a read, which the machine sends only when
      -- nothing was read in this session; and again, once, when it is given
      -- a new session.
      Initialize -> H.raise Opened
      Receive input -> do
        before <- gets (\st -> st.input.model.session)
        modify (\st -> st {input})
        when (input.model.session /= before) (H.raise Opened)
      Happen e -> H.raise e
      FilesChosen replacing ev -> do
        files <- H.sync (pluginFilesChosen ev)
        unless (null files) $ do
          -- The session the files are picked in: a read that ends in
          -- another is dropped by the model.
          session <- gets (\st -> st.input.model.session)
          mine <- state (\st -> (st.picks + 1, st {picks = st.picks + 1, reading = True}))
          result <- H.async (liftIO (readPluginFiles files))
          -- Raised only if no later pick was made meanwhile.
          latest <- state (\st -> (st.picks == mine, if st.picks == mine then st {reading = False} else st))
          when latest (H.raise (FilesPicked session TheWiki replacing result))

-- | The wiki's plugins among those installed.
wikis :: ExternalPlugins -> [ExternalPlugin]
wikis installed = [p | p <- installed.external, not p.mine]

-- | What a map holds for the wiki's plugins not listed: an upload of a
-- plugin added, out or refused.
notListed :: ExternalPlugins -> Map (Owner, PluginId) a -> [a]
notListed installed = Map.elems . Map.filterWithKey (\(o, i) _ -> o == TheWiki && Set.notMember i listed)
  where
    listed = Set.fromList [p.manifest.id | p <- wikis installed]

-- | The wiki's pick for an upload, of a plugin added ('Nothing') or of the
-- one replaced.
pickFor :: Model -> Maybe PluginId -> Maybe Picked
pickFor m replaced = case m.picked of
  Just picked | picked.owner == TheWiki, (fst <$> picked.replacing) == replaced -> Just picked
  _ -> Nothing

policyValue :: ExternalPolicy -> Text
policyValue = \case
  ExternalOff -> "off"
  ExternalWiki -> "wiki"
  ExternalUsers -> "users"

policyText :: ExternalPolicy -> Text
policyText = \case
  ExternalOff -> "Nobody: external plugins are off."
  ExternalWiki -> "The wiki's only: plugins installed here, for everyone."
  ExternalUsers -> "The wiki's, and each user may add their own."
