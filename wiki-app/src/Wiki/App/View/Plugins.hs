{-# LANGUAGE QuasiQuotes #-}

-- | The settings' section "Plugins" (WP5.84 of docs/PLAN.md, decision 69):
-- the plugins built into the wiki, in the registry's order
-- ("Wiki.App.Plugin.Registry"), each turned on or off for the user, and
-- after them the external plugins the user may run.
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
-- An external plugin's row ("Wiki.Api.Plugins.External") has its name,
-- version and description, whose it is (the wiki's, or the user's own),
-- the hosts it may send what it reads to, its permissions, and the same
-- switch, on by default. While the administrator lets users add their
-- own, the user adds, replaces and removes theirs here, each upload after
-- a warning that says what the plugin reads and where it may send it
-- ("Wiki.App.View.PluginUpload"). A plugin of the user's own with the id
-- of one of the wiki's does not run: the wiki's runs in its place, and
-- the row says so. While the policy is off, the section says so and lists
-- none. The demo has no server to serve a plugin's frame, so it has no
-- external plugins.
--
-- What the section shows is the plugins' state
-- ("Wiki.App.View.Plugins.Machine"), which the shell keeps and runs, so
-- that a write sent goes on after the section is gone ("Wiki.App.Shell"):
-- this module draws the state it is given and raises the reader's acts as
-- its events. The files picked for an upload are read here, off the
-- section's loop; a read that a later pick overtook is dropped.
module Wiki.App.View.Plugins
  ( Input (..)
  , component
  )
where

import Data.Map.Strict qualified as Map
import Data.Row (Empty)
import Data.Set qualified as Set
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.Router (Follow, Router)
import Halogen.Router qualified as Router
import Halogen.TH (hh)
import Protolude hiding (State)
import Web.Event.Event qualified as Web
import Wiki.Api.Plugins (PluginChoice (..), PluginId (..), PluginState (..), Plugins (..))
import Wiki.Api.Plugins.External (ExternalManifest (..), ExternalPlugin (..), ExternalPlugins (..), ExternalPolicy (..), Owner (..), ownerOf)
import Wiki.App.Islands.Monad (MonadIslands (..))
import Wiki.App.Plugin (Manifest (..))
import Wiki.App.Plugin.External (manifestOf, runnable)
import Wiki.App.Plugin.Registry (assistantId, isOn, manifests)
import Wiki.App.Route (Route (UserSettings), SettingsSection (AssistantSection))
import Wiki.App.View.Common (attr, errorMessage)
import Wiki.App.View.PluginUpload (changeText, hostsText, ownerText, permissionText, readPluginFiles, refusalText, warning)
import Wiki.App.View.Plugins.Machine (Event (..), Model (..), Picked (..), shownState)

-- | 'Input' is what the section is given.
data Input = Input
  { model :: Model
  -- ^ The plugins' state, as the shell holds it now: the one value of
  -- what is on, and the user's changes not done yet.
  , router :: Router Route
  -- ^ For the link to a plugin's options.
  , demo :: Bool
  -- ^ Whether this is the demo, which has no external plugins.
  }

data State = State
  { input :: Input
  , picks :: Int
  -- ^ How many times the user picked files: a read of files raises its
  -- result only while no later pick was made.
  , reading :: Bool
  -- ^ Whether picked files are being read.
  }

data Action
  = Initialize
  | Receive Input
  | Happen Event
  | Followed (Follow Route)
  | FilesChosen (Maybe (PluginId, Text)) Web.Event

-- | The section. Its output is the machine's events, which the shell
-- steps.
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
              <div>
                <ul class="plugin-list">
                  {% for manifest in manifests %}
                    {{ row st m known manifest }}
                  {% end %}
                </ul>
                {{ externalView st m known }}
              </div>
            |]

    row :: State -> Model -> Plugins -> Manifest -> H.ComponentHTML Action Empty m
    row st m known manifest =
      [hh|
        <li class={{ "plugin plugin-" <> if isOnNow then "on" else "off" :: Text }} data-plugin={{ manifest.id.text }}>
          {{ switchView m known manifest }}
          <p class="plugin-description account-note">{{ manifest.description }}</p>
          {{ stateNote m known manifest }}
          {% if manifest.id == assistantId %}
            <a class="plugin-options" {{ Router.linkProps st.input.router Followed (UserSettings AssistantSection) }}>Its options are under Writing assistant</a>
          {% end %}
        </li>
      |]
      where
        isOnNow = shownOn m known manifest

    -- The external plugins, after the built-in ones.
    externalView :: State -> Model -> Plugins -> H.ComponentHTML Action Empty m
    externalView st m known
      | st.input.demo =
          [hh|
            <section class="plugins-external" aria-label="External plugins">
              <h3>External plugins</h3>
              <p class="account-note plugins-external-none">The demo has no server to run external plugins.</p>
            </section>
          |]
      | otherwise = case m.installed of
          Nothing -> [hh|<p class="loading">Loading…</p>|]
          Just (Left e) -> [hh|<p class="error plugins-external-error" role="alert">{{ "The external plugins could not be read. " <> errorMessage e }}</p>|]
          Just (Right installed)
            | installed.policy == ExternalOff ->
                [hh|
                  <section class="plugins-external" aria-label="External plugins">
                    <h3>External plugins</h3>
                    <p class="account-note plugins-external-off" role="note">External plugins are off on this wiki: none runs.</p>
                    {{ unlistedView m Set.empty }}
                  </section>
                |]
            | otherwise ->
                [hh|
                  <section class="plugins-external" aria-label="External plugins">
                    <h3>External plugins</h3>
                    <p class="account-note">Plugins someone else wrote, which run in a sandbox beside the editor.</p>
                    <ul class="plugin-list plugin-list-external">
                      {% for p in installed.external %}
                        {{ externalRow m known installed p }}
                      {% end %}
                    </ul>
                    {% if installed.policy == ExternalUsers %}
                      {{ adding st m }}
                    {% end %}
                    {{ unlistedView m (Set.fromList [p.manifest.id | p <- installed.external, p.mine]) }}
                  </section>
                |]

    externalRow :: Model -> Plugins -> ExternalPlugins -> ExternalPlugin -> H.ComponentHTML Action Empty m
    externalRow m known installed p =
      [hh|
        <li class={{ "plugin plugin-external plugin-" <> if isOnNow then "on" else "off" :: Text }} data-plugin={{ p.manifest.id.text }} data-owner={{ if p.mine then "mine" else "wiki" :: Text }}>
          {{ switchView m known manifest }}
          <p class="plugin-about account-note">{{ ownerText (ownerOf p) <> " · version " <> p.manifest.version }}</p>
          {% if not (T.null p.manifest.description) %}
            <p class="plugin-description account-note">{{ p.manifest.description }}</p>
          {% end %}
          <ul class="plugin-permissions">
            {% for perm in Set.toList p.manifest.permissions %}
              <li>{{ permissionText perm }}</li>
            {% end %}
          </ul>
          <p class="plugin-hosts">{{ hostsText p.manifest.hosts }}</p>
          {% if hidden %}
            <p class="plugin-hidden account-note" role="note">The wiki has a plugin with the same id, which runs in this one's place.</p>
          {% else %}
            {{ stateNote m known manifest }}
          {% end %}
          {% for c in maybeToList (Map.lookup key m.changing) %}
            <p class="plugin-changing" role="status">{{ changeText c }}</p>
          {% end %}
          {% if p.mine %}
            <div class="plugin-own-actions">
              <label class="button plugin-replace">
                Replace…
                <input type="file" multiple aria-label={{ "Replace " <> p.manifest.name }} {{ [attr "accept" ".json,.js,.mjs"] }} onChange={{ FilesChosen (Just (p.manifest.id, p.bundle)) }}>
              </label>
              <button type="button" class="plugin-remove" onClick={{ const (Happen (RemoveChosen Mine p.manifest.id p.bundle)) }}>Remove</button>
            </div>
          {% end %}
          {% for e in maybeToList (Map.lookup key m.externalRefused) %}
            <p class="error plugin-refused" role="alert">{{ "Not done: " <> refusalText e }}</p>
          {% end %}
          {{ pickFor (Just p.manifest.id) }}
        </li>
      |]
      where
        manifest = manifestOf p
        key = (ownerOf p, p.manifest.id)
        isOnNow = shownOn m known manifest
        -- A user's own plugin that does not run by the policy: the wiki
        -- has one of the same id ('runnable').
        hidden = p.mine && p `notElem` runnable installed
        pickFor replaced = case m.picked of
          Just picked | picked.owner == Mine, (fst <$> picked.replacing) == replaced -> warning picked Happen
          _ -> HH.text ""

    -- The input to pick the files of a plugin of the user's own, and the
    -- warning for the files picked.
    adding :: State -> Model -> H.ComponentHTML Action Empty m
    adding st m =
      [hh|
        <div class="plugin-add">
          <label class="button plugin-add-files">
            Add a plugin of your own…
            <input type="file" multiple aria-label="Add a plugin of your own" {{ [attr "accept" ".json,.js,.mjs"] }} onChange={{ FilesChosen Nothing }}>
          </label>
          <p class="field-hint">Choose its two files together: its manifest, a .json file, and its code, one JavaScript file.</p>
          {% if st.reading %}
            <p class="plugin-reading" role="status">Reading the files…</p>
          {% end %}
          {% for picked in maybeToList adding' %}
            {{ warning picked Happen }}
          {% end %}
        </div>
      |]
      where
        adding' = case m.picked of
          Just picked | picked.owner == Mine, isNothing picked.replacing -> Just picked
          _ -> Nothing

    -- The uploads and removals of the user's own plugins that no row
    -- shows, out or refused: a plugin being added, or one whose row is
    -- gone because the administrator changed the policy meanwhile. They
    -- are drawn under every policy, so that a refusal is seen even when
    -- the policy no longer lists the user's own plugins.
    unlistedView :: Model -> Set PluginId -> H.ComponentHTML Action Empty m
    unlistedView m shown
      | null unlistedChanging && null unlistedRefused = HH.text ""
      | otherwise =
          [hh|
            <div class="plugin-unlisted">
              {% for c in unlistedChanging %}
                <p class="plugin-changing" role="status">{{ changeText c }}</p>
              {% end %}
              {% for e in unlistedRefused %}
                <p class="error plugin-refused" role="alert">{{ "Not done: " <> refusalText e }}</p>
              {% end %}
            </div>
          |]
      where
        unlisted :: Map (Owner, PluginId) a -> [a]
        unlisted = Map.elems . Map.filterWithKey (\(o, i) _ -> o == Mine && Set.notMember i shown)
        unlistedChanging = unlisted m.changing
        unlistedRefused = unlisted m.externalRefused

    -- A plugin's name, its switch, and why the last change of the switch
    -- was refused. Built-in and external rows both draw their switch with
    -- it, so that a refused change is said on either.
    switchView :: Model -> Plugins -> Manifest -> H.ComponentHTML Action Empty m
    switchView m known manifest =
      [hh|
        <div class="plugin-choice">
          <div class="plugin-head">
            <span class="plugin-name">{{ manifest.name }}</span>
            <label class="plugin-switch">
              <input
                type="checkbox"
                role="switch"
                class="plugin-toggle"
                aria-label={{ manifest.name }}
                checked={{ isOnNow }}
                disabled={{ Set.member manifest.id known.offForWiki }}
                onChange={{ const (Happen (Chosen manifest.id (if isOnNow then PluginOff else PluginOn))) }}
              >
              <span class="plugin-state">{{ if isOnNow then "On" else "Off" :: Text }}</span>
            </label>
          </div>
          {% for e in maybeToList (Map.lookup manifest.id m.refused) %}
            <p class="error plugin-refused" role="alert">{{ "Not changed: " <> refusalText e }}</p>
          {% end %}
        </div>
      |]
      where
        isOnNow = shownOn m known manifest

    -- Why a plugin is off for everyone, or its default and the way back.
    stateNote :: Model -> Plugins -> Manifest -> H.ComponentHTML Action Empty m
    stateNote m known manifest
      | Set.member manifest.id known.offForWiki =
          [hh|<p class="plugin-off-for-wiki account-note" role="note">An administrator has turned this plugin off for everyone on this wiki. Your choice is kept, for when it is turned on again.</p>|]
      | shownState m manifest.id == PluginDefault = [hh|<p class="plugin-default account-note">{{ "Default: " <> onOff manifest.onByDefault <> "." }}</p>|]
      | otherwise = [hh|<button type="button" class="plugin-to-default" onClick={{ const (Happen (Chosen manifest.id PluginDefault)) }}>{{ "Back to the default (" <> onOff manifest.onByDefault <> ")" }}</button>|]

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
          when latest (H.raise (FilesPicked session Mine replacing result))

-- | Whether a plugin's switch shows it on: the state chosen while its
-- write is out, and otherwise the server's.
shownOn :: Model -> Plugins -> Manifest -> Bool
shownOn m known manifest = isOn Plugins {offForWiki = known.offForWiki, mine = Map.insert manifest.id PluginChoice {state = shownState m manifest.id, options = Nothing} known.mine} manifest

-- | On or off, in a word.
onOff :: Bool -> Text
onOff = bool "off" "on"
