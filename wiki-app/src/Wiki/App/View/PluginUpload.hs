{-# LANGUAGE QuasiQuotes #-}

-- | What the settings' section "Plugins" ("Wiki.App.View.Plugins") and the
-- administration's switch ("Wiki.App.View.WikiPlugins") share of the
-- external plugins (WP5.84 of docs/PLAN.md, decision 69): reading the two
-- files the reader picked for an upload ('readPluginFiles'), the warning
-- shown before the upload goes ('warning'), and a plugin's hosts and
-- permissions in words ('hostsText', 'permissionText').
--
-- Everything a manifest says is drawn as text by Halogen: its name, its
-- version, its description and its hosts are text nodes, and its id is an
-- attribute's value, never an attribute's name or HTML.
module Wiki.App.View.PluginUpload
  ( readPluginFiles
  , manifestFileHint
  , warning
  , hostsText
  , permissionText
  , ownerText
  , changeText
  , refusalText
  )
where

import Data.List (partition)
import Data.Set qualified as Set
import Data.Text qualified as T
import Halogen.HTML qualified as HH
import Halogen.TH (hh)
import Protolude
import Wiki.Api.Plugins (PluginId (..))
import Wiki.Api.Plugins.External (ExternalManifest (..), Owner (..), Permission (..), maxBundleBytes, maxManifestBytes)
import Wiki.App.Backend (WikiError (..))
import Wiki.App.Islands.Monad (ChosenFile (..))
import Wiki.App.View.Common (errorMessage)
import Wiki.App.View.Plugins.Machine (Change (..), Confirmation (..), Event (..), Picked (..), PluginFiles (..), chooseFiles)

-- | The two files of an external plugin, as the reader picked them,
-- checked ('chooseFiles'): the manifest, the one file whose name ends in
-- @.json@, and the bundle, the other one. A file larger than its bound
-- (16 kB for the manifest, 2 MiB for the bundle) is refused by its size,
-- before anything of it is read. This waits for the browser to read the
-- files and hashes the bundle, so a view runs it off its loop.
readPluginFiles :: [ChosenFile] -> IO (Either Text PluginFiles)
readPluginFiles files = case partition (\f -> ".json" `T.isSuffixOf` T.toLower f.name) files of
  ([manifest], [bundle])
    | manifest.size > fromIntegral maxManifestBytes ->
        pure (Left (manifest.name <> " has " <> show manifest.size <> " bytes: a plugin's manifest is at most " <> show maxManifestBytes <> " bytes (16 kB)."))
    | bundle.size > fromIntegral maxBundleBytes ->
        pure (Left (bundle.name <> " has " <> show bundle.size <> " bytes: a plugin's bundle is at most " <> show maxBundleBytes <> " bytes (2 MiB)."))
    | otherwise -> do
        manifestBytes <- manifest.bytes
        bundleBytes <- bundle.bytes
        -- Checked and hashed here, so that the view's step does not.
        case chooseFiles manifestBytes bundleBytes of
          Left why -> pure (Left why)
          Right f -> evaluate (T.length f.hash) $> Right f
  _ -> pure (Left manifestFileHint)

-- | What to pick for an upload.
manifestFileHint :: Text
manifestFileHint = "Choose two files together: the plugin's manifest, a .json file of at most 16 kB, and its code, one JavaScript file of at most 2 MiB."

-- | The warning shown before an upload goes, with the plugin the picked
-- files are, or why they are no plugin. Confirming raises 'Confirmed' with
-- what the warning showed; the model uploads only if that is still the
-- pick it holds.
warning :: Picked -> (Event -> action) -> HH.HTML w action
warning p happen = case p.files of
  Left why ->
    [hh|
      <div class="plugin-pick plugin-pick-refused" role="alert">
        <p class="error plugin-pick-error">{{ "These files cannot be uploaded: " <> why }}</p>
        <button type="button" class="plugin-pick-drop" onClick={{ const (happen PickDropped) }}>Dismiss</button>
      </div>
    |]
  Right f ->
    [hh|
      <div class="plugin-pick plugin-warning" role="alertdialog" aria-label="Before you upload this plugin">
        <p class="plugin-warning-title"><strong>{{ f.manifest.name }}</strong>{{ " " <> f.manifest.version <> " (" <> f.manifest.id.text <> ")" }}</p>
        {% if not (T.null f.manifest.description) %}
          <p class="plugin-description account-note">{{ f.manifest.description }}</p>
        {% end %}
        <p class="plugin-warning-text">{{ "This plugin's code is someone else's, and it runs " <> whose <> ". " <> "While a page's editor is open, it is sent the page's text." }}</p>
        <ul class="plugin-permissions">
          {% for perm in Set.toList f.manifest.permissions %}
            <li>{{ permissionText perm }}</li>
          {% end %}
        </ul>
        <p class="plugin-hosts">{{ hostsText f.manifest.hosts }}</p>
        <p class="account-note">Upload it only if you trust its author with what you write.</p>
        <div class="plugin-warning-actions">
          <button type="button" class="plugin-confirm" onClick={{ const (happen (Confirmed confirmation)) }}>{{ if isJust p.replacing then "Replace it" else "Upload it" :: Text }}</button>
          <button type="button" class="plugin-pick-drop" onClick={{ const (happen PickDropped) }}>Cancel</button>
        </div>
      </div>
    |]
    where
      whose :: Text
      whose = case p.owner of
        TheWiki -> "for everyone on this wiki"
        Mine -> "for you"
      confirmation = Confirmation {pick = p.number, owner = p.owner, id = f.manifest.id, hash = f.hash, hosts = f.manifest.hosts, replaces = snd <$> p.replacing}

-- | Where a plugin may send what it reads, in a sentence.
hostsText :: [Text] -> Text
hostsText = \case
  [] -> "It may connect to no other site: what it reads stays in your browser."
  hosts -> "It may send what it reads to " <> T.intercalate ", " hosts <> ", and to no other site."

-- | What a permission lets a plugin do, in a sentence.
permissionText :: Permission -> Text
permissionText = \case
  DocumentRead -> "It reads the text of the page being edited, while its editor is open."

-- | Whose a plugin is, in words.
ownerText :: Owner -> Text
ownerText = \case
  TheWiki -> "The wiki's"
  Mine -> "Yours"

-- | What is out for a plugin, in words.
changeText :: Change -> Text
changeText = \case
  Installing -> "Uploading…"
  Removing -> "Removing…"

-- | Why a change was refused, in the server's words where it has some.
refusalText :: WikiError -> Text
refusalText = \case
  Invalid why -> why
  Conflict why -> why
  e -> errorMessage e
