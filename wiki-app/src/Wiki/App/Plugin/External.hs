-- | External plugins on the application's side (WP5.84 of docs/PLAN.md,
-- decision 69, issue #186): code someone else wrote, which the server
-- keeps and serves in a sandboxed frame ("Wiki.Api.Plugins.External",
-- @Wiki.Server.PluginFrame@), and which the editor runs beside the text.
--
-- This module is the host's, not a plugin's: the test of the plugins'
-- imports counts @Wiki.App.Plugin.External@ and the modules under it with
-- the registry and the host. An external plugin is not a Haskell value of
-- the registry's list, since its code is not the application's. What it
-- contributes is the same as a built-in plugin's: 'Diagnostics', problems
-- in the text, which the editor counts and marks with the others
-- ('diagnosticsOf').
--
-- The parts:
--
-- * which installed plugins may run, by the policy ('runnable'), and their
--   'Manifest' for the rule of what is on ("Wiki.App.Plugin.Registry"'s
--   @externalOn@);
-- * the messages of the plugins' protocol, version 1, and the problems a
--   plugin's answer stands for ("Wiki.App.Plugin.External.Protocol");
-- * the frames and their asks, a machine ("Wiki.App.Plugin.External.Machine"),
--   run by a hidden component of the editor
--   ("Wiki.App.Plugin.External.Frames");
-- * the frame in the browser ("Wiki.App.Plugin.External.Frame").
--
-- An external plugin does not run in an encrypted space: the text there is
-- the author's alone, and a plugin's code may send it to the hosts its
-- manifest names.
module Wiki.App.Plugin.External
  ( manifestOf
  , runnable
  , Heard (..)
  , diagnosticsOf
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Protolude
import Wiki.Api.Plugins.External (ExternalManifest (..), ExternalPlugin (..), ExternalPlugins (..), ExternalPolicy (..))
import Wiki.App.Plugin (Diagnostics (..), Drawn (..), Manifest (..))
import Wiki.Core.Markdown.Problems (Problem)

-- | The manifest of an external plugin, as the registry reads manifests:
-- its id, name and description. An external plugin is on for a user who
-- made no choice: the wiki's was installed for everyone by the
-- administrator, and a user's own was added by that user. It keeps no
-- secrets: named secrets for external plugins are left for later (the
-- external plugins' design), and the manifest is read only for whether
-- the plugin is on. Nothing of the host's ("Wiki.App.Plugin.Host") is
-- given to it: it runs in its frame, and is sent the page's text alone.
manifestOf :: ExternalPlugin -> Manifest
manifestOf p = Manifest {id = p.manifest.id, name = p.manifest.name, description = p.manifest.description, onByDefault = True, secrets = []}

-- | The installed plugins that may run, by the policy, as the server
-- serves their frames: the wiki's while the policy is not off (an
-- administrator is listed them always, to manage them), and a user's own
-- while users may add theirs, unless the wiki has a plugin of the same id,
-- which runs in its place.
runnable :: ExternalPlugins -> [ExternalPlugin]
runnable known = filter runs known.external
  where
    wikiIds = Set.fromList [p.manifest.id | p <- known.external, not p.mine]
    runs p
      | p.mine = known.policy == ExternalUsers && not (Set.member p.manifest.id wikiIds)
      | otherwise = known.policy /= ExternalOff

-- | 'Heard' is what an external plugin last answered: the text it was
-- sent, and the problems it found in that text. The problems' lines are
-- lines of that text, so they count only while the editor's text checked
-- is the same ('diagnosticsOf').
data Heard = Heard
  { text :: Text
  , problems :: [Problem]
  }
  deriving stock (Eq, Show)

-- | The diagnostics of the external plugins heard, one for each, as a
-- built-in plugin's are: the problems a plugin answered for exactly the
-- text checked, and none for another text, whose lines would be wrong.
diagnosticsOf :: Map k Heard -> [Diagnostics]
diagnosticsOf heard = [Diagnostics (\drawn -> if drawn.text == h.text then h.problems else []) | h <- Map.elems heard]
