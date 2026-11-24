-- | The registry of plugins: every plugin built into the wiki, and what
-- the plugins that are on contribute ("Wiki.App.Plugin").
--
-- A built-in plugin is added by one line in 'plugins', as a step of the
-- schema written in Haskell is ("Wiki.Server.Storage.Migrate.Code").
-- Which plugins are on enters in one place, 'pluginsOn', and what they
-- give through one pure function, 'contributed'. Today every plugin is
-- on; a user's and the administrator's choices will be read in
-- 'pluginsOn'.
module Wiki.App.Plugin.Registry
  ( plugins
  , builtinIds
  , pluginsOn
  , Contributed (..)
  , contributed
  )
where

import Data.Set qualified as Set
import Protolude
import Wiki.App.Plugin (Agent, Diagnostics, Manifest (..), Options, Plugin (..), PluginId)
import Wiki.App.Plugin.Errors (errors)
import Wiki.App.View.Common (Context)

-- | Every plugin built into the wiki, in the order their contributions are
-- shown.
plugins :: [Plugin m]
plugins = [errors]

-- | The ids of every plugin built into the wiki.
builtinIds :: Set PluginId
builtinIds = Set.fromList [p.manifest.id | p <- plugins @IO]

-- | The plugins that are on for the reader of a view. Every built-in one,
-- for now.
pluginsOn :: Context -> Set PluginId
pluginsOn _ = builtinIds

-- | 'Contributed' is what the plugins that are on give the wiki, each kind
-- of contribution in the plugins' order. The editor has room for one
-- agent: the first plugin's that is on.
data Contributed m = Contributed
  { diagnostics :: [Diagnostics]
  , agent :: Maybe (Manifest, Agent m)
  , options :: [(PluginId, Options m)]
  }

-- | What the plugins among the given ones whose ids are in the set
-- contribute. An id in the set that no plugin has is ignored.
contributed :: Set PluginId -> [Plugin m] -> Contributed m
contributed chosenIds given =
  Contributed
    { diagnostics = mapMaybe (\p -> p.diagnostics) chosen
    , agent = head [(p.manifest, a) | p <- chosen, Just a <- [p.agent]]
    , options = [(p.manifest.id, o) | p <- chosen, Just o <- [p.options]]
    }
  where
    chosen = filter (\p -> Set.member p.manifest.id chosenIds) given
