-- | The registry of plugins: every plugin built into the wiki, and what
-- the plugins that are on contribute ("Wiki.App.Plugin").
--
-- A built-in plugin is added by one line in 'plugins', as a step of the
-- schema written in Haskell is ("Wiki.Server.Storage.Migrate.Code").
-- Which plugins are on enters in one place, 'pluginsOn', and what they
-- give through one pure function, 'contributed'.
--
-- Which plugins are on is decided from what the server keeps
-- ("Wiki.Api.Plugins"): the plugins the administrator turned off for the
-- whole wiki, and the reader's own choice for each. The shell reads both
-- with the session and gives them to every view in its 'Context'
-- ('Wiki.App.View.Common.plugins'). 'isOn' is the rule.
module Wiki.App.Plugin.Registry
  ( plugins
  , manifests
  , builtinIds
  , pluginsOn
  , isOn
  , onAmong
  , Contributed (..)
  , contributed
  , optionsOf
  , assistantId
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.UUID (MonadUUID)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Protolude
import Wiki.Api.Plugins (PluginChoice (..), PluginState (..), Plugins (..))
import Wiki.App.Plugin (Agent, Diagnostics, Manifest (..), Options, Plugin (..), PluginId)
import Wiki.App.Plugin.Assistant (assistant, assistantId)
import Wiki.App.Plugin.Errors (errors)
import Wiki.App.View.Common (Context (..))

-- | Every plugin built into the wiki, in the order their contributions are
-- shown.
plugins :: (Typeable m, MonadUUID m, MonadUnliftIO m) => [Plugin m]
plugins = [assistant, errors]

-- | The manifests of every plugin built into the wiki, in the registry's
-- order: what the settings list.
manifests :: [Manifest]
manifests = [p.manifest | p <- plugins @IO]

-- | The ids of every plugin built into the wiki.
builtinIds :: Set PluginId
builtinIds = Set.fromList (map (\m -> m.id) manifests)

-- | The plugins that are on for the reader of a view, by what the shell
-- last read ('isOn').
pluginsOn :: Context -> Set PluginId
pluginsOn ctx = onAmong manifests ctx.plugins

-- | The ids of the plugins among these that are on ('isOn').
onAmong :: [Manifest] -> Plugins -> Set PluginId
onAmong given known = Set.fromList [m.id | m <- given, isOn known m]

-- | Whether a plugin is on, by what the server keeps: off when the
-- administrator turned it off for the whole wiki; otherwise on or off as
-- the user chose; otherwise, when the user's state is the default or they
-- made no choice, the plugin's own default ('onByDefault').
isOn :: Plugins -> Manifest -> Bool
isOn known m
  | Set.member m.id known.offForWiki = False
  | otherwise = case (\c -> c.state) <$> Map.lookup m.id known.mine of
      Just PluginOn -> True
      Just PluginOff -> False
      Just PluginDefault -> m.onByDefault
      Nothing -> m.onByDefault

-- | 'Contributed' is what the plugins that are on give the editor and the
-- pages, each kind of contribution in the plugins' order. The editor has
-- room for one agent: the first plugin's that is on. A plugin's options
-- are not here: they are reachable whether the plugin is on or off
-- ('optionsOf').
data Contributed m = Contributed
  { diagnostics :: [Diagnostics]
  , agent :: Maybe (Manifest, Agent m)
  }

-- | What the plugins among the given ones whose ids are in the set
-- contribute. An id in the set that no plugin has is ignored.
contributed :: Set PluginId -> [Plugin m] -> Contributed m
contributed chosenIds given =
  Contributed
    { diagnostics = mapMaybe (\p -> p.diagnostics) chosen
    , agent = head [(p.manifest, a) | p <- chosen, Just a <- [p.agent]]
    }
  where
    chosen = filter (\p -> Set.member p.manifest.id chosenIds) given

-- | The options of the given plugins that have any, by plugin, in the
-- plugins' order, whether each plugin is on or off: turning a plugin off
-- takes away what it gives the editor and the pages, not its settings,
-- where the user may keep its keys and turn it on again.
optionsOf :: [Plugin m] -> [(PluginId, Options m)]
optionsOf given = [(p.manifest.id, o) | p <- given, Just o <- [p.options]]
