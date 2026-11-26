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
  , externalOn
  , Contributed (..)
  , contributed
  , optionsOf
  , optionsFor
  , refusalFor
  , assistantId
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.UUID (MonadUUID)
import Data.Aeson (Value)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Protolude
import Wiki.Api.Plugins (PluginChoice (..), PluginState (..), Plugins (..))
import Wiki.Api.Plugins.External (ExternalPlugin, ExternalPlugins)
import Wiki.App.Backend (WikiError (..))
import Wiki.App.Plugin (Agent, Diagnostics, Manifest (..), Options, Plugin (..), PluginId)
import Wiki.App.Plugin.Assistant (assistant, assistantId)
import Wiki.App.Plugin.Errors (errors)
import Wiki.App.Plugin.External (manifestOf, runnable)
import Wiki.App.View.Common (Context (..), errorMessage)

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

-- | The external plugins that are on for the reader, among those
-- installed ("Wiki.App.Plugin.External"): each that may run by the policy
-- ('runnable') and is on by the same rule as a built-in plugin ('isOn').
externalOn :: Plugins -> ExternalPlugins -> [ExternalPlugin]
externalOn known installed = [p | p <- runnable installed, isOn known (manifestOf p)]

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

-- | A plugin's options for the reader, as what is on holds them: what its
-- surfaces are given ('Wiki.App.Plugin.AgentInput''s @options@). The
-- context's plugins hold the options last chosen while their write is out
-- or waits ("Wiki.App.View.Plugins.Machine"'s @held@), and otherwise the
-- server's.
optionsFor :: PluginId -> Plugins -> Maybe Value
optionsFor p known = (\c -> c.options) =<< Map.lookup p known.mine

-- | Why the reader's last write of a plugin's options was refused, in the
-- server's words: what its surfaces are told with the options
-- ('Wiki.App.Plugin.OptionsInput''s @refused@), from the context's
-- refusals ('Context''s @optionsRefused@).
refusalFor :: PluginId -> Map PluginId WikiError -> Maybe Text
refusalFor p refusals = sentence <$> Map.lookup p refusals
  where
    sentence = \case
      Invalid why -> why
      e -> errorMessage e
