-- | The plugins' interface: what a plugin is, and what it can give the
-- wiki (#186; decision 69 and WP5.84 of docs/PLAN.md).
--
-- A plugin is a 'Manifest' and its contributions. A contribution is what
-- the plugin gives the wiki, and the host calls it: today 'Diagnostics',
-- problems the editor marks in the text. A built-in plugin is Haskell
-- compiled into the application, a value of 'Plugin' in the list of
-- "Wiki.App.Plugin.Registry". It imports this module and nothing else of
-- the application's inside: a test of the imports keeps that
-- (@Test.PluginImports@). An external plugin, a later package, will
-- implement the same names over messages, in a sandboxed frame.
--
-- The interface changes freely while every plugin is built in. It is
-- frozen when the first external plugin ships.
module Wiki.App.Plugin
  ( -- * A plugin
    Plugin (..)
  , Manifest (..)
  , PluginId (..)
  , builtin

    -- * Diagnostics
  , Diagnostics (..)
  , Drawn (..)
  )
where

import Protolude
import Wiki.Core.Markdown.Problems (IslandPlace, Problem)
import Wiki.Core.Markdown.Render (IslandKey)

-- | 'PluginId' is a plugin's name for the program: short, in lower case,
-- and the same in every version of the plugin. Which plugins are on is a
-- set of these.
newtype PluginId = PluginId Text
  deriving stock (Eq, Ord, Show)

-- | 'Manifest' is what the wiki knows of a plugin before it runs any of
-- it: its id, and its name and description as the settings list them.
data Manifest = Manifest
  { id :: PluginId
  , name :: Text
  , description :: Text
  }
  deriving stock (Eq, Show)

-- | The manifest of a plugin built into the wiki: its id, its name and its
-- description.
builtin :: Text -> Text -> Text -> Manifest
builtin pluginId name description = Manifest {id = PluginId pluginId, name, description}

-- | 'Plugin' is a plugin: its manifest, and each contribution it makes,
-- or 'Nothing' for one it does not make.
data Plugin = Plugin
  { manifest :: Manifest
  , diagnostics :: Maybe Diagnostics
  }

-- | 'Diagnostics' is a contribution of problems in the text being edited,
-- which the editor marks, counts and steps through with the problems it
-- finds itself. 'diagnose' is called each time the editor publishes its
-- problems: after the check that follows the typing, and each time an
-- island of the preview fails or is drawn after failing. It is pure, so
-- the problems appear and go exactly when what it reads changes.
newtype Diagnostics = Diagnostics {diagnose :: Drawn -> [Problem]}

-- | 'Drawn' is what the editor gives 'Diagnostics': the text it last
-- checked, where each island of that text that can fail is written
-- ('Wiki.Core.Markdown.Problems.checkText'), and the islands whose library
-- failed as the preview drew them, by key, with the library's message. A
-- failure may name a key the text no longer has: the preview may still
-- show an older text.
data Drawn = Drawn
  { text :: Text
  , islands :: Map IslandKey IslandPlace
  , failures :: Map IslandKey Text
  }
