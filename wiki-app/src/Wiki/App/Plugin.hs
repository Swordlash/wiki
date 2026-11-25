-- | The plugins' interface: what a plugin is, what it can give the wiki,
-- and what it may ask of the wiki (#186; decision 69 and WP5.84 of
-- docs/PLAN.md).
--
-- A plugin is a 'Manifest' and its contributions. A contribution is what
-- the plugin gives the wiki, and the host calls it: 'Diagnostics',
-- problems the editor marks in the text; an 'Agent', which works on the
-- page beside the author; and 'Options', the plugin's settings. A
-- plugin's own options, which the server keeps for each user in the clear
-- (decision 69), come with the input of each of its surfaces, and a
-- surface changes them by telling the host ('SetOptions',
-- 'SaveOptions'): the host writes them, and the write goes on after the
-- surface has gone. A
-- capability is what the plugin may ask of the host: read the document
-- and propose a change to it, link to its settings ('Capabilities'). A
-- surface is what the plugin draws itself: for a built-in plugin, a
-- Halogen component. The small things are data that the host draws, so
-- that they look like the wiki's own: an agent's buttons
-- ('AgentButton'), and problems.
--
-- A built-in plugin is Haskell compiled into the application, a value of
-- 'Plugin' in the list of "Wiki.App.Plugin.Registry". It imports this
-- module, the capabilities under "Wiki.App.Plugin.Capability", its own
-- modules, wiki-core and packages that are not the wiki's; nothing else of
-- the application's inside. A test of the imports keeps that
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

    -- * An agent
  , Agent (..)
  , AgentQuery (..)
  , AgentOutput (..)
  , AgentInput (..)
  , Editing (..)
  , AgentButton (..)
  , Face (..)

    -- * Options
  , Options (..)
  , OptionsInput (..)
  , OptionsOutput (..)

    -- * Capabilities
  , Capabilities (..)
  , Document (..)
  , OptionsLink
  , optionsLinkTo
  , LinkClick
  , optionsLinkProps
  , followOptionsLink

    -- * Helpers for surfaces
  , attr
  , focusElement
  , clearField
  )
where

import DOM.HTML.Indexed qualified as I
import Data.Aeson (Value)
import Halogen qualified as H
import Halogen.HTML.Properties qualified as HP
import Halogen.Router (Follow, Router)
import Halogen.Router qualified as Router
import Protolude
import Wiki.Api.Plugins (PluginId (..))
import Wiki.App.Editor.Browser (clearField, focusElement)
import Wiki.App.Plugin.Capability.Assistant (AssistantCapabilities)
import Wiki.App.Route (Route)
import Wiki.App.View.Common (attr)
import Wiki.Core.Assistant.Edit (Change, Decision, Edit, Refusal, Replacement)
import Wiki.Core.Assistant.Prompt (Span)
import Wiki.Core.Id (UserId)
import Wiki.Core.Markdown.Problems (IslandPlace, Problem)
import Wiki.Core.Markdown.Render (IslandKey)

-- 'PluginId' is a plugin's name for the program, the server's type
-- ("Wiki.Api.Plugins"): short, in lower case, and the same in every
-- version of the plugin. Which plugins are on is a set of these.

-- | 'Manifest' is what the wiki knows of a plugin before it runs any of
-- it: its id, its name and description as the settings list them, and
-- whether it is on for a user who made no choice ('onByDefault'). The
-- server keeps only the users' choices and the administrator's set, so
-- the default is the manifest's.
data Manifest = Manifest
  { id :: PluginId
  , name :: Text
  , description :: Text
  , onByDefault :: Bool
  }
  deriving stock (Eq, Show)

-- | The manifest of a plugin built into the wiki: its id, its name and its
-- description. A built-in plugin is on by default.
builtin :: Text -> Text -> Text -> Manifest
builtin pluginId name description = Manifest {id = PluginId pluginId, name, description, onByDefault = True}

-- | 'Plugin' is a plugin, in the monad the application's components run
-- in: its manifest, and each contribution it makes, or 'Nothing' for one
-- it does not make.
data Plugin m = Plugin
  { manifest :: Manifest
  , diagnostics :: Maybe Diagnostics
  , agent :: Maybe (Agent m)
  , options :: Maybe (Options m)
  }

--------------------------------------------------------------------------------
-- Diagnostics

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

--------------------------------------------------------------------------------
-- An agent

-- | 'Agent' is a contribution to the editor: something that works on the
-- page beside the author, as the writing assistant does. Its 'surface' is
-- mounted with the editor and kept until the editor closes. It draws
-- nothing until its first button opens it; from then on it draws its
-- panel, which the host holds in a frame of its own beside the text and
-- hides while the agent is closed. The agent's buttons are data
-- ('Buttons'), which the host draws over the text.
newtype Agent m = Agent {surface :: H.Component AgentQuery (AgentInput m) AgentOutput m}

-- | 'AgentQuery' is what the host tells an agent: one of its buttons was
-- pressed ('AgentButton''s @key@), and whether the press opens the agent
-- ('True') or closes it. The host has closed its own menus and shown the
-- frame before it tells an opening, and hidden the frame before it tells
-- a closing.
data AgentQuery a = Press Text Bool a

-- | 'AgentOutput' is what an agent tells the host: the buttons it has now;
-- that it closed itself (its own close button, or Escape); that the author
-- accepted or rejected all of the changes it proposed; or the plugin's
-- options as they are to be kept from now on ('Nothing' clears them).
data AgentOutput
  = Buttons [AgentButton]
  | Closed
  | DecideAll Decision
  | SetOptions (Maybe Value)

-- | 'AgentInput' is what the host gives an agent each time the editor
-- renders: who is signed in, the plugin's options, the page being edited
-- once it is loaded, and the capabilities.
data AgentInput m = AgentInput
  { user :: Maybe UserId
  , options :: Maybe Value
  -- ^ The plugin's options for the user: as the server keeps them, or as
  -- last set while their write is out. 'Nothing' when there are none.
  , editing :: Maybe Editing
  , can :: Capabilities m
  }

-- | 'Editing' is the page being edited, as the editor holds it: its title
-- and text as last typed, the selection, how many of the changes an agent
-- proposed are still to decide, and what the author decided of each, by
-- the change's number. 'encrypted' says the page is in an encrypted space,
-- so that what an agent sends elsewhere leaves the browser decrypted.
-- 'document' reaches the editor's text itself, once it is in place.
data Editing = Editing
  { title :: Text
  , body :: Text
  , selection :: Maybe Span
  , pending :: Int
  , decisions :: Map Int Decision
  , encrypted :: Bool
  , document :: Maybe Document
  }

-- | 'AgentButton' is a button of an agent's, which the host draws over the
-- editor's text. 'key' names it in 'Press' and in the page's
-- @data-provider@ attribute; 'label' is its text, shown on hover;
-- 'title' and 'ariaLabel' are its tooltip and its name for screen
-- readers; 'selected' says it is the one the agent uses now, which is
-- pressed while the agent is open; 'unreachable' dims it, since what it
-- reaches cannot be reached from here.
data AgentButton = AgentButton
  { key :: Text
  , label :: Text
  , title :: Text
  , ariaLabel :: Text
  , selected :: Bool
  , unreachable :: Bool
  , face :: Face
  }
  deriving stock (Eq, Show)

-- | 'Face' is what an 'AgentButton' shows: a logo (one SVG path in a 24 by
-- 24 box, in its colour or in the text's), the wiki's speech bubble, or a
-- letter.
data Face = Logo Text (Maybe Text) | ChatGlyph | Letter Text
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Options

-- | 'Options' is a plugin's settings: a component the settings page shows
-- in a section of its own.
newtype Options m = Options {surface :: H.Component (Const Void) (OptionsInput m) OptionsOutput m}

-- | 'OptionsInput' is what the settings page gives a plugin's options: who
-- is signed in, the plugin's options ('AgentInput''s @options@), and the
-- capabilities.
data OptionsInput m = OptionsInput
  { user :: Maybe UserId
  , options :: Maybe Value
  , can :: Capabilities m
  }

-- | 'OptionsOutput' is what a plugin's settings tell the host: the
-- plugin's options as they are to be kept from now on ('Nothing' clears
-- them).
newtype OptionsOutput = SaveOptions (Maybe Value)
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Capabilities

-- | 'Capabilities' is what a plugin may ask of the host, as functions:
-- whether a browser is there to run it ('here'; a page rendered ahead of
-- the program has none); whether the page is a secure context ('secure');
-- a link to the plugin's settings ('optionsLink'); and the capabilities
-- that only the writing assistant uses
-- ("Wiki.App.Plugin.Capability.Assistant").
data Capabilities m = Capabilities
  { here :: m Bool
  , secure :: m Bool
  , optionsLink :: OptionsLink
  , assistant :: AssistantCapabilities m
  }

-- | 'Document' is the editor's text, which an agent reads and changes:
-- the text as it is now ('text'); the focus given back to it ('focus');
-- a change proposed as a tracked change with the given number, which the
-- author accepts or rejects ('propose': where it went and the numbers of
-- the changes it merged, or why it cannot be made); and the proposed
-- changes still undecided ('tracked'). Each returns at once.
data Document = Document
  { text :: IO Text
  , focus :: IO ()
  , propose :: Int -> Edit -> IO (Either Refusal (Replacement, [Int]))
  , tracked :: IO [Change]
  }

-- | 'OptionsLink' is a link to a plugin's settings, which the application
-- follows itself ('optionsLinkProps', 'followOptionsLink').
data OptionsLink = OptionsLink (Router Route) Route

-- | The link to a route of this router: for the host, which makes the
-- capabilities.
optionsLinkTo :: Router Route -> Route -> OptionsLink
optionsLinkTo = OptionsLink

-- | 'LinkClick' is a plain click on an 'OptionsLink', to follow.
newtype LinkClick = LinkClick (Follow Route)

-- | The properties of an @\<a>@ that links to a plugin's settings: its
-- address, and its plain click, handed to the component as an action.
optionsLinkProps :: OptionsLink -> (LinkClick -> action) -> [HP.IProp I.HTMLa action]
optionsLinkProps (OptionsLink router route) onFollow = Router.linkProps router (onFollow . LinkClick) route

-- | Follow a plain click on an 'OptionsLink'.
followOptionsLink :: (MonadIO m) => OptionsLink -> LinkClick -> H.HalogenM st act slots output m ()
followOptionsLink (OptionsLink router _) (LinkClick f) = Router.follow router f
