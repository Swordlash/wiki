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
-- and propose a change to it, link to its settings, keep secrets in the
-- user's vault ('Secrets') and a cache in this browser ('Local')
-- ('Capabilities'). A
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
  , SecretSpec (..)

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

    -- * Secrets
  , Secrets (..)
  , SecretName (..)
  , Unavailable (..)
  , SecretField (..)
  , FieldSlot
  , fieldSlot
  , FieldInput (..)
  , FieldOutput (..)
  , secretField
  , Local (..)

    -- * Links
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
import Data.Row (HasType)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Halogen.Router (Follow, Router)
import Halogen.Router qualified as Router
import Halogen.Subscription (Emitter)
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
import Wiki.Core.Vault.Plugin (SecretName (..))

-- 'PluginId' is a plugin's name for the program, the server's type
-- ("Wiki.Api.Plugins"): short, in lower case, and the same in every
-- version of the plugin. Which plugins are on is a set of these.

-- | 'Manifest' is what the wiki knows of a plugin before it runs any of
-- it: its id, its name and description as the settings list them,
-- whether it is on for a user who made no choice ('onByDefault'), and the
-- secrets it keeps in the user's vault ('secrets'). The server keeps only
-- the users' choices and the administrator's set, so the default is the
-- manifest's.
data Manifest = Manifest
  { id :: PluginId
  , name :: Text
  , description :: Text
  , onByDefault :: Bool
  , secrets :: [SecretSpec]
  -- ^ The secrets the plugin may keep, by name. The host refuses any
  -- other name ('Secrets').
  }
  deriving stock (Eq, Show)

-- | The manifest of a plugin built into the wiki: its id, its name and its
-- description. A built-in plugin is on by default, and keeps no secrets
-- until it declares them.
builtin :: Text -> Text -> Text -> Manifest
builtin pluginId name description = Manifest {id = PluginId pluginId, name, description, onByDefault = True, secrets = []}

-- | 'SecretSpec' is a secret a plugin declares in its manifest: its name
-- ('SecretName'), and the label and description the host's field shows
-- for it ('SecretField').
data SecretSpec = SecretSpec
  { name :: SecretName
  , label :: Text
  , description :: Text
  }
  deriving stock (Eq, Show)

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
-- renders: who is signed in, the plugin's options and why their last
-- write was refused, the page being edited once it is loaded, and the
-- capabilities.
data AgentInput m = AgentInput
  { user :: Maybe UserId
  , options :: Maybe Value
  -- ^ The plugin's options for the user: as the server keeps them, or as
  -- last set while their write is out. 'Nothing' when there are none.
  , refused :: Maybe Text
  -- ^ Why the server refused the last write of the plugin's options for
  -- the user, in the server's words; 'options' are then the server's. It
  -- stays until a later write of the options succeeds, or options are set
  -- again ('SetOptions'). 'Nothing' when the last write was not refused.
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
-- is signed in, the plugin's options and why their last write was refused
-- ('AgentInput''s @options@ and @refused@), whether the page was adopted,
-- and the capabilities.
data OptionsInput m = OptionsInput
  { user :: Maybe UserId
  , options :: Maybe Value
  , refused :: Maybe Text
  , adopted :: Bool
  -- ^ Whether the settings are shown at the address the page started at,
  -- from what the page carries: a page the server rendered asks the server
  -- nothing as the program adopts it. The secrets then show what this
  -- browser holds of them. False once the reader has gone to another
  -- address: settings shown then read the secrets from the server as they
  -- open ('Secrets''s @refresh@), since the copy this browser holds can be
  -- older than the vault.
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
-- a link to the plugin's settings ('optionsLink'); its secrets, in the
-- user's vault ('secrets'); its cache in this browser ('local'); and the
-- capabilities that only the writing assistant uses
-- ("Wiki.App.Plugin.Capability.Assistant").
--
-- The host makes the record for one plugin and one reader
-- ("Wiki.App.Plugin.Host"), and makes it again each time it renders. The
-- plugin's id and the reader are inside its functions: no function takes
-- a plugin's id, so a plugin can reach only its own secrets and its own
-- cache.
data Capabilities m = Capabilities
  { here :: m Bool
  , secure :: m Bool
  , optionsLink :: OptionsLink
  , secrets :: Secrets m
  , local :: Local m
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

--------------------------------------------------------------------------------
-- Secrets

-- | 'Secrets' is a plugin's secrets, such as an API key: kept sealed in the
-- user's vault, which follows them to their other devices, under the names
-- the plugin's manifest declares ('SecretSpec'). Where the user has no
-- vault to keep them in (the demo, a page over plain HTTP, a user who has
-- not made one), they are kept in this browser, sealed where it can seal.
--
-- 'get' reads every secret the plugin keeps, by name, or says why it
-- cannot ('Unavailable'). 'put' keeps a value under a name, in the place of
-- the one it had; 'remove' forgets one. A name the manifest does not
-- declare is refused before anything is read. 'field' is the host's form
-- for one secret, which unlocks the vault where it is locked
-- ('secretField'). 'changes' tells each time what the secrets read as may
-- have changed: the vault unlocked or locked in this browser, a secret
-- kept or forgotten, here or in another tab; a surface that shows them
-- reads them again. It is made in place: call it with 'Halogen.liftEffect',
-- and subscribe. Each other function but 'field' waits: call it in
-- 'H.async'.
data Secrets m = Secrets
  { get :: m (Either Unavailable (Map SecretName Text))
  -- ^ As this browser holds them where it can: what a surface reads as it
  -- starts, which asks the server nothing ("Wiki.App.Plugin.Host.Secrets").
  , refresh :: m ()
  -- ^ Read them from the server, for when they are needed fresh (a
  -- request about to be made, settings that show them opened): a change
  -- since is announced, and 'changes' tells of it.
  , put :: SecretName -> Text -> m (Either Text ())
  , remove :: SecretName -> m (Either Text ())
  , field :: SecretField m
  , changes :: m (Emitter IO ())
  }

-- | 'Unavailable' is why a plugin's secrets cannot be read: the user's
-- vault is locked in this browser ('Locked': the host's field unlocks it);
-- nobody is signed in ('NoUser'); or something failed, with what to tell
-- the reader ('Failed').
data Unavailable = Locked | NoUser | Failed Text
  deriving stock (Eq, Show)

-- | 'SecretField' is the host's form for one of a plugin's secrets: its
-- label, whether it is set, a field to type a new value into, "Keep" and
-- "Forget", and the vault's unlock where the vault is locked. A value kept
-- is never shown again. The plugin puts it in its surface with
-- 'secretField'.
data SecretField m = SecretField
  { component :: H.Component (Const Void) (FieldInput m) FieldOutput m
  , reader :: (Int, Maybe UserId)
  -- ^ The reader the field's functions are made for: the session's number
  -- and the user.
  }

-- | 'FieldSlot' is the address of a 'SecretField' among a surface's
-- children: the reader it is made for, and the secret's name. A new reader
-- is a new address, so the field is made again for them, and none is kept
-- that still reads the former reader's secrets.
data FieldSlot = FieldSlot Int (Maybe UserId) SecretName
  deriving stock (Eq, Ord)

-- | The address of a field of these secrets for a secret's name.
fieldSlot :: Secrets m -> SecretName -> FieldSlot
fieldSlot s name = let (session, user) = s.field.reader in FieldSlot session user name

-- | 'FieldInput' is what a plugin gives a 'SecretField': the secret's name,
-- one the manifest declares, and a check of a value before it is kept,
-- with what to tell the reader when it fails (the assistant tries an API
-- key). The check waits; the field shows that it is checking. A check
-- that passes answers the value to keep, as the plugin makes it from the
-- text typed: the field keeps that value, so that what is kept is what was
-- checked (the assistant trims the spaces around a key, and tries the key
-- trimmed). A plugin that keeps the text as typed answers it unchanged.
--
-- 'binding' names what the check is for, when it depends on more than the
-- value: the assistant's is the service the key is tried against, its
-- provider and its address. A value is kept only if the binding is the same
-- when the check answers as when it was asked; otherwise the field keeps
-- nothing and says so. A plugin whose check depends on the value alone
-- gives an empty binding.
data FieldInput m = FieldInput
  { name :: SecretName
  , check :: Text -> m (Either Text Text)
  , binding :: Text
  }

-- | 'FieldOutput' is what a 'SecretField' tells the plugin: a value was kept
-- under the name, or the name's value was forgotten, or the field unlocked
-- the vault, so that the plugin reads its secrets again ('get'). And
-- 'KeepOver': the keep of a value typed under the name is over, kept
-- (after 'Kept') or not (the check refused it, the write failed), so that
-- a plugin that shows the check as going on shows it no more.
data FieldOutput = Kept SecretName | Removed SecretName | Unlocked | KeepOver SecretName
  deriving stock (Eq, Show)

-- | A 'SecretField' for one secret, in a surface whose children include
-- fields under the given label:
--
-- > type Slots = ("secret" .== H.Slot (Const Void) FieldOutput FieldSlot)
-- > ... secretField "secret" can.secrets FieldInput {name, check, binding = ""} FromField
secretField
  :: forall label
  ->forall action slots m
   . (HasType label (H.Slot (Const Void) FieldOutput FieldSlot) slots, KnownSymbol label)
  => Secrets m
  -> FieldInput m
  -> (FieldOutput -> action)
  -> H.ComponentHTML action slots m
secretField label s input onOutput = HH.slot label (fieldSlot s input.name) s.field.component input onOutput

-- | 'Local' is the plugin's cache in this browser, for this user: one JSON
-- value, kept in the clear, which no other device sees (the assistant's
-- lists of models). 'load' reads it, 'Nothing' when nothing is kept;
-- 'save' replaces it, or says why it could not. Both wait.
data Local m = Local
  { load :: m (Maybe Value)
  , save :: Value -> m (Either Text ())
  }

--------------------------------------------------------------------------------
-- Links

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
