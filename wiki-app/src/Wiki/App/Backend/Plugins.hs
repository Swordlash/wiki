-- | Which plugins are on, the backend interface (WP5.84 of docs/PLAN.md,
-- "Wiki.Api.Plugins"): 'MonadPlugins' reads what the server keeps of the
-- plugins for whoever asks, sets the user's own state for a plugin, and
-- reads and sets the plugins the administrator turned off for the whole
-- wiki.
--
-- The shell reads 'pluginsNow' with the session and gives it to every
-- view ("Wiki.App.Plugin.Registry"'s @pluginsOn@). The settings' section
-- "Plugins" ("Wiki.App.View.Plugins") sets the user's state, and the
-- administration's switch ("Wiki.App.View.WikiPlugins") the wiki's set.
--
-- The HTTP backend asks the server ("Wiki.App.Backend.Http"). The
-- in-memory backend keeps both for the tab's life by the server's rules
-- ("Wiki.App.Backend.Mem.State"'s 'setPluginStateIn' and
-- 'setWikiPluginsIn'), so that the demo and the native tests behave as
-- the server does. A plugin's options are set and cleared here too, apart
-- from its state, so that a change of one never writes the other back.
module Wiki.App.Backend.Plugins
  ( MonadPlugins (..)
  )
where

import Control.Monad.Trans.Class (MonadTrans)
import Data.Aeson (Value)
import Data.Time (getCurrentTime)
import Data.Type.Equality (type (~))
import Halogen.Query.HalogenM (HalogenM)
import Halogen.Query.HalogenM qualified as H
import Protolude
import Wiki.Api.Plugins (PluginChoice, PluginId, PluginState, Plugins, WikiPlugins)
import Wiki.Api.Plugins.External (ExternalPlugin, ExternalPlugins, ExternalPolicy, Owner)
import Wiki.App.Backend (WikiError (..))
import Wiki.App.Backend.Mem (MemWiki, memQuery, memWrite)
import Wiki.App.Backend.Mem.State (externalPluginsIn, installExternalIn, pluginsIn, removeExternalIn, setExternalPolicyIn, setPluginOptionsIfNoneIn, setPluginOptionsIn, setPluginStateIn, setWikiPluginsIn, wikiPluginsIn)

-- | 'MonadPlugins' is the backend interface for which plugins are on.
--
-- Every method has a default that lifts it through a monad transformer,
-- as 'Wiki.App.Backend.MonadWiki''s do, so that a component's handlers
-- call them in 'HalogenM'.
class (Monad m) => MonadPlugins m where
  -- | The plugins off for the whole wiki, and the choices of whoever asks;
  -- someone not signed in has none. A closed wiki refuses someone not
  -- signed in ('Forbidden').
  pluginsNow :: m (Either WikiError Plugins)
  default pluginsNow :: (MonadTrans t, m ~ t n, MonadPlugins n) => m (Either WikiError Plugins)
  pluginsNow = lift pluginsNow

  -- | Turn a plugin on or off for the signed-in user, or give it back to
  -- its default. The result is the user's choice for the plugin after it.
  -- Nobody signed in is 'Forbidden'; an id that is not one, the server's
  -- operator, and a first choice past the most a user keeps are
  -- 'Invalid'.
  setPluginState :: PluginId -> PluginState -> m (Either WikiError PluginChoice)
  default setPluginState :: (MonadTrans t, m ~ t n, MonadPlugins n) => PluginId -> PluginState -> m (Either WikiError PluginChoice)
  setPluginState p = lift . setPluginState p

  -- | Replace the signed-in user's options for a plugin with a JSON
  -- value, leaving its state as it is. The result is the user's choice
  -- for the plugin after it. @null@, and a value whose JSON is longer than
  -- 'Wiki.Api.Plugins.maxOptionsBytes', are 'Invalid'; the other refusals
  -- are 'setPluginState''s.
  setPluginOptions :: PluginId -> Value -> m (Either WikiError PluginChoice)
  default setPluginOptions :: (MonadTrans t, m ~ t n, MonadPlugins n) => PluginId -> Value -> m (Either WikiError PluginChoice)
  setPluginOptions p = lift . setPluginOptions p

  -- | 'setPluginOptions', only if the user has no options for the plugin
  -- (@If-None-Match: *@): options there already are 'Conflict', and
  -- nothing is written. The server checks it in the write's own
  -- transaction, so two such writes cannot both find none. The move of
  -- the writing assistant's legacy settings adopts them this way.
  setPluginOptionsIfNone :: PluginId -> Value -> m (Either WikiError PluginChoice)
  default setPluginOptionsIfNone :: (MonadTrans t, m ~ t n, MonadPlugins n) => PluginId -> Value -> m (Either WikiError PluginChoice)
  setPluginOptionsIfNone p = lift . setPluginOptionsIfNone p

  -- | Clear the signed-in user's options for a plugin, leaving its state
  -- as it is. The result and the refusals are 'setPluginState''s.
  clearPluginOptions :: PluginId -> m (Either WikiError PluginChoice)
  default clearPluginOptions :: (MonadTrans t, m ~ t n, MonadPlugins n) => PluginId -> m (Either WikiError PluginChoice)
  clearPluginOptions = lift . clearPluginOptions

  -- | The plugins off for the whole wiki, for an administrator; anyone
  -- else is 'Forbidden'.
  wikiPluginsOff :: m (Either WikiError WikiPlugins)
  default wikiPluginsOff :: (MonadTrans t, m ~ t n, MonadPlugins n) => m (Either WikiError WikiPlugins)
  wikiPluginsOff = lift wikiPluginsOff

  -- | Replace the plugins off for the whole wiki, as an administrator. The
  -- result is the set as it now is. The users' own choices are kept, and
  -- apply again to a plugin turned back on.
  setWikiPluginsOff :: WikiPlugins -> m (Either WikiError WikiPlugins)
  default setWikiPluginsOff :: (MonadTrans t, m ~ t n, MonadPlugins n) => WikiPlugins -> m (Either WikiError WikiPlugins)
  setWikiPluginsOff = lift . setWikiPluginsOff

  -- | The external plugins the user may run, and who may add them, as
  -- @GET api\/v1\/external-plugins@ answers ("Wiki.Api.Plugins.External"):
  -- the wiki's while the policy is not off (and always to an
  -- administrator), the user's own while it lets users add theirs. The
  -- plugins' state reads it as the second request of each read of what is
  -- on ("Wiki.App.View.Plugins.Machine").
  externalPluginsNow :: m (Either WikiError ExternalPlugins)
  default externalPluginsNow :: (MonadTrans t, m ~ t n, MonadPlugins n) => m (Either WikiError ExternalPlugins)
  externalPluginsNow = lift externalPluginsNow

  -- | Set who may add external plugins, as an administrator; anyone else
  -- is 'Forbidden'. The answer is what 'externalPluginsNow' then answers
  -- the administrator.
  setExternalPolicy :: ExternalPolicy -> m (Either WikiError ExternalPlugins)
  default setExternalPolicy :: (MonadTrans t, m ~ t n, MonadPlugins n) => ExternalPolicy -> m (Either WikiError ExternalPlugins)
  setExternalPolicy = lift . setExternalPolicy

  -- | Install an external plugin for an owner: its manifest, as the JSON
  -- text of the file, its bundle's bytes, and, to replace the owner's
  -- plugin of the same id, the hash of the bundle being replaced
  -- (@POST api\/v1\/external-plugins\/wiki@ or @…\/mine@). The answer is the
  -- plugin as installed. The refusals are the server's, in its words: a
  -- manifest or a bundle that breaks the rules is 'Invalid'; a plugin of
  -- the id with another hash than @replaces@, or one too many, is
  -- 'Conflict'; the wiki's are an administrator's, a user's own need the
  -- policy 'Wiki.Api.Plugins.External.ExternalUsers' ('Forbidden').
  installExternal :: Owner -> Text -> ByteString -> Maybe Text -> m (Either WikiError ExternalPlugin)
  default installExternal :: (MonadTrans t, m ~ t n, MonadPlugins n) => Owner -> Text -> ByteString -> Maybe Text -> m (Either WikiError ExternalPlugin)
  installExternal o manifest bundle = lift . installExternal o manifest bundle

  -- | Remove an owner's external plugin by its id: one of the wiki's, as
  -- an administrator, or one of the user's own, whatever the policy. One
  -- the owner does not have is 'NotFound'.
  removeExternal :: Owner -> PluginId -> m (Either WikiError ())
  default removeExternal :: (MonadTrans t, m ~ t n, MonadPlugins n) => Owner -> PluginId -> m (Either WikiError ())
  removeExternal o = lift . removeExternal o

-- | A component's handlers reach the backend through its monad.
-- Each method runs in 'H.async': the handler waits for it, and the rest
-- of the tree goes on meanwhile.
instance (MonadPlugins m) => MonadPlugins (HalogenM st act slots output m) where
  pluginsNow = H.async pluginsNow
  setPluginState p = H.async . setPluginState p
  setPluginOptions p = H.async . setPluginOptions p
  setPluginOptionsIfNone p = H.async . setPluginOptionsIfNone p
  clearPluginOptions = H.async . clearPluginOptions
  wikiPluginsOff = H.async wikiPluginsOff
  setWikiPluginsOff = H.async . setWikiPluginsOff
  externalPluginsNow = H.async externalPluginsNow
  setExternalPolicy = H.async . setExternalPolicy
  installExternal o manifest bundle = H.async . installExternal o manifest bundle
  removeExternal o = H.async . removeExternal o

-- | The demo's instance: the in-memory state, by the server's rules.
instance (MonadIO m) => MonadPlugins (MemWiki m) where
  pluginsNow = memQuery pluginsIn
  setPluginState p = memWrite . setPluginStateIn p
  setPluginOptions p = memWrite . setPluginOptionsIn p . Just
  setPluginOptionsIfNone p = memWrite . setPluginOptionsIfNoneIn p
  clearPluginOptions p = memWrite (setPluginOptionsIn p Nothing)
  wikiPluginsOff = memQuery wikiPluginsIn
  setWikiPluginsOff = memWrite . setWikiPluginsIn
  externalPluginsNow = memQuery externalPluginsIn
  setExternalPolicy = memWrite . setExternalPolicyIn
  installExternal o manifest bundle replaces = liftIO getCurrentTime >>= \now -> memWrite (installExternalIn now o manifest bundle replaces)
  removeExternal o = memWrite . removeExternalIn o
