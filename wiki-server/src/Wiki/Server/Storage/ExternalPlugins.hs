-- | The statements that keep the external plugins ("Wiki.Api.Plugins.External",
-- WP5.84 of docs/PLAN.md): the plugins installed, in @external_plugins@
-- (step 26), and who may add them, in the setting @external-plugins@.
-- "Wiki.Server.Storage" runs the operations over them, with their checks
-- and the audit log.
--
-- A plugin is one row: the wiki's, installed for everyone, has no owner;
-- a user's own has that user. The database keeps an id once among the
-- wiki's and once among each user's (two partial unique indexes), so a
-- replacement updates the row in place rather than adding one.
module Wiki.Server.Storage.ExternalPlugins
  ( -- * Who may add plugins
    externalPolicySetting
  , loadPolicy
  , storePolicy

    -- * The plugins installed
  , Installed (..)
  , loadInstalled
  , loadOneInstalled
  , insertInstalled
  , replaceInstalled
  , deleteInstalled
  )
where

import Data.Aeson (decodeStrict', encode)
import Data.ByteString.Lazy qualified as BSL
import Data.Time (UTCTime)
import Database.Beam (QExpr, all_, asc_, delete, filter_, insert, insertValues, isNothing_, just_, orderBy_, runDelete, runInsert, runSelectReturningList, runSelectReturningOne, runUpdate, select, update, val_, (&&.), (<-.), (==.))
import Protolude
import Wiki.Api.Plugins (PluginId (..))
import Wiki.Api.Plugins.External (ExternalManifest (..), ExternalPolicy (..), externalPolicyName)
import Wiki.Core.Id (BlobId (..), UserId (..))
import Wiki.Server.Storage.Backend (Backend (..))
import Wiki.Server.Storage.Rows (settingValue, storeSetting)
import Wiki.Server.Storage.Schema (InstalledPlugins (..), Settings (..), WikiDb (..), wikiDb)

--------------------------------------------------------------------------------
-- Who may add plugins

-- | The name of the setting that holds who may add plugins.
externalPolicySetting :: Text
externalPolicySetting = "external-plugins"

-- | Who may add plugins. No row, or a value the program does not know, is
-- 'ExternalOff': external plugins are off until the administrator turns
-- them on.
loadPolicy :: (Backend be) => Db be ExternalPolicy
loadPolicy = maybe ExternalOff (\t -> fromMaybe ExternalOff (find (\p -> externalPolicyName p == t) [minBound .. maxBound])) <$> settingValue externalPolicySetting

-- | Store who may add plugins. 'ExternalOff' has no row.
storePolicy :: (Backend be) => ExternalPolicy -> Db be ()
storePolicy = \case
  ExternalOff -> runDelete (delete wikiDb.settings (\r -> r.key ==. val_ externalPolicySetting))
  p -> storeSetting externalPolicySetting (externalPolicyName p)

--------------------------------------------------------------------------------
-- The plugins installed

-- | 'Installed' is an external plugin as the database keeps it: its row's
-- id, its owner ('Nothing' for the wiki's), its manifest, the blob of its
-- bundle, the bundle's size, and when it was installed or last replaced.
data Installed = Installed
  { rowId :: Text
  , owner :: Maybe UserId
  , manifest :: ExternalManifest
  , bundle :: BlobId
  , size :: Int64
  , installedAt :: UTCTime
  }
  deriving stock (Eq, Show)

-- | The plugins of an owner ('Nothing' for the wiki's), in the order of
-- their ids. A row whose manifest the program cannot read is left out.
loadInstalled :: (Backend be) => Maybe UserId -> Db be [Installed]
loadInstalled owner =
  mapMaybe installedOf
    <$> runSelectReturningList (select (orderBy_ (\r -> asc_ r.plugin_id) (filter_ (ownedBy owner) (all_ wikiDb.external_plugins))))

-- | The plugin of an owner with an id, if there is one.
loadOneInstalled :: (Backend be) => Maybe UserId -> PluginId -> Db be (Maybe Installed)
loadOneInstalled owner p =
  (>>= installedOf) <$> runSelectReturningOne (select (filter_ (\r -> ownedBy owner r &&. r.plugin_id ==. val_ p.text) (all_ wikiDb.external_plugins)))

-- | Store a plugin with an id its owner has none of.
insertInstalled :: (Backend be) => Installed -> Db be ()
insertInstalled i = runInsert (insert wikiDb.external_plugins (insertValues [rowOf i]))

-- | Replace the manifest, the bundle and the time of an owner's plugin
-- with an id, keeping its row.
replaceInstalled :: (Backend be) => Installed -> Db be ()
replaceInstalled i =
  runUpdate
    ( update
        wikiDb.external_plugins
        (\r -> mconcat [r.manifest <-. val_ row.manifest, r.bundle <-. val_ row.bundle, r.bundle_size <-. val_ row.bundle_size, r.installed_at <-. val_ row.installed_at])
        (\r -> r.id ==. val_ i.rowId)
    )
  where
    row = rowOf i

-- | Delete an owner's plugin with an id.
deleteInstalled :: (Backend be) => Maybe UserId -> PluginId -> Db be ()
deleteInstalled owner p = runDelete (delete wikiDb.external_plugins (\r -> ownedBy owner r &&. r.plugin_id ==. val_ p.text))

-- | Whether a row is an owner's ('Nothing' for the wiki's).
ownedBy :: (Backend be) => Maybe UserId -> InstalledPlugins (QExpr be s) -> QExpr be s Bool
ownedBy = \case
  Nothing -> \r -> isNothing_ r.owner_id
  Just u -> \r -> r.owner_id ==. just_ (val_ u.text)

rowOf :: Installed -> InstalledPlugins Identity
rowOf i =
  InstalledPluginRow
    { id = i.rowId
    , owner_id = (\u -> u.text) <$> i.owner
    , plugin_id = i.manifest.id.text
    , manifest = decodeUtf8 (BSL.toStrict (encode i.manifest))
    , bundle = i.bundle.text
    , bundle_size = i.size
    , installed_at = i.installedAt
    }

installedOf :: InstalledPlugins Identity -> Maybe Installed
installedOf r = do
  manifest <- decodeStrict' (encodeUtf8 r.manifest)
  pure Installed {rowId = r.id, owner = UserId <$> r.owner_id, manifest, bundle = BlobId r.bundle, size = r.bundle_size, installedAt = r.installed_at}
