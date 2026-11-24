-- | The statements that keep which plugins are on ("Wiki.Api.Plugins",
-- WP5.84 of docs/PLAN.md): each user's choices, in @user_plugins@ (step
-- 25), and the plugins the administrator turned off for the whole wiki,
-- in the setting @plugins-off@. "Wiki.Server.Plugins" runs the users'
-- operations over them, and "Wiki.Server.Storage" the administrator's.
--
-- A user's choice for a plugin is one row with two columns, the state and
-- the options. Each change writes one column of the row and leaves the
-- other as the database has it ('writeState', 'writeOptions'), so two
-- requests that change one each, in any order, both take effect: the
-- switch flipped in one tab and the options saved in another. A row that
-- would hold neither is deleted, so that a user's rows are the plugins
-- they chose something for, and 'maxPluginsPerUser' counts those.
--
-- The wiki's set is one row of @settings@, its ids as a JSON array: the
-- table keeps the wiki's other settings, and a set the administrator
-- replaces whole fits one value. An empty set has no row.
module Wiki.Server.Storage.Plugins
  ( -- * The wiki's set
    pluginsOffSetting
  , loadPluginsOff
  , storePluginsOff

    -- * The users' choices
  , loadChoices
  , loadChoice
  , writeState
  , writeOptions
  )
where

import Data.Aeson (decodeStrict', encode)
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Database.Beam (all_, delete, filter_, insert, insertValues, runDelete, runInsert, runSelectReturningList, runSelectReturningOne, runUpdate, select, update, val_, (&&.), (<-.), (==.))
import Protolude
import Wiki.Api.Plugins (PluginChoice (..), PluginId (..), PluginState (..), maxPluginsPerUser, parsePluginId, pluginStateName)
import Wiki.App.Backend (WikiError (..))
import Wiki.Core.Id (UserId (..))
import Wiki.Server.Storage.Backend (Backend (..))
import Wiki.Server.Storage.Rows (settingValue, storeSetting)
import Wiki.Server.Storage.Schema (Settings (..), UserPlugins (..), WikiDb (..), wikiDb)

--------------------------------------------------------------------------------
-- The wiki's set

-- | The name of the setting that holds the plugins off for the wiki.
pluginsOffSetting :: Text
pluginsOffSetting = "plugins-off"

-- | The plugins off for the whole wiki. No row, or a value that is not a
-- list of ids, is none.
loadPluginsOff :: (Backend be) => Db be (Set PluginId)
loadPluginsOff = fromMaybe Set.empty . (>>= decodeStrict' . encodeUtf8) <$> settingValue pluginsOffSetting

-- | Store the plugins off for the whole wiki, in place of the set before.
storePluginsOff :: (Backend be) => Set PluginId -> Db be ()
storePluginsOff off
  | Set.null off = runDelete (delete wikiDb.settings (\r -> r.key ==. val_ pluginsOffSetting))
  | otherwise = storeSetting pluginsOffSetting (decodeUtf8 (BSL.toStrict (encode off)))

--------------------------------------------------------------------------------
-- The users' choices

-- | A user's choices, by plugin. A row whose id is not one is left out.
loadChoices :: (Backend be) => UserId -> Db be (Map PluginId PluginChoice)
loadChoices u = do
  rows <- runSelectReturningList (select (filter_ (\r -> r.user_id ==. val_ u.text) (all_ wikiDb.user_plugins)))
  pure (Map.fromList [(p, choiceOf r) | r <- rows, Right p <- [parsePluginId r.plugin_id]])

-- | A user's choice for a plugin: the default with no options when they
-- have none.
loadChoice :: (Backend be) => UserId -> PluginId -> Db be PluginChoice
loadChoice u p = maybe nothing choiceOf <$> rowOf u p
  where
    nothing = PluginChoice {state = PluginDefault, options = Nothing}

-- | Set a user's state for a plugin, leaving its options as they are. The
-- result is the choice after it; a first choice of a user who has
-- 'maxPluginsPerUser' already is 'Invalid'.
writeState :: (Backend be) => UserId -> PluginId -> PluginState -> Db be (Either WikiError PluginChoice)
writeState u p s = writeColumn u p StateColumn $ case s of
  PluginDefault -> Nothing
  _ -> Just (pluginStateName s)

-- | Set a user's options for a plugin, as JSON text, or clear them
-- ('Nothing'), leaving its state as it is. The result is 'writeState''s.
writeOptions :: (Backend be) => UserId -> PluginId -> Maybe Text -> Db be (Either WikiError PluginChoice)
writeOptions u p = writeColumn u p OptionsColumn

-- | 'Column' is one of the two columns of a user's row for a plugin that
-- a change writes.
data Column = StateColumn | OptionsColumn

-- | Write one column of a user's row for a plugin, and only that one. The
-- row is made when it is missing and the value is something, and deleted
-- when the value is nothing and the other column is nothing too.
writeColumn :: (Backend be) => UserId -> PluginId -> Column -> Maybe Text -> Db be (Either WikiError PluginChoice)
writeColumn u p column value =
  rowOf u p >>= \case
    Nothing
      | isNothing value -> Right <$> loadChoice u p
      | otherwise -> do
          kept <- length <$> runSelectReturningList (select (filter_ (\r -> r.user_id ==. val_ u.text) (all_ wikiDb.user_plugins)))
          if kept >= maxPluginsPerUser
            then pure (Left (Invalid ("You have choices kept for " <> show maxPluginsPerUser <> " plugins already, the most a user has: give one of them back to its default, with no options, first.")))
            else do
              let (stateText, optionsText) = case column of
                    StateColumn -> (value, Nothing)
                    OptionsColumn -> (Nothing, value)
              runInsert (insert wikiDb.user_plugins (insertValues [UserPluginRow {user_id = u.text, plugin_id = p.text, state = stateText, options = optionsText}]))
              Right <$> loadChoice u p
    Just r
      | isNothing value && isNothing (other r) -> do
          runDelete (delete wikiDb.user_plugins (\x -> x.user_id ==. val_ u.text &&. x.plugin_id ==. val_ p.text))
          Right <$> loadChoice u p
      | otherwise -> do
          case column of
            StateColumn -> runUpdate (update wikiDb.user_plugins (\x -> x.state <-. val_ value) (\x -> x.user_id ==. val_ u.text &&. x.plugin_id ==. val_ p.text))
            OptionsColumn -> runUpdate (update wikiDb.user_plugins (\x -> x.options <-. val_ value) (\x -> x.user_id ==. val_ u.text &&. x.plugin_id ==. val_ p.text))
          Right <$> loadChoice u p
  where
    other r = case column of
      StateColumn -> r.options
      OptionsColumn -> r.state

rowOf :: (Backend be) => UserId -> PluginId -> Db be (Maybe (UserPlugins Identity))
rowOf u p = runSelectReturningOne (select (filter_ (\r -> r.user_id ==. val_ u.text &&. r.plugin_id ==. val_ p.text) (all_ wikiDb.user_plugins)))

-- | A row as the API gives it. A state the program does not know is the
-- default, and options that are not JSON are none.
choiceOf :: UserPlugins Identity -> PluginChoice
choiceOf r =
  PluginChoice
    { state = case r.state of
        Just "on" -> PluginOn
        Just "off" -> PluginOff
        _ -> PluginDefault
    , options = r.options >>= decodeStrict' . encodeUtf8
    }
