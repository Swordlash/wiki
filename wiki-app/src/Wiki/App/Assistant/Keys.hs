{-# LANGUAGE MultiWayIf #-}

-- | The writing assistant's settings as they were kept before plugins
-- (WP5.21 of docs/PLAN.md, "Wiki.Core.Assistant.Vault"): one record per
-- user in this browser, and the vault's item @assistant@ when they synced
-- across devices. The assistant keeps its settings as a plugin's now: its
-- keys among the plugin's secrets, the rest among its options
-- ("Wiki.Core.Assistant.Split"). The move from here to there is
-- "Wiki.App.Assistant.Migrate"'s. This module reads the legacy records
-- for it ('readLegacy'), and writes nothing.
--
-- = The record
--
-- The settings are sealed with a key kept in the assistant's slot of this
-- browser (the user's id). A browser that followed the vault keeps a copy
-- of the vault's item, which the vault key opens; while the vault was
-- locked there, what was typed on it was sealed with a key of the
-- assistant's own. A page the browser offers no WebCrypto to (plain HTTP)
-- kept the settings as text. The model lists and whether each provider
-- could be reached sit beside them in the clear.
module Wiki.App.Assistant.Keys
  ( -- * The legacy record, read for the move
    readLegacy
  , legacyRecordHere
  , recordSource
  )
where

import Data.Aeson (eitherDecodeStrict, toJSON)
import Data.ByteString.Lazy qualified as BSL
import Data.Digest.Pure.SHA (sha256, showDigest)
import Data.Text qualified as T
import Protolude
import Wiki.App.Assistant (KeyRef, MonadAssistant (..))
import Wiki.App.Vault (Step, forgetting)
import Wiki.Core.Assistant (ProviderSettings (..), Settings (..))
import Wiki.Core.Assistant.Migrate (OnDevice (..))
import Wiki.Core.Assistant.Vault
import Wiki.Core.Id (UserId)

--------------------------------------------------------------------------------
-- The legacy record, read for the move

-- | Whether this browser keeps a legacy record for the user, as stored:
-- the record is read, not opened.
legacyRecordHere :: (MonadAssistant m) => UserId -> m Bool
legacyRecordHere user = either (const False) isJust <$> deviceRecord user

-- | A record's identity as a legacy place of the move
-- ('Wiki.Core.Assistant.Migrate.source'): @record:@ and the first 32 hex
-- digits of the SHA-256 of its bytes as stored. The same bytes give the
-- same identity on every run, and a record written again (by a tab of an
-- earlier version) is another place. A plain record (L3) has no nonce of
-- its own to tell it by, so every record is told by its hash.
recordSource :: ByteString -> Text
recordSource bytes = "record:" <> T.take 32 (toS (showDigest (sha256 (BSL.fromStrict bytes))))

-- | This browser's legacy record for the user, opened as far as it opens,
-- with the vault key this browser holds, if any (the caller's to forget).
-- Nothing is written, and every key this reads is forgotten: a run of the
-- move reads it again after each step.
--
-- A record that follows the vault is an L1 record when the assistant's
-- key opens the vault's copy it holds: the copy is the vault's settings,
-- and the record holds nothing of its own. Otherwise (L4) it holds what
-- was typed while the vault was locked, sealed with the assistant's key,
-- and the vault key, if this browser holds it now, opens the copy. A
-- record that does not follow the vault holds its own settings (L2,
-- sealed; L3, as text).
readLegacy :: (MonadAssistant m) => UserId -> Maybe KeyRef -> Step m (Maybe OnDevice)
readLegacy user vaultKey =
  ExceptT (deviceRecord user) >>= \case
    Nothing -> pure Nothing
    Just bytes -> case readStored bytes of
      Left _ -> pure (Just (unreadable bytes))
      Right (OldSettings s) -> pure (Just OnDevice {bytes, source = recordSource bytes, own = Just s, copy = Nothing, copied = Nothing, follows = False, unopenable = False, askEachTime = False, learnt = learntIn s})
      Right (Current r) -> do
        slotKey <- lift (deviceKey user)
        opened <- forgettingMaybe slotKey $ case followed r of
          Just copySealed ->
            lift (opening slotKey copySealed) >>= \case
              -- L1: the assistant's key is the vault key.
              Just copy -> pure OnDevice {bytes, source = recordSource bytes, own = Nothing, copy = Just copy, copied = Just copySealed.nonce.text, follows = True, unopenable = False, askEachTime = r.askEachTime, learnt = learntIn r.learnt}
              -- L4: what was typed while the vault was locked, sealed with
              -- the assistant's own key. Or L1 after a reload on "ask each
              -- time": the assistant's slot is empty, and what the record
              -- sealed is the vault's, which the vault key here opens.
              Nothing -> do
                copy <- lift (opening vaultKey copySealed)
                typed <- join <$> traverse (lift . opening slotKey) r.sealed
                vaults :: Maybe Settings <- join <$> traverse (lift . opening vaultKey) r.sealed
                let unopenable = isJust r.sealed && isNothing typed && isNothing vaults
                pure OnDevice {bytes, source = recordSource bytes, own = typed, copy, copied = Just copySealed.nonce.text, follows = True, unopenable, askEachTime = r.askEachTime, learnt = learntIn r.learnt}
          Nothing -> do
            own <- join <$> traverse (lift . opening slotKey) r.sealed
            pure OnDevice {bytes, source = recordSource bytes, own, copy = Nothing, copied = Nothing, follows = False, unopenable = isJust r.sealed && isNothing own, askEachTime = r.askEachTime, learnt = learntIn r.learnt}
        pure (Just opened)
  where
    unreadable bytes = OnDevice {bytes, source = recordSource bytes, own = Nothing, copy = Nothing, copied = Nothing, follows = False, unopenable = True, askEachTime = False, learnt = Nothing}
    opening key sealed = case key of
      Nothing -> pure Nothing
      Just k ->
        openWith k sealed sealingData <&> \case
          Right text -> rightToMaybe (eitherDecodeStrict (encodeUtf8 text))
          Left _ -> Nothing
    -- What was learnt, when anything was.
    learntIn s
      | any (\(ProviderSettings _ _ _ _ f u) -> isJust f || u) s.byProvider = Just (toJSON (learntOf s))
      | otherwise = Nothing
    forgettingMaybe = maybe identity forgetting

-- | The vault's sealed settings, in the copy of the vault a record
-- follows, if it follows one.
followed :: DeviceRecord -> Maybe Sealed
followed record = record.server >>= \copy -> sealedOf copy.vault
