-- | The move of the writing assistant's settings from their legacy places
-- to the plugin's (WP5.84 of docs/PLAN.md): a pure plan. The application
-- reads what is where ('Found'), takes the first step of the plan, reads
-- again, and goes on until the plan is empty ("Wiki.App.Assistant.Migrate").
-- This module decides; it reads and writes nothing.
--
-- = Where the settings were
--
-- Before plugins, the assistant kept its settings sealed in one record
-- ("Wiki.Core.Assistant.Vault"): in the vault's item @assistant@ when they
-- synced across devices, and in a record of this browser ('OnDevice'),
-- sealed with a key in the assistant's slot of this browser ('Slot'). A
-- browser's record is in one of four states:
--
-- * L1: it follows the vault, and holds a copy of the vault's settings,
--   which the vault key opens ('copy'). The vault key may be in the
--   assistant's slot only, on a browser that turned syncing on before the
--   vault had a slot of its own.
-- * L2: syncing off. The settings are sealed with a key of the
--   assistant's own ('own').
-- * L3: a page over plain HTTP, which cannot seal: the settings as text
--   ('own').
-- * L4: it follows the vault, which is locked here. It holds what was typed
--   on this browser meanwhile, sealed with a key of the assistant's own
--   ('own').
--
-- = Where they go
--
-- The keys go to the plugin's secrets ('KeepKeys'), sealed in the user's
-- vault, or kept in this browser where the user has no vault. The rest of
-- the settings go to the plugin's options, which the server keeps in the
-- clear ('AdoptOptions'). What was learnt from the providers goes to the
-- plugin's cache in this browser ('KeepLearnt'). An L1 browser's "ask each
-- time" becomes the vault's own ('KeepAsk'). The legacy places are emptied
-- last ('DropVaultItem', 'DropRecord', 'DropSlotKey').
--
-- = Why a plan read again after each step
--
-- Every step is idempotent, and is planned only when what was read says it
-- is still needed. A run stopped after any step, and run again, so reads
-- the state the stop left and plans the rest: the same end as a run never
-- stopped. Two browsers that move the same vault item at once each read
-- the other's steps. A tab of an earlier version of the wiki that adds the
-- legacy item again is moved again by the next run.
--
-- = Each legacy place taken once
--
-- The keys of a legacy place (the vault's item, this browser's record)
-- go to the plugin's item together with the place's identity ('source':
-- the item's nonce, the record's hash), in one write ('KeepKeys'). The
-- item keeps the places it took ("Wiki.Core.Vault.Plugin"'s 'taken').
-- A place taken is never taken again: a name the plugin's item lacks
-- afterwards was forgotten since, and is not filled from that place, even
-- while the place is still there (a run stopped before its drops, the
-- options not yet the server's). A place with no key is taken too, so
-- that the plugin's item is made, and says the settings were moved.
--
-- The plugin's item is never deleted once made, even with no key left
-- in it ("Wiki.App.Plugin.Host.Secrets"). An L1 record's copy of the
-- vault's settings, on a browser that has not moved yet, then never
-- counts as the last copy once another browser moved the vault's item
-- (see 'copyRedundant'):
-- a key forgotten after the move does not come back from it. Only a reset
-- of the password deletes the vault with the item in it. A browser that
-- still holds a legacy copy and a key that opens it may then move that
-- copy into the new vault: after a reset, nothing was forgotten one key at
-- a time, and the copy is what is left of the settings.
--
-- = What wins
--
-- A key the plugin's secrets already hold wins over a legacy one of the
-- same name: those secrets are what every browser that moved uses, and a
-- user may have changed them since. Among the legacy places, the vault's
-- item wins over what one browser holds, as when a locked browser was
-- unlocked before ('Wiki.Core.Assistant.Vault.mergeUnlocked'). The options
-- are taken only when the server has none: options already there were
-- moved by another browser, or chosen since.
--
-- = Nothing deleted too early
--
-- A legacy place is emptied only when the plugin's item, read back, says
-- the place was taken, the options it holds are the server's, its cache
-- was kept, and its "ask each time" taken. What cannot be opened is never
-- emptied.
module Wiki.Core.Assistant.Migrate
  ( -- * What is found
    Found (..)
  , InVault (..)
  , OnDevice (..)
  , Slot (..)

    -- * The plan
  , Step (..)
  , plan
  , isDrop

    -- * What the legacy places hold
  , sources
  , legacyKeys
  , legacyOptions
  )
where

import Data.Aeson (Value)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Protolude
import Text.Show (Show (showsPrec))
import Wiki.Core.Assistant (Settings)
import Wiki.Core.Assistant.Split (blankOptions, keysOf, optionsValue)
import Wiki.Core.Vault (Nonce (..), Sealed (..))
import Wiki.Core.Vault.Plugin (SecretName)

-- | 'InVault' is the vault's legacy item @assistant@, as this browser finds
-- it: none (or no vault); opened with the vault key this browser holds,
-- with the sealed text as read; there, but this browser holds no key that
-- opens the vault; or there, and the vault key does not open it.
data InVault
  = NoItem
  | ItemOpened Sealed Settings
  | ItemLocked
  | ItemUnopenable
  deriving stock (Eq, Show)

-- | 'OnDevice' is this browser's legacy record for the user, as read.
data OnDevice = OnDevice
  { bytes :: ByteString
  -- ^ The record as stored: only this record is deleted.
  , source :: Text
  -- ^ The record's identity as a legacy place ('KeepKeys'): the same for
  -- the same bytes.
  , own :: Maybe Settings
  -- ^ What only this browser holds: an L2 or L3 record's settings, or
  -- what was typed on an L4 browser while it was locked.
  , copy :: Maybe Settings
  -- ^ An L1 record's copy of the vault's settings, when the vault key
  -- here opens it.
  , copied :: Maybe Text
  -- ^ The nonce of the vault's item the copy is of: two browsers' copies
  -- of one item are one legacy place ('sources').
  , follows :: Bool
  -- ^ Whether the record follows the vault (L1, L4).
  , unopenable :: Bool
  -- ^ Whether a part of the record could not be opened or read.
  , askEachTime :: Bool
  -- ^ Whether the record asks for an unlock each time the wiki opens.
  , learnt :: Maybe Value
  -- ^ What was learnt from the providers, when anything was.
  }
  deriving stock (Eq)

-- | Shown without what it holds.
instance Show OnDevice where
  showsPrec _ r rest = "OnDevice {follows = " <> show r.follows <> ", unopenable = " <> show r.unopenable <> "}" <> rest

-- | 'Slot' is what the assistant's slot of keys in this browser holds:
-- nothing, the vault key, or a key of the assistant's own.
data Slot = NoKey | VaultKey | OwnKey
  deriving stock (Eq, Show)

-- | 'Found' is what a run reads before each step.
data Found = Found
  { vault :: InVault
  , record :: Maybe OnDevice
  , slot :: Slot
  , vaultSlot :: Bool
  -- ^ Whether the vault's slot in this browser holds a key that opens
  -- the vault.
  , target :: Maybe (Map SecretName Text)
  -- ^ The plugin's secrets as read now; 'Nothing' when they cannot be
  -- read (the vault is locked here).
  , taken :: Set Text
  -- ^ The legacy places the plugin's item took, as read with 'target';
  -- none when it cannot be read.
  , options :: Maybe (Maybe Value)
  -- ^ The plugin's options as the server keeps them, 'Nothing' until the
  -- server's answer is known.
  , local :: Bool
  -- ^ Whether the plugin's cache in this browser holds anything.
  , askHere :: Bool
  -- ^ Whether the vault asks for an unlock each time in this browser.
  }
  deriving stock (Eq, Show)

-- | 'Step' is one step of the move.
data Step
  = -- | The vault asks for an unlock each time in this browser, as the
    -- assistant's record did.
    KeepAsk
  | -- | The vault key in the assistant's slot kept in the vault's slot.
    CopyVaultKey
  | -- | The keys of these legacy places, by each place's identity, kept
    -- in the plugin's secrets in this order, and the places recorded as
    -- taken, in one write. A place the plugin's item took before is
    -- skipped, as read in that write; a place taken now fills only the
    -- names the secrets lack.
    KeepKeys [(Text, Map SecretName Text)]
  | -- | What was learnt kept as the plugin's cache, which holds nothing.
    KeepLearnt Value
  | -- | These options kept on the server, if it holds none. The plugins'
    -- state writes them ("Wiki.App.View.Plugins.Machine"), once it has read
    -- what the server holds.
    AdoptOptions Value
  | -- | The vault's item @assistant@ deleted, if it is still this one.
    DropVaultItem Sealed
  | -- | This browser's record deleted, if it is still this one.
    DropRecord ByteString
  | -- | The assistant's slot of keys emptied.
    DropSlotKey
  deriving stock (Eq)

-- | Shown without the keys and the options.
instance Show Step where
  showsPrec _ s rest =
    ( case s of
        KeepAsk -> "KeepAsk"
        CopyVaultKey -> "CopyVaultKey"
        KeepKeys ks -> "KeepKeys " <> show [(place, Map.keys k) | (place, k) <- ks]
        KeepLearnt _ -> "KeepLearnt"
        AdoptOptions _ -> "AdoptOptions"
        DropVaultItem _ -> "DropVaultItem"
        DropRecord _ -> "DropRecord"
        DropSlotKey -> "DropSlotKey"
    )
      <> rest

-- | Whether a step empties a legacy place.
isDrop :: Step -> Bool
isDrop = \case
  DropVaultItem _ -> True
  DropRecord _ -> True
  DropSlotKey -> True
  _ -> False

-- | The steps still to take, in order, given what is found. Empty when
-- nothing is left to move, or when the rest waits for something a run
-- cannot do: an unlock, or the server's options to be known.
plan :: Found -> [Step]
plan f = asking <> copyKey <> keys <> learnt <> adopt <> drops
  where
    asking = [KeepAsk | Just r <- [f.record], r.follows, r.askEachTime, not f.askHere]
    copyKey = [CopyVaultKey | f.slot == VaultKey, not f.vaultSlot]
    -- The places not taken yet. Where the plugin's secrets cannot be read,
    -- what they took is unknown: every place waits.
    pending = [p | p@(place, _) <- sources f, isNothing f.target || Set.notMember place f.taken]
    keys = [KeepKeys pending | isJust f.target, not (null pending)]
    keysSafe = null pending
    learnt = [KeepLearnt v | Just r <- [f.record], Just v <- [r.learnt], not f.local]
    options = mfilter (not . blankOptions) (optionsValue <$> legacyOptions f)
    -- Also while the server's options are not known yet: the plugins'
    -- state holds them until it has read the server's, and adopts them
    -- only if the server has none.
    adopt = case (f.options, options) of
      (Just (Just _), _) -> []
      (_, Just v) -> [AdoptOptions v]
      _ -> []
    optionsSafe = case options of
      Nothing -> not (optionsBehindLock f)
      Just _ -> maybe False isJust f.options
    settled = null asking && null copyKey && keysSafe && null learnt && optionsSafe
    drops
      | not settled = []
      | otherwise =
          -- The record first: whether its copy of the vault's settings is
          -- the last one depends on the vault's item still being there.
          [DropRecord r.bytes | Just r <- [f.record], recordDone r]
            <> [DropVaultItem s | ItemOpened s _ <- [f.vault]]
            <> [DropSlotKey | isNothing f.record, f.slot /= NoKey]
    recordDone r = not r.unopenable && (not r.follows || isJust r.copy || copyRedundant f)

-- | Whether an L1 record's copy of the vault's settings says nothing the
-- vault does not: the vault still holds its item, or the plugin's item
-- took a vault's item, which another browser moved. Otherwise (syncing was
-- turned off on another browser, by an earlier version of the wiki) the
-- copy is the settings' last copy, and is moved as this browser's own.
--
-- A plugin's item that took only browsers' own records (L2, L3) does not
-- make the copy redundant: those never held the vault's settings. The
-- plugin's item is never deleted once made, so once it took a vault's
-- item, a copy here never counts as the last one again, and a key
-- forgotten since does not come back from it.
copyRedundant :: Found -> Bool
copyRedundant f = case f.vault of
  ItemOpened _ _ -> True
  ItemLocked -> True
  _ -> any (itemPlace `T.isPrefixOf`) f.taken

-- | The start of a vault's item's identity as a legacy place ('sources').
itemPlace :: Text
itemPlace = "item:"

-- | Whether the options wait for an unlock: the vault's item is there,
-- locked, and this browser holds settings of its own, which the vault's
-- would win over.
optionsBehindLock :: Found -> Bool
optionsBehindLock f = f.vault == ItemLocked && any (\r -> isJust r.own) f.record

-- | The legacy places this browser can read, each by its identity, with
-- the keys it holds, in the order their keys win: the vault's item (by its
-- nonce); an L1 copy that is the settings' last copy, by the nonce of the
-- item it is a copy of, so that the copies two browsers keep of one item
-- are one place, taken once; and this browser's record, for what it holds
-- of its own. A record counts even with no key, and so does an item, so
-- that the move of a place with none is recorded too.
--
-- Copies of two versions of the item are two places: a key forgotten after
-- one was moved can come back from the other, if the other holds it.
sources :: Found -> [(Text, Map SecretName Text)]
sources f =
  [(itemPlace <> s.nonce.text, keysOf settings) | ItemOpened s settings <- [f.vault]]
    <> [(copyPlace <> n, keysOf c) | Just c <- [lastCopy f], Just r <- [f.record], Just n <- [r.copied]]
    <> [(r.source, maybe mempty keysOf r.own) | Just r <- [f.record]]

-- | The start of a last copy's identity as a legacy place ('sources').
copyPlace :: Text
copyPlace = "copy:"

-- | The keys the legacy places hold, by secret's name: the vault's item's,
-- then an L1 copy that is the last one, then this browser's own; the first
-- wins for a name more than one has.
legacyKeys :: Found -> Map SecretName Text
legacyKeys f = Map.unions (map snd (sources f))

-- | The settings whose options the legacy places hold: the first of the
-- vault's item's, an L1 copy's, and this browser's own whose options are
-- not the defaults. None while the vault's item is locked here.
--
-- Settings whose options are the defaults say nothing to adopt, and do not
-- hide a browser's own: otherwise a browser's own options would be adopted
-- or not depending on whether another browser had emptied the vault's item
-- first.
--
-- An L1 copy counts for the options even when the vault holds the
-- plugin's item: that item is what this run's own keys made of it as
-- much as another browser's move, and options another browser moved are
-- the server's already, which win.
legacyOptions :: Found -> Maybe Settings
legacyOptions f = case f.vault of
  ItemLocked -> Nothing
  v ->
    find
      (not . blankOptions . optionsValue)
      ([s | ItemOpened _ s <- [v]] <> [c | Just r <- [f.record], r.follows, Just c <- [r.copy]] <> [o | Just r <- [f.record], Just o <- [r.own]])

-- | An L1 copy that is the settings' last copy ('copyRedundant').
lastCopy :: Found -> Maybe Settings
lastCopy f
  | copyRedundant f = Nothing
  | otherwise = f.record >>= \r -> if r.follows then r.copy else Nothing
