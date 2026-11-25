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
-- A legacy place is emptied only when every key it holds was read back
-- from the plugin's secrets, the options it holds are the server's, its
-- cache was kept, and its "ask each time" taken. What cannot be opened is
-- never emptied.
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
  , legacyKeys
  , legacyOptions
  )
where

import Data.Aeson (Value)
import Data.Map.Strict qualified as Map
import Protolude
import Text.Show (Show (showsPrec))
import Wiki.Core.Assistant (Settings)
import Wiki.Core.Assistant.Split (blankOptions, keysOf, optionsValue)
import Wiki.Core.Vault (Sealed)
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
  , own :: Maybe Settings
  -- ^ What only this browser holds: an L2 or L3 record's settings, or
  -- what was typed on an L4 browser while it was locked.
  , copy :: Maybe Settings
  -- ^ An L1 record's copy of the vault's settings, when the vault key
  -- here opens it.
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
  , pluginItem :: Bool
  -- ^ Whether the vault holds the plugin's item: another browser moved
  -- the settings, or the plugin kept a secret since.
  , record :: Maybe OnDevice
  , slot :: Slot
  , vaultSlot :: Bool
  -- ^ Whether the vault's slot in this browser holds a key that opens
  -- the vault.
  , target :: Maybe (Map SecretName Text)
  -- ^ The plugin's secrets as read now; 'Nothing' when they cannot be
  -- read (the vault is locked here).
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
  | -- | These keys kept in the plugin's secrets, under the names the
    -- secrets lack. A name they hold keeps its value.
    KeepKeys (Map SecretName Text)
  | -- | What was learnt kept as the plugin's cache, which holds nothing.
    KeepLearnt Value
  | -- | These options kept on the server, which holds none. The plugins'
    -- state writes them ("Wiki.App.View.Plugins.Machine").
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
        KeepKeys ks -> "KeepKeys " <> show (Map.keys ks)
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
    wanted = legacyKeys f
    keys = case f.target of
      Just held
        | missing <- Map.difference wanted held
        , not (Map.null missing) ->
            [KeepKeys missing]
      _ -> []
    keysSafe = maybe (Map.null wanted) (\held -> Map.null (Map.difference wanted held)) f.target
    learnt = [KeepLearnt v | Just r <- [f.record], Just v <- [r.learnt], not f.local]
    options = mfilter (not . blankOptions) (optionsValue <$> legacyOptions f)
    adopt = case (f.options, options) of
      (Just Nothing, Just v) -> [AdoptOptions v]
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
-- vault does not: the vault still holds its item, or holds the plugin's,
-- which another browser moved it to. Otherwise (syncing was turned off on
-- another browser, by an earlier version of the wiki) the copy is the
-- settings' last copy, and is moved as this browser's own.
copyRedundant :: Found -> Bool
copyRedundant f = case f.vault of
  ItemOpened _ _ -> True
  ItemLocked -> True
  _ -> f.pluginItem

-- | Whether the options wait for an unlock: the vault's item is there,
-- locked, and this browser holds settings of its own, which the vault's
-- would win over.
optionsBehindLock :: Found -> Bool
optionsBehindLock f = f.vault == ItemLocked && any (\r -> isJust r.own) f.record

-- | The keys the legacy places hold, by secret's name: the vault's item's,
-- then an L1 copy that is the last one, then this browser's own; the first
-- wins for a name more than one has.
legacyKeys :: Found -> Map SecretName Text
legacyKeys f = Map.unions [maybe mempty keysOf (vaultSettings f), maybe mempty keysOf (lastCopy f), maybe mempty keysOf (f.record >>= \r -> r.own)]

-- | The settings whose options the legacy places hold: the vault's item's,
-- or else an L1 copy that is the last one, or else this browser's own.
-- None while the vault's item is locked here.
legacyOptions :: Found -> Maybe Settings
legacyOptions f = case f.vault of
  ItemOpened _ s -> Just s
  ItemLocked -> Nothing
  _ -> lastCopy f <|> (f.record >>= \r -> r.own)

vaultSettings :: Found -> Maybe Settings
vaultSettings f = case f.vault of
  ItemOpened _ s -> Just s
  _ -> Nothing

-- | An L1 copy that is the settings' last copy ('copyRedundant').
lastCopy :: Found -> Maybe Settings
lastCopy f
  | copyRedundant f = Nothing
  | otherwise = f.record >>= \r -> if r.follows then r.copy else Nothing
