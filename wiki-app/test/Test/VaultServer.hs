-- | The server's vault, as a model the tests run the application's code
-- against ("Wiki.App.Assistant.Sync"'s 'MonadVault'): one vault, stored
-- whole over the version its tag names, refused as a 'Conflict' over
-- another, with the limits "Wiki.Server.Vault" checks on what it stores
-- (at least one item, at most 16, named in lower-case letters, digits and
-- hyphens, none empty or longer than 256 KiB). A passkey's wrap that a new
-- version does not name is kept, as the server keeps it. Everything else
-- of the class is the monad's below.
--
-- A test can put work between a device's fetch and its store
-- ('beforeNextStore'): another device's change, which the server then
-- holds when the first device's store arrives.
module Test.VaultServer
  ( ServerState (..)
  , Served
  , served
  , newServer
  , serverState
  , beforeNextStore
  , setVault
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Char (isAsciiLower)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Halogen.VDom.DOM.Monad.Class (MonadAttributes, MonadBrowserDOM, MonadDOM)
import Protolude
import Wiki.Api.Vault (StoredVault (..), VaultChange (..))
import Wiki.App.Assistant (MonadAssistant (..))
import Wiki.App.Assistant.Sync (Fetched (..), MonadVault (..))
import Wiki.App.Backend (WikiError (..))
import Wiki.Core.Vault (Ciphertext (..), Item (..), ItemName (..), Nonce (..), Sealed (..), Unlock (..), Vault (..), VaultTag (..))

-- | 'ServerState' is what the model of the server holds.
data ServerState = ServerState
  { kept :: Maybe (Int, Vault)
  -- ^ The vault and its revision, the tag's number.
  , fetches :: Int
  -- ^ How many times the vault was fetched.
  , stores :: Int
  -- ^ How many versions were stored.
  , hooks :: [IO ()]
  -- ^ What runs before each of the next stores, one each, first first.
  }

-- | A server with this vault, if any, at revision 1.
newServer :: Maybe Vault -> IO (IORef ServerState)
newServer v = newIORef ServerState {kept = (1,) <$> v, fetches = 0, stores = 0, hooks = []}

-- | What the server holds now.
serverState :: IORef ServerState -> IO ServerState
serverState = readIORef

-- | Run this before the next store that has no work of its own put before
-- it yet.
beforeNextStore :: IORef ServerState -> IO () -> IO ()
beforeNextStore ref act = modifyIORef' ref (\s -> s {hooks = s.hooks <> [act]})

-- | Store a vault as another device would, at the next revision.
setVault :: IORef ServerState -> Vault -> IO ()
setVault ref v = modifyIORef' ref (\s -> s {kept = Just (maybe 1 ((+ 1) . fst) s.kept, v)})

-- | 'Served' is a monad whose vault is the model's.
newtype Served m a = Served (ReaderT (IORef ServerState) m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, PrimMonad)

instance MonadTrans Served where
  lift = Served . lift

deriving newtype instance (MonadDOM m) => MonadDOM (Served m)

deriving newtype instance (MonadAttributes m) => MonadAttributes (Served m)

deriving newtype instance (MonadBrowserDOM m) => MonadBrowserDOM (Served m)

-- | Run with the vault of this server.
served :: IORef ServerState -> Served m a -> m a
served ref (Served r) = runReaderT r ref

instance (MonadAssistant m) => MonadAssistant (Served m)

instance (MonadVault m, MonadAssistant m, MonadIO m) => MonadVault (Served m) where
  vaultSync = pure True
  fetchVault tag = Served $ do
    ref <- ask
    liftIO $ atomicModifyIORef' ref $ \s ->
      ( s {fetches = s.fetches + 1}
      , Right $ case s.kept of
          Nothing -> NoVault
          Just (r, v)
            | Just (tagOf r) == tag -> Unchanged
            | otherwise -> Fetched StoredVault {tag = tagOf r, vault = v}
      )
  storeVault tag v = storing tag v
  storeVaultChange tag c = storing tag c.vault

  -- A password proved again gives the page a key.
  provePasswordBy _ _ = Right . Just <$> lift (newVaultKey False)

-- | The tag of a revision.
tagOf :: Int -> VaultTag
tagOf r = VaultTag (show r)

-- | Store a version of the vault over the one the tag names, after the
-- work put before this store, if any.
storing :: (MonadIO m) => Maybe VaultTag -> Vault -> Served m (Either WikiError StoredVault)
storing tag v = Served $ do
  ref <- ask
  hook <- liftIO $ atomicModifyIORef' ref $ \s -> case s.hooks of
    h : rest -> (s {hooks = rest}, Just h)
    [] -> (s, Nothing)
  liftIO (sequence_ hook)
  liftIO $ atomicModifyIORef' ref $ \s -> case (problem v, s.kept, tag) of
    (Just e, _, _) -> (s, Left e)
    (_, Nothing, Nothing) -> stored s 1 v
    (_, Just (r, old), Just t)
      | t == tagOf r -> stored s (r + 1) v {wraps = Map.union v.wraps (Map.filterWithKey (\u _ -> isPasskey u) old.wraps)}
    (_, Nothing, Just _) -> (s, Left (Conflict "There is no vault any more: synchronisation was turned off on another device."))
    (_, Just _, Nothing) -> (s, Left (Conflict "There is a vault already: unlock it with a passkey instead."))
    _ -> (s, Left (Conflict "The vault was changed on another device meanwhile."))
  where
    stored s r new = (s {kept = Just (r, new), stores = s.stores + 1}, Right StoredVault {tag = tagOf r, vault = new})
    isPasskey = \case
      ByPasskey _ -> True
      ByPassword -> False

-- | Why the server refuses to store a vault, if it does: "Wiki.Server.Vault"'s
-- limits on its items.
problem :: Vault -> Maybe WikiError
problem v
  | Map.null v.items = Just (Invalid "A vault holds at least one item.")
  | Map.size v.items > 16 || not (all nameOk (Map.keys v.items)) = Just (Invalid "A vault holds at most 16 items, named in lower-case letters, digits and hyphens.")
  | any (\i -> bad i.sealed.nonce.text || bad i.sealed.ciphertext.text) (Map.elems v.items) = Just (Invalid "This is not a vault this wiki keeps: a part of it is empty or too long.")
  | otherwise = Nothing
  where
    nameOk n = not (T.null n.text) && T.length n.text <= 32 && T.all (\c -> isAsciiLower c || isDigit c || c == '-') n.text
    bad t = T.null t || T.length t > 256 * 1024
