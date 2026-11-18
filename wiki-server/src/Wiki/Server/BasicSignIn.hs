-- | Signing in with a login and a password in each request (HTTP Basic
-- authentication): how the API for a page's Markdown ("Wiki.Server.MarkdownApi",
-- WP5.138 of docs/PLAN.md) knows who asks. It is an API for programs on the
-- user's own machine. The password travels in the request, over the
-- wiki's HTTPS, outside the browser's OPAQUE exchange; the owner chose that
-- knowingly for this first cut.
--
-- = Checking the password
--
-- The server keeps no hash of a password, only the user's OPAQUE record.
-- So a password is checked by running both sides of an OPAQUE sign-in in
-- this process, against the stored record ('checkHere'). That costs the
-- record's stretching (Argon2id, 128 MiB by default) every time. One check
-- runs at a time ('BasicSignIns' holds the lock), so that a burst of
-- requests does not ask for that memory many times over.
--
-- = Remembered sign-ins
--
-- A login checked as right is remembered for 'rememberFor' (five
-- minutes), so that a program that sends many requests pays the stretching
-- once. What is remembered is an HMAC-SHA-256, under a key made at the
-- process's start and never stored, of the user's id, the password, and
-- the stored record the check ran against. The password itself is not
-- kept. The next request with the same login computes the same HMAC and
-- compares it in constant time. A change of password or a reset gives
-- the user a new record, so the HMAC no longer matches, and the next
-- request checks the password again. Every request also reads the account
-- afresh: a disabled or removed account is refused at once, remembered or
-- not.
--
-- = The limit on failed sign-ins
--
-- The limit is the form's own ("Wiki.Server.Limiter", the server's one
-- 'Limiter'), by login and address. A full check is counted as a failure
-- before it runs, and given back when the password was right, as the
-- form's OPAQUE sign-in does. A refused verdict refuses a remembered login
-- too. As with the form, the verdict and the count are two steps: requests
-- sent at the same moment may all pass the verdict before any is counted.
--
-- = What is refused
--
-- A wrong password, a login nobody has, an account from before OPAQUE
-- (which has a hash, not a record, until it signs in once in the browser),
-- an account with passkeys only, and a disabled account all get the same
-- answer, 'Refused', after the same stretching. An account with a second
-- factor gets 'SecondFactorOn': a password alone is not enough to sign it
-- in, and a request has no place for the code.
module Wiki.Server.BasicSignIn
  ( BasicSignIns
  , newBasicSignIns
  , rememberFor
  , fullChecks
  , Outcome (..)
  , signInBasic
  , credentialsOf
  )
where

import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC (HMAC (..), hmac)
import Crypto.Random (getRandomBytes)
import Data.ByteArray (constEq, convert)
import Data.ByteString qualified as BS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import Network.Wai.Middleware.HttpAuth (extractBasicAuth)
import Protolude
import Wiki.Api.Account (Account (..))
import Wiki.Core.Id (UserId (..))
import Wiki.Core.User (Password (..), User (..))
import Wiki.Server.Limiter (Limiter, Verdict (Allowed))
import Wiki.Server.Limiter qualified as Limiter
import Wiki.Server.Opaque (checkHere, recordFromStored)
import Wiki.Server.Storage (Storage, reading, serverSetup, storageStretch)
import Wiki.Server.Storage.Rows (StoredPassword (..), StoredRecord (..), loadPasswordByLogin, totpEnabled)

-- | 'BasicSignIns' is the server's memory of the logins checked as right,
-- with what checking them needs: the key of the HMAC, the lock that lets
-- one check run at a time, and how many full checks have run (the
-- metric @wiki_api_password_checks_total@). It lives as long as the
-- process, and starts empty.
data BasicSignIns = BasicSignIns
  { key :: ByteString
  -- ^ The HMAC's key: 32 random bytes made at the process's start.
  , remembered :: IORef (Map UserId (ByteString, UTCTime))
  -- ^ By user: the HMAC of the last login checked as right, and until
  -- when it is remembered.
  , checking :: MVar ()
  , checks :: IORef Int
  }

-- | Nothing remembered, under a new key.
newBasicSignIns :: IO BasicSignIns
newBasicSignIns = do
  key <- getRandomBytes 32
  remembered <- newIORef mempty
  checking <- newMVar ()
  checks <- newIORef 0
  pure BasicSignIns {key, remembered, checking, checks}

-- | How long a login checked as right is remembered: five minutes, the
-- time a proof of a password lasts ('Wiki.Server.Opaque.proofLifetime').
rememberFor :: NominalDiffTime
rememberFor = 5 * 60

-- | How many full checks of a password (each with its stretching) have run
-- since the process started.
fullChecks :: BasicSignIns -> IO Int
fullChecks s = readIORef s.checks

-- | 'Outcome' is what signing in with a login and a password gave.
data Outcome
  = -- | The login and password do not sign anyone in: see the module's
    -- haddock for every case this covers.
    Refused
  | -- | Too many failed sign-ins for this login from this address: not
    -- before this many seconds.
    Limited Int
  | -- | The password is right, but the account has a second factor.
    SecondFactorOn
  | SignedIn UserId
  deriving stock (Eq, Show)

-- | The login and the password of an @Authorization: Basic@ header, if it
-- is one and both decode as UTF-8.
credentialsOf :: ByteString -> Maybe (Text, Text)
credentialsOf header = do
  (login, password) <- extractBasicAuth header
  (,) <$> rightToMaybe (decodeUtf8' login) <*> rightToMaybe (decodeUtf8' password)

-- | Sign in with the login and password a request carries, from an
-- address, at a time: the checks of the module's haddock, in order. The
-- limit's verdict comes first. Then the account and its record are read.
-- A login remembered with this password and record signs in without the
-- stretching. Otherwise the check is counted as a failure, run, and the
-- failure given back if the password was right.
signInBasic :: Storage -> Limiter -> BasicSignIns -> UTCTime -> Text -> Text -> Text -> IO Outcome
signInBasic storage limiter s now address login password =
  Limiter.verdict limiter now login address >>= \case
    Limiter.Refused seconds -> pure (Limited seconds)
    Allowed -> do
      found <- reading storage $ do
        loaded <- loadPasswordByLogin (T.strip login)
        for loaded $ \(a, stored) -> (a,stored,) <$> totpEnabled a.user.id
      let usable = case found of
            Just (a, WithRecord r, factor) | not a.disabled -> Just (a.user.id, r, factor)
            _ -> Nothing
          digestOf (u, r, _) = mac s.key u password r
      known <- case usable of
        Just found'@(u, _, _) -> do
          kept <- Map.lookup u <$> readIORef s.remembered
          pure (any (\(d, until') -> until' > now && constEq d (digestOf found')) kept)
        Nothing -> pure False
      right <-
        if known
          then pure True
          else do
            Limiter.failed limiter now login address
            -- A long password is refused without the stretching, as the
            -- form's old sign-in refuses one ('Wiki.Server.Accounts.signIn').
            ok <-
              if T.length password > 1024
                then pure False
                else do
                  setup <- serverSetup storage
                  let record = case found of
                        Just (_, WithRecord r, _) -> recordFromStored r
                        _ -> Nothing
                      credential = case found of
                        Just (a, _, _) -> encodeUtf8 a.user.id.text
                        -- As the form's sign-in names a login nobody has.
                        Nothing -> "unknown:" <> encodeUtf8 (T.toCaseFold (T.strip login))
                  withMVar s.checking $ \() -> do
                    atomicModifyIORef' s.checks (\n -> (n + 1, ()))
                    checkHere setup (storageStretch storage) record credential (Password password)
            when (ok && isJust usable) $ Limiter.succeededAfter limiter now login address
            pure ok
      case usable of
        Just found'@(u, _, factor) | right -> do
          Limiter.succeeded limiter login address
          unless known $ atomicModifyIORef' s.remembered $ \m ->
            (Map.insert u (digestOf found', addUTCTime rememberFor now) (Map.filter (\(_, until') -> until' > now) m), ())
          pure (if factor then SecondFactorOn else SignedIn u)
        _ -> pure Refused

-- | The HMAC remembered for a user's login: of the user's id, the
-- password and the stored record, each with its length before it, so that
-- no two different triples are written the same.
mac :: ByteString -> UserId -> Text -> StoredRecord -> ByteString
mac key u password r = convert (hmac key (BS.concat (concatMap framed [encodeUtf8 u.text, encodeUtf8 password, encodeUtf8 r.record, encodeUtf8 r.stretch])) :: HMAC SHA256)
  where
    framed bytes = [show (BS.length bytes) <> ":", bytes]
