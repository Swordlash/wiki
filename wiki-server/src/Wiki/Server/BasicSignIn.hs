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
-- A record's stretching was chosen by the browser that registered it, so
-- it is not trusted: a record that asks for more passes or more memory
-- than the operator's budget ('budget', @--api-max-stretch@) is never run.
-- Its account is answered as one without a record: a check against the
-- server's fake record, with the stretching new records get (or the
-- budget, if that is less), and 'Refused'. That check costs what any other check costs, and its answer
-- and time are those of a wrong password.
--
-- Requests that carry the same login, password and record while a check of
-- them runs do not start another: they wait for that check's result. Only
-- the first is counted as a failure, and only one stretching runs. If that
-- check ends without a result (it threw, or its request was cancelled),
-- the requests that waited for it start again from the beginning; none is
-- let in without a check.
--
-- = Read again after the check
--
-- The check waits for the lock and runs the stretching, and the account
-- may change meanwhile: a reset, a new password, a second factor turned on,
-- the account disabled. So after the check every request reads the account
-- again, and is let in only if the record it checked is still the stored
-- one and the account is still enabled. Whether a second factor is on is
-- taken from that second read.
--
-- = Remembered sign-ins
--
-- A login checked as right is remembered for 'rememberFor' (five
-- minutes), so that a program that sends many requests pays the stretching
-- once. What is remembered is an HMAC-SHA-256, under a key made at the
-- process's start and never stored, of the user's id, the password, and
-- the stored record the check ran against. The HMAC is computed in full
-- before it is stored, so nothing remembered holds the password. The next
-- request with the same login computes the same HMAC and compares it in
-- constant time. A change of password or a reset gives the user a new
-- record, so the HMAC no longer matches, and the next request checks the
-- password again. Every request also reads the account afresh: a disabled
-- or removed account is refused at once, remembered or not. Every request
-- drops the entries that have expired.
--
-- = The limit on failed sign-ins
--
-- The limit is the form's own ("Wiki.Server.Limiter", the server's one
-- 'Limiter'), by login and address. A refused verdict refuses a remembered
-- login too. A full check is counted as a failure in the same step as its
-- verdict ('Wiki.Server.Limiter.reserve'), so requests sent at the same
-- moment cannot all pass the verdict before any is counted. The failure is
-- given back when the password was right, as the form's OPAQUE sign-in
-- does.
--
-- = What is refused
--
-- A wrong password, a login nobody has, an account from before OPAQUE
-- (which has a hash, not a record, until it signs in once in the browser),
-- an account with passkeys only, an account whose record asks for more
-- stretching than the budget, and a disabled account all get the same
-- answer, 'Refused', after a stretching of the same cost. An account with
-- a second factor gets 'SecondFactorOn': a password alone is not enough to
-- sign it in, and a request has no place for the code.
module Wiki.Server.BasicSignIn
  ( BasicSignIns
  , newBasicSignIns
  , newBasicSignInsWith
  , rememberFor
  , fullChecks
  , rememberedLogins
  , sharingChecks
  , withinBudget
  , Outcome (..)
  , signInBasic
  , credentialsOf
  )
where

import Control.Concurrent.STM (TMVar, TVar, modifyTVar', newEmptyTMVar, newTVarIO, readTMVar, readTVar, readTVarIO, tryPutTMVar, writeTVar)
import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC (HMAC (..), hmac)
import Crypto.OPAQUE (RegistrationRecord)
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
import Wiki.Api.Opaque (Stretch (..))
import Wiki.Core.Id (UserId (..))
import Wiki.Core.User (Password (..), User (..))
import Wiki.Server.Limiter (Limiter, Verdict (Allowed))
import Wiki.Server.Limiter qualified as Limiter
import Wiki.Server.Opaque (checkHere, recordFromStored)
import Wiki.Server.Storage (Storage, reading, serverSetup, storageStretch)
import Wiki.Server.Storage.Rows (StoredPassword (..), StoredRecord (..), loadPasswordByLogin, totpEnabled)

-- | 'BasicSignIns' is the server's memory of the logins checked as right,
-- with what checking them needs: the key of the HMAC, the budget of
-- stretching, the checks under way, the lock that lets one check run at a
-- time, and how many full checks have run (the metric
-- @wiki_api_password_checks_total@). It lives as long as the process, and
-- starts empty.
data BasicSignIns = BasicSignIns
  { key :: ByteString
  -- ^ The HMAC's key: 32 random bytes made at the process's start.
  , budget :: Stretch
  -- ^ The most stretching a check runs ('withinBudget').
  , memory :: TVar Memory
  , checking :: MVar ()
  , checks :: IORef Int
  , around :: IO Bool -> IO Bool
  -- ^ What runs a full check: the check itself, or, in the tests, the
  -- check with something done before or after it.
  }

-- | 'Memory' is what 'BasicSignIns' changes as requests come: the logins
-- remembered and the checks under way.
data Memory = Memory
  { remembered :: Map UserId (ByteString, UTCTime)
  -- ^ By user: the HMAC of the last login checked as right, and until
  -- when it is remembered.
  , underWay :: Map ByteString (TMVar (Maybe Bool), Int)
  -- ^ By the HMAC of what is being checked: where the check's result will
  -- be put ('Nothing' when it ended without one), and how many requests
  -- share it, the one running it included.
  }

-- | Nothing remembered, under a new key, with this budget of stretching.
newBasicSignIns :: Stretch -> IO BasicSignIns
newBasicSignIns b = newBasicSignInsWith b identity

-- | 'newBasicSignIns' with what runs each full check: the tests use it to
-- change an account while a check runs, or to hold a check until other
-- requests have joined it.
newBasicSignInsWith :: Stretch -> (IO Bool -> IO Bool) -> IO BasicSignIns
newBasicSignInsWith budget around = do
  key <- getRandomBytes 32
  memory <- newTVarIO Memory {remembered = mempty, underWay = mempty}
  checking <- newMVar ()
  checks <- newIORef 0
  pure BasicSignIns {key, budget, memory, checking, checks, around}

-- | How long a login checked as right is remembered: five minutes, the
-- time a proof of a password lasts ('Wiki.Server.Opaque.proofLifetime').
rememberFor :: NominalDiffTime
rememberFor = 5 * 60

-- | How many full checks of a password (each with its stretching) have run
-- since the process started.
fullChecks :: BasicSignIns -> IO Int
fullChecks s = readIORef s.checks

-- | How many logins are remembered, expired ones not yet dropped included.
rememberedLogins :: BasicSignIns -> IO Int
rememberedLogins s = Map.size . (.remembered) <$> readTVarIO s.memory

-- | How many requests wait for, or run, the checks under way.
sharingChecks :: BasicSignIns -> STM Int
sharingChecks s = sum . map snd . Map.elems . (.underWay) <$> readTVar s.memory

-- | Whether a record's stretching is within a budget: no more passes and
-- no more memory.
withinBudget :: Stretch -> Stretch -> Bool
withinBudget b r = r.opsLimit <= b.opsLimit && r.memLimit <= b.memLimit

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

-- | An account as one request reads it: whether it may sign in with this
-- password at all, and if so, against which record.
data Found
  = -- | An enabled account whose record decodes and is within the budget,
    -- with whether its second factor is on.
    Usable UserId StoredRecord (RegistrationRecord, Stretch) Bool
  | -- | Anything else: no account, no record, a disabled account, a record
    -- that does not decode or asks for too much. The credential id the
    -- fake record is answered under.
    Unusable ByteString

-- | What a request does once it has read the account.
data Step
  = -- | The login is remembered with this password and record.
    Known
  | -- | A check of the same login, password and record is under way.
    Join (TMVar (Maybe Bool))
  | -- | This request runs the check, and puts its result here.
    Lead (TMVar (Maybe Bool))

-- | Sign in with the login and password a request carries, from an
-- address, at a time: the checks of the module's haddock, in order. The
-- remembered logins that have expired are dropped, and the limit's verdict
-- comes first. Then the account and its record are read. A login
-- remembered with this password and record signs in without the
-- stretching. A request that finds a check of the same login, password and
-- record under way waits for its result. Otherwise the check is counted as
-- a failure, run, and the failure given back if the password was right.
-- After a check, the account is read again.
signInBasic :: Storage -> Limiter -> BasicSignIns -> UTCTime -> Text -> Text -> Text -> IO Outcome
signInBasic storage limiter s now address login password = do
  atomically $ modifyTVar' s.memory $ \m -> m {remembered = Map.filter (\(_, until') -> until' > now) m.remembered}
  Limiter.verdict limiter now login address >>= \case
    Limiter.Refused seconds -> pure (Limited seconds)
    Allowed -> do
      found <- readAccount
      case found of
        Usable u r _ _ -> do
          -- Computed in full here, so that nothing stored or shared holds
          -- the password ('mac' gives a strict ByteString).
          digest <- evaluate (mac s.key u password r)
          -- A check registered as under way is always given a result, even
          -- if this thread is killed right after registering it: the step
          -- and the handler that ends the check are taken with exceptions
          -- masked.
          mask $ \restore ->
            atomically (stepFor u digest) >>= \case
              Lead v -> lead restore found digest v
              Known -> restore (decide found found True)
              Join v ->
                restore $
                  atomically (readTMVar v) >>= \case
                    -- The check ended without a result: start again.
                    Nothing -> signInBasic storage limiter s now address login password
                    Just right -> readAccount >>= \again -> decide found again right
        Unusable _ ->
          Limiter.reserve limiter now login address >>= \case
            Limiter.Refused seconds -> pure (Limited seconds)
            Allowed -> fullCheck found $> Refused
  where
    -- Remembered, joining a check under way, or leading a new one: decided
    -- in one transaction, so that two requests never both lead.
    stepFor :: UserId -> ByteString -> STM Step
    stepFor u digest = do
      m <- readTVar s.memory
      case (Map.lookup u m.remembered, Map.lookup digest m.underWay) of
        (Just (d, _), _) | constEq d digest -> pure Known
        (_, Just (v, n)) -> do
          writeTVar s.memory m {underWay = Map.insert digest (v, n + 1) m.underWay}
          pure (Join v)
        _ -> do
          v <- newEmptyTMVar
          writeTVar s.memory m {underWay = Map.insert digest (v, 1) m.underWay}
          pure (Lead v)

    readAccount :: IO Found
    readAccount = do
      loaded <- reading storage $ do
        loaded <- loadPasswordByLogin (T.strip login)
        for loaded $ \(a, stored) -> (a,stored,) <$> totpEnabled a.user.id
      pure $ case loaded of
        Just (a, WithRecord r, factor)
          | not a.disabled
          , Just record <- recordFromStored r
          , withinBudget s.budget (snd record) ->
              Usable a.user.id r record factor
        Just (a, _, _) -> Unusable (encodeUtf8 a.user.id.text)
        -- As the form's sign-in names a login nobody has.
        Nothing -> Unusable ("unknown:" <> encodeUtf8 (T.toCaseFold (T.strip login)))

    -- Run the check this request leads, put its result for the requests
    -- that wait for it, and remember the login if it signs in. Whatever
    -- ends the check, the requests that wait get an answer: 'Nothing' if
    -- the check ended without one.
    lead :: (forall a. IO a -> IO a) -> Found -> ByteString -> TMVar (Maybe Bool) -> IO Outcome
    lead restore found digest v = flip onException (atomically (finish Nothing Nothing)) . restore $ do
      Limiter.reserve limiter now login address >>= \case
        Limiter.Refused seconds -> atomically (finish Nothing Nothing) $> Limited seconds
        Allowed -> do
          right <- fullCheck found
          again <- readAccount
          outcome <- decide found again right
          let admitted = case found of
                Usable u _ _ _ | outcome `elem` [SignedIn u, SecondFactorOn] -> Just u
                _ -> Nothing
          when (isJust admitted) $ Limiter.succeededAfter limiter now login address
          atomically (finish (Just right) admitted)
          pure outcome
      where
        finish result admitted = do
          m <- readTVar s.memory
          let remembered = case admitted of
                Just u -> Map.insert u (digest, addUTCTime rememberFor now) m.remembered
                Nothing -> m.remembered
              underWay = Map.update (\(v', n) -> if v' == v then Nothing else Just (v', n)) digest m.underWay
          writeTVar s.memory m {remembered, underWay}
          void (tryPutTMVar v result)

    -- The full check, with its stretching, one at a time: against the
    -- record of a usable account, or against the fake record with the
    -- stretching new records get, and never more than the budget. Whether
    -- the password is right.
    fullCheck :: Found -> IO Bool
    fullCheck found
      -- A long password is refused without the stretching, as the form's
      -- old sign-in refuses one ('Wiki.Server.Accounts.signIn').
      | T.length password > 1024 = pure False
      | otherwise = do
          setup <- serverSetup storage
          let (record, credential) = case found of
                Usable u _ r _ -> (Just r, encodeUtf8 u.text)
                Unusable c -> (Nothing, c)
          right <- s.around $ withMVar s.checking $ \() -> do
            atomicModifyIORef' s.checks (\n -> (n + 1, ()))
            checkHere setup (fakeStretch (storageStretch storage)) record credential (Password password)
          evaluate right

    fakeStretch current = Stretch {opsLimit = min current.opsLimit s.budget.opsLimit, memLimit = min current.memLimit s.budget.memLimit}

    -- The outcome, from the account read before the check and the account
    -- read after it: a sign-in only if the password was right, the
    -- account is still enabled, and its record is still the one checked.
    decide :: Found -> Found -> Bool -> IO Outcome
    decide before after right = case (before, after) of
      (Usable u r _ _, Usable u' r' _ factor) | right && u == u' && r == r' -> do
        Limiter.succeeded limiter login address
        pure (if factor then SecondFactorOn else SignedIn u)
      _ -> pure Refused

-- | The HMAC remembered for a user's login: of the user's id, the
-- password and the stored record, each with its length before it, so that
-- no two different triples are written the same.
mac :: ByteString -> UserId -> Text -> StoredRecord -> ByteString
mac key u password r = convert (hmac key (BS.concat (concatMap framed [encodeUtf8 u.text, encodeUtf8 password, encodeUtf8 r.record, encodeUtf8 r.stretch])) :: HMAC SHA256)
  where
    framed bytes = [show (BS.length bytes) <> ":", bytes]
