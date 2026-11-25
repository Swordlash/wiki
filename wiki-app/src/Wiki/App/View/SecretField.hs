{-# LANGUAGE QuasiQuotes #-}

-- | The host's field for one of a plugin's secrets
-- ('Wiki.App.Plugin.SecretField'): the secret's label and description,
-- whether it is set, a field to type a new value into, "Keep" (after the
-- plugin's check; Enter in the field too) and "Forget". Where the user's
-- vault is locked in this browser, the field offers the unlock in its
-- place, with a passkey or the password ("Wiki.App.Vault"). For a user
-- who lost every way to unlock it, the field says what is left: a reset
-- of the password makes the vault anew when the password was its only way
-- to unlock it, and everything in it is lost
-- ('Wiki.App.Vault.resetLosses'). It links to the settings' Security
-- section, which says how the password is reset. Nothing else empties a
-- plugin's secrets without the vault's key. A value kept is never shown
-- again: the field only says that the secret is set.
--
-- The field's logic is a machine ("Wiki.App.View.SecretField.Machine").
-- The host makes one field for one reader and one plugin
-- ("Wiki.App.Plugin.Host"); a plugin mounts it with
-- 'Wiki.App.Plugin.secretField', whose slot changes with the reader, so a
-- field never outlives its reader.
--
-- Every ask of the vault runs with the keys it registers in the page under
-- an owner of its own ('Wiki.App.Assistant.KeyOwner'), made in place and
-- kept in the state before the ask starts, and let go when the ask answers
-- or the field closes: a field closed during an unlock leaves no key of it
-- in the page.
--
-- The field listens, from its start, for the changes of what the secrets
-- read as ('Wiki.App.Assistant.secretsChanges': the vault unlocked or
-- locked in this browser, a secret kept or forgotten, here or in another
-- tab), and reads again on each. Halogen ends the subscription when the
-- field closes.
module Wiki.App.View.SecretField
  ( component
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Map.Strict qualified as Map
import Data.Row (Empty)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML.Events qualified as HE
import Halogen.Machine qualified as Machine
import Halogen.Sync qualified as Sync
import Halogen.TH (hh)
import Protolude hiding (State)
import Wiki.App.Assistant (KeyOwner, MonadAssistant (..), closeKeyOwner, openKeyOwner)
import Wiki.App.Assistant.Sync (MonadVault, owned)
import Wiki.App.Plugin (FieldInput (..), FieldOutput (..), LinkClick, Manifest (..), OptionsLink, SecretName (..), SecretSpec (..), Unavailable (..), followOptionsLink, optionsLinkProps)
import Wiki.App.Plugin.Host.Secrets (putSecret, readSecrets, removeSecret)
import Wiki.App.Vault (unlockWithPasskey, unlockWithPassword)
import Wiki.App.View.SecretField.Machine (Answer (..), Effect (..), Event (..), Job (..), Model (..), Shown (..), Told (..), busy, initialBound, needsOwner, step)
import Wiki.Core.Id (UserId)
import Wiki.Core.User (Password (..))

data State m = State
  { input :: FieldInput m
  , machine :: Machine.Machine () Event Model
  -- ^ What the field knows, run by "Halogen.Machine".
  , owners :: Map Int KeyOwner
  -- ^ The owner of keys of each ask of the vault out, by the ask's number.
  }

data Action m
  = Initialize
  | Finalize
  | Receive (FieldInput m)
  | Happened Event
  | Followed LinkClick

-- | The field for one reader (the user signed in, if anyone is) and one
-- plugin, by its manifest, with the link to the settings' Security
-- section.
component :: forall m. (Typeable m, MonadAssistant m, MonadVault m, MonadUnliftIO m) => Maybe UserId -> Manifest -> OptionsLink -> H.Component (Const Void) (FieldInput m) FieldOutput m
component user manifest security =
  H.mkComponent
    H.ComponentSpec
      { initialState = \input -> pure State {input, machine = Machine.new (initialBound input.binding), owners = mempty}
      , render
      , eval =
          H.mkEval
            H.defaultEval
              { H.handleAction = handleAction
              , H.initialize = Just Initialize
              , H.finalize = Just Finalize
              , H.receive = Just . Receive
              }
      }
  where
    render :: State m -> H.ComponentHTML (Action m) Empty m
    render st =
      [hh|
        <div class="secret-field" data-secret={{ st.input.name.text }}>
          <div class="secret-field-head">
            <span class="secret-field-label">{{ label }}</span>
            {% if m.shown == Here True %}<span class="secret-field-state secret-field-set">Set</span>{% end %}
            {% if m.shown == Here False %}<span class="secret-field-state">Not set</span>{% end %}
          </div>
          {% for s in maybeToList spec %}
            {% if not (T.null s.description) %}<p class="secret-field-description">{{ s.description }}</p>{% end %}
          {% end %}
          {% if isNothing spec %}
            <p class="secret-field-problem" role="alert">{{ "The plugin " <> manifest.name <> " declares no secret named " <> st.input.name.text <> "." }}</p>
          {% else %}
            {{ shownPart }}
            {% for (_, job) in maybeToList m.doing %}
              {% for doing in maybeToList (doingText job) %}<p class="secret-field-busy" role="status">{{ doing }}</p>{% end %}
            {% end %}
            {% for p in maybeToList m.problem %}<p class="secret-field-problem" role="alert">{{ p }}</p>{% end %}
          {% end %}
        </div>
      |]
      where
        m = Machine.current st.machine
        spec = find (\s -> s.name == st.input.name) manifest.secrets
        label = maybe st.input.name.text (\s -> s.label) spec
        shownPart = case m.shown of
          Loading -> [hh|<p class="secret-field-loading">Loading…</p>|]
          NoReader -> [hh|<p class="secret-field-note">{{ "Sign in to keep " <> label <> "." }}</p>|]
          Unreadable why -> [hh|<p class="secret-field-problem" role="alert">{{ why }}</p>|]
          LockedHere ->
            [hh|
              <div class="secret-field-unlock">
                <p class="secret-field-locked">{{ "Your vault is locked in this browser. Unlock it to keep " <> label <> "." }}</p>
                <button type="button" class="secret-field-unlock-passkey" disabled={{ busy m }} onClick={{ const (Happened PasskeyPressed) }}>Unlock with a passkey</button>
                <input type="password" class="secret-field-password" autocomplete="current-password" aria-label="Your password" value={{ m.password }} disabled={{ busy m }} onValueInput={{ Happened . TypedPassword }}>
                <button type="button" class="secret-field-unlock-password" disabled={{ busy m || T.null m.password }} onClick={{ const (Happened PasswordPressed) }}>Unlock with your password</button>
                <p class="secret-field-note secret-field-lost">
                  Lost every way to unlock it? A reset of your password makes your vault anew if the password was its only way to unlock it, and everything in the vault is lost: the secrets of every plugin, such as this one, and your key pair. You then keep this secret again.
                  <a class="secret-field-security" {{ optionsLinkProps security Followed }}>Security says how your password is reset.</a>
                </p>
              </div>
            |]
          Here set ->
            [hh|
              <form class="secret-field-change" {{ HE.preventingDefault (HE.onSubmit (const (Happened KeepPressed))) }}>
                <input type="password" class="secret-field-value" autocomplete="off" aria-label={{ label }} placeholder={{ if set then "A new value" else "" :: Text }} value={{ m.typed }} disabled={{ busy m }} onValueInput={{ Happened . Typed }}>
                <button type="submit" class="secret-field-keep" disabled={{ busy m || T.null (T.strip m.typed) }}>Keep</button>
                {% if set %}
                  <button type="button" class="secret-field-forget" disabled={{ busy m }} onClick={{ const (Happened ForgetPressed) }}>Forget</button>
                {% end %}
              </form>
            |]

    handleAction :: Action m -> H.HalogenM (State m) (Action m) Empty FieldOutput m ()
    handleAction = \case
      Initialize -> do
        -- Made in place, before the first read is asked, so that no
        -- change after that read goes unheard.
        changes <- Sync.sync secretsChanges
        void (H.subscribe (Happened Changed <$ changes))
        happen Opened
      Finalize -> Machine.finish machineSpec Finished
      -- The plugin's check may be another function: the next check uses
      -- it. The name is the slot's, which does not change. What the check
      -- is for goes to the machine in this step, before any wait: a check
      -- out answers into the binding as it is now.
      Receive input -> do
        modify (\st -> st {input})
        happen (Rebound input.binding)
      Happened e -> happen e
      Followed f -> followOptionsLink security f

    happen :: Event -> H.HalogenM (State m) (Action m) Empty FieldOutput m ()
    happen = Machine.happen machineSpec

    machineSpec :: Machine.Spec () (State m) (Action m) Empty FieldOutput m Event Effect Model
    machineSpec = Machine.Spec {Machine.step = step, Machine.carry = carry, Machine.get = \st -> st.machine, Machine.set = \machine st -> st {machine}}

    carry :: Effect -> Machine.Carry () (State m) (Action m) Empty FieldOutput m Event
    carry = \case
      Ask n job -> case user of
        -- Nobody is signed in: there is no vault, and nothing to ask.
        Nothing -> Machine.Ask (Machine.run (pure (ReadAs NoReader))) (Answered n . answerOf)
        Just u
          | needsOwner job ->
              Machine.AskWith
                ( do
                    o <- Sync.sync (liftIO (openKeyOwner u))
                    modify (\st -> st {owners = Map.insert n o st.owners})
                    name <- gets (\st -> st.input.name)
                    pure (o, name)
                )
                (\(o, name) -> Machine.run (owned o (work u name job)))
                (Answered n . answerOf)
          | otherwise -> Machine.AskWith (gets (\st -> st.input)) (\input -> Machine.run (checked input job)) (Answered n . answerOf)
      Release n -> Machine.Now $ do
        gets (\st -> Map.lookup n st.owners) >>= traverse_ (Sync.sync . liftIO . closeKeyOwner)
        modify (\st -> st {owners = Map.delete n st.owners})
      Tell told -> Machine.Now $ do
        name <- gets (\st -> st.input.name)
        Sync.raise $ case told of
          KeptIt -> Kept name
          RemovedIt -> Removed name
          UnlockedIt -> Unlocked
      where
        -- A job of the vault's.
        work u name = \case
          Read -> ReadAs . shownOf name <$> readSecrets u manifest
          Put value -> Done <$> putSecret u manifest name value
          Remove -> Done <$> removeSecret u manifest name
          UnlockByPasskey -> Done . void <$> unlockWithPasskey u
          UnlockByPassword p -> Done . void <$> unlockWithPassword u (Password p)
          Check _ _ -> pure (Done (Left "A check is the plugin's."))
        -- The plugin's check of a value.
        checked input = \case
          Check value _ -> Done <$> input.check value
          _ -> pure (Done (Left "Only a check is the plugin's."))
        shownOf name = \case
          Right secrets -> Here (Map.member name secrets)
          Left Locked -> LockedHere
          Left NoUser -> NoReader
          Left (Failed why) -> Unreadable why

-- | What an ask's answer is, when its work threw: a failure, saying why.
answerOf :: Either SomeException Answer -> Answer
answerOf = either (\e -> Done (Left ("Something went wrong: " <> toS (displayException e)))) identity

-- | What the field says while a job is out.
doingText :: Job -> Maybe Text
doingText = \case
  Read -> Nothing
  Check _ _ -> Just "Checking…"
  Put _ -> Just "Keeping…"
  Remove -> Just "Forgetting…"
  UnlockByPasskey -> Just "Unlocking with a passkey…"
  UnlockByPassword _ -> Just "Unlocking…"
