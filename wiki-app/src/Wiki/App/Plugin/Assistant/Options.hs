{-# LANGUAGE QuasiQuotes #-}

-- | The writing assistant's settings, the signed-in user's own: the
-- settings' section "Writing assistant" (WP4.19 of docs/PLAN.md,
-- "Wiki.App.Shell"). It holds an API key per provider, the provider the
-- assistant uses, the model to use, and the effort a new conversation
-- starts with. The model is one from the provider's own list, or any
-- other id typed in. The list is fetched with the key when the key is
-- saved or tested and on "Refresh the list", and grouped into the
-- recommended models and all the others
-- ('Wiki.Core.Assistant.modelChoices'). Until there is a key, the list is
-- the wiki's short list. The effort ("Effort") is medium unless the user
-- chooses another ('Wiki.Core.Assistant.defaultEffort'). The chat's own
-- picker changes the effort for one conversation only. One setting is the
-- user's for every provider: the page's cap, the number of characters of
-- a page, or of a selection, a message carries
-- ('Wiki.Core.Assistant.pageCapOf'), with about how many tokens that is
-- beside it ('Wiki.Core.Assistant.tokenEstimate'), following the field as
-- it is typed. A value that is no number, or outside the bounds, is not
-- kept, and the field says why.
--
-- The assistant is bring-your-own-key (decision 21). A key belongs to the
-- user and is billed by its provider. It is one of the plugin's secrets,
-- sealed in the user's vault, and the browser sends it only to its
-- provider. The rest of the settings are the plugin's options, which the
-- server keeps in the clear, and the model lists its cache in this
-- browser ("Wiki.App.Plugin.Assistant.Settings"). The page says so, and
-- also that every request sends the wiki's Markdown reference, about how
-- many tokens it is ('Wiki.Core.Assistant.aboutTokens' of its length), and
-- that the providers cache it where they can.
--
-- Each provider's key is the host's field ('Wiki.App.Plugin.secretField'),
-- which unlocks the vault where it is locked here. A typed key is kept
-- only once it works (WP5.59): the field's check ('checkFor') fetches the
-- model list with the typed key, then makes the smallest possible request
-- with it (looking up the model, which is free); the field keeps the key
-- only when the provider accepts both, and the model the check passed
-- with is kept with it. A refused key is not kept: the field shows why.
-- "Test the key" checks a kept key with that small request. Once kept, a
-- key is never shown again; the field only shows that one is set.
-- Managing the vault itself (its unlock, passkeys, "ask each time") is
-- the settings' section "Security".
--
-- Over plain HTTP (anywhere except the local machine's address) the page
-- warns that it is not secure. The key is still sent to the provider over
-- TLS, but anyone who can modify this page in transit could read what is
-- typed into it.
--
-- Each provider's settings choose the service its requests go to
-- ("Service", WP5.60): the provider's own, or one of the services that
-- speak its API which the wiki's administrator lists (Administration,
-- Server, The assistant's services), as the session carries them. For
-- OpenAI's API that may be GitHub Models, OpenRouter, or a server of
-- Ollama or LM Studio, which needs no key. For Anthropic's it is a
-- gateway, such as an organisation's own in front of Anthropic (WP5.57).
-- The SDK sends the key as @Authorization: Bearer@, which such a gateway
-- takes. Choosing a service sets the base URL the settings keep
-- ('withBaseUrl'), so nothing else changes. There is no field to type an
-- address in: the page's Content Security Policy (WP5.21 of
-- docs/PLAN.md) lets the browser reach only the administrator's services,
-- and a typed address would be refused with an error that explains
-- nothing. A base URL kept from before, which no service offers any
-- more, is shown as such until another service is chosen. A service over
-- plain HTTP to another machine gets the same warning as the page: the key
-- would cross the network unencrypted. A provider that could not be
-- reached on the last attempt (because a network blocks it) is marked as
-- unreachable.
--
-- Everything is read in the initializer, in the browser. A page rendered
-- ahead of the program (by the prerender or the server), which has no
-- access to the settings, says that the settings are being read, and the
-- program adopts that page unchanged.
module Wiki.App.Plugin.Assistant.Options
  ( component
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.UUID (MonadUUID)
import Data.Map.Strict qualified as Map
import Data.Row (type (.==))
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Halogen qualified as H
import Halogen.Machine qualified as Machine
import Halogen.Subscription qualified as HS
import Halogen.Sync qualified as Sync
import Halogen.TH (hh)
import Protolude hiding (State)
import Wiki.App.Plugin (Capabilities (..), FieldInput (..), FieldOutput (..), FieldSlot, Local (..), OptionsInput (..), OptionsOutput (..), SecretField (..), Secrets (..), Unavailable (Failed), attr, secretField)
import Wiki.App.Plugin.Assistant.Settings qualified as Settings
import Wiki.App.Plugin.Capability.Assistant
  ( AssistantApi (..)
  , AssistantCapabilities (..)
  , AssistantService (..)
  , Models (..)
  )
import Wiki.Core.Assistant
  ( AssistantError (..)
  , ErrorKind (BadKey, Unreachable)
  , Listed
  , Model (..)
  , ModelChoices (..)
  , ModelName (..)
  , Provider (..)
  , ProviderSettings (..)
  , Ready (..)
  , Settings (..)
  , aboutTokens
  , baseUrlOf
  , chooseProvider
  , chosenModel
  , defaultBodyCap
  , defaultEffort
  , effortOf
  , efforts
  , errorText
  , insecureUrl
  , keyPage
  , modelChoices
  , pageCapFrom
  , pageCapOf
  , providerKey
  , providerLabel
  , providers
  , readyFor
  , readyWith
  , serviceName
  , serviceOf
  , settingsOf
  , thousands
  , tokenEstimate
  , usable
  , withBaseUrl
  , withEffort
  , withKey
  , withModel
  , withModels
  , withPageCap
  , withReached
  )
import Wiki.Core.Assistant.Reference (markdownReference)
import Wiki.Core.Assistant.Split (secretOf)
import Wiki.Core.Url (Url (..))

-- | What the settings page gives the view ('Wiki.App.Plugin.OptionsInput').
type Input m = OptionsInput m

type Slots = ("secret" .== H.Slot (Const Void) FieldOutput FieldSlot)

-- | 'Check': the result of "Test the key", "Test the connection (no
-- key)", or the check of a key typed in a provider's field. 'Kept' is a
-- typed key that worked and is now kept.
data Check = Checking | Works | KeptNow | Fails Text

-- | 'Listing': the state of fetching a provider's model list.
data Listing = Listing | Fetched Int | NotFetched Text

data State m = State
  { input :: Input m
  , machine :: Machine.Machine () Settings.Event Settings.Model
  -- ^ The user's assistant settings, as read and changed
  -- ("Wiki.App.Plugin.Assistant.Settings").
  , secure :: Bool
  , scripted :: Bool
  , typedModels :: Map Provider Text
  -- ^ A model typed in the "Another" field, kept while another model is chosen.
  , checks :: Map Provider Check
  , listings :: Map Provider Listing
  , otherModels :: Set Provider
  -- ^ The providers for which "Other…" is chosen, which shows its text field.
  , typedCap :: Maybe Text
  -- ^ The page's cap as typed in its field, until it is kept: the estimate
  -- beside the field follows it.
  , testing :: Set Provider
  -- ^ The providers whose field checks a typed key.
  , tested :: Map Provider ModelName
  -- ^ The model the last check of a typed key passed with, for each
  -- provider, to keep with the key once the field has kept it.
  , heard :: Maybe (HS.Listener IO (Action m))
  -- ^ Where the checks the fields run tell the view what they found.
  }

data Action m
  = Initialize
  | Receive (Input m)
  | SecretsMoved
  | FromField Provider FieldOutput
  | CheckStarted Provider
  | CheckListed Provider Ready UTCTime (Either AssistantError [Listed])
  | CheckFailed Provider Text Bool
  | CheckPassed Provider ModelName
  | Choose Provider
  | ChooseModel Provider Text
  | TypedModel Provider Text
  | Test Provider
  | Refresh Provider
  | ChooseService Provider Text
  | ChooseEffort Provider Text
  | TypedCap Text
  | KeepCap Text

component :: forall query m. (Typeable m, MonadUUID m, MonadUnliftIO m) => H.Component query (Input m) OptionsOutput m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = \input ->
          pure
            State
              { input
              , machine = Machine.new (Settings.initial (readerOf input) input.options)
              , secure = True
              , scripted = False
              , typedModels = mempty
              , checks = mempty
              , listings = mempty
              , otherModels = mempty
              , typedCap = Nothing
              , testing = mempty
              , tested = mempty
              , heard = Nothing
              }
      , render
      , eval =
          H.mkEval
            H.defaultEval
              { H.handleAction = handleAction
              , H.initialize = Just Initialize
              , H.receive = Just . Receive
              }
      }
  where
    render :: State m -> H.ComponentHTML (Action m) Slots m
    render st =
      [hh|
        <section class="assistant-settings">
          <h1>Writing assistant</h1>
          {{ body }}
        </section>
      |]
      where
        m = Machine.current st.machine
        body = case (st.input.user, Settings.settings m) of
          (Nothing, _) -> [hh|<p class="sign-in-hint">Sign in to set the assistant up: its settings are kept for each user.</p>|]
          -- A page rendered ahead of the program shows this too: the keys
          -- are opened in this browser only.
          (_, Nothing) -> [hh|<p class="loading">Reading your settings…</p>|]
          (_, Just s) -> settingsView st s

    settingsView :: State m -> Settings -> H.ComponentHTML (Action m) Slots m
    settingsView st s =
      [hh|
        <div class="assistant-settings-body">
          <p class="assistant-intro">
            The assistant in the editor edits a page with you, through Anthropic's Claude or OpenAI's models, with an API key of your own.
            These keys are a provider's, which you get from Anthropic or OpenAI; the keys a program signs in to this wiki with are under API keys.
            Each key is billed by its provider, to your account there, for every request the assistant makes.
            It is sealed in your vault, which follows you to your other devices and which only you can unlock: this wiki's server keeps the vault and cannot open it. The browser sends a key to its provider only.
            Your choice of provider, model, effort and service is kept on this wiki's server, unencrypted, so that it follows you too.
            Each request also sends the wiki's Markdown reference, about {{ aboutTokens (T.length markdownReference) }} of English text, which the provider caches where it can, so that a request soon after another costs less.
          </p>
          <p class="account-note assistant-vault-note">Unlocking your vault, its passkeys, and whether this browser asks for an unlock each time the wiki opens, are under Settings, Security.</p>
          {% if not st.secure %}
            <p class="warning assistant-insecure" role="alert">
              This page was not served over HTTPS. Your key still goes to its provider over an encrypted connection, but anyone between you and this wiki could change this page to read what you type into it. Use the assistant from an HTTPS address of this wiki.
            </p>
            <p class="warning assistant-unsealed" role="alert">
              This browser offers no encryption to a page served over plain HTTP, so your keys are kept in this browser as text, not sealed in your vault. Open this wiki at its HTTPS address to have them sealed.
            </p>
          {% end %}
          <fieldset class="assistant-provider">
            <legend>The assistant asks</legend>
            {% for p in providers st.scripted %}
              <label class="choice">
                <input type="radio" name="assistant-provider" value={{ providerKey p }} checked={{ s.provider == p }} onChange={{ const (Choose p) }}>
                <span>{{ providerLabel p }}</span>
              </label>
            {% end %}
          </fieldset>
          {% for p in providers st.scripted %}
            {{ providerView st s p }}
          {% end %}
          {{ capView st s }}
          {% for why in maybeToList (Machine.current st.machine).problem %}
            <p class="error" role="alert">{{ why }}</p>
          {% end %}
        </div>
      |]

    -- The page's cap: a number field, with the estimate of its tokens
    -- beside it as typed, or why what is typed cannot be kept.
    capView :: State m -> Settings -> H.ComponentHTML (Action m) Slots m
    capView st s =
      [hh|
        <section class="assistant-page-cap-section" aria-label="The page's cap">
          <h2>The page</h2>
          <label class="field assistant-page-cap-field">
            <span>Characters of a page a message carries</span>
            <input type="text" inputmode="numeric" class="assistant-page-cap" value={{ shown }} onValueInput={{ TypedCap }} onValueChange={{ KeepCap }}>
          </label>
          {% for n in rightToMaybe estimate %}
            <p class="account-note assistant-page-cap-estimate">{{ tokenEstimate n }}</p>
          {% end %}
          {% for why in leftToMaybe estimate %}
            <p class="error assistant-page-cap-error" role="alert">{{ why }}</p>
          {% end %}
          <p class="account-note assistant-page-cap-hint">The first message of a conversation carries the page up to this many characters, and later ones only what changed; a selection is cut at the same length. A longer page costs more, and a model's context window may not hold it. {{ thousands defaultBodyCap }} unless you choose another.</p>
        </section>
      |]
      where
        kept = pageCapOf s
        shown = fromMaybe (show kept) st.typedCap
        estimate = maybe (Right kept) pageCapFrom st.typedCap

    providerView :: State m -> Settings -> Provider -> H.ComponentHTML (Action m) Slots m
    providerView st s p =
      [hh|
        <section class="assistant-provider-settings" {{ attr "data-provider" (providerKey p) }} aria-label={{ providerLabel p }}>
          <h2>{{ providerLabel p }}</h2>
          {% if p == Scripted %}
            <p class="account-note">Answers from a script the tests install; it needs no key.</p>
            <div class="actions">
              <button type="button" class="assistant-test" disabled={{ checking }} onClick={{ const (Test p) }}>Test the key</button>
            </div>
          {% else %}
            {% if ps.unreachable %}
              <p class="warning assistant-unreachable" role="alert">{{ cannotReach }}</p>
            {% end %}
            <label class="field assistant-service-field">
              <span>Service</span>
              <select class="assistant-service" disabled={{ checking }} onValueChange={{ ChooseService p }}>
                <option value="" selected={{ isNothing ps.baseUrl }}>{{ ownService }}</option>
                {% for service in offered %}
                  <option value={{ service.baseUrl.text }} selected={{ ps.baseUrl == Just service.baseUrl }}>{{ service.name }} ({{ service.baseUrl.text }})</option>
                {% end %}
                {% for url in maybeToList stale %}
                  <option value={{ url.text }} selected>{{ url.text }} (no longer offered by this wiki)</option>
                {% end %}
              </select>
            </label>
            {% if isJust stale %}
              <p class="warning assistant-service-stale" role="alert">This address is no longer among the services this wiki offers, and the browser may be refused it: choose another service.</p>
            {% end %}
            <p class="account-note assistant-service-hint">{{ serviceHint }}</p>
            {% for url in maybeToList ps.baseUrl %}
              {% if insecureUrl url.text %}
                <p class="warning assistant-insecure-base-url" role="alert">This address is plain HTTP to another machine: your key and your pages would cross the network unencrypted, for anyone on the way to read. Use an HTTPS address, or a server on this machine.</p>
              {% end %}
            {% end %}
            {{ secretField "secret" st.input.can.secrets FieldInput {name = secretOf p, check = checkFor st p s, binding = serviceBinding p s} (FromField p) }}
            {% if isJust ps.key %}
              <div class="actions">
                <button type="button" class="assistant-test" disabled={{ checking }} onClick={{ const (Test p) }}>Test the key</button>
              </div>
            {% else %}
              {% if isNothing (baseUrlOf p ps) %}
                {% for url in maybeToList (keyPage p) %}
                  <p class="account-note">Make one at <a href={{ url }} target="_blank" rel="noopener noreferrer">{{ url }}</a>.</p>
                {% end %}
              {% end %}
              {% if usable p ps && not locked %}
                <div class="actions">
                  <button type="button" class="assistant-test" disabled={{ checking }} onClick={{ const (Test p) }}>Test the connection (no key)</button>
                </div>
              {% end %}
            {% end %}
          {% end %}
          <fieldset class="assistant-model" disabled={{ checking }}>
            <legend>Model</legend>
            <div class="assistant-model-row">
              <select class="assistant-model-choice" aria-label={{ "The model of " <> providerLabel p }} onValueChange={{ ChooseModel p }}>
                {% if not (null choices.recommended) %}
                  <optgroup label="Recommended">
                    {% for m in choices.recommended %}
                      {{ option m }}
                    {% end %}
                  </optgroup>
                {% end %}
                {% if not (null choices.others) %}
                  <optgroup label="All models">
                    {% for m in choices.others %}
                      {{ option m }}
                    {% end %}
                  </optgroup>
                {% end %}
                <option value="" selected={{ other }}>Other…</option>
              </select>
              {% if canList %}
                <button type="button" class="assistant-refresh" disabled={{ listing }} onClick={{ const (Refresh p) }}>Refresh the list</button>
              {% end %}
            </div>
            {% if other %}
              <input type="text" class="assistant-model-name" spellcheck="false" placeholder="A model's id" aria-label={{ "Another model of " <> providerLabel p }} value={{ if listed then typedModel else chosen.text }} onValueChange={{ TypedModel p }}>
            {% end %}
            {{ listingView }}
          </fieldset>
          <label class="field assistant-effort-field">
            <span>Effort</span>
            <select class="assistant-default-effort" disabled={{ checking }} onValueChange={{ ChooseEffort p }}>
              {% for e in efforts p %}
                <option value={{ e }} selected={{ effortOf ps == e }}>{{ effortName e }}</option>
              {% end %}
            </select>
          </label>
          <p class="account-note assistant-effort-hint">How hard the model thinks before it answers, at the start of each conversation. A higher effort answers more carefully, more slowly, and costs more. The chat can change it for one conversation.</p>
          {{ checkView }}
        </section>
      |]
      where
        ps = settingsOf p s
        locked = isJust (Settings.lockedKeys (Machine.current st.machine))
        choices = modelChoices p ps
        chosen = chosenModel p ps
        -- At another service's address there are two more causes. The
        -- page's Content Security Policy may not allow the address
        -- (WP5.21), and the service may not answer a browser page's
        -- requests (CORS). The browser reports both as no connection.
        cannotReach :: Text
        cannotReach =
          "Cannot reach "
            <> serviceName p ps
            <> " from this network: the last request found no way to it. A network that blocks it, a firewall or a proxy, is the likely cause."
            <> if isJust (baseUrlOf p ps)
              then " Or the service does not answer requests from a browser page (CORS), or its address is no longer one this wiki offers (its administrator lists them in the server settings: Administration, Server, The assistant's services)."
              else ""
        -- The session's services that speak this provider's API, and the
        -- base URL kept if none of them is at it.
        offered = [service | service <- st.input.can.assistant.services, apiProvider service.api == p]
        stale = mfilter (\url -> url `notElem` map (\service -> service.baseUrl) offered) ps.baseUrl
        ownService = (if p == OpenAI then "OpenAI's own" else "Anthropic's own") :: Text
        serviceHint :: Text
        serviceHint
          | null offered = providerLabel p <> "'s own service is the only one this wiki offers. Its administrator can add others that speak its API, such as your organisation's gateway or a server of Ollama (Administration, Server, The assistant's services)."
          | otherwise = providerLabel p <> "'s own service, or another that speaks its API, as this wiki's administrator lists them (Administration, Server, The assistant's services). Some take a key of their own (GitHub Models a GitHub token), and a server of Ollama or LM Studio takes none."
        listed = chosen `elem` map (\m -> m.id) (choices.recommended <> choices.others)
        other = Set.member p st.otherModels || not listed
        option m = [hh|<option value={{ m.id.text }} selected={{ not other && chosen == m.id }}>{{ m.label }}</option>|]
        canList = usable p ps
        listing = case Map.lookup p st.listings of
          Just Listing -> True
          _ -> False
        listingView = case Map.lookup p st.listings of
          Nothing -> Nothing
          Just Listing -> Just [hh|<p class="assistant-listing" role="status">Fetching {{ providerLabel p }}'s models…</p>|]
          Just (Fetched n) -> Just [hh|<p class="assistant-listing assistant-listed" role="status">{{ show n :: Text }} models fetched from {{ providerLabel p }}, {{ show (length choices.recommended + length choices.others) :: Text }} of them offered for a chat.</p>|]
          Just (NotFetched why) -> Just [hh|<p class="error assistant-listing assistant-not-listed" role="alert">The list of models could not be fetched. {{ why }}</p>|]
        typedModel = Map.findWithDefault "" p st.typedModels
        effortName e = if e == defaultEffort then e <> " (default)" else e
        -- The service and the model cannot change while a key is tested,
        -- so that the key kept is the one tested at that address.
        checking =
          Set.member p st.testing || case Map.lookup p st.checks of
            Just Checking -> True
            _ -> False
        checkView = case Map.lookup p st.checks of
          Nothing -> Nothing
          Just Checking -> Just [hh|<p class="assistant-check" role="status">Asking {{ providerLabel p }}…</p>|]
          Just Works
            | p /= Scripted && isNothing ps.key -> Just [hh|<p class="assistant-check assistant-check-works" role="status">The connection works without a key, and {{ chosen.text }} is there.</p>|]
            | otherwise -> Just [hh|<p class="assistant-check assistant-check-works" role="status">The key works, and {{ chosen.text }} is there.</p>|]
          Just KeptNow -> Just [hh|<p class="assistant-check assistant-check-works assistant-check-kept" role="status">The key works and is kept.</p>|]
          Just (Fails why) -> Just [hh|<p class="error assistant-check assistant-check-fails" role="alert">{{ why }}</p>|]

    handleAction :: Action m -> H.HalogenM (State m) (Action m) Slots OptionsOutput m ()
    handleAction = \case
      -- On a page rendered ahead of the program, the page stays as first
      -- rendered, and the program adopts it.
      Initialize -> whenM (H.liftEffect =<< gets (\st -> st.input.can.here)) $ do
        can <- gets (\st -> st.input.can)
        secure <- H.liftEffect can.secure
        scripted <- H.liftEffect can.assistant.models.scripted
        -- Where the fields' checks tell what they found, and the secrets'
        -- changes, heard from before the first read.
        HS.Subscribe {listener, emitter} <- H.liftEffect (liftIO HS.create)
        void (H.subscribe emitter)
        changes <- H.liftEffect can.secrets.changes
        void (H.subscribe (SecretsMoved <$ changes))
        modify (\st -> st {secure, scripted, heard = Just listener})
        gets (\st -> Settings.Opened (readerOf st.input) st.input.options Nothing) >>= happen
      -- The input, and the reader and the options in it, in one step.
      Receive input -> do
        before <- gets (\st -> readerOf st.input)
        modify (\st -> st {input})
        -- Another reader: nothing shown of the former reader's stays.
        when (readerOf input /= before) $
          modify (\st -> st {typedModels = mempty, checks = mempty, listings = mempty, otherModels = mempty, testing = mempty, tested = mempty})
        whenM (H.liftEffect input.can.here) (happen (Settings.Given (readerOf input) input.options))
      SecretsMoved -> happen Settings.SecretsChanged
      -- A field's check of a typed key ("Test and keep", the owner,
      -- 2026-11-04): the model list fetched with it, then the smallest
      -- request with the model ('checkFor'). The list is shown at once,
      -- so that another model can be chosen if the request fails.
      CheckStarted p -> modify (\st -> st {testing = Set.insert p st.testing, checks = Map.insert p Checking st.checks, listings = Map.insert p Listing st.listings})
      CheckListed p asked at answer -> case answer of
        Left e
          | e.kind == BadKey -> modify (\st -> st {listings = Map.delete p st.listings})
          | otherwise -> modify (\st -> st {listings = Map.insert p (NotFetched (errorText (serviceOf asked) (Just asked.key) e)) st.listings})
        Right models -> do
          -- Kept for the address it was fetched at only.
          happen (Settings.Changed (\s -> if baseUrlOf p (settingsOf p s) == asked.baseUrl then withModels p models at s else s))
          modify (\st -> st {listings = Map.insert p (Fetched (length models)) st.listings})
      CheckFailed p why reached -> do
        modify (\st -> st {testing = Set.delete p st.testing, checks = Map.insert p (Fails why) st.checks})
        happen (Settings.Changed (withReached p reached))
      CheckPassed p model -> modify (\st -> st {testing = Set.delete p st.testing, tested = Map.insert p model st.tested})
      -- The key is kept: the model its check passed with is kept with it.
      FromField p (Kept _) -> do
        model <- gets (\st -> Map.lookup p st.tested)
        modify (\st -> st {checks = Map.insert p KeptNow st.checks, tested = Map.delete p st.tested})
        for_ model $ \m -> happen (Settings.Changed (\s -> if chosenModel p (settingsOf p s) == m then s else withModel p m.text s))
        happen Settings.SecretsChanged
      -- What was learnt with the key goes with it, as a new key may be
      -- another account's.
      FromField p (Removed _) -> do
        modify (\st -> st {checks = Map.delete p st.checks, listings = Map.delete p st.listings})
        happen (Settings.Changed (withKey p Nothing))
        happen Settings.SecretsChanged
      FromField _ Unlocked -> happen Settings.SecretsChanged
      Choose p -> happen (Settings.Changed (chooseProvider p))
      -- "Other…": show its field, with the model typed there last, if any.
      ChooseModel p "" -> do
        modify (\st -> st {otherModels = Set.insert p st.otherModels})
        typed <- gets (\st -> Map.findWithDefault "" p st.typedModels)
        unless (T.null (T.strip typed)) $ do
          happen (Settings.Changed (withModel p typed))
          modify (\st -> st {checks = Map.delete p st.checks})
      ChooseModel p m -> do
        happen (Settings.Changed (withModel p m))
        modify (\st -> st {checks = Map.delete p st.checks, otherModels = Set.delete p st.otherModels})
      TypedModel p m -> do
        modify (\st -> st {typedModels = Map.insert p m st.typedModels})
        unless (T.null (T.strip m)) $ do
          happen (Settings.Changed (withModel p m))
          modify (\st -> st {checks = Map.delete p st.checks})
      Test p ->
        gets (\st -> Settings.settings (Machine.current st.machine)) >>= \case
          Just s | Right r <- readyFor p s -> do
            modify (\st -> st {checks = Map.insert p Checking st.checks})
            result <- askProvider (\ms -> ms.check r)
            modify (\st -> st {checks = Map.insert p (maybe Works (Fails . errorText (serviceOf r) (Just r.key)) result) st.checks})
            -- Record whether the provider responded at all; and once the
            -- key is known to work, fetch its model list too.
            for_ result (\e -> happen (Settings.Changed (withReached p (e.kind /= Unreachable))))
            when (isNothing result) (fetchModels p)
          _ -> pass
      Refresh p -> fetchModels p
      ChooseEffort p e -> happen (Settings.Changed (withEffort p e))
      TypedCap t -> modify (\st -> st {typedCap = Just t})
      -- A value that cannot be kept stays in the field, which says why.
      KeepCap t -> case pageCapFrom t of
        Right n -> do
          modify (\st -> st {typedCap = Nothing})
          happen (Settings.Changed (withPageCap n))
        Left _ -> modify (\st -> st {typedCap = Just t})
      ChooseService p url -> do
        happen (Settings.Changed (withBaseUrl p url))
        modify (\st -> st {checks = Map.delete p st.checks, listings = Map.delete p st.listings})
        -- Fetch the other service's own list, if it can be reached.
        gets (\st -> Settings.settings (Machine.current st.machine)) >>= \case
          Just s | usable p (settingsOf p s) && isJust (baseUrlOf p (settingsOf p s)) -> fetchModels p
          _ -> pass

    -- Fetch a provider's model list with its key, and store it. The fetch
    -- also tells whether the provider can be reached from here.
    fetchModels :: Provider -> H.HalogenM (State m) (Action m) Slots OptionsOutput m ()
    fetchModels p =
      gets (\st -> Settings.settings (Machine.current st.machine)) >>= \case
        Just s | Right r <- readyFor p s -> do
          modify (\st -> st {listings = Map.insert p Listing st.listings})
          now <- H.sync (liftIO getCurrentTime)
          result <- askProvider (\ms -> ms.list r)
          happen (Settings.Listed p r now result)
          modify $ \st -> case result of
            Left e -> st {listings = Map.insert p (NotFetched (errorText (serviceOf r) (Just r.key) e)) st.listings}
            Right listed -> st {listings = Map.insert p (Fetched (length listed)) st.listings}
        _ -> pass

    -- A request to a provider, through the capabilities the page gave,
    -- waited for.
    askProvider :: (Models m -> m a) -> H.HalogenM (State m) (Action m) Slots OptionsOutput m a
    askProvider request = gets (\st -> st.input.can.assistant.models) >>= H.async . request

    happen :: Settings.Event -> H.HalogenM (State m) (Action m) Slots OptionsOutput m ()
    happen = Machine.happen machineSpec

    machineSpec :: Machine.Spec () (State m) (Action m) Slots OptionsOutput m Settings.Event Settings.Effect Settings.Model
    machineSpec = Machine.Spec {Machine.step = Settings.step, Machine.carry = carry, Machine.get = \st -> st.machine, Machine.set = \machine st -> st {machine}}

    -- Each ask made with the capabilities the input last gave: those of
    -- the reader the step asked for.
    carry :: Settings.Effect -> Machine.Carry () (State m) (Action m) Slots OptionsOutput m Settings.Event
    carry = \case
      Settings.ReadKeys n -> Machine.AskWith (gets (\st -> st.input.can.secrets.get)) Machine.run (Settings.KeysRead n . either (Left . Failed . toS . displayException) identity)
      Settings.ReadCache n -> Machine.AskWith (gets (\st -> st.input.can.local.load)) Machine.run (Settings.CacheRead n . join . rightToMaybe)
      Settings.SaveCache n v -> Machine.AskWith (gets (\st -> st.input.can.local.save v)) Machine.run (Settings.CacheSaved n . either (Left . toS . displayException) identity)
      Settings.SetOptions o -> Machine.Now (Sync.raise (SaveOptions o))
      Settings.ListModels p r -> Machine.AskWith (gets (\st -> st.input.can.assistant.models.list r)) (\listing -> Machine.run ((,) <$> liftIO getCurrentTime <*> listing)) (Settings.listedAnswer p r)
      Settings.Shown -> Machine.Now pass

    -- The check of a key typed in a provider's field, before the field
    -- keeps it: the model list fetched with the key, where a refused key
    -- stops; then the smallest request with the model chosen if the list
    -- has it, else with the list's first model. It tells the view what it
    -- found as it goes. The settings are those the field was last given.
    checkFor :: State m -> Provider -> Settings -> Text -> m (Either Text ())
    checkFor st p s typed = case (st.heard, readyWith typed p s) of
      (Nothing, _) -> pure (Left "The settings are still being read.")
      (_, Nothing) -> pure (Left "Type a key first.")
      (Just listener, Just asked) -> do
        let tell = liftIO . HS.notify listener
            models = st.input.can.assistant.models
        tell (CheckStarted p)
        at <- liftIO getCurrentTime
        listed <- models.list asked
        tell (CheckListed p asked at listed)
        let refused e = let why = errorText (serviceOf asked) (Just asked.key) e in tell (CheckFailed p why (e.kind /= Unreachable)) $> Left why
        case listed of
          Left e | e.kind == BadKey -> refused e
          _ -> do
            let model = either (const asked.model) (\l -> modelOf p (withModels p l at s)) listed
            models.check Ready {provider = asked.provider, model, key = asked.key, effort = asked.effort, baseUrl = asked.baseUrl} >>= \case
              Just e -> refused e
              Nothing -> tell (CheckPassed p model) $> Right ()

-- | The service a key's check tries it against, as a key's field binds
-- the check to it ('Wiki.App.Plugin.FieldInput'): the provider, and the
-- address chosen, none for the provider's own. A key checked against one
-- service is then never kept for another, if the options change while the
-- check is out.
serviceBinding :: Provider -> Settings -> Text
serviceBinding p s = providerKey p <> " " <> maybe "" (\u -> u.text) (baseUrlOf p (settingsOf p s))

-- | The model a key is tested with: the one chosen, if the provider's
-- list has it, else the list's first.
modelOf :: Provider -> Settings -> ModelName
modelOf p s = if chosen `elem` offered then chosen else fromMaybe chosen (head offered)
  where
    ps = settingsOf p s
    choices = modelChoices p ps
    offered = map (\m -> m.id) (choices.recommended <> choices.others)
    chosen = chosenModel p ps

-- | The provider whose API a service speaks.
apiProvider :: AssistantApi -> Provider
apiProvider = \case
  AnthropicApi -> Anthropic
  OpenAiApi -> OpenAI

-- | The reader the input is for: the session's number and the user, as
-- the host made its secrets for.
readerOf :: Input m -> Settings.Reader
readerOf input = input.can.secrets.field.reader
