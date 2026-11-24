{-# LANGUAGE AllowAmbiguousTypes #-}

-- | The parts the servant type of the API for programs
-- ("Wiki.Server.ProgramApi") is made of besides servant's own, each with
-- what servant does with it on the server ('HasServer') and what it says
-- of itself in the OpenAPI document ('HasOpenApi').
--
-- They exist because the API answered by hand before it was a servant
-- type, and its answers stay as they were, byte for byte. Servant's own
-- combinators differ in small ways that a client can see:
--
-- * A method an address does not take is a 405 before the request signs
--   in, without @Allow@ and without a body. Here the request signs in
--   first, and the 405 names the methods ('OtherMethods'), after a handler
--   that may refuse the address first (404 where no page is).
-- * An address no endpoint has is a 404 before the request signs in. Here
--   the addresses beside it answer it, signed in first ('Undocumented').
-- * A parameter given without a value (@?top@) is absent for servant's
--   'Servant.QueryParam', and a value that is not UTF-8 is read with
--   replacement characters. Here the handler is given the parameter as
--   the query has it ('QueryArg'): @?top@ alone means true, and a value
--   that is not UTF-8 is refused.
-- * A capture gives the address's segments, decoded. Here the
--   application's router also reads the address as sent, query included
--   ('AsSent'), as it always did, with an escaped slash read as a plain
--   one.
-- * A body is read only with a @Content-Type@ the endpoint names, or
--   refused with a 415. Here every body is taken, whatever its
--   @Content-Type@, as @curl --data-binary@ sends it ('Body').
-- * servant writes a media type's parameters without a space
--   (@text\/markdown;charset=utf-8@). Here Markdown is
--   @text\/markdown; charset=utf-8@, as it always was ('RespondMarkdown').
--
-- The answers that are not refusals are servant's 'MultiVerb' with no
-- content types of the request's choosing: the @Accept@ header is not
-- read, and each answer has the one media type the type names, 'Json' or
-- 'RespondMarkdown'. Refusals are thrown ('refuse'), and the document
-- learns them from 'Refuses' and from 'SignIn'.
module Wiki.Server.ProgramApi.Combinators
  ( -- * Refusals
    Refusal (..)
  , RefusalBody (..)
  , refusal
  , refusalWith
  , wikiRefusal
  , notAllowed
  , refusalResponse
  , refusalError
  , refuse

    -- * Signing in
  , SignIn
  , Rule (..)
  , Signed
  , SignInCheck (..)
  , EverySignedIn
  , Holds (..)

    -- * The request
  , QueryArg
  , Given
  , Sent (..)
  , AsSent
  , Body
  , OtherMethods
  , Refuses
  , Named
  , Undocumented

    -- * The answers
  , Json
  , RespondMarkdown
  , markdownMediaType
  , Version (..)
  , CacheControl (..)
  , JsonBodies
  )
where

import Control.Lens ((%~), (.~), (<>~), (?~), (^.))
import Data.Aeson (Options (..), ToJSON (..), defaultOptions, encode, genericToEncoding, genericToJSON)
import Data.ByteString qualified as BS
import Data.HashMap.Strict.InsOrd.Compat qualified as InsOrd
import Data.List (lookup)
import Data.OpenApi (Definitions, Referenced (..), Response, Schema, SecurityRequirement (..), ToParamSchema (..), ToSchema (..), genericDeclareNamedSchema)
import Data.OpenApi qualified as O
import Data.OpenApi.Declare (Declare, runDeclare)
import Data.SOP (I (..), NP (..))
import Data.Sequence qualified as Seq
import Data.String (fromString)
import Data.Text qualified as T
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Network.HTTP.Media ((//), (/:))
import Network.HTTP.Types (Header, Method, Status (..), hAllow, hCacheControl, hContentType, status405)
import Network.Wai (Request (..), responseLBS, strictRequestBody)
import Network.Wai qualified as Wai
import Protolude hiding (Handler, TypeError)
import Servant (Accept (..), CaptureAll, Handler, HasContextEntry (..), HasServer (..), MimeRender (..), ReflectMethod (..), ServerError (..), StdMethod, (:<|>), (:>))
import Servant.API.MultiVerb (AsConstructor (..), MultiVerb, RespondAs, ResponseType, WithHeaders)
import Servant.API.Status (KnownStatus, statusVal)
import Servant.OpenApi (HasOpenApi (..))
import Servant.OpenApi.Internal (IsSwaggerResponse (..))
import Servant.Server.Internal (addAuthCheck, addBodyCheck, delayedFailFatal, err405, leafRouter, passToServer, runAction, withRequest)
import Servant.Server.Internal.ResponseRender (ResponseRender (..))
import Servant.Server.Internal.RouteResult (RouteResult (..))
import Servant.Types.Internal.Response (InternalResponse (InternalResponse))
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)
import Wiki.Api.Error (ApiError, errorStatus)
import Wiki.Api.Version (PageVersion)
import Wiki.App.Backend (WikiError)
import Wiki.Core.Id (UserId)
import Wiki.Server.Api (toApiError)

--------------------------------------------------------------------------------
-- Refusals

-- | 'Refusal' is an answer of the API for programs that refuses the
-- request: its status, headers of its own (@Allow@, @Retry-After@,
-- @WWW-Authenticate@) and its JSON body. A handler throws it ('refuse'),
-- the sign-in answers with it ('SignIn'), and the MCP server
-- ("Wiki.Server.McpApi") sends it as a WAI response ('refusalResponse').
data Refusal = Refusal
  { status :: Status
  , headers :: [Header]
  , body :: LByteString
  }

-- | 'RefusalBody' is the JSON of a refusal, in the shape the application's
-- API uses ("Wiki.Api.Error"): the error's name, and why. @state@ is the
-- state a 409 @Stale@ names. It is the schema the document gives every
-- refusal.
data RefusalBody = RefusalBody
  { error :: Text
  , message :: Maybe Text
  , state :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

-- | The fields in the order of the record, which is the order of their
-- names, as the application's API writes them; a field that is 'Nothing'
-- left out.
refusalOptions :: Options
refusalOptions = defaultOptions {omitNothingFields = True}

instance ToJSON RefusalBody where
  toJSON = genericToJSON refusalOptions
  toEncoding = genericToEncoding refusalOptions

instance ToSchema RefusalBody where
  declareNamedSchema = genericDeclareNamedSchema (O.fromAesonOptions refusalOptions)

-- | A refusal: its status, the error's name, and why.
refusal :: Status -> Text -> Maybe Text -> Refusal
refusal s = refusalWith s []

-- | 'refusal' with headers of its own (@Allow@, @Retry-After@).
refusalWith :: Status -> [Header] -> Text -> Maybe Text -> Refusal
refusalWith s hs name why =
  Refusal
    { status = s
    , headers = (hContentType, "application/json") : (hCacheControl, "no-store") : hs
    , body = encode RefusalBody {error = name, message = why, state = Nothing}
    }

-- | A refusal of the wiki's domain, as the application's API answers it:
-- its 'ApiError', with that error's status.
wikiRefusal :: WikiError -> Refusal
wikiRefusal e = case toApiError e of
  Just (apiError :: ApiError) -> Refusal {status = toEnum (errorStatus apiError), headers = [(hContentType, "application/json"), (hCacheControl, "no-store")], body = encode apiError}
  Nothing -> refusal (toEnum 503) "Unreachable" (Just "The wiki's storage could not be reached.")

-- | A method the address does not take, with the ones it does.
notAllowed :: [Method] -> Refusal
notAllowed methods = refusalWith status405 [(hAllow, BS.intercalate ", " methods)] "Invalid" (Just "This address does not take that method.")

-- | A refusal as a WAI response.
refusalResponse :: Refusal -> Wai.Response
refusalResponse r = responseLBS r.status r.headers r.body

-- | A refusal as servant's error: the same status, headers and body.
refusalError :: Refusal -> ServerError
refusalError r =
  ServerError
    { errHTTPCode = statusCode r.status
    , errReasonPhrase = toS (decodeUtf8With lenientDecode (statusMessage r.status))
    , errBody = r.body
    , errHeaders = r.headers
    }

-- | Answer a request with a refusal, from a handler.
refuse :: Refusal -> Handler a
refuse = throwError . refusalError

-- | The document's answer for a refusal: why, and the refusal's JSON.
refusalDoc :: Text -> Declare (Definitions Schema) Response
refusalDoc why = do
  ref <- O.declareSchemaRef (Proxy @RefusalBody)
  pure (mempty & O.description .~ why & O.content .~ InsOrd.fromList [("application" // "json", mempty & O.schema ?~ ref)])

--------------------------------------------------------------------------------
-- Signing in

-- | 'Rule' is when a request must sign in: 'Always', or 'WhenClosed', only
-- when the wiki is closed to anyone who has not signed in (the Markdown
-- reference, the OpenAPI document).
data Rule = Always | WhenClosed

-- | Who a request under 'SignIn' signed in as: the user, or for
-- 'WhenClosed' 'Nothing' when the wiki is open and nobody signed in.
type Signed :: Rule -> Type
type family Signed rule where
  Signed 'Always = UserId
  Signed 'WhenClosed = Maybe UserId

-- | 'SignInCheck' is how a request signs in under a rule: who it signed in
-- as, or the refusal that answers it. The application gives one for each
-- rule in servant's context ("Wiki.Server.ProgramApi").
newtype SignInCheck (rule :: Rule) = SignInCheck (Request -> IO (Either Refusal (Signed rule)))

-- | 'SignIn' signs a request in to the API for programs, under a rule, and
-- gives the handler who it signed in as. It runs after the address and the
-- method have matched, before anything else of the request is read; a
-- refusal ends the request, so no other endpoint signs it in again.
data SignIn (rule :: Rule)

instance (HasServer api ctx, HasContextEntry ctx (SignInCheck rule)) => HasServer (SignIn rule :> api) ctx where
  type ServerT (SignIn rule :> api) m = Signed rule -> ServerT api m
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy @api) pc nt . s
  route _ ctx sub = route (Proxy @api) ctx (sub `addAuthCheck` withRequest signedIn)
    where
      SignInCheck signIn = getContextEntry ctx :: SignInCheck rule
      signedIn request = liftIO (signIn request) >>= either (delayedFailFatal . refusalError) pure

-- | What the document says of a rule: who may ask without signing in, and
-- when a refusal of the sign-in comes.
class SignInDoc (rule :: Rule) where
  -- | An empty requirement first where a request may come without a
  -- credential.
  anonymous :: [SecurityRequirement]

  -- | Said before each refusal of the sign-in.
  condition :: Text

instance SignInDoc 'Always where
  anonymous = []
  condition = ""

instance SignInDoc 'WhenClosed where
  anonymous = [mempty]
  condition = "In a wiki closed to anyone who has not signed in: "

instance (HasOpenApi api, SignInDoc rule) => HasOpenApi (SignIn rule :> api) where
  toOpenApi _ =
    toOpenApi (Proxy @api)
      & O.components . O.securitySchemes .~ schemes
      & O.allOperations . O.security .~ (anonymous @rule <> map required' ["basic", "bearer", "apiKey"])
      & signInRefusal 400 "The request carries more than one credential: an API key beside a login and a password, two different keys, or one of the headers twice." []
      & signInRefusal 401 "The request has no credential, or one that does not sign in: a wrong login or password, an account that cannot sign in with a password (one with passkeys only), or an API key that is not this wiki's, has expired or has been revoked, or whose user cannot sign in. WWW-Authenticate says how to sign in." [("WWW-Authenticate", stringHeader)]
      & signInRefusal 403 "A password of an account with a second factor (an API key signs it in), a key whose right does not allow the method (read allows GET and HEAD), or a key with the right admin whose user is no longer an administrator." []
      & signInRefusal 429 "Too many failed sign-ins: of the login from the address, or of keys from the address. Retry-After says in how many seconds to try again." [("Retry-After", integerHeader)]
    where
      required' n = SecurityRequirement (InsOrd.singleton n [])
      signInRefusal code why hs =
        O.setResponseWith joined code (refusalDoc (condition @rule <> why) <&> (O.headers .~ InsOrd.fromList [(h, Inline s) | (h, s) <- hs]))
      -- An endpoint's own refusal of the same status first, then the
      -- sign-in's.
      joined old new = old & O.description <>~ (" Or the sign-in refused it: " <> new ^. O.description) & O.headers <>~ (new ^. O.headers)
      stringHeader = mempty & O.schema ?~ Inline (toParamSchema (Proxy @Text))
      integerHeader = mempty & O.schema ?~ Inline (toParamSchema (Proxy @Int))

-- | The three ways a request presents a credential.
schemes :: O.SecurityDefinitions
schemes =
  O.SecurityDefinitions . InsOrd.fromList $
    [ ("basic", O.SecurityScheme (O.SecuritySchemeHttp O.HttpSchemeBasic) (Just "The user's login and password, as curl -u sends them."))
    , ("bearer", O.SecurityScheme (O.SecuritySchemeHttp (O.HttpSchemeBearer (Just "wiki_<id>_<secret>"))) (Just "An API key, made in the user's settings: Authorization: Bearer <key>."))
    , ("apiKey", O.SecurityScheme (O.SecuritySchemeApiKey (O.ApiKeyParams "X-API-Key" O.ApiKeyHeader)) (Just "An API key, as Bearer takes it, in a header of its own."))
    ]

-- | 'EverySignedIn' holds of an API whose every endpoint is under
-- 'SignIn', with one of the rules. An endpoint that needs no sign-in says
-- so in the type, with 'WhenClosed'. The application's server asks it of
-- the whole API, so that an endpoint added without a sign-in does not
-- compile. 'OtherMethods' signs in where it stands, as the endpoints
-- beside it do, or not at all.
type EverySignedIn :: Type -> Constraint
type family EverySignedIn api where
  EverySignedIn (a :<|> b) = (EverySignedIn a, EverySignedIn b)
  EverySignedIn (SignIn rule :> api) = ()
  EverySignedIn (OtherMethods methods) = ()
  EverySignedIn ((x :: k) :> api) = EverySignedIn api
  EverySignedIn api =
    TypeError
      ( 'Text "An endpoint of the API for programs is not under SignIn: "
          ':$$: 'ShowType api
          ':$$: 'Text "Put it under SignIn 'Always, or under SignIn 'WhenClosed if anyone may read it."
      )

-- | 'Holds' is a constraint that holds, as a value: @Holds :: Holds c@
-- compiles only where @c@ can be solved, which is how the application asks
-- 'EverySignedIn' of its API.
data Holds (c :: Constraint) where
  Holds :: (c) => Holds c

--------------------------------------------------------------------------------
-- The request

-- | A parameter of the query as the request gives it: 'Nothing' when it is
-- not there, @Just Nothing@ when it is there without a value (@?top@), and
-- the value's bytes, still percent-decoded, as sent.
type Given = Maybe (Maybe ByteString)

-- | 'QueryArg' gives the handler a parameter of the query as the request
-- has it ('Given'), to be read by the handler, which says why a value does
-- not do. The document gives the parameter with the schema of @a@ and the
-- description @desc@; the parameter is never required.
data QueryArg (name :: Symbol) (a :: Type) (desc :: Symbol)

instance (HasServer api ctx, KnownSymbol name) => HasServer (QueryArg name a desc :> api) ctx where
  type ServerT (QueryArg name a desc :> api) m = Given -> ServerT api m
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy @api) pc nt . s
  route _ ctx sub = route (Proxy @api) ctx (passToServer sub (lookup (encodeUtf8 (toS (symbolVal (Proxy @name)))) . queryString))

instance (HasOpenApi api, KnownSymbol name, KnownSymbol desc, ToParamSchema a) => HasOpenApi (QueryArg name a desc :> api) where
  toOpenApi _ = toOpenApi (Proxy @api) & O.allOperations . O.parameters %~ (Inline param :)
    where
      param =
        mempty
          & O.name .~ toS (symbolVal (Proxy @name))
          & O.description ?~ toS (symbolVal (Proxy @desc))
          & O.required ?~ False
          & O.in_ .~ O.ParamQuery
          & O.schema ?~ Inline (toParamSchema (Proxy @a))

-- | 'Body' gives the handler the request's body, whole, whatever its
-- @Content-Type@: the handler decodes it, and says why a body does not do.
-- The document gives it as of the media type @ct@, with the schema of @a@
-- and the description @desc@.
data Body (ct :: Symbol) (a :: Type) (desc :: Symbol)

instance (HasServer api ctx) => HasServer (Body ct a desc :> api) ctx where
  type ServerT (Body ct a desc :> api) m = LByteString -> ServerT api m
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy @api) pc nt . s
  route _ ctx sub = route (Proxy @api) ctx (addBodyCheck sub (pure ()) (const (withRequest (liftIO . strictRequestBody))))

instance (HasOpenApi api, KnownSymbol ct, KnownSymbol desc, ToSchema a) => HasOpenApi (Body ct a desc :> api) where
  toOpenApi _ =
    toOpenApi (Proxy @api)
      & O.components . O.schemas %~ (<> defs)
      & O.allOperations . O.requestBody ?~ Inline requestBody
    where
      (defs, ref) = runDeclare (O.declareSchemaRef (Proxy @a)) mempty
      requestBody =
        mempty
          & O.description ?~ toS (symbolVal (Proxy @desc))
          & O.content .~ InsOrd.fromList [(fromString (symbolVal (Proxy @ct)), mempty & O.schema ?~ ref)]

-- | 'Sent' is a request's path and query as sent, percent-encoded, the
-- query with its @?@: what the application's router reads, which is how
-- the API reads a page's or a space's address ("Wiki.App.Route").
data Sent = Sent
  { path :: ByteString
  , query :: ByteString
  }

-- | 'AsSent' gives the handler the request's path and query as sent
-- ('Sent'): the application's router reads a page's or a space's address
-- from it, so that the API has no grammar of its own for its addresses.
-- It comes before the capture of the rest of the address (a 'CaptureAll'
-- named @capture@), whose segments the handler is given too: the document
-- describes that capture as @desc@ says, since a capture of servant's
-- cannot be given a description.
--
-- An escaped slash (@%2F@) separates segments as a plain one does, in the
-- path and in the segments alike ('plainSlashes'). A client generated from
-- the document escapes a path parameter whole, the address's slashes
-- included, and a slug never holds a slash, so this changes the meaning
-- of no address that names something.
data AsSent (capture :: Symbol) (desc :: Symbol)

instance (HasServer (CaptureAll capture Text :> api) ctx) => HasServer (AsSent capture desc :> CaptureAll capture Text :> api) ctx where
  type ServerT (AsSent capture desc :> CaptureAll capture Text :> api) m = Sent -> ServerT (CaptureAll capture Text :> api) m
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy @(CaptureAll capture Text :> api)) pc nt . s
  route _ ctx sub = route (Proxy @(CaptureAll capture Text :> api)) ctx (passToServer sub sent <&> (. concatMap (T.splitOn "/")))
    where
      sent request = Sent {path = plainSlashes (rawPathInfo request), query = rawQueryString request}

-- | 'plainSlashes' is a path as sent with each escaped slash (@%2F@ or
-- @%2f@) written as a plain slash. No other escape is undone, so the
-- router still decodes the path once: @%252F@ stays, and is a @%2F@ in a
-- segment, which is no slug.
plainSlashes :: ByteString -> ByteString
plainSlashes p = case BS.elemIndex 37 p of
  Nothing -> p
  Just i ->
    let (before, rest) = BS.splitAt i p
     in if BS.take 3 rest `elem` ["%2F", "%2f"]
          then before <> "/" <> plainSlashes (BS.drop 3 rest)
          else before <> BS.take 1 rest <> plainSlashes (BS.drop 1 rest)

instance (HasOpenApi api, KnownSymbol capture, KnownSymbol desc) => HasOpenApi (AsSent capture desc :> api) where
  toOpenApi _ = toOpenApi (Proxy @api) & O.allOperations . O.parameters . traverse %~ described
    where
      described = \case
        Inline p | p ^. O.name == toS (symbolVal (Proxy @capture)), p ^. O.in_ == O.ParamPath -> Inline (p & O.description ?~ toS (symbolVal (Proxy @desc)))
        other -> other

-- | 'OtherMethods' answers, at an address, a request with a method that
-- none of its endpoints takes: 405, with these methods as @Allow@. Put last
-- among the endpoints of the address, under the same 'SignIn' as they are
-- where they sign in first, so that such a request is signed in, and
-- refused for its credential, before the method is refused. Its handler
-- runs before the 405, and may refuse the request otherwise: an address
-- that names no page is 404 whatever the method. The document gives it
-- nothing.
data OtherMethods (methods :: [StdMethod])

-- | The names of a list of methods.
class Methods (methods :: [StdMethod]) where
  methodNames :: [Method]

instance Methods '[] where
  methodNames = []

instance (Methods ms, ReflectMethod m) => Methods (m ': ms) where
  methodNames = reflectMethod (Proxy @m) : methodNames @ms

instance (Methods methods) => HasServer (OtherMethods methods) ctx where
  type ServerT (OtherMethods methods) m = m ()
  hoistServerWithContext _ _ nt s = nt s
  route _ _ action = leafRouter $ \env request respond ->
    if requestMethod request `elem` methodNames @methods
      then respond (Fail err405)
      else runAction action env request respond (const (Route (refusalResponse (notAllowed (methodNames @methods)))))

instance HasOpenApi (OtherMethods methods) where
  toOpenApi _ = mempty

-- | 'Refuses' says, in the document, that the endpoints under it may refuse
-- a request with this status, and why, with a 'RefusalBody'. It does
-- nothing on the server: the handler throws the refusal ('refuse'). Put it
-- before the endpoint's parameters and headers, whose own refusals it then
-- describes, and before a capture of the address for a 404.
data Refuses (code :: Nat) (desc :: Symbol)

instance (HasServer api ctx) => HasServer (Refuses code desc :> api) ctx where
  type ServerT (Refuses code desc :> api) m = ServerT api m
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)
  route _ = route (Proxy @api)

instance (HasOpenApi api, KnownNat code, KnownSymbol desc) => HasOpenApi (Refuses code desc :> api) where
  toOpenApi _ = toOpenApi (Proxy @api) & O.setResponseWith (\_ new -> new) (fromIntegral (natVal (Proxy @code))) (refusalDoc (toS (symbolVal (Proxy @desc))))

-- | 'Undocumented' serves the endpoints under it and leaves them out of the
-- document: the answers to addresses that name nothing of the API's, which
-- are served as the addresses beside them are (signed in first, then 404),
-- and are no operations of their own.
data Undocumented

instance (HasServer api ctx) => HasServer (Undocumented :> api) ctx where
  type ServerT (Undocumented :> api) m = ServerT api m
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)
  route _ = route (Proxy @api)

instance HasOpenApi (Undocumented :> api) where
  toOpenApi _ = mempty

-- | 'Named' gives the endpoints under it this name in the document, its
-- @operationId@, by which a client generated from the document names the
-- call. It does nothing on the server. (servant's own @OperationId@ comes
-- with a later version of servant than the repository's.)
data Named (name :: Symbol)

instance (HasServer api ctx) => HasServer (Named name :> api) ctx where
  type ServerT (Named name :> api) m = ServerT api m
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)
  route _ = route (Proxy @api)

instance (HasOpenApi api, KnownSymbol name) => HasOpenApi (Named name :> api) where
  toOpenApi _ = toOpenApi (Proxy @api) & O.allOperations . O.operationId ?~ toS (symbolVal (Proxy @name))

--------------------------------------------------------------------------------
-- The answers

-- | 'Json' is the media type of the JSON answers, @application\/json@,
-- without the charset servant's own @JSON@ writes.
data Json

instance Accept Json where
  contentType _ = "application" // "json"

instance (ToJSON a) => MimeRender Json a where
  mimeRender _ = encode

-- | The media type of every Markdown this API sends.
markdownMediaType :: ByteString
markdownMediaType = "text/markdown; charset=utf-8"

-- | 'RespondMarkdown' is an answer of Markdown text with this status, as
-- 'markdownMediaType', and the description @desc@ in the document.
data RespondMarkdown (s :: Nat) (desc :: Symbol)

type instance ResponseType (RespondMarkdown s desc) = Text

instance (KnownStatus s) => ResponseRender cs (RespondMarkdown s desc) where
  type ResponseStatus (RespondMarkdown s desc) = s
  type ResponseBody (RespondMarkdown s desc) = LByteString
  responseRender _ text =
    pure (InternalResponse (statusVal (Proxy @s)) (Seq.singleton (hContentType, markdownMediaType)) (toS (encodeUtf8 text)))

instance AsConstructor '[Text] (RespondMarkdown s desc) where
  toConstructor text = I text :* Nil
  fromConstructor (I text :* Nil) = text

instance (KnownSymbol desc) => IsSwaggerResponse (RespondMarkdown s desc) where
  responseSwagger =
    pure $
      mempty
        & O.description .~ toS (symbolVal (Proxy @desc))
        & O.content .~ InsOrd.fromList [("text" // "markdown" /: ("charset", "utf-8"), mempty & O.schema ?~ Inline (toParamSchema (Proxy @Text)))]

-- | 'Version' is a page's version in a header (@ETag@, @If-Match@): the
-- time of its last save, quoted.
newtype Version = Version PageVersion
  deriving newtype (Eq, Show, ToHttpApiData, FromHttpApiData)

instance ToParamSchema Version where
  toParamSchema _ = mempty & O.type_ ?~ O.OpenApiString & O.pattern ?~ "^\"[^\"]+\"$"

-- | 'CacheControl' is the value of an answer's @Cache-Control@.
newtype CacheControl = CacheControl Text
  deriving newtype (Eq, Show, ToHttpApiData, FromHttpApiData)

instance ToParamSchema CacheControl where
  toParamSchema _ = toParamSchema (Proxy @Text)

-- | 'JsonBodies' is the types of the JSON bodies of an API: of the answers
-- sent as 'Json', and of the bodies the document gives as
-- @application\/json@. The test of the schemas checks each against its
-- JSON.
type JsonBodies :: Type -> [Type]
type family JsonBodies api where
  JsonBodies (a :<|> b) = Append (JsonBodies a) (JsonBodies b)
  JsonBodies (Body "application/json" a desc :> api) = a ': JsonBodies api
  JsonBodies ((x :: k) :> api) = JsonBodies api
  JsonBodies (MultiVerb method cs as r) = JsonAnswers as
  JsonBodies api = '[]

-- | The bodies of the answers of a 'MultiVerb' sent as 'Json'.
type JsonAnswers :: [Type] -> [Type]
type family JsonAnswers as where
  JsonAnswers '[] = '[]
  JsonAnswers (WithHeaders hs a r ': as) = JsonAnswers (r ': as)
  JsonAnswers (RespondAs Json s desc a ': as) = a ': JsonAnswers as
  JsonAnswers (r ': as) = JsonAnswers as

type Append :: [Type] -> [Type] -> [Type]
type family Append xs ys where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys
