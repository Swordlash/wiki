-- | The API for a page's Markdown (WP5.138 of docs/PLAN.md, its first
-- cut): what programs on a user's own machine (a local assistant, later an
-- MCP server) use to read a page's Markdown and write it back. It lives
-- under @api\/markdown\/@ on the public listener, beside the application's
-- own API under @api\/v1\/@: the application's later versions would be
-- @api\/v2\/@ and so on, which @markdown@ can never be.
--
-- = Addresses
--
-- * @GET api\/markdown\/s\/\<space path>\/p\/\<page path>\/@: the page's
--   Markdown, as stored, with its version as the @ETag@. A page's address
--   here is its reading address in the browser with @api\/markdown@ in
--   front, read by the application's own router ("Wiki.App.Route"), so
--   nested spaces, a space's home page (@s\/\<space path>\/@) and a slug
--   written with a @~@ are read exactly as the browser reads them.
-- * @PUT@ at the same address: the request's body, UTF-8, becomes the
--   page's whole Markdown, through the same 'savePage' as a save from the
--   editor (the same new version in the history under the user's name, the
--   same push to readers with the page open, the same notifications). The
--   title stays. An @If-Match@ with the version from the @ETag@ makes the
--   write conditional: a page saved since is refused with 412 and nothing
--   is written. Without it the write goes through. The answer is 204 with
--   the new @ETag@.
-- * @PUT@ at the address of a page that does not exist creates it, as a
--   program that writes a file does not first ask whether the file is
--   there. The address's last slug is the new page's slug, and the slugs
--   before it name its parent, which has to exist: nothing is created on
--   the way. The title is @?title=@, or else made from the slug
--   ('titleOfSlug'); the kind is @?kind=@ (@document@ by default). It goes
--   through 'createPageAt', the application's "New page" with the slug
--   given: the same checks, the same first version under the user's name,
--   the same notifications. The answer is 201 with the page's address as
--   @Location@ and its @ETag@. @If-None-Match: *@ makes the @PUT@ create
--   only: a page already there is refused with 412. @If-Match@ never
--   creates: it names a version of a page that is there.
-- * @GET api\/markdown\/reference@: what the writing assistant is told
--   about the wiki's Markdown ('markdownReference', the same text its
--   prompt sends), as @text\/markdown@. Anyone may read it, unless the wiki
--   is closed; then it needs a sign-in, as everything else there does.
--
-- = Signing in
--
-- HTTP Basic, with the user's login and password
-- ("Wiki.Server.BasicSignIn"), or an API key ("Wiki.Server.ApiKeys"), one
-- of them in each request ('signedInProgram'). The session cookie is never
-- read here, so a
-- page in a browser cannot drive these addresses with its reader's
-- session; and the application's marker header is not asked for
-- ("Wiki.Server.Session"'s 'Wiki.Server.Session.sameOrigin'), since no
-- cookie means nothing for another site's page to borrow.
--
-- = Refusals
--
-- Every refusal is a small JSON object in the shape the application's API
-- uses ("Wiki.Api.Error"): @{"error": "NotFound"}@, or with a
-- @"message"@ that says why. A page the user may not read is 404, as one
-- that is not there. A page of an encrypted space is 409 @Stale@: the
-- server cannot read or write its text, which is encrypted in its
-- members' browsers. A @PUT@ to an address with a segment that is not a
-- slug is 400: no page can ever be there.
module Wiki.Server.MarkdownApi
  ( markdownApplication
  , markdownMediaType

    -- * A page's Markdown, read and written
  , findPage
  , Put (..)
  , putMarkdown

    -- * What the API for programs shares
  , Asking (..)
  , Credential (..)
  , credentialOf
  , signedInProgram
  , signInWithKey
  , runAs
  , refusal
  , refusalWith
  , wikiRefusal
  , notAllowed
  , parameter
  , addressUnder
  , titleOfSlug
  , notASlug
  )
where

import Data.Aeson (encode, object, (.=))
import Data.ByteString qualified as BS
import Data.List (lookup)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Halogen.Router.Duplex qualified as Duplex
import Network.HTTP.Types (Header, Status, hAllow, hAuthorization, hCacheControl, hContentType, hIfMatch, hIfNoneMatch, hLocation, methodGet, methodHead, methodPut, status200, status201, status204, status400, status401, status403, status404, status405, status412, status429)
import Network.Wai (Application, Request (..), Response, ResponseReceived, responseLBS, strictRequestBody)
import Protolude
import Web.HttpApiData (parseHeader, toHeader)
import Wiki.Api.Admin (WikiSettings (..))
import Wiki.Api.ApiKeys (ApiKeyId (..), KeyRight (..), rightName)
import Wiki.Api.Error (ApiError, errorStatus)
import Wiki.Api.Version (PageVersion (..), pageVersion)
import Wiki.App.Backend (MonadWiki (getPage, getSpace, savePage), WikiError (..))
import Wiki.App.Route qualified as Route
import Wiki.Core.Access (Access (..))
import Wiki.Core.Assistant.Reference (markdownReference)
import Wiki.Core.Id (UserId (..))
import Wiki.Core.Page (Page (..))
import Wiki.Core.PageKind (PageKind (..), pageKindOf)
import Wiki.Core.Path (renderPath)
import Wiki.Core.Slug (Slug, mkSlug, slugText, slugify)
import Wiki.Core.Space (Space (id))
import Wiki.Server.Api (Env (..), toApiError)
import Wiki.Server.ApiKeys (KeyHolder (..), KeyOutcome (..), allows, isAdministrator, refusalName, signInByKey)
import Wiki.Server.BasicSignIn (Outcome (..), credentialsOf, signInBasic)
import Wiki.Server.Log (Level (..), logWith)
import Wiki.Server.Render (routeOf)
import Wiki.Server.Storage (StoredWiki, Who (..), createPageAt, readSettings, runStoredWikiFrom)

-- | The media type of every Markdown this API sends.
markdownMediaType :: ByteString
markdownMediaType = "text/markdown; charset=utf-8"

-- | The API for a page's Markdown, on a listener that serves the wiki
-- under this path (with a slash at each end), for a request from this
-- address, whose path after @api\/markdown@ is these segments.
markdownApplication :: Env -> Text -> Asking -> [Text] -> Application
markdownApplication env mount asking@Asking {address} segments request respond = case segments of
  ["reference"]
    | readOnly -> do
        closed <- (\s -> s.access == Closed) <$> readSettings env.storage
        if closed then signedIn (const reference) else reference
    | otherwise -> respond (notAllowed [methodGet, methodHead])
  _ -> signedIn $ \u ->
    routeOf "/" pagePath >>= \case
      Route.Page sp pp -> onPage u sp pp
      Route.Space sp -> onPage u sp []
      _
        | requestMethod request == methodPut, Just why <- notASlug segments -> respond (refusal status400 "Invalid" (Just why))
        | otherwise -> respond (refusal status404 "NotFound" Nothing)
  where
    readOnly = requestMethod request `elem` [methodGet, methodHead]
    reference = respond (responseLBS status200 [(hContentType, markdownMediaType), (hCacheControl, "no-cache")] (toS (encodeUtf8 markdownReference)))
    -- The path after the prefix, as sent (percent-encoded), with its query:
    -- what the application's router reads.
    prefix = encodeUtf8 mount <> "api/markdown"
    pagePath = fromMaybe "/" (BS.stripPrefix prefix (rawPathInfo request)) <> rawQueryString request
    signedIn = signedInProgram env asking request respond
    onPage u sp pp
      | readOnly =
          findPage env address u sp pp >>= \case
            Left e -> respond (wikiRefusal e)
            Right p -> respond (responseLBS status200 [(hContentType, markdownMediaType), etag p, (hCacheControl, "private, no-store")] (toS (encodeUtf8 p.body)))
      | requestMethod request == methodPut =
          case (traverse (parseHeader @PageVersion) (lookup hIfMatch (requestHeaders request)), createOnlyOf (lookup hIfNoneMatch (requestHeaders request))) of
            (Left why, _) -> respond (refusal status400 "Invalid" (Just ("The If-Match header is not a page's version from an ETag: " <> why)))
            (_, Nothing) -> respond (refusal status400 "Invalid" (Just "The If-None-Match header here is * (create the page only if there is none), or nothing."))
            (Right expected, Just createOnly) ->
              decodeUtf8' . toS <$> strictRequestBody request >>= \case
                Left _ -> respond (refusal status400 "Invalid" (Just "The body is not UTF-8 text."))
                Right body -> putMarkdown env address u sp pp expected createOnly newPage body >>= respond . putAnswer sp pp
      | otherwise = respond (notAllowed [methodGet, methodHead, methodPut])
    -- Whether an If-None-Match makes the PUT create only: 'Nothing' for a
    -- value other than @*@, which this API does not take.
    createOnlyOf = \case
      Nothing -> Just False
      Just v | T.strip (decodeUtf8With lenientDecode v) == "*" -> Just True
      Just _ -> Nothing
    -- The title and the kind of a page the PUT creates, from the query.
    newPage = case (parameter request "title", parameter request "kind") of
      (Left why, _) -> Left why
      (_, Left why) -> Left why
      (Right title, Right kind) -> Right (title, maybe Document pageKindOf kind)
    -- The answer to a PUT: 204 for a save, 201 for a new page, each with
    -- the page's new version; 412 for a page saved since the version in
    -- If-Match, or for one there when If-None-Match: * creates only.
    putAnswer sp pp = \case
      Saved saved -> responseLBS status204 [etag saved] ""
      Created page -> responseLBS status201 [(hLocation, encodeUtf8 (addressUnder mount "api/markdown" (Route.pageRoute sp pp))), etag page, (hCacheControl, "no-store")] ""
      Moved -> refusal status412 "Conflict" (Just "The page has been saved since the version in If-Match: read it again, and write over that version.")
      Exists -> refusal status412 "Conflict" (Just "There is a page here already, and If-None-Match: * asks to create one only where there is none.")
      NoPage -> refusal status404 "NotFound" (Just "There is no page here, and If-Match names a version of a page that is: leave If-Match out to create the page.")
      NoPageAbove above -> refusal status404 "NotFound" (Just ("The page above it, " <> renderPath above <> ", is not there: create that page first."))
      NewPageInvalid why -> refusal status400 "Invalid" (Just why)
      PutRefused e -> wikiRefusal e
    etag p = ("ETag", toHeader (pageVersion p))

-- | The page that a space's path and a page's path under its home page
-- name, as a user may see it, for a request from this address. A space's
-- home page is the page at the empty path.
findPage :: Env -> Text -> UserId -> [Slug] -> [Slug] -> IO (Either WikiError Page)
findPage env address u sp pp = runAs env address u (getSpace sp) >>= either (pure . Left) (\s -> runAs env address u (getPage s.id pp))

-- | 'Put' is what a write of a page's whole Markdown came to
-- ('putMarkdown'). Only 'Saved' and 'Created' wrote anything.
data Put
  = -- | Saved over the page that was there: the page after the save.
    Saved Page
  | -- | Created where there was no page: the new page.
    Created Page
  | -- | The page has been saved since the version the write named.
    Moved
  | -- | A page is there, and the write asked to create one only.
    Exists
  | -- | No page is there, and the write named a version of one.
    NoPage
  | -- | No page is there, and neither is the page above the address, at
    -- this path: nothing is created on the way.
    NoPageAbove [Slug]
  | -- | The title or the kind given for a new page is not valid: why.
    NewPageInvalid Text
  | -- | The wiki refused: not found, forbidden, an encrypted space.
    PutRefused WikiError

-- | 'putMarkdown' writes a page's whole Markdown at a space's path and a
-- page's path, as a user, for a request from this address. The HTTP API's
-- @PUT@ and the MCP server's tools ("Wiki.Server.McpApi") both write
-- through it, so that a page is written one way whoever asks.
--
-- The body goes over the page there, through the same 'savePage' as a
-- save from the editor, or makes a new page where there is none. A version
-- ('Just') makes the write conditional: a page saved since is 'Moved', and
-- nothing is written; a write with a version never creates. Without a
-- version, the write goes over whatever is there: the page is read, and a
-- save between that read and the write makes both be tried again, a few
-- times. Create only ('True') writes nowhere a page already is.
--
-- A new page is made through 'createPageAt', the application's "New page"
-- with the slug given (the address's last slug), under the page that the
-- slugs before it name. Its title and kind are the ones given, or 'Left'
-- why the ones given are not valid, which matters only when a page is
-- created. Without a title, the title is made from the slug.
putMarkdown :: Env -> Text -> UserId -> [Slug] -> [Slug] -> Maybe PageVersion -> Bool -> Either Text (Maybe Text, PageKind) -> Text -> IO Put
putMarkdown env address u sp pp expected createOnly newPage body =
  run (getSpace sp) >>= \case
    Left e -> pure (PutRefused e)
    Right s ->
      run (getPage s.id pp) >>= \case
        Right _
          | createOnly -> pure Exists
          | otherwise -> write (3 :: Int)
        Left NotFound
          | Just slug <- lastMay pp ->
              if isJust expected then pure NoPage else create s slug
        Left e -> pure (PutRefused e)
  where
    run :: StoredWiki IO a -> IO a
    run = runAs env address u
    -- Create the page at the address. A page made at the address since the
    -- read above is written over, as any page there is, unless the write
    -- creates only.
    create s slug = case newPage of
      Left why -> pure (NewPageInvalid why)
      Right (title, kind) ->
        parentOf s >>= \case
          Left refused -> pure refused
          Right parent ->
            run (createPageAt kind s.id parent slug (fromMaybe (titleOfSlug slug) title) body) >>= \case
              Right page -> pure (Created page)
              Left (Conflict _)
                | createOnly -> pure Exists
                | otherwise -> write 3
              Left e -> pure (PutRefused e)
    -- The page above a new page's address: 'Nothing' for a page at the top
    -- of the space's pages, under its home page.
    parentOf s = case initMay pp of
      Just above@(_ : _) ->
        run (getPage s.id above) <&> \case
          Right p -> Right (Just p.id)
          Left NotFound -> Left (NoPageAbove above)
          Left e -> Left (PutRefused e)
      _ -> pure (Right Nothing)
    -- Save the body over the page, against the version given, or against
    -- the version just read when there is none.
    write tries =
      findPage env address u sp pp >>= \case
        Left e -> pure (PutRefused e)
        Right p
          | Just v <- expected, v /= pageVersion p -> pure Moved
          | otherwise ->
              run (savePage p.id (maybe p.updatedAt (\v -> v.time) expected) p.title body) >>= \case
                Right saved -> pure (Saved saved)
                Left (Conflict _)
                  | isJust expected -> pure Moved
                  | tries > 1 -> write (tries - 1)
                Left e -> pure (PutRefused e)

-- | 'Asking' is what the listener knows of a request to the API for
-- programs before it is signed in: the address it came from, and how to
-- name in the request's log line who it signed in ("Wiki.Server.Log").
data Asking = Asking
  { address :: Text
  , loggedAs :: [(Text, Text)] -> IO ()
  }

-- | 'Credential' is what a request to the API for programs signs in with
-- ('credentialOf').
data Credential
  = -- | No @Authorization@ header and no @X-API-Key@.
    NoCredential
  | -- | An @Authorization@ header that is neither a Basic one that decodes
    -- nor a bearer token.
    Unreadable
  | -- | HTTP Basic: a login and a password.
    LoginPassword Text Text
  | -- | An API key, from @Authorization: Bearer@ or @X-API-Key@, as sent.
    Key Text

-- | The one credential a request carries, or why it carries more than one:
-- an API key beside a login and a password, two different keys, or a
-- header twice. One credential is one way the request was meant to sign
-- in; with two, whichever the server took, it could take the wrong one.
-- The same key in both @Authorization: Bearer@ and @X-API-Key@ is one
-- credential.
credentialOf :: Request -> Either Text Credential
credentialOf request = case (named hAuthorization, named "X-API-Key") of
  (_ : _ : _, _) -> Left "The request has more than one Authorization header: send one credential."
  (_, _ : _ : _) -> Left "The request has more than one X-API-Key header: send one key."
  ([], []) -> Right NoCredential
  ([], [k]) -> Right (Key (text k))
  ([a], given) -> case (T.toCaseFold (T.takeWhile (/= ' ') (text a)), given) of
    ("bearer", []) -> Right (Key (bearer a))
    ("bearer", [k]) | text k == bearer a -> Right (Key (bearer a))
    ("bearer", _) -> Left "The request has two different keys, in Authorization and in X-API-Key: send one."
    (_, _ : _) -> Left "The request has an API key in X-API-Key and another credential in Authorization: send one of them."
    ("basic", []) -> Right (maybe Unreadable (uncurry LoginPassword) (credentialsOf a))
    _ -> Right Unreadable
  where
    named n = [v | (n', v) <- requestHeaders request, n' == n]
    -- A header that is not UTF-8 is no key: it is read as the empty text,
    -- which no key is.
    text = T.strip . either (const "") identity . decodeUtf8'
    bearer = T.strip . T.dropWhile (/= ' ') . text

-- | 'signedInProgram' signs a request in to the API for programs, and goes
-- on as the user it signs in. Every address of that API signs in through
-- it: this module's, and the listing and the search of
-- "Wiki.Server.ListApi", so that none takes one credential and not the
-- other. A request signs in with the user's login and password (HTTP
-- Basic, "Wiki.Server.BasicSignIn") or with an API key
-- ("Wiki.Server.ApiKeys"), and with one of them only: a request with two
-- credentials is 400 ('credentialOf'). The session's cookie is never read.
--
-- Without credentials, or with wrong ones, the answer is 401. A login with
-- too many failed attempts from the address is 429, and so is an address
-- with too many failed keys. An account with a second factor is 403 with
-- a password, and signs in with a key: the key was made in a session the
-- second factor let in. A key whose right does not allow the request's
-- method is 403, and so is a key with the right @admin@ whose user is no
-- longer an administrator. Then the request goes on as the key's user,
-- with what the user may do at that moment.
--
-- The request's log line names the user signed in, and the key's id for
-- a key ('Asking'). A refused key is logged with why, and with its id
-- when the key was well formed; never the key itself.
signedInProgram :: Env -> Asking -> Request -> (Response -> IO ResponseReceived) -> (UserId -> IO ResponseReceived) -> IO ResponseReceived
signedInProgram env asking request respond continue = case credentialOf request of
  Left why -> respond (refusal status400 "Invalid" (Just why))
  Right NoCredential -> unauthenticated
  Right Unreadable -> unauthenticated
  Right (LoginPassword login password) -> do
    now <- getCurrentTime
    signInBasic env.storage env.limiter env.basicSignIns now asking.address login password >>= \case
      SignedIn u -> do
        asking.loggedAs [("principal", "user:" <> u.text)]
        continue u
      Refused -> unauthenticated
      Limited seconds ->
        respond (refusalWith status429 [("Retry-After", show seconds)] "TooManyRequests" (Just ("Too many failed attempts to sign in as " <> T.strip login <> ": try again in " <> show seconds <> " seconds.")))
      SecondFactorOn ->
        respond (refusal status403 "Forbidden" (Just "This account signs in with a second factor, which a password alone cannot give: use an API key, which the account makes in its settings."))
  Right (Key presented) ->
    signInWithKey env asking presented >>= \case
      Left refused -> respond refused
      Right holder
        | not (allows holder.right (requestMethod request)) ->
            respond . refusal status403 "Forbidden" . Just $
              "This key has the right " <> rightName holder.right <> ", which only reads: a " <> decodeUtf8With lenientDecode (requestMethod request) <> " needs a key with the right write."
        | otherwise -> continue holder.user
  where
    unauthenticated = do
      respond . refusalWith status401 [("WWW-Authenticate", "Basic realm=\"wiki\", charset=\"UTF-8\"")] "Unauthenticated" $
        Just "Sign in with your login and password (HTTP Basic), or with an API key (Authorization: Bearer, or X-API-Key). The login and password are wrong, or the account cannot sign in with a password: an account with passkeys only, or one that has not signed in since the wiki moved to its present passwords (sign in once in the browser)."

-- | 'signInWithKey' signs in the API key a request presents, as sent: for
-- the API for programs ('signedInProgram') and for the MCP server
-- ("Wiki.Server.McpApi"), so that a key is checked one way wherever it is
-- used. The result is who the key signed in, or the answer that refuses
-- the request.
--
-- A key that does not sign in ("Wiki.Server.ApiKeys"' 'signInByKey') is
-- 401, with one answer for every reason, and 429 once the request's
-- address has failed too often. A key with the right @admin@ whose user is
-- no longer an administrator is 403. The request's log line names the
-- key's user and the key's id ('Asking'). A refused key is logged with
-- why, and with its id when the key was well formed; never the key itself.
signInWithKey :: Env -> Asking -> Text -> IO (Either Response KeyHolder)
signInWithKey env asking presented = do
  now <- getCurrentTime
  signInByKey env.storage env.keyFailures now asking.address presented >>= \case
    KeySignedIn holder -> do
      asking.loggedAs [("principal", "user:" <> holder.user.text), ("key", holder.key.text)]
      lapsed <- case holder.right of
        AdminRight -> not <$> isAdministrator env.storage asking.address holder.user
        _ -> pure False
      pure $
        if lapsed
          then Left (refusal status403 "Forbidden" (Just "This key has the right admin, which only an administrator's key has, and its user is not an administrator any more: make another key."))
          else Right holder
    KeyRefused i why -> do
      refusedKey i why
      pure (Left keyUnauthenticated)
    KeyLimited i why seconds -> do
      refusedKey i why
      pure (Left (refusalWith status429 [("Retry-After", show seconds)] "TooManyRequests" (Just ("Too many requests with a wrong API key from this address: try again in " <> show seconds <> " seconds."))))
  where
    -- One answer for every refused key: the caller is not told whether
    -- the key is unknown, wrong, expired or revoked, or its user disabled.
    keyUnauthenticated =
      refusalWith status401 [("WWW-Authenticate", "Bearer realm=\"wiki\", error=\"invalid_token\"")] "Unauthenticated" $
        Just "This API key does not sign in: it is not a key of this wiki, or it has expired or been revoked, or its user cannot sign in."
    refusedKey i why =
      logWith env.logger Warning ([("event", "api-key-refused"), ("why", refusalName why)] <> [("key", k.text) | Just k <- [i]] <> [("from", asking.address)])

-- | 'runAs' runs a program of the wiki's domain as a signed-in user, for a
-- request from this address, against the server's storage: the same
-- permissions as the user's requests from the browser. The program is the
-- application's ('MonadWiki'), or one of the storage's own calls beside it
-- ('createPageAt').
runAs :: Env -> Text -> UserId -> StoredWiki IO a -> IO a
runAs env address u = runStoredWikiFrom env.storage (AsUser u) address

-- | A refusal: the error's name, and why, as the application's API writes
-- an 'ApiError'.
refusal :: Status -> Text -> Maybe Text -> Response
refusal status = refusalWith status []

-- | 'refusal' with headers of its own (@Allow@, @Retry-After@).
refusalWith :: Status -> [Header] -> Text -> Maybe Text -> Response
refusalWith status hs name why =
  responseLBS status ((hContentType, "application/json") : (hCacheControl, "no-store") : hs) (encode (object (("error" .= name) : maybe [] (\w -> ["message" .= w]) why)))

-- | A refusal of the wiki's domain, as the application's API answers it:
-- its 'ApiError', with that error's status.
wikiRefusal :: WikiError -> Response
wikiRefusal e = case toApiError e of
  Just (apiError :: ApiError) -> responseLBS (toEnum (errorStatus apiError)) [(hContentType, "application/json"), (hCacheControl, "no-store")] (encode apiError)
  Nothing -> refusal (toEnum 503) "Unreachable" (Just "The wiki's storage could not be reached.")

-- | A method the address does not take, with the ones it does.
notAllowed :: [ByteString] -> Response
notAllowed methods = refusalWith status405 [(hAllow, BS.intercalate ", " methods)] "Invalid" (Just "This address does not take that method.")

-- | The value of a parameter of the request's query, as UTF-8 text.
parameter :: Request -> ByteString -> Either Text (Maybe Text)
parameter request name = case join (lookup name (queryString request)) of
  Nothing -> Right Nothing
  Just bytes -> bimap (const ("The parameter " <> decodeUtf8 name <> " is not UTF-8 text.")) Just (decodeUtf8' bytes)

-- | The address of a route of the application under the API's prefix
-- (@api\/list@, @api\/markdown@), on a listener that serves the wiki under
-- this path: the route's address as the application writes it, with the
-- prefix in front.
addressUnder :: Text -> Text -> Route.Route -> Text
addressUnder mount prefix r = mount <> prefix <> Duplex.print Route.route r

-- | 'titleOfSlug' is the title of a page or a space created without one:
-- its slug with spaces for the dashes and a capital first letter, so that
-- @release-notes@ is titled "Release notes". The title's own slug
-- ('slugify') is the slug again.
titleOfSlug :: Slug -> Text
titleOfSlug slug = case T.uncons (T.replace "-" " " (slugText slug)) of
  Just (c, rest) -> T.cons (toUpper c) rest
  Nothing -> ""

-- | Why the address of a page or a space, as the segments of its path after
-- the API's prefix, can name nothing: the first segment that is not a slug
-- ("Wiki.Core.Slug"), with the slug the wiki would make of it, or that is
-- one of the router's own words (@history@, @edit@, …) written bare. Such
-- a word is a slug, written with a @~@ in front ("Wiki.App.Route"): the
-- router reads @history@ as a page's history, and @~history@ as a page.
-- 'Nothing' when every segment is a slug written as the router reads one,
-- or for an address that is not a space's or a page's. The first @p@
-- separates the space's path from the page's.
notASlug :: [Text] -> Maybe Text
notASlug = \case
  "s" : rest -> head (mapMaybe refused (withoutSeparator (filter (/= "") rest)))
  _ -> Nothing
  where
    withoutSeparator xs = let (space, page) = break (== "p") xs in space <> drop 1 page
    refused t = case mkSlug (fromMaybe t (T.stripPrefix "~" t)) of
      Right s
        | not ("~" `T.isPrefixOf` t)
        , "~" `T.isInfixOf` Duplex.print Route.route (Route.Space [s]) ->
            Just ("The segment " <> t <> " is one of the addresses' own words: a slug that is one is written ~" <> t <> ".")
        | otherwise -> Nothing
      Left _ ->
        Just
          ( "The segment "
              <> t
              <> " is not a slug: a slug is lowercase ASCII letters, digits and single dashes between them"
              <> maybe "." (\s -> ", such as " <> slugText s <> ".") (slugify t)
          )
