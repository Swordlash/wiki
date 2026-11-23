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
-- Only HTTP Basic, with the user's login and password
-- ("Wiki.Server.BasicSignIn"). The session cookie is never read here, so a
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

    -- * What the API for programs shares
  , signedInBasic
  , runAs
  , refusal
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
import Wiki.Api.Error (ApiError, errorStatus)
import Wiki.Api.Version (PageVersion (..), pageVersion)
import Wiki.App.Backend (MonadWiki (getPage, getSpace, savePage), WikiError (..))
import Wiki.App.Route qualified as Route
import Wiki.Core.Access (Access (..))
import Wiki.Core.Assistant.Reference (markdownReference)
import Wiki.Core.Id (UserId)
import Wiki.Core.Page (Page (..))
import Wiki.Core.PageKind (PageKind (..), pageKindOf)
import Wiki.Core.Path (renderPath)
import Wiki.Core.Slug (Slug, mkSlug, slugText, slugify)
import Wiki.Core.Space (Space (id))
import Wiki.Server.Api (Env (..), toApiError)
import Wiki.Server.BasicSignIn (Outcome (..), credentialsOf, signInBasic)
import Wiki.Server.Render (routeOf)
import Wiki.Server.Storage (StoredWiki, Who (..), createPageAt, readSettings, runStoredWikiFrom)

-- | The media type of every Markdown this API sends.
markdownMediaType :: ByteString
markdownMediaType = "text/markdown; charset=utf-8"

-- | The API for a page's Markdown, on a listener that serves the wiki
-- under this path (with a slash at each end), for a request from this
-- address, whose path after @api\/markdown@ is these segments.
markdownApplication :: Env -> Text -> Text -> [Text] -> Application
markdownApplication env mount address segments request respond = case segments of
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
    signedIn = signedInBasic env address request respond
    run :: UserId -> StoredWiki IO a -> IO a
    run = runAs env address
    -- The page a space's path and the page's path under its home page name,
    -- as the user may see it.
    pageOf :: UserId -> [Slug] -> [Slug] -> IO (Either WikiError Page)
    pageOf u sp pp = run u (getSpace sp) >>= either (pure . Left) (\s -> run u (getPage s.id pp))
    onPage u sp pp
      | readOnly =
          pageOf u sp pp >>= \case
            Left e -> respond (wikiRefusal e)
            Right p -> respond (responseLBS status200 [(hContentType, markdownMediaType), etag p, (hCacheControl, "private, no-store")] (toS (encodeUtf8 p.body)))
      | requestMethod request == methodPut =
          case (traverse (parseHeader @PageVersion) (lookup hIfMatch (requestHeaders request)), createOnlyOf (lookup hIfNoneMatch (requestHeaders request))) of
            (Left why, _) -> respond (refusal status400 "Invalid" (Just ("The If-Match header is not a page's version from an ETag: " <> why)))
            (_, Nothing) -> respond (refusal status400 "Invalid" (Just "The If-None-Match header here is * (create the page only if there is none), or nothing."))
            (Right expected, Just createOnly) ->
              decodeUtf8' . toS <$> strictRequestBody request >>= \case
                Left _ -> respond (refusal status400 "Invalid" (Just "The body is not UTF-8 text."))
                Right body -> writeAt u sp pp expected createOnly body
      | otherwise = respond (notAllowed [methodGet, methodHead, methodPut])
    -- Whether an If-None-Match makes the PUT create only: 'Nothing' for a
    -- value other than @*@, which this API does not take.
    createOnlyOf = \case
      Nothing -> Just False
      Just v | T.strip (decodeUtf8With lenientDecode v) == "*" -> Just True
      Just _ -> Nothing
    -- Write the body at the address: over the page there, or as a new page
    -- where there is none and nothing in the request forbids making one.
    writeAt u sp pp expected createOnly body =
      run u (getSpace sp) >>= \case
        Left e -> respond (wikiRefusal e)
        Right s ->
          run u (getPage s.id pp) >>= \case
            Right _
              | createOnly -> respond exists
              | otherwise -> write u sp pp expected body (3 :: Int)
            Left NotFound
              | Just slug <- lastMay pp ->
                  if isJust expected
                    then respond (refusal status404 "NotFound" (Just "There is no page here, and If-Match names a version of a page that is: leave If-Match out to create the page."))
                    else create u s sp pp slug createOnly body
            Left e -> respond (wikiRefusal e)
    -- Create the page at the address, under the page that the slugs before
    -- its own slug name. A page made at the address since the read above is
    -- written over, as any page there is, unless the request creates only.
    create u s sp pp slug createOnly body = case (parameter request "title", parameter request "kind") of
      (Left why, _) -> respond (refusal status400 "Invalid" (Just why))
      (_, Left why) -> respond (refusal status400 "Invalid" (Just why))
      (Right title, Right kind) ->
        parentOf u s pp >>= \case
          Left refused -> respond refused
          Right parent ->
            run u (createPageAt (maybe Document pageKindOf kind) s.id parent slug (fromMaybe (titleOfSlug slug) title) body) >>= \case
              Right page -> respond (responseLBS status201 [(hLocation, encodeUtf8 (addressUnder mount "api/markdown" (Route.pageRoute sp pp))), etag page, (hCacheControl, "no-store")] "")
              Left (Conflict _)
                | createOnly -> respond exists
                | otherwise -> write u sp pp Nothing body (3 :: Int)
              Left e -> respond (wikiRefusal e)
    -- The page above a new page's address: 'Nothing' for a page at the top
    -- of the space's pages, under its home page.
    parentOf u s pp = case initMay pp of
      Just above@(_ : _) ->
        run u (getPage s.id above) <&> \case
          Right p -> Right (Just p.id)
          Left NotFound -> Left (refusal status404 "NotFound" (Just ("The page above it, " <> renderPath above <> ", is not there: create that page first.")))
          Left e -> Left (wikiRefusal e)
      _ -> pure (Right Nothing)
    exists = refusal status412 "Conflict" (Just "There is a page here already, and If-None-Match: * asks to create one only where there is none.")
    -- Save the body over the page, against the version in If-Match, or
    -- against the version just read when there is none. A save between that
    -- read and this write is a conflict: with If-Match the answer is 412,
    -- without it the read and the write are tried again, a few times.
    write u sp pp expected body tries =
      pageOf u sp pp >>= \case
        Left e -> respond (wikiRefusal e)
        Right p
          | Just v <- expected, v /= pageVersion p -> respond moved
          | otherwise ->
              run u (savePage p.id (maybe p.updatedAt (\v -> v.time) expected) p.title body) >>= \case
                Right saved -> respond (responseLBS status204 [etag saved] "")
                Left (Conflict _)
                  | isJust expected -> respond moved
                  | tries > 1 -> write u sp pp expected body (tries - 1)
                Left e -> respond (wikiRefusal e)
    moved = refusal status412 "Conflict" (Just "The page has been saved since the version in If-Match: read it again, and write over that version.")
    etag p = ("ETag", toHeader (pageVersion p))

-- | 'signedInBasic' signs a request in by its HTTP Basic credentials, the
-- user's login and password ("Wiki.Server.BasicSignIn"), and goes on as the
-- user they sign in. Every address of the API for programs signs in through
-- it: this module's, and the listing and the search of "Wiki.Server.ListApi".
-- Without credentials, or with wrong ones, the answer is 401; a login with
-- too many failed attempts is 429, and an account with a second factor 403.
-- The session's cookie is never read.
signedInBasic :: Env -> Text -> Request -> (Response -> IO ResponseReceived) -> (UserId -> IO ResponseReceived) -> IO ResponseReceived
signedInBasic env address request respond continue = case lookup hAuthorization (requestHeaders request) >>= credentialsOf of
  Nothing -> unauthenticated
  Just (login, password) -> do
    now <- getCurrentTime
    signInBasic env.storage env.limiter env.basicSignIns now address login password >>= \case
      SignedIn u -> continue u
      Refused -> unauthenticated
      Limited seconds ->
        respond (refusalWith status429 [("Retry-After", show seconds)] "TooManyRequests" (Just ("Too many failed attempts to sign in as " <> T.strip login <> ": try again in " <> show seconds <> " seconds.")))
      SecondFactorOn ->
        respond (refusal status403 "Forbidden" (Just "This account signs in with a second factor, which this API cannot take yet: a password alone does not sign it in."))
  where
    unauthenticated = do
      respond . refusalWith status401 [("WWW-Authenticate", "Basic realm=\"wiki\", charset=\"UTF-8\"")] "Unauthenticated" $
        Just "Sign in with your login and password (HTTP Basic). They are wrong, or the account cannot sign in with a password: an account with passkeys only, or one that has not signed in since the wiki moved to its present passwords (sign in once in the browser)."

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
-- ("Wiki.Core.Slug"), with the slug the wiki would make of it. 'Nothing'
-- when every segment is one, or for an address that is not a space's or a
-- page's. A slug that is one of the router's own words is written with a
-- @~@ in front ("Wiki.App.Route"), and is a slug.
notASlug :: [Text] -> Maybe Text
notASlug = \case
  "s" : rest -> head (mapMaybe refused (filter (`notElem` ["p", ""]) rest))
  _ -> Nothing
  where
    refused t = case mkSlug (fromMaybe t (T.stripPrefix "~" t)) of
      Right _ -> Nothing
      Left _ ->
        Just
          ( "The segment "
              <> t
              <> " is not a slug: a slug is lowercase ASCII letters, digits and single dashes between them"
              <> maybe "." (\s -> ", such as " <> slugText s <> ".") (slugify t)
          )
