-- | The API for programs (WP5.138 of docs/PLAN.md, the owner's issue #338)
-- as one servant type, 'ProgramApi': a page's Markdown, read and written
-- whole or a part at a time, a page's table of contents, the listing of the
-- spaces and their pages, the search, the Markdown reference, and the
-- OpenAPI document of the API itself. The application serves it on the
-- public listener, outside @api\/v1@, signed in by a login and password
-- or an API key and never by the session's cookie ("Wiki.Server.App").
--
-- The handlers are those of "Wiki.Server.MarkdownApi",
-- "Wiki.Server.ContentsApi" and "Wiki.Server.ListApi". Every endpoint
-- signs in through one combinator, 'SignIn', whose check is
-- 'signedInProgram', and the type does not compile with an endpoint that
-- is not under it ('EverySignedIn'). The reference and the document are
-- under @SignIn 'WhenClosed@: anyone may read them, unless the wiki is
-- closed.
--
-- The OpenAPI document ('programDocument') is made from the type by
-- servant-openapi3, and served at @api\/openapi.json@, so that it says
-- what the server does: every address, its methods, parameters, headers
-- and answers by status, the three ways to present a credential, and the
-- schemas of the JSON. The MCP server at @mcp@ is not REST, and is named
-- in the document's description only; the application's own API,
-- @api\/v1@, is not in it.
module Wiki.Server.ProgramApi
  ( ProgramApi
  , programApplication
  , programDocument
  , OpenApiDocument (..)
  )
where

import Control.Lens ((.~), (?~))
import Data.Aeson (ToJSON (..))
import Data.OpenApi (NamedSchema (..), OpenApi, OpenApiType (..), ToSchema (..))
import Data.OpenApi qualified as O
import Data.Text qualified as T
import Network.HTTP.Types (status404)
import Network.Wai (Application)
import Protolude hiding (Handler)
import Servant (CaptureAll, Context (..), Description, ErrorFormatters (..), Header, Header', Lenient, Optional, Server, StdMethod (..), Summary, defaultErrorFormatters, serveWithContext, (:<|>) (..), (:>))
import Servant.API.MultiVerb (MultiVerb, RespondAs, WithHeaders)
import Servant.OpenApi (toOpenApi)
import Servant.OpenApi.Internal ()
import Wiki.Api.Admin (WikiSettings (..))
import Wiki.Core.Access (Access (..))
import Wiki.Core.PageKind (PageKind)
import Wiki.Server.Api (Env (..))
import Wiki.Server.ContentsApi (Contents, ContentsAnswer, MarkdownWrite, WriteAnswers, readContents, readMarkdown, refuseOtherMethods, writeMarkdown)
import Wiki.Server.ListApi (Hits, NewSpace, SpaceCreated, SpaceEntry, TopLevel, createSpace, listSpace, listTop, search)
import Wiki.Server.MarkdownApi (Asking (..), PageText, readReference, signedInProgram)
import Wiki.Server.ProgramApi.Combinators
import Wiki.Server.Storage (readSettings)

-- | 'ProgramApi' is the API for programs, from @api\/@ on, on a listener
-- that serves the wiki under some path.
type ProgramApi =
  "api"
    :> ( "markdown" :> (MarkdownAddress :<|> Reference :<|> Undocumented :> MarkdownElsewhere)
           :<|> "contents" :> (ContentsAddress :<|> Undocumented :> ContentsElsewhere)
           :<|> "list" :> (ListTopAddress :<|> ListSpaceAddress :<|> Undocumented :> ListElsewhere)
           :<|> "search" :> SearchAddress
           :<|> "openapi.json" :> DocumentAddress
       )

-- | An address under @api\/markdown@ that is neither a page's nor the
-- reference: answered by the page's handlers, which sign the request in
-- and then find no page there, so that a request without a credential is
-- 401 there as at a page's address, and a failed sign-in counts. The
-- document leaves it out. Likewise for the contents and the listing.
type MarkdownElsewhere =
  AsSent "address" "" :> CaptureAll "address" Text :> SignIn 'Always :> (ReadMarkdown :<|> WriteMarkdown :<|> OtherMethods '[ 'GET, 'HEAD, 'PUT])

type ContentsElsewhere =
  AsSent "address" "" :> CaptureAll "address" Text :> SignIn 'Always :> (ReadContents :<|> OtherMethods '[ 'GET, 'HEAD])

type ListElsewhere =
  AsSent "space" "" :> CaptureAll "space" Text :> SignIn 'Always :> (ListSpace :<|> CreateSpace :<|> OtherMethods '[ 'GET, 'HEAD, 'PUT])

-- | The address of a page's Markdown.
type MarkdownAddress =
  "s"
    :> Refuses 404 "There is no page at the address, or one the user may not read: the same answer for both. A section's anchor the page does not have (the message names the page's anchors). For a PUT that would create a page: no page above the address (nothing is created on the way), or If-Match where there is no page."
    :> AsSent "address" "The page's address in the browser after /s/: the space's path, then /p/ and the page's path under the space's home page, ending in a slash (team/projects/p/the-wiki/). The space's path alone (team/) is the space's home page. A slug that is one of the addresses' own words is written with a ~ (p/~history/). The address spans several segments, which a path parameter cannot say."
    :> CaptureAll "address" Text
    :> SignIn 'Always
    :> (ReadMarkdown :<|> WriteMarkdown :<|> OtherMethods '[ 'GET, 'HEAD, 'PUT])

type ReadMarkdown =
  Named "readMarkdown"
    :> Summary "Read a page's Markdown, or one part of it"
    :> Description "The page's Markdown as stored, with the page's version as the ETag. With section, one section of it, character for character: from its heading's line to the next heading of the same or a higher level, its subsections included, or with own=true to the next heading of any level. With top=true, the text before the first heading. The top and every section with own=true, in order, are the whole page. The ETag is the page's: a part has no version of its own."
    :> Refuses 400 "A parameter that does not do: section with top, own or top other than true or false, section or top without the other's value, a value that is not UTF-8."
    :> Refuses 409 "The page is in an encrypted space: its text is encrypted in its members' browsers, and the server cannot read it (Stale)."
    :> QueryArg "section" Text "The anchor of a section, one of the anchors of the page's contents (api/contents): that section is read instead of the whole page."
    :> QueryArg "own" Bool "With section, true reads the section's own text only, without its subsections. Alone (?own) it is true."
    :> QueryArg "top" Bool "true reads the text before the first heading instead of the whole page; not with section. Alone (?top) it is true."
    :> MultiVerb 'GET '() '[WithHeaders '[Header "ETag" Version, Header "Cache-Control" CacheControl] PageText (RespondMarkdown 200 "The Markdown, as text/markdown, with the page's version as the ETag.")] PageText

type WriteMarkdown =
  Named "writeMarkdown"
    :> Summary "Write a page's Markdown, one part of it, or a new page"
    :> Description "The body becomes the page's whole Markdown, as a save in the editor: a new version in the history under the user's name, the change pushed to everyone with the page open, the same notifications; the title stays. Where there is no page, the PUT creates one, as the wiki's New page does: the address's last slug is its slug, and the slugs before it name the page above it, which has to exist. With section or top, the body takes the place of that part of the page alone, and the rest of the page stays as it is; the server ends the body with a line break when it has none. If-Match with the version from the ETag makes the write conditional: a page saved since is refused, and nothing is written. Without it, the write goes over whatever is there, and a part is written in the page as it is when the request comes."
    :> Refuses 400 "A parameter or a header that does not do, as for the GET; If-Match that is not a version from an ETag; If-None-Match other than *, or any If-None-Match with a part; a body that is not UTF-8; a segment of the address that is not a slug; a title or a kind that a new page cannot have."
    :> Refuses 403 "The user may read the page but not edit it, or may not create a page there."
    :> Refuses 409 "The page, or the page above a new one, is in an encrypted space, whose text the server can neither read nor write (Stale)."
    :> Refuses 412 "The page has been saved since the version in If-Match, or there is a page already and If-None-Match: * asks to create one only where there is none. Nothing is written."
    :> Refuses 422 "A part's body is not that one part, or would change the page outside it: the message says what would change, and nothing is saved. An empty body, but for the top."
    :> QueryArg "section" Text "The anchor of the section to write instead of the whole page."
    :> QueryArg "own" Bool "With section, true writes the section's own text only: the body has no subsections, and the section's subsections stay. Alone (?own) it is true."
    :> QueryArg "top" Bool "true writes the text before the first heading; an empty body takes it out. Alone (?top) it is true."
    :> QueryArg "title" Text "A new page's title; without it, the title is made from the slug (release-notes is titled Release notes). Only for a page the PUT creates."
    :> QueryArg "kind" PageKind "A new page's kind: document (the default) or presentation. Only for a page the PUT creates."
    :> Header' '[Optional, Lenient, Description "The version the write is made over, as the ETag gave it: a page saved since is refused (412). Never creates a page."] "If-Match" Version
    :> Header' '[Optional, Lenient, Description "* creates the page only where there is none (412 where there is one). Not with a part."] "If-None-Match" Text
    :> Body "text/markdown" Text "The Markdown, in UTF-8, whatever the Content-Type says."
    :> MultiVerb 'PUT '() WriteAnswers MarkdownWrite

-- | The Markdown reference.
type Reference =
  "reference"
    :> ( SignIn 'WhenClosed :> ReadReference
           :<|> OtherMethods '[ 'GET, 'HEAD]
       )

type ReadReference =
  Named "readReference"
    :> Summary "Read the wiki's Markdown reference"
    :> Description "What the writing assistant is told about the wiki's Markdown: the same text its prompt starts with. Anyone may read it, unless the wiki is closed to anyone who has not signed in."
    :> MultiVerb 'GET '() '[WithHeaders '[Header "Cache-Control" CacheControl] (Text, CacheControl) (RespondMarkdown 200 "The reference, as text/markdown.")] (Text, CacheControl)

-- | The address of a page's table of contents.
type ContentsAddress =
  "s"
    :> Refuses 404 "There is no page at the address, or one the user may not read: the same answer for both."
    :> AsSent "address" "The page's address in the browser after /s/, as for its Markdown: team/projects/p/the-wiki/, or team/ for the space's home page."
    :> CaptureAll "address" Text
    :> SignIn 'Always
    :> (ReadContents :<|> OtherMethods '[ 'GET, 'HEAD])

type ReadContents =
  Named "readContents"
    :> Summary "Read a page's table of contents"
    :> Description "The page's title, its version (the value of the Markdown's ETag, quotes included, to send as If-Match), the address of its Markdown, the address of its top when it has text before its first heading, and its headings in the page's order, nested by level, each with its anchor and the address of its section. The headings are those of the page's Contents list; a heading inside a block spoiler is listed under the heading whose section holds the spoiler, with inSpoiler true."
    :> Refuses 409 "The page is in an encrypted space, whose text the server cannot read (Stale)."
    :> MultiVerb 'GET '() '[WithHeaders '[Header "ETag" Version, Header "Cache-Control" CacheControl] ContentsAnswer (RespondAs Json 200 "The table of contents, with the page's version as the ETag." Contents)] ContentsAnswer

-- | The address of the listing of the top level.
type ListTopAddress =
  SignIn 'Always
    :> ( ListTop
           :<|> OtherMethods '[ 'GET, 'HEAD]
       )

type ListTop =
  Named "listSpaces"
    :> Summary "List every space the user may read"
    :> Description "Every space the user may read, from the top level down, each with its subspaces and the pages under its home page. A space above one the user may read is listed with its title, its path and its subspaces only. An encrypted space is listed with encrypted true and no pages."
    :> Refuses 400 "depth is not a number from 1 up."
    :> QueryArg "depth" Int "How many levels down the tree goes: 1 is the top level's spaces with neither their subspaces nor their pages. Without it, the whole tree. Where the tree is cut, subspaces, pages and children are left out."
    :> MultiVerb 'GET '() '[WithHeaders '[Header "Cache-Control" CacheControl] (TopLevel, CacheControl) (RespondAs Json 200 "The spaces of the top level." TopLevel)] (TopLevel, CacheControl)

-- | The address of a space's listing.
type ListSpaceAddress =
  "s"
    :> Refuses 404 "There is no space at the path, or one the user may not read: the same answer for both. For a PUT: no space above the path."
    :> AsSent "space" "The space's path from the top level, its slugs separated by slashes and ending in a slash: team/projects/."
    :> CaptureAll "space" Text
    :> SignIn 'Always
    :> (ListSpace :<|> CreateSpace :<|> OtherMethods '[ 'GET, 'HEAD, 'PUT])

type ListSpace =
  Named "listSpace"
    :> Summary "List a space"
    :> Description "One space, with its subspaces and the pages under its home page, as trees."
    :> Refuses 400 "depth is not a number from 1 up."
    :> QueryArg "depth" Int "How many levels down the tree goes: 1 is the space's subspaces and the pages under its home page, without what is under them. Without it, the whole tree."
    :> MultiVerb 'GET '() '[WithHeaders '[Header "Cache-Control" CacheControl] (SpaceEntry, CacheControl) (RespondAs Json 200 "The space." SpaceEntry)] (SpaceEntry, CacheControl)

type CreateSpace =
  Named "createSpace"
    :> Summary "Create a space"
    :> Description "Creates the space at the path, as the wiki's New space does: a private space with its home page, and the user its administrator. The path's last slug is the new space's slug, and the slugs before it name the space it goes under, which has to exist. A PUT here only creates: it never changes a space."
    :> Refuses 400 "The body is not a space's JSON, it asks for an encrypted space (made in the browser only), or a segment of the path is not a slug."
    :> Refuses 403 "The user may not create a space there."
    :> Refuses 409 "There is a space at the path already, or the space above it is encrypted."
    :> Body "application/json" NewSpace "The new space's title, or an empty body: the title is then made from the slug. A field of another name is refused."
    :> MultiVerb 'PUT '() '[WithHeaders '[Header "Cache-Control" CacheControl, Header "Location" Text] SpaceCreated (RespondAs Json 201 "The space created: its listing, whose address is the Location." SpaceEntry)] SpaceCreated

-- | The address of the search.
type SearchAddress =
  SignIn 'Always
    :> ( Search
           :<|> OtherMethods '[ 'GET, 'HEAD]
       )

type Search =
  Named "search"
    :> Summary "Search the pages the user may read"
    :> Description "The wiki's own search, the one behind its search box, with the same rules: what the user may read, and no encrypted space. The hits come best first, each with the page's title, its space, the address of its Markdown and an excerpt of its text around the first match, as plain text and as marked pieces."
    :> Refuses 400 "No words to search for, a space that is not a space's path, or a limit that is not a number."
    :> QueryArg "q" Text "The words to search for."
    :> QueryArg "space" Text "A space's path (team/projects): the search keeps to that space and its subspaces. A space the user may not read finds nothing."
    :> QueryArg "limit" Int "How many hits: 50 by default, 200 at most."
    :> MultiVerb 'GET '() '[WithHeaders '[Header "Cache-Control" CacheControl] (Hits, CacheControl) (RespondAs Json 200 "The hits." Hits)] (Hits, CacheControl)

-- | The address of this document.
type DocumentAddress =
  SignIn 'WhenClosed
    :> ( ReadDocument
           :<|> OtherMethods '[ 'GET, 'HEAD]
       )

type ReadDocument =
  Named "readDescription"
    :> Summary "Read this description of the API"
    :> Description "This OpenAPI document, made from the server's own type of the API, so that it says what the server does. Anyone may read it, unless the wiki is closed to anyone who has not signed in."
    :> MultiVerb 'GET '() '[WithHeaders '[Header "Cache-Control" CacheControl] (OpenApiDocument, CacheControl) (RespondAs Json 200 "The OpenAPI 3 document." OpenApiDocument)] (OpenApiDocument, CacheControl)

-- | 'OpenApiDocument' is the OpenAPI document as an answer.
newtype OpenApiDocument = OpenApiDocument OpenApi
  deriving stock (Eq, Show)

instance ToJSON OpenApiDocument where
  toJSON (OpenApiDocument d) = toJSON d
  toEncoding (OpenApiDocument d) = toEncoding d

instance ToSchema OpenApiDocument where
  declareNamedSchema _ =
    pure . NamedSchema (Just "OpenApiDocument") $
      mempty
        & O.type_ ?~ OpenApiObject
        & O.required .~ ["openapi", "info", "paths"]
        & O.additionalProperties ?~ O.AdditionalPropertiesAllowed True
        & O.description ?~ "An OpenAPI 3.0 document: https://spec.openapis.org/oas/v3.0.3"

-- | The OpenAPI document of 'ProgramApi', on a listener that serves the
-- wiki under this path (with a slash at each end): its addresses are under
-- that path, which the document gives as its server.
programDocument :: Text -> OpenApi
programDocument mount =
  toOpenApi (Proxy @ProgramApi)
    & O.info . O.title .~ "The wiki's API for programs"
    & O.info . O.version .~ "1"
    & O.info . O.description ?~ about
    & O.servers .~ [O.Server (T.dropWhileEnd (== '/') mount) Nothing mempty | mount /= "/"]
  where
    about =
      T.unwords
        [ "What a program on the user's own machine (a local assistant, a script) reads and writes in the wiki: a page's Markdown, whole or a section at a time, its table of contents, the listing of the spaces and their pages, and the search."
        , "Every request signs in with an API key of the user's (Authorization: Bearer, or X-API-Key) or with the user's login and password (HTTP Basic), and acts as the user: the spaces' permissions apply as they are."
        , "A key's right narrows what the user may do: read allows GET and HEAD, write the rest too."
        , "The session's cookie is never read here."
        , "Every refusal is JSON, {\"error\": name, \"message\": why}."
        , "The wiki's MCP server, for assistants, is at /mcp (POST, JSON-RPC over HTTP, signed in by a key as Bearer); it is not described here, as it is no REST API."
        ]

-- | The server of 'ProgramApi', on a listener that serves the wiki under
-- this path, for a request from this address.
programServer :: Env -> Text -> Text -> Server ProgramApi
programServer env mount address =
  (markdown ("s" :) :<|> (const readReference :<|> pass) :<|> markdown identity)
    :<|> (contents :<|> contents)
    :<|> ((\u -> listTop env mount address u :<|> pass) :<|> spaces ("s" :) :<|> spaces identity)
    :<|> (\u -> search env mount address u :<|> pass)
    :<|> const (pure (OpenApiDocument (programDocument mount), CacheControl "no-cache") :<|> pass)
  where
    -- The handlers of an address, given the segments the capture took,
    -- written as the segments after the API's prefix ('notASlug' reads
    -- them).
    markdown written sent segments u =
      readMarkdown env mount address u sent
        :<|> writeMarkdown env mount address u sent (written segments)
        :<|> refuseOtherMethods mount sent
    contents sent _ u = readContents env mount address u sent :<|> pass
    spaces written sent segments u =
      listSpace env mount address u sent
        :<|> createSpace env mount address u sent (written segments)
        :<|> pass

-- | The API for programs as a WAI application, on a listener that serves
-- the wiki under this path (with a slash at each end), for a request the
-- listener knows this of ('Asking'), whose path is given from @api@ on.
-- An address under @api\/markdown@, @api\/contents@ or @api\/list@ that
-- names nothing is answered by the handlers of the addresses beside it
-- ('Undocumented'): signed in, then 404. Servant's own refusal of an
-- address, should one reach it, is the API's 404; every other refusal is
-- the handlers' or the sign-in's.
programApplication :: Env -> Text -> Asking -> Application
programApplication env mount asking = case everySignedIn of
  Holds -> serveWithContext (Proxy @ProgramApi) context (programServer env mount asking.address)
  where
    context =
      SignInCheck @'Always (signedInProgram env asking)
        :. SignInCheck @'WhenClosed whenClosed
        :. defaultErrorFormatters {notFoundErrorFormatter = const (refusalError (refusal status404 "NotFound" Nothing))}
        :. EmptyContext
    -- Anyone, unless the wiki is closed: then the request signs in as
    -- every other one does.
    whenClosed request = do
      closed <- (\s -> s.access == Closed) <$> readSettings env.storage
      if closed then fmap Just <$> signedInProgram env asking request else pure (Right Nothing)

-- | Every endpoint of 'ProgramApi' is under 'SignIn': this does not
-- compile otherwise.
everySignedIn :: Holds (EverySignedIn ProgramApi)
everySignedIn = Holds
