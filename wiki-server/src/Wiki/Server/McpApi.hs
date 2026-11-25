-- | The wiki's MCP server (WP5.138 of docs/PLAN.md, the owner's issue
-- #338): the Model Context Protocol at @mcp@ on the public listener, so
-- that an assistant such as Claude Code or Codex reads and writes the
-- wiki's pages through tools, signed in with the user's API key.
--
-- The protocol is Hackage's @mcp-server@: its Streamable HTTP transport,
-- a WAI application ('Mcp.mcpApplication'), answers both of the
-- protocol's eras, the revision of 2026-07-28 and the older @initialize@
-- handshake. The wiki writes only its tools. Each tool calls the function
-- the HTTP API's handler calls for the same thing ('findPage',
-- 'putMarkdown', 'listing', 'searching', 'contentsOf', 'sectionOf',
-- 'putSection', 'attachmentList'), as the key's user ('runAs'): the spaces' permissions hold
-- as in the browser, and a page the user may not read is not found.
--
-- = Signing in
--
-- With an API key only, as @Authorization: Bearer \<key>@, through the
-- same check as the HTTP API's ('signInWithKey'): its expiry, its
-- revocation, its user's, the limit on failed keys, the time of its last
-- use, and the request's log line, which names the key's user and the
-- key's id. A request without a key is 401 and is not counted as a failed
-- key, as on the HTTP API. The sign-in comes before the library's own
-- checks, since the library's callback for a key is given the key alone:
-- not the request's address, which the limit counts by, nor its log line.
-- So the application is put together for each request, around the key's
-- user; what it keeps between requests, the key that seals the state of a
-- request of several round trips, is made once at the server's start
-- ('Env''s @mcpState@).
--
-- A browser's page is refused whatever its origin (the library's list of
-- allowed origins is empty): a program sends no @Origin@, and no page of a
-- browser has any business here. The session's cookie is never read.
--
-- = Rights
--
-- A key with the right @read@ is listed the tools that read only, and a
-- tool that writes, called with it all the same, is refused and writes
-- nothing. Keys with the rights @write@ and @admin@ are listed every tool.
--
-- = Answers
--
-- A failure a model can act on is a tool's result marked @isError@, with a
-- sentence that says what to do: a page that is not there, one the user
-- may not write, an encrypted space, a path or a version that is not one,
-- a page saved since the version a write names, and a section's body that
-- would change the page outside the section (with the HTTP API's
-- sentence). A JSON-RPC error is left for a request that is not a tool's
-- call at all: an unknown tool, or a request the library cannot read. Every result is at most
-- 'maxResult' characters, or says how to ask for less.
module Wiki.Server.McpApi
  ( mcpApplication
  , instructions
  , maxResult
  )
where

import Data.Aeson (Result (..), ToJSON (..), Value (..), encode, fromJSON, object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Halogen.Router.Duplex qualified as Duplex
import MCP.Server qualified as Mcp
import Network.HTTP.Types (status400, status401)
import Network.URI (URI (..), parseAbsoluteURI, parseRelativeReference, unEscapeString)
import Network.Wai (Application, Request (..))
import Protolude
import Web.HttpApiData (parseHeader, parseUrlPiece, toHeader)
import Wiki.Api.ApiKeys (KeyRight (..), rightName)
import Wiki.Api.Version (PageVersion (..), VersionTime (..), pageVersion)
import Wiki.App.Backend (WikiError (..))
import Wiki.App.Route qualified as Route
import Wiki.Core.Assistant.Reference (markdownReference)
import Wiki.Core.Markdown.Sections (Outline (..), Section (..), Target (..), outline, outlineAnchors, sectionRange, sectionText)
import Wiki.Core.Page (Page (..))
import Wiki.Core.PageKind (PageKind (..))
import Wiki.Core.Path (renderPath)
import Wiki.Core.Slug (Slug)
import Wiki.Server.Address (Origin, originOfUri)
import Wiki.Server.Api (Env (..))
import Wiki.Server.ApiKeys (KeyHolder (..))
import Wiki.Server.AttachmentsApi (AttachmentList (..), attachmentList)
import Wiki.Server.ContentsApi (SectionPut (..), contentsOf, putSection, sectionOf)
import Wiki.Server.ContentsApi qualified as Contents
import Wiki.Server.ListApi (Addresses (..), HitEntry (..), HitSpace (..), TopLevel (..), hasSearchWords, listing, searchSpace, searching)
import Wiki.Server.MarkdownApi (Asking (..), Credential (..), Put (..), credentialOf, findPage, notASlug, putMarkdown, runAs, signInWithKey)
import Wiki.Server.ProgramApi.Combinators (refusal, refusalResponse, refusalWith)
import Wiki.Server.Render (routeOf)

-- | The MCP server, on a listener that serves the wiki under this path
-- (with a slash at each end), for a request the listener knows this of
-- ('Asking'), whose own origins are these ('Wiki.Server.Address.ownOrigins'):
-- a full address in a tool's argument names a page only at one of them.
-- The listener routes @mcp@ under its path here.
mcpApplication :: Env -> Text -> [Origin] -> Asking -> Application
mcpApplication env mount own asking request respond = case bearerOf request of
  Left why -> respond (refusalResponse (refusal status400 "Invalid" (Just why)))
  Right Nothing -> respond (refusalResponse noKey)
  Right (Just presented) ->
    signInWithKey env asking presented >>= \case
      Left refused -> respond (refusalResponse refused)
      Right holder -> do
        app <- Mcp.mcpApplication config serverInfo Mcp.noHandlers {Mcp.tools = Just (toolHandlers env mount own asking.address holder)}
        app request respond
  where
    config =
      Mcp.defaultHttpConfig
        { Mcp.httpEndpoint = toS (mount <> "mcp")
        , Mcp.httpAllowedOrigins = Just []
        , Mcp.httpStateCodec = Just env.mcpState
        }
    serverInfo = Mcp.McpServerInfo {Mcp.serverName = "wiki", Mcp.serverVersion = env.version, Mcp.serverInstructions = instructions}
    noKey =
      refusalWith status401 [("WWW-Authenticate", "Bearer realm=\"wiki\"")] "Unauthenticated" $
        Just "The wiki's MCP server signs in with an API key of the wiki only, sent as Authorization: Bearer <key>."

-- | The key a request presents as @Authorization: Bearer \<key>@, read by
-- the HTTP API's own 'credentialOf'; or why the request's credentials
-- cannot be read (two @Authorization@ headers). 'Nothing' for a request
-- without such a key: one with none, or with a login and a password. A
-- key in @X-API-Key@, which the HTTP API also takes, is not taken here,
-- and a request with that header presents no key.
bearerOf :: Request -> Either Text (Maybe Text)
bearerOf request
  | any ((== "X-API-Key") . fst) (requestHeaders request) = Right Nothing
  | otherwise =
      credentialOf request <&> \case
        Key presented -> Just presented
        _ -> Nothing

-- | What the server tells an assistant when it connects, before any tool:
-- what the wiki is, how a page is named, and how to read and write. Claude
-- Code keeps the first 4,096 characters of it.
instructions :: Text
instructions =
  T.unlines
    [ "This is a wiki: spaces, which may have subspaces, hold pages written in Markdown, the wiki's own dialect with math, diagrams, boxes, spoilers and links between pages. The tools act as the user whose API key signed them in, and see only what that user may read."
    , ""
    , "A page is named by its path, the one the browser's address bar shows: /s/<space path>/p/<page path>/, such as /s/team/p/on-call/. A subspace's path follows its space's (/s/team/projects/p/roadmap/), and a page under another page adds its own slug (/s/team/p/on-call/rota/). A space's home page is /s/<space path>/. A full address of the wiki, https://wiki.example/s/team/p/on-call/, is taken too. Find pages with wiki_list and wiki_search."
    , ""
    , "Read before you write, and pass the version. wiki_read_page gives a page's Markdown and its version. wiki_write_page replaces the whole page, and takes base_version, the version you read: if the page has been saved since, nothing is written, and you read it again, merge your change into what it says now, and write with its new version. For a long page, read its outline (wiki_page_outline), then one section at a time (wiki_read_section), and the text before its first heading with top true. To change one section, write that section alone with wiki_write_section, from its heading on, over the version read: nothing outside it can change."
    , ""
    , "Look a construct of the wiki's Markdown up in wiki_markdown_reference before using it: a box, math, a diagram, a spoiler, a link to another page, a presentation. Called without a section, it lists the reference's sections; with one, it gives that section."
    , ""
    , "A page's files are listed by wiki_list_attachments, each with the Markdown that shows it in a page."
    , ""
    , "Every write is a version in the page's history, under the user's name: a change can be seen, compared and undone in the browser."
    ]

-- | The most characters a tool's result holds. Claude Code keeps a
-- longer result in a file rather than in the conversation.
maxResult :: Int
maxResult = 50000

-- | 'Tool' is one of the server's tools: its name, what it does, its
-- arguments with the names of the required ones, whether it writes, what
-- it says when its result would be too long (given the length), and what
-- it does with the arguments of a call.
data Tool = Tool
  { name :: Text
  , description :: Text
  , arguments :: [(Text, Mcp.Schema)]
  , required :: [Text]
  , writes :: Bool
  , tooLong :: Int -> Text
  , run :: Arguments -> IO Mcp.ToolResult
  }

-- | The arguments of a tool's call, by name, as JSON.
type Arguments = Map Text Value

-- | The tools, for a key's user, for a request from this address, on a
-- listener that serves the wiki under this path with these origins: what
-- @tools\/list@ lists
-- and what @tools\/call@ calls. A key with the right @read@ is listed the
-- tools that only read, and is refused the others.
toolHandlers :: Env -> Text -> [Origin] -> Text -> KeyHolder -> (Mcp.ToolListHandler, Mcp.ToolCallHandler)
toolHandlers env mount own address holder = (listed, call)
  where
    -- @write@ and @admin@ write; @read@ only reads.
    mayWrite = holder.right >= WriteRight
    available = [t | t <- tools env mount own address holder, mayWrite || not t.writes]
    listed _ = pure (map definition available)
    call _ named args = case find (\t -> t.name == named) (tools env mount own address holder) of
      Nothing -> pure (Left (Mcp.UnknownTool named))
      Just t -> Right . Mcp.Complete <$> answer t args
    answer t args
      | t.writes && not mayWrite =
          pure (Mcp.toolError ("This key has the right " <> rightName holder.right <> ", which only reads: " <> t.name <> " needs a key with the right write. Nothing was written."))
      | unknown : _ <- filter (`notElem` map fst t.arguments) (Map.keys args) =
          pure (Mcp.toolError ("The tool " <> t.name <> " has no argument " <> unknown <> ". Its arguments are " <> T.intercalate ", " (map fst t.arguments) <> "."))
      | otherwise = bounded t <$> t.run args
    -- A result longer than 'maxResult' is replaced by the tool's
    -- sentence on how to ask for less.
    bounded t result
      | size > maxResult = Mcp.toolError (t.tooLong size)
      | otherwise = result
      where
        size = sum [T.length s | Mcp.ContentText s <- Mcp.toolResultContent result]

-- | A tool's definition, as @tools\/list@ gives it, with the hints a
-- client shows: whether it only reads, and, for a tool that writes, that
-- it may change what is there.
definition :: Tool -> Mcp.ToolDefinition
definition t =
  (Mcp.mkToolDefinition t.name t.description (Mcp.schema (Mcp.SchemaObject t.arguments t.required)))
    { Mcp.toolDefinitionAnnotations =
        Just
          Mcp.defaultToolAnnotations
            { Mcp.toolReadOnlyHint = Just (not t.writes)
            , Mcp.toolDestructiveHint = guard t.writes $> True
            , Mcp.toolOpenWorldHint = Just False
            }
    }

-- | Every tool, for a key's user, for a request from this address, on a
-- listener that serves the wiki under this path with these origins.
tools :: Env -> Text -> [Origin] -> Text -> KeyHolder -> [Tool]
tools env mount own address holder =
  [ Tool
      { name = "wiki_list"
      , description = "List the spaces and pages the user may read, as a tree: each space with its title, its path, its subspaces and its pages, each page with its title, its kind and its path in the field markdown, to pass to the other tools as path. A space's home page is its field home. Without space, the whole wiki; with it, one space. depth cuts the tree: 1 is the space's own subspaces and top pages."
      , arguments =
          [ ("space", Mcp.describedSchema "The space to list, by its path such as team/projects (a listed space's field path). Leave it out for every space." (Mcp.SchemaString Nothing))
          , ("depth", Mcp.describedSchema "How many levels to list, from 1 up. Leave it out for all of them." Mcp.SchemaInteger)
          ]
      , required = []
      , writes = False
      , tooLong = \n -> "The tree has " <> show n <> " characters, more than one answer holds: list one space (space) or fewer levels (depth), such as depth 1, and go down from there."
      , run = \args -> withArguments ((,) <$> (textArgument "space" args >>= searchSpace) <*> (intArgument "depth" args >>= depthOf)) $ \(space, depth) ->
          runAs env address holder.user (runExceptT (listing toolAddresses depth space)) <&> \case
            Left NotFound -> Mcp.toolError ("There is no space at " <> maybe "" renderPath space <> " that the user may read: see the spaces with wiki_list without space.")
            Left e -> Mcp.toolError (refusalText (maybe "" renderPath space) e)
            Right (Left spaces) -> jsonResult (TopLevel spaces)
            Right (Right entry) -> jsonResult entry
      }
  , Tool
      { name = "wiki_search"
      , description = "Search the pages the user may read with the wiki's own search, the one behind its search box: the hits best first, each with the page's title, its space (path and title), its path to pass to the other tools, and an excerpt with the words found."
      , arguments =
          [ ("query", Mcp.describedSchema "The words to search for." (Mcp.SchemaString Nothing))
          , ("space", Mcp.describedSchema "Keep to this space and its subspaces, by its path such as team/projects." (Mcp.SchemaString Nothing))
          , ("limit", Mcp.describedSchema "How many hits at most: 50 when left out, 200 at most." Mcp.SchemaInteger)
          ]
      , required = ["query"]
      , writes = False
      , tooLong = \n -> "The hits have " <> show n <> " characters, more than one answer holds: ask for fewer (limit), or keep to a space (space)."
      , run = \args -> withArguments ((,,) <$> (requiredArgument "query" (textArgument "query" args) >>= words') <*> (textArgument "space" args >>= searchSpace) <*> intArgument "limit" args) $ \(q, within, limit) ->
          jsonResult . object . pure . ("hits" .=) . map hit <$> runAs env address holder.user (searching toolAddresses q within limit)
      }
  , Tool
      { name = "wiki_read_page"
      , description = "Read a page: its Markdown, its title, and its version, to pass as base_version when you write it. A long page is read by its outline (wiki_page_outline) and its sections (wiki_read_section)."
      , arguments = [pathArgument "The page's path, such as /s/team/p/on-call/."]
      , required = ["path"]
      , writes = False
      , tooLong = \n -> "The page has more than one answer holds (" <> show n <> " characters with its title and version): read its outline with wiki_page_outline, then its sections one at a time with wiki_read_section, and the text before its first heading with wiki_read_section and top true."
      , run = \args -> withPage args $ \at sp pp ->
          findPage env address holder.user sp pp <&> \case
            Left e -> Mcp.toolError (refusalText at e)
            Right p -> withText (object ["path" .= at, "title" .= p.title, "version" .= versionOf p]) p.body
      }
  , Tool
      { name = "wiki_page_outline"
      , description = "Read a page's outline: its title, its version, topCharacters, the length of the text before its first heading (0 when there is none; read it with wiki_read_section and top true), and its headings as a tree, each with its level, its text, its anchor (the name of its section, for wiki_read_section) and whether it is inside a spoiler."
      , arguments = [pathArgument "The page's path, such as /s/team/p/on-call/."]
      , required = ["path"]
      , writes = False
      , tooLong = \n -> "The page's outline has " <> show n <> " characters, more than one answer holds: read the page's sections by their anchors with wiki_read_section."
      , run = \args -> withPage args $ \at sp pp ->
          findPage env address holder.user sp pp <&> \case
            Left e -> Mcp.toolError (refusalText at e)
            Right p -> jsonResult (withTop p (toJSON (contentsOf Nothing p)))
      }
  , Tool
      { name = "wiki_read_section"
      , description = "Read one part of a page: a section by its anchor, its Markdown from its heading to the next heading of the same or a higher level, its subsections included, or with own true only to the next heading of any level; or, with top true instead of section, the text before the first heading. Also the page's version, to pass as base_version when you write the section (wiki_write_section) or the page."
      , arguments =
          [ pathArgument "The page's path, such as /s/team/p/on-call/."
          , ("section", Mcp.describedSchema "The section's anchor, from the page's outline (wiki_page_outline). Give section or top, not both." (Mcp.SchemaString Nothing))
          , ("own", Mcp.describedSchema "True for the section's own text only, without its subsections." Mcp.SchemaBoolean)
          , ("top", Mcp.describedSchema "True for the text before the first heading, in place of section." Mcp.SchemaBoolean)
          ]
      , required = ["path"]
      , writes = False
      , tooLong = \n -> "The part has " <> show n <> " characters, more than one answer holds: for a section, read its own text (own true), then its subsections, whose anchors the outline (wiki_page_outline) gives. A section's own text and the text before the first heading are not read in smaller parts."
      , run = \args -> withArguments (partArgument args) $ \(asked, ownOnly) ->
          withPage args $ \at sp pp ->
            findPage env address holder.user sp pp <&> \case
              Left e -> Mcp.toolError (refusalText at e)
              Right p -> case sectionOf p.body asked ownOnly of
                Nothing -> noSection at (fromMaybe "" asked) (outline p.body)
                Just part -> withText (object (["path" .= at, "title" .= p.title] <> maybe ["top" .= True] (\anchor -> ["section" .= anchor]) asked <> ["version" .= versionOf p])) part
      }
  , Tool
      { name = "wiki_write_page"
      , description = "Replace a page's whole Markdown, as a save in the editor does: a new version in the page's history under the user's name, shown at once to whoever has the page open. base_version is the version you read (wiki_read_page, wiki_page_outline or wiki_read_section): if the page has been saved since, nothing is written, and you read it again, merge, and write with its new version. The title stays. Look a construct of the wiki's Markdown up in wiki_markdown_reference before using it. Every write can be seen and undone in the page's history. The result is the page's new version."
      , arguments =
          [ pathArgument "The page's path, such as /s/team/p/on-call/."
          , ("markdown", Mcp.describedSchema "The page's whole new Markdown." (Mcp.SchemaString Nothing))
          , ("base_version", Mcp.describedSchema "The version you read the page at, as the reading tools give it." (Mcp.SchemaString Nothing))
          ]
      , required = ["path", "markdown", "base_version"]
      , writes = True
      , tooLong = const "The answer is too long."
      , run = \args -> withArguments ((,) <$> requiredArgument "markdown" (textArgument "markdown" args) <*> (requiredArgument "base_version" (textArgument "base_version" args) >>= versionArgument)) $ \(body, base) ->
          withPage args $ \at sp pp ->
            putMarkdown env address holder.user sp pp (Just base) False (Right (Nothing, Document)) body <&> \case
              Saved p -> jsonResult (object ["path" .= at, "version" .= versionOf p])
              Created p -> jsonResult (object ["path" .= at, "version" .= versionOf p])
              Moved -> Mcp.toolError ("The page at " <> at <> " has been saved since the version " <> decodeUtf8 (toHeader base) <> ", and nothing was written. Read it again with wiki_read_page, merge your change into what it says now, and write with its new version.")
              NoPage -> Mcp.toolError ("There is no page at " <> at <> ": create it with wiki_create_page.")
              Exists -> Mcp.toolError ("There is a page at " <> at <> " already.")
              NoPageAbove above -> Mcp.toolError ("The page above it, " <> pathOf sp above <> ", is not there.")
              NewPageInvalid why -> Mcp.toolError why
              PutRefused e -> Mcp.toolError (refusalText at e)
      }
  , Tool
      { name = "wiki_write_section"
      , description = "Replace one part of a page: a section by its anchor, or with top true the text before the first heading. Prefer it to wiki_write_page for a change to one section of a long page: it sends fewer tokens, and the other sections cannot be damaged. markdown is the whole section, starting with its heading at the same level as the section's (it may change the heading's text), its subsections included, or with own true without them; for top, the text before the first heading, with no heading. A write that would change anything outside the part is refused, and nothing is written: the error says why and what to do. base_version is the version you read (wiki_read_section or wiki_page_outline): if the page has been saved since, nothing is written, and you read the section again and write with the page's new version. The result is the section's anchor after the write and the page's new version."
      , arguments =
          [ pathArgument "The page's path, such as /s/team/p/on-call/."
          , ("section", Mcp.describedSchema "The section's anchor, from the page's outline (wiki_page_outline). Give section or top, not both." (Mcp.SchemaString Nothing))
          , ("own", Mcp.describedSchema "True to write the section's own text only, its subsections staying as they are: markdown then has no subsections." Mcp.SchemaBoolean)
          , ("top", Mcp.describedSchema "True to write the text before the first heading, in place of section." Mcp.SchemaBoolean)
          , ("markdown", Mcp.describedSchema "The part's whole new Markdown: for a section, from its heading on." (Mcp.SchemaString Nothing))
          , ("base_version", Mcp.describedSchema "The version you read the page at, as the reading tools give it." (Mcp.SchemaString Nothing))
          ]
      , required = ["path", "markdown", "base_version"]
      , writes = True
      , tooLong = const "The answer is too long."
      , run = \args -> withArguments ((,,) <$> partArgument args <*> requiredArgument "markdown" (textArgument "markdown" args) <*> (requiredArgument "base_version" (textArgument "base_version" args) >>= versionArgument)) $ \((asked, ownOnly), body, base) ->
          withPage args $ \at sp pp ->
            putSection env address holder.user sp pp (maybe Top (if ownOnly then OwnPart else WholeSection) asked) (Just base) body <&> \case
              SectionSaved anchor p -> jsonResult (object (["path" .= at] <> maybe ["top" .= True] (const ["section" .= anchor]) asked <> ["version" .= versionOf p]))
              SectionMoved -> Mcp.toolError ("The page at " <> at <> " has been saved since the version " <> decodeUtf8 (toHeader base) <> ", and nothing was written. Read the section again with wiki_read_section, write your change into what it says now, and write with the page's new version.")
              SectionNotThere anchor o -> noSection at anchor o
              SectionRefused why -> Mcp.toolError (Contents.refusalText why <> " Nothing was written.")
              SectionPutRefused e -> Mcp.toolError (refusalText at e)
      }
  , Tool
      { name = "wiki_create_page"
      , description = "Create a page at a path where there is none, with its Markdown, as the browser's \"New page\" does: its first version under the user's name. The path's last slug is the new page's slug, and the page above it, named by the slugs before, has to exist; nothing is created on the way. Without a title, the title is made from the slug. A page already at the path is not written over. Look a construct of the wiki's Markdown up in wiki_markdown_reference before using it. The result is the page's path and its version."
      , arguments =
          [ pathArgument "The new page's path, such as /s/team/p/on-call/rota/: its space, the page above it, and its own slug (lowercase letters, digits and dashes)."
          , ("markdown", Mcp.describedSchema "The new page's Markdown." (Mcp.SchemaString Nothing))
          , ("title", Mcp.describedSchema "The new page's title. Leave it out to make it from the slug." (Mcp.SchemaString Nothing))
          ]
      , required = ["path", "markdown"]
      , writes = True
      , tooLong = const "The answer is too long."
      , run = \args -> withArguments ((,) <$> requiredArgument "markdown" (textArgument "markdown" args) <*> textArgument "title" args) $ \(body, title) ->
          withPage args $ \at sp pp ->
            putMarkdown env address holder.user sp pp Nothing True (Right (title, Document)) body <&> \case
              Created p -> jsonResult (object ["path" .= at, "version" .= versionOf p])
              Saved p -> jsonResult (object ["path" .= at, "version" .= versionOf p])
              Exists -> Mcp.toolError ("There is a page at " <> at <> " already, and nothing was written: read it with wiki_read_page and write it with wiki_write_page, or create the page at another path.")
              NoPageAbove above -> Mcp.toolError ("The page above it, " <> pathOf sp above <> ", is not there: create that page first.")
              NoPage -> Mcp.toolError ("There is no page at " <> at <> ".")
              Moved -> Mcp.toolError ("The page at " <> at <> " has been saved meanwhile.")
              NewPageInvalid why -> Mcp.toolError why
              PutRefused NotFound -> Mcp.toolError ("There is no space at " <> pathOf sp [] <> " that the user may read, or the path names a space's home page, which every space has: see the spaces with wiki_list.")
              PutRefused e -> Mcp.toolError (refusalText at e)
      }
  , Tool
      { name = "wiki_list_attachments"
      , description = "List the files attached to a page, in the page's order: each with its name, its size in bytes, its media type, its version, address (where the wiki's HTTP API serves its bytes) and markdown, the Markdown that shows it in a page (an image, a video, a sound) or links to it (any other file), to write into the page's text as it is. The bytes are not given here: fetch them with GET at address on the wiki's origin, with the same API key as Authorization: Bearer, or with the shell client: wiki.sh attachment PAGE NAME."
      , arguments = [pathArgument "The page's path, such as /s/team/p/on-call/."]
      , required = ["path"]
      , writes = False
      , tooLong = \n -> "The page's list of files has " <> show n <> " characters, more than one answer holds: read it through the HTTP API, GET /api/attachments/ followed by the page's path."
      , run = \args -> withPage args $ \at sp pp ->
          findPage env address holder.user sp pp <&> \case
            Left e -> Mcp.toolError (refusalText at e)
            Right p -> jsonResult (object ["path" .= at, "attachments" .= (attachmentList mount (Route.pageRoute sp pp) p.attachments).attachments])
      }
  , Tool
      { name = "wiki_markdown_reference"
      , description = "The reference of the wiki's Markdown: how to write its boxes, math, diagrams, spoilers, links between pages, presentations and the rest. Without section, its sections, each with its anchor and a line on what it covers; with section, the whole of that section."
      , arguments = [("section", Mcp.describedSchema "The section's anchor (such as notes-and-boxes) or its heading." (Mcp.SchemaString Nothing))]
      , required = []
      , writes = False
      , tooLong = \n -> "The section has " <> show n <> " characters, more than one answer holds."
      , run = \args -> withArguments (textArgument "section" args) (pure . reference)
      }
  ]
  where
    toolAddresses = Addresses {list = listedSpace, markdown = Duplex.print Route.route}
    -- A space's listing is named by the space's path, as the tools'
    -- argument space takes it.
    listedSpace = \case
      Route.Space sp -> renderPath sp
      r -> Duplex.print Route.route r
    hit :: HitEntry -> Value
    hit h = object ["title" .= h.title, "space" .= object ["path" .= h.space.path, "title" .= h.space.title], "path" .= h.markdown, "excerpt" .= h.excerpt]
    words' q
      | hasSearchWords q = Right q
      | otherwise = Left "Give words to search for in query."
    depthOf = \case
      Just n | n < 1 -> Left "The depth is a number from 1 up."
      d -> Right d
    -- The page a call's argument path names, read as the browser's
    -- router reads it, with the path as the tools write it.
    withPage args continue = withArguments (requiredArgument "path" (textArgument "path" args) >>= localPath mount own) $ \given ->
      routeOf "/" (encodeUtf8 given) >>= \case
        Route.Page sp pp -> continue (pathOf sp pp) sp pp
        Route.Space sp -> continue (pathOf sp []) sp []
        _ -> pure (Mcp.toolError (fromMaybe (show given <> " is not a page's path: a page's path is /s/<space path>/p/<page path>/, such as /s/team/p/on-call/, and a space's home page is /s/<space path>/.") (notASlug (pathSegments given))))

-- | The part of a page that @wiki_read_section@ reads and
-- @wiki_write_section@ writes: a section by its anchor, or the text before
-- the first heading ('Nothing', from @top@), and whether only the
-- section's own text is asked for. Exactly one of @section@ and @top@.
partArgument :: Arguments -> Either Text (Maybe Text, Bool)
partArgument args = do
  anchor <- textArgument "section" args
  top <- fromMaybe False <$> boolArgument "top" args
  ownOnly <- fromMaybe False <$> boolArgument "own" args
  case (anchor, top) of
    (Just a, False) -> Right (Just a, ownOnly)
    (Nothing, True)
      | ownOnly -> Left "own goes with a section: the text before the first heading has no subsections."
      | otherwise -> Right (Nothing, False)
    (Just _, True) -> Left "Give section or top, not both."
    (Nothing, False) -> Left "Give section, a section's anchor from the page's outline, or top true for the text before the first heading."

-- | Why an anchor names no section of the page at a path, whose outline is
-- this: the page's anchors.
noSection :: Text -> Text -> Outline -> Mcp.ToolResult
noSection at anchor o =
  Mcp.toolError ("The page at " <> at <> " has no section with the anchor " <> show anchor <> ". Its anchors: " <> (if null anchors then "none" else T.intercalate ", " anchors) <> ".")
  where
    anchors = outlineAnchors o

-- | A page's outline as the tools give it, with @topCharacters@: how long
-- the text before the first heading is, 0 when it is blank, so that a
-- model knows to read it with @top@.
withTop :: Page -> Value -> Value
withTop p = \case
  Object o -> Object (KeyMap.insert "topCharacters" (toJSON characters) o)
  v -> v
  where
    top = fromMaybe "" (sectionOf p.body Nothing False)
    characters = if T.any (not . isSpace) top then T.length top else 0

-- | The path of the page at a space's path and a page's path, as the
-- browser's address bar shows it under the wiki's own part.
pathOf :: [Slug] -> [Slug] -> Text
pathOf sp pp = Duplex.print Route.route (Route.pageRoute sp pp)

-- | The path a tool's argument @path@ names, as the router reads it
-- (percent-encoded, as sent), with a slash at each end; or why it names no
-- page, which the tool answers without reading or writing anything. The
-- argument is read by network-uri, as "Wiki.Server.Address" reads
-- addresses, and is exactly one of two forms:
--
-- * An absolute address, @http@ or @https@ with a host, whose origin is
--   one of the wiki's own ('ownOrigins', as the live channel checks a
--   page's; an address with a user, @https:\/\/a\@b\/@, has none), with no
--   query and no fragment: its path, under the listener's path, with the
--   listener's path taken off at a segment's end. It is the one form that
--   carries the listener's path.
-- * An absolute path (@\/s\/team\/p\/on-call\/@), as the tools give paths:
--   one leading slash, no scheme, no host, no query, no fragment. It is
--   taken as it is, without the listener's path, whatever it starts with.
--
-- Everything else is refused: an empty text, another scheme, an address
-- with no host, a network-path reference (@\/\/host\/…@, which names a
-- host), a relative path, a query or a fragment (no page's reading address
-- has one), a @.@ or @..@ segment, written plainly or percent-encoded, and
-- what network-uri does not read as a reference at all (spaces,
-- backslashes, control characters).
localPath :: Text -> [Origin] -> Text -> Either Text Text
localPath mount own given
  | T.null t = Left "The path is empty: pass the page's path, such as /s/team/p/on-call/."
  | Just uri <- parseAbsoluteURI (toS t) = absolute uri
  | Just uri <- parseRelativeReference (toS t) = relative uri
  | otherwise = Left (fromMaybe (show t <> " is not a page's path: a page's path is /s/<space path>/p/<page path>/, such as /s/team/p/on-call/.") (notASlug (pathSegments t)))
  where
    t = T.strip given
    absolute uri
      | uri.uriScheme `notElem` ["http:", "https:"] = Left (show t <> " is not an address of the web: pass the page's path, such as /s/team/p/on-call/.")
      | isNothing uri.uriAuthority = Left (show t <> " names no host: pass the page's path, such as /s/team/p/on-call/.")
      | not (maybe False (`elem` own) (originOfUri uri)) = Left (show t <> " is not an address of this wiki: pass the page's path, such as /s/team/p/on-call/.")
      | otherwise = whole uri *> underMount (toS uri.uriPath) >>= plain
    relative uri
      | isJust uri.uriAuthority = Left (show t <> " names a host (//host/…): pass the page's path, such as /s/team/p/on-call/, or the wiki's full address.")
      | not ("/" `T.isPrefixOf` toS uri.uriPath) = Left (show t <> " is not a path from the wiki's root: a page's path starts with /, such as /s/team/p/on-call/.")
      | otherwise = whole uri *> plain (toS uri.uriPath)
    -- No page's reading address has a query or a fragment
    -- ('Wiki.App.Route.Page' and 'Wiki.App.Route.Space' have none).
    whole uri =
      unless (null uri.uriQuery && null uri.uriFragment) $
        Left (show t <> " has a query or a fragment (?… or #…), which no page's address has: pass the page's address without them.")
    -- The path of an absolute address under the listener's path, which ends
    -- with a slash: @\/wiki\/s\/…@ under @\/wiki\/@, never @\/wikis\/…@.
    underMount path
      | mount == "/" = Right path
      | path == T.dropWhileEnd (== '/') mount = Right "/"
      | Just rest <- T.stripPrefix mount path = Right ("/" <> rest)
      | otherwise = Left (show t <> " is not under the wiki's path, " <> mount <> ": pass the page's path, such as /s/team/p/on-call/.")
    -- A path without dot segments, plain or percent-encoded, with a slash
    -- at each end.
    plain path
      | any ((`elem` [".", ".."]) . unEscapeString . T.unpack) (T.splitOn "/" path) =
          Left (show t <> " has a . or .. segment: pass the page's own path, such as /s/team/p/on-call/.")
      | otherwise = Right ("/" <> T.dropAround (== '/') path <> (if T.null (T.dropAround (== '/') path) then "" else "/"))

-- | The segments of a path, without the empty ones.
pathSegments :: Text -> [Text]
pathSegments = filter (not . T.null) . T.splitOn "/"

-- | A page's version, as the tools give it and take it back: the value of
-- the HTTP API's @ETag@, quotes included.
versionOf :: Page -> Text
versionOf p = decodeUtf8 (toHeader (pageVersion p))

-- | The version a write names: the value the tools gave, with its quotes
-- or without them.
versionArgument :: Text -> Either Text PageVersion
versionArgument given =
  first
    (const ("base_version " <> show given <> " is not a page's version: pass the version a reading tool gave, such as \"2026-11-18T09:30:12.345678Z\"."))
    -- The entity tag, quoted, as the HTTP API reads If-Match; or the
    -- version's time without the quotes, as the API reads a version in a
    -- path ('VersionTime').
    (parseHeader (encodeUtf8 t) <> ((\v -> PageVersion v.time) <$> parseUrlPiece @VersionTime t))
  where
    t = T.strip given

-- | The sentence for a refusal of the wiki's domain, about the page at a
-- path.
refusalText :: Text -> WikiError -> Text
refusalText at = \case
  NotFound -> "There is no page at " <> at <> " that the user may read: find pages with wiki_list or wiki_search."
  Forbidden -> "The user may read the page at " <> at <> " but not write it, and nothing was written."
  Stale _ _ -> "The page at " <> at <> " is in an encrypted space, whose text is encrypted in its members' browsers: the server can neither read nor write it."
  Conflict why -> why
  Invalid why -> why
  Unreachable _ -> "The wiki's storage could not be reached: try again in a moment."

-- | The reference of the wiki's Markdown, or one section of it: the
-- sections of the reference's heading (its @##@ sections), and the
-- heading's own text before them.
reference :: Maybe Text -> Mcp.ToolResult
reference = \case
  Nothing -> textResult (T.unlines ("The sections of the wiki's Markdown reference, by anchor; pass one as section:" : "" : map line entries))
  Just asked -> case find (named asked . fst) entries of
    Nothing -> Mcp.toolError ("The reference has no section " <> show asked <> ". Its sections: " <> T.intercalate ", " (map ((.id) . fst) entries) <> ".")
    Just (s, own) -> textResult (maybe "" (sectionText markdownReference) (sectionRange o s.id own))
  where
    o = outline markdownReference
    -- The reference's heading with its own text only, then each section
    -- under it whole.
    entries :: [(Section, Bool)]
    entries = concatMap (\s -> (s, True) : map (,False) s.below) o.sections
    named asked s = let a = T.toCaseFold (T.strip (T.dropWhile (== '#') asked)) in a == T.toCaseFold s.id || a == T.toCaseFold s.text
    line (s, own) = "- " <> s.id <> ": " <> s.text <> ". " <> maybe "" (summary . sectionText markdownReference) (sectionRange o s.id own)
    -- The first sentence of a section's text, after its heading: a line
    -- of prose or a list's item, never a table's row or a fence.
    summary t = case [l | l <- drop 1 (map T.strip (T.lines t)), not (T.null l), not (any (`T.isPrefixOf` l) ["|", "```", "~~~", "<", "#", "[["])] of
      first' : _ ->
        let prose = fromMaybe first' (T.stripPrefix "- " first')
            (sentence, rest) = T.breakOn ". " prose
         in T.take 200 (if T.null rest then sentence else sentence <> ".")
      [] -> ""

-- | 'withArguments' goes on with a call's arguments read, or answers with
-- why they are not valid.
withArguments :: Either Text a -> (a -> IO Mcp.ToolResult) -> IO Mcp.ToolResult
withArguments read' continue = either (pure . Mcp.toolError) continue read'

-- | A text argument, if the call gives one.
textArgument :: Text -> Arguments -> Either Text (Maybe Text)
textArgument name args = case Map.lookup name args of
  Nothing -> Right Nothing
  Just Null -> Right Nothing
  Just (String t) -> Right (Just t)
  Just _ -> Left ("The argument " <> name <> " is text.")

-- | A whole number argument, if the call gives one.
intArgument :: Text -> Arguments -> Either Text (Maybe Int)
intArgument name args = case Map.lookup name args of
  Nothing -> Right Nothing
  Just Null -> Right Nothing
  Just v | Success i <- fromJSON v -> Right (Just i)
  Just _ -> Left ("The argument " <> name <> " is a whole number.")

-- | A true or false argument, if the call gives one.
boolArgument :: Text -> Arguments -> Either Text (Maybe Bool)
boolArgument name args = case Map.lookup name args of
  Nothing -> Right Nothing
  Just Null -> Right Nothing
  Just (Bool b) -> Right (Just b)
  Just _ -> Left ("The argument " <> name <> " is true or false.")

-- | An argument the tool cannot go without.
requiredArgument :: Text -> Either Text (Maybe a) -> Either Text a
requiredArgument name = (>>= maybe (Left ("The argument " <> name <> " is required.")) Right)

-- | The argument @path@, described for its tool.
pathArgument :: Text -> (Text, Mcp.Schema)
pathArgument described = ("path", Mcp.describedSchema described (Mcp.SchemaString Nothing))

-- | A result of one text.
textResult :: Text -> Mcp.ToolResult
textResult t = Mcp.toolResult [Mcp.ContentText t]

-- | A result of a value as JSON, in one text.
jsonResult :: (ToJSON a) => a -> Mcp.ToolResult
jsonResult = textResult . jsonText

-- | A result of what is known of a text (JSON, in one text), then the text
-- itself as it is, in another: Markdown is easier to read and to change
-- unquoted.
withText :: Value -> Text -> Mcp.ToolResult
withText about t = Mcp.toolResult [Mcp.ContentText (jsonText about), Mcp.ContentText t]

jsonText :: (ToJSON a) => a -> Text
jsonText = decodeUtf8 . BSL.toStrict . encode
