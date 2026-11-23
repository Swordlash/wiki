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
-- 'putMarkdown', 'listing', 'searching', 'contentsOf', 'sectionOf'), as
-- the key's user ('runAs'): the spaces' permissions hold as in the
-- browser, and a page the user may not read is not found.
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
-- and a page saved since the version a write names. A JSON-RPC error is
-- left for a request that is not a tool's call at all: an unknown tool,
-- or a request the library cannot read. Every result is at most
-- 'maxResult' characters, or says how to ask for less.
module Wiki.Server.McpApi
  ( mcpApplication
  , instructions
  , maxResult
  )
where

import Data.Aeson (Result (..), ToJSON (..), Value (..), encode, fromJSON, object, (.=))
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Halogen.Router.Duplex qualified as Duplex
import MCP.Server qualified as Mcp
import Network.HTTP.Types (hAuthorization, status401)
import Network.Wai (Application, Request (..))
import Protolude
import Web.HttpApiData (parseHeader, toHeader)
import Wiki.Api.ApiKeys (KeyRight (..), rightName)
import Wiki.Api.Version (PageVersion, pageVersion)
import Wiki.App.Backend (WikiError (..))
import Wiki.App.Route qualified as Route
import Wiki.Core.Assistant.Reference (markdownReference)
import Wiki.Core.Markdown.Sections (Outline (..), Section (..), outline, outlineAnchors, sectionRange, sectionText)
import Wiki.Core.Page (Page (..))
import Wiki.Core.PageKind (PageKind (..))
import Wiki.Core.Path (renderPath)
import Wiki.Core.Slug (Slug)
import Wiki.Server.Api (Env (..))
import Wiki.Server.ApiKeys (KeyHolder (..))
import Wiki.Server.ContentsApi (contentsOf, sectionOf)
import Wiki.Server.ListApi (Addresses (..), HitEntry (..), HitSpace (..), TopLevel (..), hasSearchWords, listing, searchSpace, searching)
import Wiki.Server.MarkdownApi (Asking (..), Put (..), findPage, notASlug, putMarkdown, refusalWith, runAs, signInWithKey)
import Wiki.Server.Render (routeOf)

-- | The MCP server, on a listener that serves the wiki under this path
-- (with a slash at each end), for a request the listener knows this of
-- ('Asking'). The listener routes @mcp@ under its path here.
mcpApplication :: Env -> Text -> Asking -> Application
mcpApplication env mount asking request respond = case bearerOf request of
  Nothing -> respond noKey
  Just presented ->
    signInWithKey env asking presented >>= \case
      Left refused -> respond refused
      Right holder -> do
        app <- Mcp.mcpApplication config serverInfo Mcp.noHandlers {Mcp.tools = Just (toolHandlers env mount asking.address holder)}
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

-- | The key a request presents in its one @Authorization@ header, as
-- @Bearer \<key>@, the scheme's name in any case; 'Nothing' for a request
-- with no such header, or with more than one.
bearerOf :: Request -> Maybe Text
bearerOf request = case [v | (n, v) <- requestHeaders request, n == hAuthorization] of
  [v]
    | (scheme, rest) <- T.break (== ' ') (decodeUtf8With lenientDecode v)
    , T.toCaseFold scheme == "bearer" ->
        Just (T.strip rest)
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
    , "Read before you write, and pass the version. wiki_read_page gives a page's Markdown and its version. wiki_write_page replaces the whole page, and takes base_version, the version you read: if the page has been saved since, nothing is written, and you read it again, merge your change into what it says now, and write with its new version. For a long page, read its outline (wiki_page_outline) and then one section at a time (wiki_read_section)."
    , ""
    , "Look a construct of the wiki's Markdown up in wiki_markdown_reference before using it: a box, math, a diagram, a spoiler, a link to another page, a presentation. Called without a section, it lists the reference's sections; with one, it gives that section."
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
  , destructive :: Bool
  , tooLong :: Int -> Text
  , run :: Arguments -> IO Mcp.ToolResult
  }

-- | The arguments of a tool's call, by name, as JSON.
type Arguments = Map Text Value

-- | The tools, for a key's user, for a request from this address, on a
-- listener that serves the wiki under this path: what @tools\/list@ lists
-- and what @tools\/call@ calls. A key with the right @read@ is listed the
-- tools that only read, and is refused the others.
toolHandlers :: Env -> Text -> Text -> KeyHolder -> (Mcp.ToolListHandler, Mcp.ToolCallHandler)
toolHandlers env mount address holder = (listed, call)
  where
    -- @write@ and @admin@ write; @read@ only reads.
    mayWrite = holder.right >= WriteRight
    available = [t | t <- tools env mount address holder, mayWrite || not t.writes]
    listed _ = pure (map definition available)
    call _ named args = case find (\t -> t.name == named) (tools env mount address holder) of
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
-- client shows: whether it only reads, and whether a write can take away
-- what was there.
definition :: Tool -> Mcp.ToolDefinition
definition t =
  (Mcp.mkToolDefinition t.name t.description (Mcp.schema (Mcp.SchemaObject t.arguments t.required)))
    { Mcp.toolDefinitionAnnotations =
        Just
          Mcp.defaultToolAnnotations
            { Mcp.toolReadOnlyHint = Just (not t.writes)
            , Mcp.toolDestructiveHint = guard t.writes $> t.destructive
            , Mcp.toolOpenWorldHint = Just False
            }
    }

-- | Every tool, for a key's user, for a request from this address, on a
-- listener that serves the wiki under this path.
tools :: Env -> Text -> Text -> KeyHolder -> [Tool]
tools env mount address holder =
  [ Tool
      { name = "wiki_list"
      , description = "List the spaces and pages the user may read, as a tree: each space with its title, its path, its subspaces and its pages, each page with its title, its kind and its path in the field markdown, to pass to the other tools as path. A space's home page is its field home. Without space, the whole wiki; with it, one space. depth cuts the tree: 1 is the space's own subspaces and top pages."
      , arguments =
          [ ("space", Mcp.describedSchema "The space to list, by its path such as team/projects (a listed space's field path). Leave it out for every space." (Mcp.SchemaString Nothing))
          , ("depth", Mcp.describedSchema "How many levels to list, from 1 up. Leave it out for all of them." Mcp.SchemaInteger)
          ]
      , required = []
      , writes = False
      , destructive = False
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
      , destructive = False
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
      , destructive = False
      , tooLong = \n -> "The page has more than one answer holds (" <> show n <> " characters with its title and version): read its outline with wiki_page_outline, then its sections one at a time with wiki_read_section."
      , run = \args -> withPage args $ \at sp pp ->
          findPage env address holder.user sp pp <&> \case
            Left e -> Mcp.toolError (refusalText at e)
            Right p -> withText (object ["path" .= at, "title" .= p.title, "version" .= versionOf p]) p.body
      }
  , Tool
      { name = "wiki_page_outline"
      , description = "Read a page's outline: its title, its version, and its headings as a tree, each with its level, its text, its anchor (the name of its section, for wiki_read_section) and whether it is inside a spoiler."
      , arguments = [pathArgument "The page's path, such as /s/team/p/on-call/."]
      , required = ["path"]
      , writes = False
      , destructive = False
      , tooLong = \n -> "The page's outline has " <> show n <> " characters, more than one answer holds: read the page's sections by their anchors with wiki_read_section."
      , run = \args -> withPage args $ \at sp pp ->
          findPage env address holder.user sp pp <&> \case
            Left e -> Mcp.toolError (refusalText at e)
            Right p -> jsonResult (contentsOf Nothing p)
      }
  , Tool
      { name = "wiki_read_section"
      , description = "Read one section of a page: its Markdown from its heading to the next heading of the same or a higher level, its subsections included, or with own true only to the next heading of any level. Also the page's version, to pass as base_version when you write the page."
      , arguments =
          [ pathArgument "The page's path, such as /s/team/p/on-call/."
          , ("section", Mcp.describedSchema "The section's anchor, from the page's outline (wiki_page_outline)." (Mcp.SchemaString Nothing))
          , ("own", Mcp.describedSchema "True for the section's own text only, without its subsections." Mcp.SchemaBoolean)
          ]
      , required = ["path", "section"]
      , writes = False
      , destructive = False
      , tooLong = \n -> "The section has " <> show n <> " characters, more than one answer holds: read its own text (own true), then its subsections, whose anchors the outline (wiki_page_outline) gives."
      , run = \args -> withArguments ((,) <$> requiredArgument "section" (textArgument "section" args) <*> (fromMaybe False <$> boolArgument "own" args)) $ \(anchor, own) ->
          withPage args $ \at sp pp ->
            findPage env address holder.user sp pp <&> \case
              Left e -> Mcp.toolError (refusalText at e)
              Right p -> case sectionOf p.body (Just anchor) own of
                Nothing ->
                  let anchors = outlineAnchors (outline p.body)
                   in Mcp.toolError ("The page at " <> at <> " has no section with the anchor " <> show anchor <> ". Its anchors: " <> (if null anchors then "none" else T.intercalate ", " anchors) <> ".")
                Just part -> withText (object ["path" .= at, "title" .= p.title, "section" .= anchor, "version" .= versionOf p]) part
      }
  , Tool
      { name = "wiki_write_page"
      , description = "Replace a page's whole Markdown, as a save in the editor does: a new version in the page's history under the user's name, shown at once to whoever has the page open. base_version is the version you read (wiki_read_page, wiki_page_outline or wiki_read_section): if the page has been saved since, nothing is written, and you read it again, merge, and write with its new version. The title stays. Every write can be seen and undone in the page's history. The result is the page's new version."
      , arguments =
          [ pathArgument "The page's path, such as /s/team/p/on-call/."
          , ("markdown", Mcp.describedSchema "The page's whole new Markdown." (Mcp.SchemaString Nothing))
          , ("base_version", Mcp.describedSchema "The version you read the page at, as the reading tools give it." (Mcp.SchemaString Nothing))
          ]
      , required = ["path", "markdown", "base_version"]
      , writes = True
      , destructive = True
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
      { name = "wiki_create_page"
      , description = "Create a page at a path where there is none, with its Markdown, as the browser's \"New page\" does: its first version under the user's name. The path's last slug is the new page's slug, and the page above it, named by the slugs before, has to exist; nothing is created on the way. Without a title, the title is made from the slug. A page already at the path is not written over. The result is the page's path and its version."
      , arguments =
          [ pathArgument "The new page's path, such as /s/team/p/on-call/rota/: its space, the page above it, and its own slug (lowercase letters, digits and dashes)."
          , ("markdown", Mcp.describedSchema "The new page's Markdown." (Mcp.SchemaString Nothing))
          , ("title", Mcp.describedSchema "The new page's title. Leave it out to make it from the slug." (Mcp.SchemaString Nothing))
          ]
      , required = ["path", "markdown"]
      , writes = True
      , destructive = False
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
      { name = "wiki_markdown_reference"
      , description = "The reference of the wiki's Markdown: how to write its boxes, math, diagrams, spoilers, links between pages, presentations and the rest. Without section, its sections, each with its anchor and a line on what it covers; with section, the whole of that section."
      , arguments = [("section", Mcp.describedSchema "The section's anchor (such as notes-and-boxes) or its heading." (Mcp.SchemaString Nothing))]
      , required = []
      , writes = False
      , destructive = False
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
    withPage args continue = withArguments (requiredArgument "path" (textArgument "path" args)) $ \given ->
      routeOf "/" (encodeUtf8 (localPath mount given)) >>= \case
        Route.Page sp pp -> continue (pathOf sp pp) sp pp
        Route.Space sp -> continue (pathOf sp []) sp []
        _ -> pure (Mcp.toolError (fromMaybe (show given <> " is not a page's path: a page's path is /s/<space path>/p/<page path>/, such as /s/team/p/on-call/, and a space's home page is /s/<space path>/.") (notASlug (pathSegments (localPath mount given)))))

-- | The path of the page at a space's path and a page's path, as the
-- browser's address bar shows it under the wiki's own part.
pathOf :: [Slug] -> [Slug] -> Text
pathOf sp pp = Duplex.print Route.route (Route.pageRoute sp pp)

-- | The path a tool is given, as the router reads it under the listener's
-- path: a full address's scheme and host taken off, then the listener's
-- path, then a query or a fragment; with a slash at each end.
localPath :: Text -> Text -> Text
localPath mount given = withSlashes (T.takeWhile (`notElem` ['?', '#']) (underMount (withoutOrigin (T.strip given))))
  where
    withoutOrigin t = case T.breakOn "://" t of
      (_, rest) | not (T.null rest) -> T.dropWhile (/= '/') (T.drop 3 rest)
      _ -> t
    underMount t
      | mount /= "/", Just rest <- T.stripPrefix (T.dropWhileEnd (== '/') mount) t = rest
      | otherwise = t
    withSlashes t = "/" <> T.dropAround (== '/') t <> (if T.null (T.dropAround (== '/') t) then "" else "/")

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
versionArgument given = first (const ("base_version " <> show given <> " is not a page's version: pass the version a reading tool gave, such as \"2026-11-18T09:30:12.345678Z\".")) (parseHeader (encodeUtf8 quoted))
  where
    t = T.strip given
    quoted = if "\"" `T.isPrefixOf` t then t else "\"" <> t <> "\""

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
