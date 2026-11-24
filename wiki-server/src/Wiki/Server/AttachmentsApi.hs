{-# LANGUAGE DeriveAnyClass #-}

-- | A page's attachments in the API for programs (WP5.138 of
-- docs/PLAN.md, the owner's issue #367): what a program on a user's own
-- machine reads to list a page's files and fetch one, and sends to attach
-- a file or take one off. The requests sign in as the rest of the API does
-- ('Wiki.Server.MarkdownApi.signedInProgram'): a key with the right @read@
-- lists and fetches, and attaching or deleting needs one with the right
-- @write@. Their addresses are endpoints of the servant type of the API
-- for programs ("Wiki.Server.ProgramApi"), and this module has their
-- handlers.
--
-- = Addresses
--
-- One address per page, @api\/attachments\/s\/\<space path>\/p\/\<page
-- path>\/@, the page's reading address with the API's prefix, as for its
-- Markdown. A file is named by the query (@?name=@), never by a further
-- segment of the path, since a segment after a page's address is a child
-- page's slug.
--
-- * @GET@ without @name@: the page's attachments, as JSON, each with its
--   name, size, media type, version (its @ETag@), the address of its bytes
--   here, and the Markdown that refers to it in a page
--   ('Wiki.Core.Markdown.Link.attachmentReference', what the editor
--   inserts after an upload).
-- * @GET@ with @name@: the file's bytes, with the same media type and the
--   same protective headers as the browser's route for them
--   ('Wiki.Server.Api.attachmentAnswer'), the @ETag@ the hash of the bytes,
--   and a range when one is asked for.
-- * @PUT@ with @name@: the body is the file, its @Content-Type@ the media
--   type stored ('uploadAttachmentFileIf', the upload of the browser's
--   route with what the page must have under the name). The browser's
--   upload replaces an attachment of the same name; here a replace is made
--   only over the version read: @If-Match@ with the @ETag@ replaces (412
--   when the file has changed since), @If-None-Match: *@ only creates (412
--   when the name is taken), and a @PUT@ with neither to a name that is
--   taken is 428.
-- * @DELETE@ with @name@: the attachment is taken off the page, as the
--   browser's delete does.
--
-- = A name
--
-- A file's name is its identity here: the value of @name@ in the query as
-- sent, percent-decoded once (a @+@ is a plus, a space is @%20@), compared
-- byte for byte with the names stored, never changed. A name the
-- application's rule refuses ('parseFileName') is refused with its words,
-- and so is one the rule would store otherwise (with white space at an
-- end, which the rule trims).
--
-- = A body
--
-- An upload is never read into memory: its chunks are written to a
-- temporary file in the data directory as they come, as the browser's
-- form is, and the reading stops at the first chunk past
-- 'maxAttachmentSize'. A @Content-Length@ over the limit is refused before
-- anything of the body is read, so that a client that sends
-- @Expect: 100-continue@ (curl does for a large file) sends nothing. Both
-- are 413, with the storage's words ('attachmentTooLarge'). The right to
-- attach to the page is asked before the body is read, too.
module Wiki.Server.AttachmentsApi
  ( -- * The handlers
    readAttachments
  , writeAttachment
  , deleteAttachmentAt
  , refuseOtherAttachmentMethods
  , AttachmentPut (..)
  , Stored (..)
  , PutAnswers

    -- * A page's attachments
  , AttachmentEntry (..)
  , AttachmentList (..)
  , attachmentList

    -- * The body of an upload
  , Spooled (..)
  , spool
  )
where

import Data.Aeson (Options (..), ToJSON (..), defaultOptions, encode, genericToEncoding, genericToJSON)
import Data.ByteString qualified as BS
import Data.OpenApi (ToSchema (..), fromAesonOptions, genericDeclareNamedSchema)
import Data.SOP (I (..), NP (..))
import Data.Text qualified as T
import Generics.SOP qualified as SOP
import Network.HTTP.Types (hCacheControl, hContentType, status200, status400, status404, status412, status413, status415, status428)
import Network.HTTP.Types.URI (urlDecode)
import Network.URI (escapeURIString, isUnreserved)
import Network.Wai (Request, Response, responseLBS)
import Protolude hiding (Handler)
import Servant (Handler, Header)
import Servant.API.MultiVerb (AsHeaders (..), AsUnion, GenericAsUnion (..), RespondAs, WithHeaders)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.IO (hClose, openBinaryTempFile)
import Wiki.App.Backend (MonadWiki (deleteAttachment), maxAttachmentSize)
import Wiki.App.Route qualified as Route
import Wiki.Core.Attachment (Attachment (..), FileName (..), MimeType (..), Source (..), mimeEssence, parseFileName)
import Wiki.Core.Id (AttachmentId, BlobId (..), UserId)
import Wiki.Core.Markdown.Link (attachmentReference)
import Wiki.Core.Page (Page (..))
import Wiki.Core.Url (Url (..))
import Wiki.Server.Api (Env (..), Form (..), attachmentAnswer, attachmentTag)
import Wiki.Server.MarkdownApi (addressUnder, pageAt, pathAfter, runAs)
import Wiki.Server.ProgramApi.Combinators (CacheControl (..), Incoming (..), Json, Refusal, Sent (..), refusal, refuse, wikiRefusal)
import Wiki.Server.Render (routeOf)
import Wiki.Server.Storage (Expected (..), Uploaded (..), Who (..), attachingTo, attachmentFile, attachmentTooLarge, uploadAttachmentFileIf)
import Wiki.Server.Storage.Layout (temporaryDir)

-- | 'AttachmentEntry' is one attachment of a page, as the API lists it.
data AttachmentEntry = AttachmentEntry
  { name :: Text
  -- ^ The file's name, as the page's Markdown refers to it.
  , size :: Integer
  -- ^ Its size in bytes.
  , mediaType :: Text
  -- ^ Its media type, as the upload declared it.
  , version :: Maybe Text
  -- ^ The value of its bytes' @ETag@, quotes included, to send back as
  -- @If-Match@ to replace it. Left out for a file on the web.
  , address :: Text
  -- ^ The address of its bytes in this API; for a file on the web, its
  -- address there.
  , markdown :: Text
  -- ^ The Markdown that shows it in a page (an image, a video, a sound)
  -- or links to it (any other file), as the editor inserts it.
  }
  deriving stock (Eq, Show, Generic)

-- | 'AttachmentList' is a page's attachments, in the page's order.
newtype AttachmentList = AttachmentList
  { attachments :: [AttachmentEntry]
  }
  deriving stock (Eq, Show, Generic)

-- | The JSON of the answers: fields in the order of the records, and a
-- field that is 'Nothing' left out. The document's schemas follow it.
entryOptions :: Options
entryOptions = defaultOptions {omitNothingFields = True}

instance ToJSON AttachmentEntry where
  toJSON = genericToJSON entryOptions
  toEncoding = genericToEncoding entryOptions

instance ToSchema AttachmentEntry where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions entryOptions)

instance ToJSON AttachmentList where
  toJSON = genericToJSON entryOptions
  toEncoding = genericToEncoding entryOptions

instance ToSchema AttachmentList where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions entryOptions)

-- | 'attachmentList' is the attachments of the page at a route, on a
-- listener that serves the wiki under this path: each with the address of
-- its bytes in the API. The HTTP API ('readAttachments') and the MCP
-- server's @wiki_list_attachments@ ("Wiki.Server.McpApi") both give it.
attachmentList :: Text -> Route.Route -> [Attachment] -> AttachmentList
attachmentList mount r = AttachmentList . map (entryOf mount r)

-- | One attachment of the page at a route, as the list gives it.
entryOf :: Text -> Route.Route -> Attachment -> AttachmentEntry
entryOf mount r a =
  AttachmentEntry
    { name = a.name.text
    , size = fromIntegral a.size
    , mediaType = a.mediaType.text
    , version = case a.source of
        SourceBlob _ -> Just (versionOf a)
        SourceUrl _ -> Nothing
    , address = case a.source of
        SourceBlob _ -> bytesAt mount r a.name.text
        SourceUrl u -> u.text
    , markdown = attachmentReference a.name.text a.mediaType.text
    }

-- | The address of a file's bytes in the API, by the page's route and the
-- file's name: the page's address under @api\/attachments@, and the name
-- in the query, every byte of its UTF-8 that is not a letter, a digit or
-- one of @-_.~@ percent-encoded (a space is @%20@, a plus @%2B@).
bytesAt :: Text -> Route.Route -> Text -> Text
bytesAt mount r name = addressUnder mount "api/attachments" r <> "?name=" <> toS (escapeURIString isUnreserved (toS name))

-- | An attachment's version: the value of its bytes' @ETag@, quotes
-- included.
versionOf :: Attachment -> Text
versionOf a = "\"" <> decodeUtf8With lenientDecode (attachmentTag a) <> "\""

-- | @GET@ at a page's attachments' address, on a listener that serves the
-- wiki under this path, for a request from this address, sent as this:
-- the list without @name@, the file's bytes with it.
readAttachments :: Env -> Text -> Text -> UserId -> Sent -> Request -> Handler Response
readAttachments env mount address u sent request = do
  asked <- either (refuse . refusal status400 "Invalid" . Just) pure (nameOf sent)
  (r, p) <- pageAt env address u (pathAfter mount "api/attachments" sent.path)
  case asked of
    Nothing ->
      pure (responseLBS status200 [(hContentType, "application/json"), (hCacheControl, "private, no-store")] (encode (attachmentList mount r p.attachments)))
    Just n -> do
      a <- named mount r p n
      case a.source of
        SourceUrl url -> refuse (refusal status404 "NotFound" (Just ("The file " <> n <> " is not kept by the wiki: it is on the web, at " <> url.text <> ".")))
        SourceBlob _ ->
          liftIO (runAs env address u (attachmentFile a.id)) >>= \case
            Left e -> refuse (wikiRefusal e)
            Right (stored, Just file) ->
              ifM
                (liftIO (doesFileExist file))
                (pure (attachmentAnswer Plain "private, no-cache" [] stored file request))
                (refuse (refusal status404 "NotFound" (Just ("The bytes of the file " <> n <> " are missing from the wiki's storage."))))
            Right (_, Nothing) -> refuse (refusal status404 "NotFound" Nothing)

-- | The attachment of a page with exactly this name, or the refusal that
-- says the page has none, with the address of the page's list.
named :: Text -> Route.Route -> Page -> Text -> Handler Attachment
named mount r p n = case find (\a -> a.name.text == n) p.attachments of
  Just a -> pure a
  Nothing -> refuse (refusal status404 "NotFound" (Just ("This page has no attachment named " <> show n <> ": its attachments are listed at " <> addressUnder mount "api/attachments" r <> ".")))

-- | 'Stored' is the answer to a file attached: its entry in the list, the
-- address of its bytes, and its version, which the answer gives as its
-- @ETag@.
data Stored = Stored
  { entry :: AttachmentEntry
  , location :: Text
  , tag :: Text
  }

instance AsHeaders '[Text, Text, CacheControl] AttachmentEntry Stored where
  toHeaders s = (I s.location :* I s.tag :* I (CacheControl "no-store") :* Nil, s.entry)
  fromHeaders (I l :* I t :* _ :* Nil, e) = Stored {entry = e, location = l, tag = t}

instance AsHeaders '[Text, CacheControl] AttachmentEntry Stored where
  toHeaders s = (I s.tag :* I (CacheControl "no-store") :* Nil, s.entry)
  fromHeaders (I t :* _ :* Nil, e) = Stored {entry = e, location = e.address, tag = t}

-- | 'AttachmentPut' is what a @PUT@ of a file answers: a file attached
-- under a new name, or one that took the place of the file of its name.
data AttachmentPut
  = AttachmentCreated Stored
  | AttachmentReplaced Stored
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (AsUnion PutAnswers) via GenericAsUnion PutAnswers AttachmentPut

-- | The answers of a @PUT@ of a file, in the order of 'AttachmentPut''s
-- constructors.
type PutAnswers =
  '[ WithHeaders '[Header "Location" Text, Header "ETag" Text, Header "Cache-Control" CacheControl] Stored (RespondAs Json 201 "The file attached under a new name: its entry in the list. The Location is the address of its bytes, and the ETag its version." AttachmentEntry)
   , WithHeaders '[Header "ETag" Text, Header "Cache-Control" CacheControl] Stored (RespondAs Json 200 "The file put in the place of the one of its name (If-Match): its entry in the list, with its new version, which is also the ETag." AttachmentEntry)
   ]

-- | @PUT@ of a file at a page's attachments' address, on a listener that
-- serves the wiki under this path, for a request from this address, sent
-- as this, with its @If-Match@ and @If-None-Match@ and its body. The page
-- is found first, then the name and the headers are read, then the right
-- to attach is asked, and only then is the body read.
writeAttachment :: Env -> Text -> Text -> UserId -> Sent -> Maybe (Either Text Text) -> Maybe (Either Text Text) -> Incoming -> Handler AttachmentPut
writeAttachment env mount address u sent ifMatch ifNoneMatch incoming = do
  (r, p) <- pageAt env address u (pathAfter mount "api/attachments" sent.path)
  n <- either invalid pure (nameOf sent >>= maybe (Left "Give the file's name in the query: ?name=diagram.png.") Right >>= storedAs)
  expected <- either invalid pure (expectation ifMatch ifNoneMatch)
  mediaType <- mediaTypeOf incoming.mediaType
  liftIO (attachingTo env.storage (AsUser u) p.id) >>= either (refuse . wikiRefusal) pure
  when (maybe False (> fromIntegral maxAttachmentSize) incoming.declared) tooLarge
  outcome <- liftIO $ do
    let dir = temporaryDir env.dataDir
    createDirectoryIfMissing True dir
    bracket (openBinaryTempFile dir "upload") (\(file, h) -> hClose h *> removeFile file) $ \(file, h) ->
      spool (fromIntegral maxAttachmentSize) incoming.chunk h >>= \case
        TooLarge -> pure Nothing
        Spooled -> do
          hClose h
          Just <$> runAs env address u (uploadAttachmentFileIf (fromMaybe NoAttachment expected) p.id n mediaType file)
  case outcome of
    Nothing -> tooLarge
    Just (Left e) -> refuse (wikiRefusal e)
    Just (Right (Right done)) ->
      let entry = entryOf mount r done.attachment
          stored = Stored {entry, location = entry.address, tag = versionOf done.attachment}
       in pure (if done.replaced then AttachmentReplaced stored else AttachmentCreated stored)
    Just (Right (Left there)) -> refuse (unmet n expected there)
  where
    invalid = refuse . refusal status400 "Invalid" . Just
    tooLarge = refuse (refusal status413 "Invalid" (Just attachmentTooLarge))

-- | What the page must have under the name for a @PUT@ to go ahead, from
-- its @If-Match@ and @If-None-Match@: 'Nothing' for neither, which only
-- creates, and is refused for a name that is taken (428) rather than for
-- a stale version (412).
expectation :: Maybe (Either Text Text) -> Maybe (Either Text Text) -> Either Text (Maybe Expected)
expectation ifMatch ifNoneMatch = case (ifMatch, ifNoneMatch) of
  (Just _, Just _) -> Left "Send If-Match (replace the file of that version) or If-None-Match: * (attach only where there is none), not both."
  (Just given, Nothing) -> case T.strip (either (const "") identity given) of
    t
      | Just inner <- T.stripPrefix "\"" t >>= T.stripSuffix "\""
      , not (T.null inner)
      , T.all (/= '"') inner ->
          Right (Just (AttachmentOf (BlobId inner)))
    _ -> Left "If-Match takes the file's version, its ETag as the list or a read of the file gives it, quotes included."
  (Nothing, Just given)
    | T.strip (either (const "") identity given) == "*" -> Right (Just NoAttachment)
    | otherwise -> Left "The If-None-Match header here is * (attach the file only where there is none), or nothing."
  (Nothing, Nothing) -> Right Nothing

-- | The refusal of a @PUT@ whose condition does not hold, for a file of
-- this name, with the condition the request named ('Nothing' for none)
-- and what the page has under the name.
unmet :: Text -> Maybe Expected -> Maybe Attachment -> Refusal
unmet n expected there = case (expected, there) of
  (Nothing, Just _) ->
    refusal status428 "Conflict" (Just ("The page has a file named " <> n <> " already. To replace it, send If-Match with its version (its ETag); to keep it, attach the new file under another name. Nothing was written."))
  (Just NoAttachment, Just _) ->
    refusal status412 "Conflict" (Just ("The page has a file named " <> n <> " already, and If-None-Match: * attaches only where there is none. Nothing was written."))
  (_, Nothing) ->
    refusal status412 "Conflict" (Just ("The page has no file named " <> n <> ", and If-Match names a version of one: leave If-Match out to attach it. Nothing was written."))
  (_, Just _) ->
    refusal status412 "Conflict" (Just ("The file " <> n <> " has been replaced since the version in If-Match: read the list again, and replace that version. Nothing was written."))

-- | @DELETE@ of a file at a page's attachments' address, as the browser's
-- delete takes it off the page.
deleteAttachmentAt :: Env -> Text -> Text -> UserId -> Sent -> Handler ()
deleteAttachmentAt env mount address u sent = do
  (r, p) <- pageAt env address u (pathAfter mount "api/attachments" sent.path)
  n <- either (refuse . refusal status400 "Invalid" . Just) pure (nameOf sent >>= maybe (Left "Give the file's name in the query: ?name=diagram.png.") Right)
  a <- named mount r p n
  liftIO (runAs env address u (deleteAttachment (a.id :: AttachmentId))) >>= either (refuse . wikiRefusal) pure

-- | Before a method the address does not take is refused (405), on a
-- listener that serves the wiki under this path, for a request sent as
-- this: an address that names no page is 404 for every method.
refuseOtherAttachmentMethods :: Text -> Sent -> Handler ()
refuseOtherAttachmentMethods mount sent =
  liftIO (routeOf "/" (pathAfter mount "api/attachments" sent.path)) >>= \case
    Route.Page _ _ -> pass
    Route.Space _ -> pass
    _ -> refuse (refusal status404 "NotFound" Nothing)

-- | The file's name a request names in its query (@name@), as sent:
-- percent-decoded once, with a @+@ kept as a plus. 'Nothing' when the
-- query has no @name@; why it cannot be read when it has it twice,
-- without a value, or not as UTF-8.
nameOf :: Sent -> Either Text (Maybe Text)
nameOf sent = case [value | (key, value) <- pairs, key == "name"] of
  [] -> Right Nothing
  [Just bytes] -> bimap (const "The parameter name is not UTF-8 text.") Just (decodeUtf8' bytes)
  [Nothing] -> Left "The parameter name needs a value: ?name=diagram.png."
  _ -> Left "The query has the parameter name more than once: name one file."
  where
    pairs =
      [ (urlDecode False key, urlDecode False <$> value)
      | part <- BS.split 38 (fromMaybe sent.query (BS.stripPrefix "?" sent.query))
      , not (BS.null part)
      , let (key, rest) = BS.break (== 61) part
            value = BS.stripPrefix "=" rest
      ]

-- | A new file's name as the request gives it, if the application's rule
-- ('parseFileName') stores it as it is: the rule's words for a name it
-- refuses, and a refusal for one it would store otherwise (trimmed of
-- white space), since a name is the file's identity here.
storedAs :: Text -> Either Text Text
storedAs n = case parseFileName n of
  Left why -> Left why
  Right stored
    | stored.text == n -> Right n
    | otherwise -> Left ("An attachment's name cannot start or end with white space: this one would be stored as " <> show stored.text <> ". Send that name.")

-- | The media type an upload's @Content-Type@ declares, as text: none is
-- the empty text, which the storage takes for @application\/octet-stream@.
-- The two types of a form are refused: curl sends
-- @application\/x-www-form-urlencoded@ for @--data-binary@ when it is not
-- told the file's type, and a form's body is not the file.
mediaTypeOf :: Maybe ByteString -> Handler Text
mediaTypeOf = \case
  Nothing -> pure ""
  Just bytes -> case decodeUtf8' bytes of
    Left _ -> refuse (refusal status400 "Invalid" (Just "The Content-Type is not UTF-8 text."))
    Right t
      | mimeEssence (MimeType t) `elem` ["application/x-www-form-urlencoded", "multipart/form-data"] ->
          refuse . refusal status415 "Invalid" . Just $
            "The body is the file itself, and its Content-Type the file's media type, such as image/png (curl -H 'Content-Type: image/png' --data-binary @file), or application/octet-stream when it has none: "
              <> mimeEssence (MimeType t)
              <> " is a form's, which curl sends for --data-binary unless told otherwise."
      | otherwise -> pure t

-- | 'Spooled' is how the reading of an upload's body ended ('spool').
data Spooled
  = -- | The whole body was written.
    Spooled
  | -- | The body is longer than the limit: the reading stopped at the
    -- first chunk past it.
    TooLarge
  deriving stock (Eq, Show)

-- | 'spool' writes a request's body, read a chunk at a time with the
-- given action, to a handle, until its end, or until it is longer than so
-- many bytes: then it stops reading, and the rest of the body is never
-- read. No more than the limit is written, and one chunk is held at a
-- time.
spool :: Int -> IO ByteString -> Handle -> IO Spooled
spool limit next h = loop 0
  where
    loop !written = next >>= wrote written
    wrote written c
      | BS.null c = pure Spooled
      | written + BS.length c > limit = pure TooLarge
      | otherwise = BS.hPut h c *> loop (written + BS.length c)
