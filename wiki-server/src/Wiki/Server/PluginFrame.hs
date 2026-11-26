{-# LANGUAGE TemplateHaskell #-}

-- | An external plugin's frame (WP5.84 of docs/PLAN.md, decision 69, issue
-- #186; "Wiki.Api.Plugins.External"): the document the wiki's editor
-- loads into an @\<iframe sandbox="allow-scripts">@ to run a plugin's
-- code, at
-- @plugin-frame\/\<wiki|mine>\/\<id>\/\<bundle's hash>\/diagnostics@
-- ('Wiki.Api.Plugins.External.framePath').
--
-- The document holds two scripts and nothing else: a line that puts the
-- plugin's code, as a JSON string, into @self.wikiPluginSource@
-- ('codeScript'), and the wiki's runtime (@web\/plugin\/runtime.js@),
-- which starts that code as a dedicated worker and passes messages
-- between the worker and the port the wiki's page gives it. The answer's
-- policy ('Wiki.Api.ContentPolicy.pluginFramePolicy') allows the two
-- scripts by their SHA-256 and no other, starts with the sandbox, so the
-- document has an opaque origin wherever it is shown, and lets the code
-- connect to the hosts its manifest names and nowhere else. What
-- Chromium does under it was tried before the design (Chromium 153): no
-- cookie, storage, IndexedDB or page of the wiki's is reached; a worker
-- has no @RTCPeerConnection@, where the frame's page would have one that
-- reaches any host; an endless loop in the worker stops neither the
-- frame nor the page; and removing the frame ends the worker.
--
-- The bundle is pinned twice. The server reads the bundle's file and
-- checks it against the hash the plugin was installed with before it
-- serves it: a file changed on the disk is answered 409, and the frame
-- runs nothing. And the browser runs the line that holds the code only if
-- its hash is the one the policy names, which the server computed from
-- those checked bytes.
--
-- Who gets the document ('Wiki.Server.Storage.pluginFrameBundle'): a
-- signed-in user who may run the plugin now, its bundle being the current
-- one; anyone else gets 404. A frame's load comes from the wiki's page
-- and carries the session's cookie. A frame that navigates itself to
-- another plugin's frame comes from an opaque origin, another site, and
-- carries none. Where the browser says what loads the document
-- (@Sec-Fetch-Dest@, @Sec-Fetch-Site@, sent in a secure context), only a
-- frame of the wiki's own page is answered: the address opened in a tab,
-- or loaded by another site, is 404 too.
module Wiki.Server.PluginFrame
  ( pluginFrameApplication
  , frameDocument
  , frameScripts
  , runtimeScript
  , scriptHash
  , codeScript
  , wikiPluginModule
  , wikiPluginApplication
  )
where

import Data.Aeson (encode)
import Data.ByteArray.Encoding (Base (..), convertToBase)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.List (lookup)
import Data.Text qualified as T
import Network.HTTP.Types (hCacheControl, hContentType, hETag, hIfNoneMatch, methodGet, methodHead, status200, status304, status404, status405, status409)
import Network.Wai (Application, Request (..), responseLBS)
import Protolude
import System.Directory (doesFileExist)
import Wiki.Api.ContentPolicy (pluginFramePolicy)
import Wiki.Api.Plugins (parsePluginId)
import Wiki.Api.Plugins.External (ExternalManifest (..), diagnosticsSegment, strictBundle)
import Wiki.Core.Id (BlobId (..))
import Wiki.Server.Sha256 (sha256)
import Wiki.Server.Storage (Owner (..), Storage, Who, pluginFrameBundle)
import Wiki.Server.Storage.Blobs (blobOf)

-- | The wiki's runtime, as @web\/plugin\/runtime.js@ holds it when the
-- server is compiled.
runtimeScript :: Text
runtimeScript = $(makeRelativeToProject "../web/plugin/runtime.js" >>= embedStringFile)

-- | The module a plugin built with a bundler imports,
-- @web\/plugin\/wiki-plugin.js@ as it is when the server is compiled
-- ("Wiki.App.Guide"'s page "Writing a plugin" says where it is).
wikiPluginModule :: Text
wikiPluginModule = $(makeRelativeToProject "../web/plugin/wiki-plugin.js" >>= embedStringFile)

-- | @\/plugin\/wiki-plugin.js@: 'wikiPluginModule', to download, for anyone.
-- It is the wiki's own code, the same on every server of this version.
wikiPluginApplication :: Application
wikiPluginApplication request respond
  | requestMethod request `notElem` [methodGet, methodHead] =
      respond (responseLBS status405 [(hContentType, "text/plain; charset=utf-8"), ("Allow", "GET, HEAD")] "The module is fetched with GET or HEAD.\n")
  | otherwise =
      respond
        ( responseLBS
            status200
            [ (hContentType, "text/javascript; charset=utf-8")
            , ("Content-Disposition", "attachment; filename=\"wiki-plugin.js\"")
            , ("X-Content-Type-Options", "nosniff")
            , (hCacheControl, "no-cache")
            ]
            (BSL.fromStrict (encodeUtf8 wikiPluginModule))
        )

-- | The script that holds a plugin's code: the code as a JSON string, in
-- @self.wikiPluginSource@, for the runtime to start, and whether the code
-- asks for strict mode, in @self.wikiPluginStrict@
-- ('Wiki.Api.Plugins.External.strictBundle'), since the runtime puts its
-- own line in front of the code. Every @<@ is written @\\u003c@, so
-- nothing in the code can end the script element.
codeScript :: Text -> Text
codeScript code =
  "self.wikiPluginSource = "
    <> T.replace "<" "\\u003c" (decodeUtf8 (BSL.toStrict (encode code)))
    <> "; self.wikiPluginStrict = "
    <> (if strictBundle code then "true" else "false")
    <> ";"

-- | The document's two scripts, in their order: the plugin's code, then
-- the runtime.
frameScripts :: Text -> [Text]
frameScripts code = [codeScript code, runtimeScript]

-- | The frame's document: the two scripts ('frameScripts'), with nothing
-- around them that a reader sees.
frameDocument :: Text -> Text
frameDocument code =
  "<!DOCTYPE html><html><head><meta charset=\"utf-8\"></head><body>"
    <> mconcat ["<script>" <> s <> "</script>" | s <- frameScripts code]
    <> "</body></html>"

-- | The base64 of a script's SHA-256, as a policy names it: the hash of
-- its text in UTF-8, which is how the browser hashes an inline script.
scriptHash :: Text -> Text
scriptHash = decodeUtf8 . convertToBase @ByteString Base64 . sha256 . encodeUtf8

-- | Serve a plugin's frame to whoever asks, at the address's segments after
-- @plugin-frame@.
pluginFrameApplication :: Storage -> Who -> [Text] -> Application
pluginFrameApplication st who segments request respond
  | requestMethod request `notElem` [methodGet, methodHead] =
      respond (responseLBS status405 [(hContentType, "text/plain; charset=utf-8"), ("Allow", "GET, HEAD")] "A plugin's frame is fetched with GET or HEAD.\n")
  | not fromOwnPage = notFound
  | otherwise = case segments of
      [ownerSegment, idSegment, hashSegment, contribution]
        | contribution == diagnosticsSegment
        , Just owner <- ownerOf ownerSegment
        , Right p <- parsePluginId idSegment ->
            pluginFrameBundle st who owner p (BlobId hashSegment) >>= \case
              Nothing -> notFound
              Just (manifest, blob, file) -> serve manifest blob file
      _ -> notFound
  where
    notFound = respond (responseLBS status404 [(hContentType, "text/plain; charset=utf-8"), (hCacheControl, "private, no-cache")] "Not found\n")
    -- A header the browser sends only from a secure context: absent, it
    -- says nothing.
    sent name = lookup name (requestHeaders request)
    fromOwnPage = maybe True (== "iframe") (sent "Sec-Fetch-Dest") && maybe True (== "same-origin") (sent "Sec-Fetch-Site")
    ownerOf = \case
      "wiki" -> Just TheWiki
      "mine" -> Just Mine
      _ -> Nothing
    serve manifest blob file = do
      present <- doesFileExist file
      bytes <- if present then Just <$> BS.readFile file else pure Nothing
      case bytes of
        Just b
          | blobOf b == blob
          , Right code <- decodeUtf8' b -> do
              -- The tag is the policy's hash. The policy names both
              -- scripts by their hashes and the manifest's hosts, so a
              -- new runtime, and a replacement that keeps the bundle but
              -- changes the hosts, each make a new tag.
              let policy = pluginFramePolicy manifest.hosts (map scriptHash (frameScripts code))
                  tag = "\"" <> encodeUtf8 (T.take 32 (scriptHash policy)) <> "\""
                  headers =
                    [ (hContentType, "text/html; charset=utf-8")
                    , ("X-Content-Type-Options", "nosniff")
                    , ("Content-Security-Policy", encodeUtf8 policy)
                    , ("Referrer-Policy", "no-referrer")
                    , (hCacheControl, "private, no-cache")
                    , ("Vary", "Cookie")
                    , (hETag, tag)
                    ]
              if lookup hIfNoneMatch (requestHeaders request) == Just tag
                then respond (responseLBS status304 headers "")
                else respond (responseLBS status200 headers (BSL.fromStrict (encodeUtf8 (frameDocument code))))
        _ ->
          respond (responseLBS status409 [(hContentType, "text/plain; charset=utf-8"), (hCacheControl, "no-store")] "The plugin's code on the server does not match the hash it was installed with, so it is not run.\n")
