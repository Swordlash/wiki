{-# LANGUAGE DeriveAnyClass #-}

-- | External plugins: code someone else wrote, uploaded to the wiki and run
-- in a sandboxed frame (WP5.84 of docs/PLAN.md, decision 69, issue #186).
-- This module holds what the server and the application agree on: the
-- plugin's manifest and the rules it is checked by ('parseManifest'), the
-- rules of its code ('checkBundle'), who may add plugins ('ExternalPolicy'),
-- and what @GET api\/v1\/external-plugins@ answers ('ExternalPlugins').
--
-- An external plugin is two files, sent in one upload: a manifest, JSON
-- that says what the plugin is and what it asks for, and a bundle, one
-- JavaScript file with all of its code. The server keeps the bundle in its
-- blob store, named by the SHA-256 of its bytes, and serves it only inside
-- the plugin's frame ("Wiki.Server.PluginFrame"), whose policy names that
-- hash: a bundle changed after it was installed does not run.
--
-- The first version runs @diagnostics@ only (the owner, 2026-11-24): the
-- plugin's code runs in a dedicated worker inside the frame, where it has
-- no page to draw on, no WebRTC, and no network but the hosts its manifest
-- names. A contribution with a panel of its own (an @agent@) needs a page,
-- where browsers cannot stop WebRTC, and waits until it can be confined.
module Wiki.Api.Plugins.External
  ( -- * The manifest
    ExternalManifest (..)
  , Contribution (..)
  , contributionName
  , Permission (..)
  , permissionName
  , parseManifest
  , manifestFromValue
  , manifestFormat
  , supportedApis
  , maxManifestBytes
  , maxHosts
  , maxNameLength
  , maxDescriptionLength
  , reservedPluginIds

    -- * The bundle
  , checkBundle
  , maxBundleBytes
  , strictBundle
  , prologueWindow

    -- * Who may add plugins
  , ExternalPolicy (..)
  , externalPolicyName
  , ExternalPolicyChange (..)

    -- * What is installed
  , ExternalPlugin (..)
  , ExternalPlugins (..)
  , maxWikiPlugins
  , maxOwnPlugins
  , framePath
  , frameSegment
  , diagnosticsSegment
  )
where

import Control.Monad.Fail (fail)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), eitherDecodeStrict', object, withText, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Char (GeneralCategory (Space), generalCategory)
import Data.Scientific (toBoundedInteger)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Vector qualified as V
import Protolude
import Wiki.Api.ContentPolicy (parseAssistantHost)
import Wiki.Api.Plugins (PluginId (..), parsePluginId)

--------------------------------------------------------------------------------
-- The manifest

-- | 'ExternalManifest' is what an external plugin says of itself, as
-- 'parseManifest' checked it: its id, its name and version as the settings
-- show them, a line of description, the version of the plugins' protocol
-- it speaks ('supportedApis'), what it contributes, the permissions it
-- asks for, and the hosts its code may connect to. The hosts are origins
-- (@https:\/\/api.example.org@), as a Content-Security-Policy names them:
-- the frame's policy lets the plugin reach these and nothing else.
data ExternalManifest = ExternalManifest
  { id :: PluginId
  , name :: Text
  , version :: Text
  , description :: Text
  , api :: Int
  , contributes :: Set Contribution
  , permissions :: Set Permission
  , hosts :: [Text]
  }
  deriving stock (Eq, Show)

-- | 'Contribution' is a kind of thing an external plugin gives the wiki. The
-- first version has one: problems in the text being edited.
data Contribution = Diagnostics
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | A contribution's name, as a manifest writes it.
contributionName :: Contribution -> Text
contributionName Diagnostics = "diagnostics"

-- | 'Permission' is what a plugin may ask of the wiki. @document.read@ is
-- the text of the page being edited, and only while its editor is open;
-- @diagnostics@ needs it.
data Permission = DocumentRead
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | A permission's name, as a manifest writes it.
permissionName :: Permission -> Text
permissionName DocumentRead = "document.read"

-- | What each contribution needs.
needs :: Contribution -> Set Permission
needs Diagnostics = Set.singleton DocumentRead

-- | The version of the manifest's own format, its field @manifest@.
manifestFormat :: Int
manifestFormat = 1

-- | The versions of the plugins' protocol this wiki speaks, the manifest's
-- field @api@.
supportedApis :: [Int]
supportedApis = [1]

-- | The most bytes a manifest has: 16 kB.
maxManifestBytes :: Int
maxManifestBytes = 16 * 1024

-- | The most hosts a manifest names.
maxHosts :: Int
maxHosts = 10

maxNameLength, maxDescriptionLength :: Int
maxNameLength = 80
maxDescriptionLength = 300

-- | The ids of the plugins built into the wiki, which no external plugin
-- may take. The application's registry ("Wiki.App.Plugin.Registry")
-- holds the built-in plugins themselves, so a plugin built in later adds
-- its id here too.
reservedPluginIds :: Set PluginId
reservedPluginIds = Set.fromList (map PluginId ["assistant", "errors"])

-- | The manifest these bytes are, or why they are none. They are at most
-- 'maxManifestBytes' bytes of one JSON object; 'manifestFromValue' says
-- what the object holds.
parseManifest :: ByteString -> Either Text ExternalManifest
parseManifest bytes
  | BS.length bytes > maxManifestBytes = Left ("A plugin's manifest is at most " <> show maxManifestBytes <> " bytes (16 kB).")
  | otherwise = case eitherDecodeStrict' bytes of
      Left e -> Left ("A plugin's manifest is JSON: " <> toS e)
      Right v -> manifestFromValue v

-- | The manifest a JSON value is, or why it is none. The value is an
-- object with these fields and no others, so that a manifest written for
-- a later version is refused rather than half read:
--
-- * @manifest@: 1 ('manifestFormat');
-- * @id@: a plugin's id ('parsePluginId'), not one of the wiki's own
--   ('reservedPluginIds');
-- * @name@: 1 to 80 characters, on one line;
-- * @version@: three numbers, @1.0.0@;
-- * @description@ (may be left out): at most 300 characters, on one line;
-- * @api@: a version of the protocol this wiki speaks ('supportedApis');
-- * @contributes@: an object with at least one contribution, each with an
--   empty object: @{"diagnostics": {}}@;
-- * @permissions@ (may be left out): what the contributions need, and
--   nothing they do not;
-- * @hosts@ (may be left out): at most 'maxHosts' origins, each as
--   'parseAssistantHost' takes it (@https:\/\/host@, @http:\/\/host:port@,
--   a host starting with @*.@ for its subdomains).
manifestFromValue :: Value -> Either Text ExternalManifest
manifestFromValue = \case
  Object o -> do
    let fields = map Key.toText (KeyMap.keys o)
        known = ["manifest", "id", "name", "version", "description", "api", "contributes", "permissions", "hosts"]
        field k = KeyMap.lookup (Key.fromText k) o
    for_ (sort fields) $ \f -> unless (f `elem` known) (Left ("A plugin's manifest has no field " <> f <> ": its fields are " <> T.intercalate ", " known <> "."))
    format <- required "manifest" (field "manifest") >>= int "manifest"
    unless (format == manifestFormat) (Left ("This wiki reads manifests of format " <> show manifestFormat <> " (the field manifest), not " <> show format <> "."))
    pid <- required "id" (field "id") >>= string "id" >>= either (\why -> Left ("The manifest's id: " <> why)) Right . parsePluginId
    when (Set.member pid reservedPluginIds) (Left ("The id " <> pid.text <> " is a plugin's built into the wiki: choose another."))
    name <- required "name" (field "name") >>= string "name" >>= oneLine "name" 1 maxNameLength
    version <- required "version" (field "version") >>= string "version" >>= versionOf
    description <- maybe (Right "") (string "description" >=> oneLine "description" 0 maxDescriptionLength) (field "description")
    api <- required "api" (field "api") >>= int "api"
    unless (api `elem` supportedApis) (Left ("The plugin speaks version " <> show api <> " of the plugins' protocol, and this wiki speaks " <> T.intercalate ", " (map show supportedApis) <> "."))
    contributes <- required "contributes" (field "contributes") >>= contributionsOf
    permissions <- maybe (Right Set.empty) permissionsOf (field "permissions")
    let wanted = foldMap needs contributes
    for_ (Set.toList (wanted `Set.difference` permissions)) $ \p ->
      Left ("The plugin's contributions need the permission " <> permissionName p <> ": add it to permissions.")
    for_ (Set.toList (permissions `Set.difference` wanted)) $ \p ->
      Left ("The permission " <> permissionName p <> " is one no contribution of the plugin uses: leave it out.")
    hosts <- maybe (Right []) hostsOf (field "hosts")
    pure ExternalManifest {id = pid, name, version, description, api, contributes, permissions, hosts}
  _ -> Left "A plugin's manifest is a JSON object."
  where
    required k = maybe (Left ("A plugin's manifest has the field " <> k <> ".")) Right
    string k = \case
      String t -> Right t
      _ -> Left ("The manifest's " <> k <> " is a string.")
    int k = \case
      Number n | Just i <- toBoundedInteger n -> Right i
      _ -> Left ("The manifest's " <> k <> " is a whole number.")
    oneLine k lo hi t
      | T.any isControl t = Left ("The manifest's " <> k <> " is one line, with no control characters.")
      | T.length stripped < lo || T.length stripped > hi = Left ("The manifest's " <> k <> " is " <> show lo <> " to " <> show hi <> " characters.")
      | otherwise = Right stripped
      where
        stripped = T.strip t
    versionOf t = case T.splitOn "." t of
      parts@[_, _, _] | all (\p -> not (T.null p) && T.length p <= 9 && T.all isDigit p) parts -> Right t
      _ -> Left ("The manifest's version is three numbers, as 1.0.0, not " <> t <> ".")
    contributionsOf = \case
      Object c -> do
        given <- for (map (first Key.toText) (KeyMap.toList c)) $ \(k, v) -> do
          contribution <- case find (\x -> contributionName x == k) [minBound .. maxBound] of
            Just x -> Right x
            Nothing
              | k == "agent" -> Left "An external plugin cannot contribute an agent yet: a panel of its own would run where browsers cannot stop it sending what it reads elsewhere. Only diagnostics are supported."
              | otherwise -> Left ("A plugin contributes diagnostics, not " <> k <> ".")
          case v of
            Object inner | KeyMap.null inner -> Right contribution
            _ -> Left ("The contribution " <> k <> " has an empty object: {}.")
        when (null given) (Left "A plugin contributes something: give contributes {\"diagnostics\": {}}.")
        Right (Set.fromList given)
      _ -> Left "The manifest's contributes is an object, as {\"diagnostics\": {}}."
    permissionsOf = \case
      Array a -> do
        given <- for (V.toList a) $ \case
          String t -> maybe (Left ("A plugin asks for document.read, not " <> t <> ".")) Right (find (\p -> permissionName p == t) [minBound .. maxBound])
          _ -> Left "The manifest's permissions are strings."
        let set = Set.fromList given
        when (Set.size set /= length given) (Left "The manifest names each permission once.")
        Right set
      _ -> Left "The manifest's permissions are a list."
    hostsOf = \case
      Array a -> do
        given <- for (V.toList a) $ \case
          String t -> first (const ("The manifest's host " <> t <> " is not an origin: write it as https://host, or http://host:port.")) (parseAssistantHost t)
          _ -> Left "The manifest's hosts are strings."
        when (length given > maxHosts) (Left ("A plugin names at most " <> show maxHosts <> " hosts."))
        let distinct = ordNub given
        when (length distinct /= length given) (Left "The manifest names each host once.")
        Right distinct
      _ -> Left "The manifest's hosts are a list."

-- | The manifest as JSON, in the form 'manifestFromValue' reads.
instance ToJSON ExternalManifest where
  toJSON m =
    object
      [ "manifest" .= manifestFormat
      , "id" .= m.id
      , "name" .= m.name
      , "version" .= m.version
      , "description" .= m.description
      , "api" .= m.api
      , "contributes" .= object [Key.fromText (contributionName c) .= object [] | c <- Set.toList m.contributes]
      , "permissions" .= map permissionName (Set.toList m.permissions)
      , "hosts" .= m.hosts
      ]

instance FromJSON ExternalManifest where
  parseJSON = either (fail . toS) pure . manifestFromValue

--------------------------------------------------------------------------------
-- The bundle

-- | The most bytes a plugin's bundle has: 2 MiB (2 × 1024 × 1024).
maxBundleBytes :: Int
maxBundleBytes = 2 * 1024 * 1024

-- | The text of a plugin's bundle, or why these bytes are none: one
-- JavaScript file of at most 'maxBundleBytes' bytes, in UTF-8, with more
-- than white space in it. The bytes are kept as they are sent. The frame
-- carries the code as a JSON string, with @<@ escaped, so nothing in it can
-- end the frame's script element ("Wiki.Server.PluginFrame").
checkBundle :: ByteString -> Either Text Text
checkBundle bytes
  | BS.length bytes > maxBundleBytes = Left ("A plugin's bundle is at most " <> show maxBundleBytes <> " bytes (2 MiB).")
  | otherwise = case decodeUtf8' bytes of
      Left _ -> Left "A plugin's bundle is JavaScript in UTF-8."
      Right t
        | T.null (T.strip t) -> Left "A plugin's bundle is empty."
        | otherwise -> Right t

-- | Whether a plugin's bundle asks to run in strict mode: whether its
-- directive prologue, the string literal statements it begins with, holds
-- a @"use strict"@ directive.
--
-- The worker runs one script, the wiki's own line and then the bundle on
-- the same line, so that the bundle's line numbers are its own (the
-- runtime, @web\/plugin\/runtime.js@). A directive of the bundle's is then
-- no longer at the start of the script, and the runtime begins the script
-- with @"use strict";@ when this says so ('Wiki.Server.PluginFrame' tells
-- it). The script runs no other code than the installed bundle's (decision
-- 69: no @blob:@ or @eval@ in the frame's @script-src@).
--
-- This is the smallest lexer of the bundle's start, by the standard's
-- rules for a script. It skips white space, line terminators and comments
-- (and a @#!@ line at the very start), then reads string literals in
-- single or double quotes, with their escapes and line continuations,
-- each a statement when a @;@ follows, or the end of the code, or a line
-- terminator after which the next token cannot go on with the expression
-- (automatic semicolon insertion). A literal is the directive when its
-- text between the quotes is exactly @use strict@: an escape in it makes
-- it another one. It stops at the first thing that is none of these, and
-- wherever it is in doubt, the prologue ends there and what is not yet a
-- directive is none. It reads no further than the prologue, and never
-- past the first 'prologueWindow' characters: a prologue not ended by
-- then gives no directive after that point.
strictBundle :: Text -> Bool
strictBundle code = statements False (skipShebang (T.unpack (T.take prologueWindow code)))
  where
    truncated = T.compareLength code prologueWindow == GT
    skipShebang = \case
      '#' : '!' : rest -> dropWhile (not . isTerminator) rest
      rest -> rest
    statements found input = case gap input of
      Nothing -> found
      Just (_, rest@(q : _))
        | q == '"' || q == '\'' -> case literal rest of
            Nothing -> found
            Just (raw, after) -> case ended after of
              Ended next -> statements (found || raw == "use strict") next
              EndedLast -> found || raw == "use strict"
              Continued -> found
      Just _ -> found
    -- After a string literal: whether its statement ends there.
    ended after = case gap after of
      Nothing -> Continued
      Just (newline, rest) -> case rest of
        [] -> if truncated then Continued else EndedLast
        ';' : next -> Ended next
        c : _
          | not newline -> Continued
          | c == '"' || c == '\'' -> Ended rest
          | c `elem` ("([.,?+-*/%=<>&|^`" :: [Char]) -> Continued
          | isLetter c || c `elem` ("_$\\" :: [Char]) ->
              if takeWhile (\x -> isAlphaNum x || x `elem` ("_$" :: [Char])) rest `elem` ["in", "instanceof"] then Continued else EndedLast
          | isDigit c || c `elem` ("{}!~#@" :: [Char]) -> EndedLast
          | otherwise -> Continued
    -- White space, line terminators and comments skipped: whether a line
    -- terminator was among them, and what follows; nothing for a comment
    -- never closed.
    gap = go False
      where
        go newline = \case
          c : rest
            | isTerminator c -> go True rest
            | isWhite c -> go newline rest
          '/' : '/' : rest -> go newline (dropWhile (not . isTerminator) rest)
          '/' : '*' : rest -> case closing rest of
            Nothing -> Nothing
            Just (body, after) -> go (newline || any isTerminator body) after
          rest -> Just (newline, rest)
        closing = search []
          where
            search acc = \case
              '*' : '/' : after -> Just (reverse acc, after)
              c : more -> search (c : acc) more
              [] -> Nothing
    -- A string literal: its text between the quotes, as written, and what
    -- follows; nothing when it is not closed, or a line ends in it.
    literal = \case
      q : rest -> go q [] rest
      [] -> Nothing
      where
        go q acc = \case
          c : after | c == q -> Just (reverse acc, after)
          '\\' : '\r' : '\n' : more -> go q ('\n' : '\r' : '\\' : acc) more
          '\\' : c : more -> go q (c : '\\' : acc) more
          c : more
            | c == '\n' || c == '\r' -> Nothing
            | otherwise -> go q (c : acc) more
          [] -> Nothing
    isTerminator c = c `elem` ("\n\r\x2028\x2029" :: [Char])
    isWhite c = c `elem` ("\t\v\f \xA0\xFEFF" :: [Char]) || generalCategory c == Space

-- | How far into a plugin's bundle 'strictBundle' reads, in characters:
-- 64 Ki.
prologueWindow :: Int
prologueWindow = 64 * 1024

-- | Whether the statement of a string literal ended after it: with the
-- rest to read on, with nothing more to read, or not (it goes on, or it
-- is in doubt).
data Ending = Ended [Char] | EndedLast | Continued

--------------------------------------------------------------------------------
-- Who may add plugins

-- | 'ExternalPolicy' is who may add external plugins, as the administrator
-- sets it: nobody, and none runs ('ExternalOff', what a wiki starts with);
-- the administrator, for everyone ('ExternalWiki'); or the administrator
-- for everyone and each user for themself ('ExternalUsers'). In JSON:
-- @"off"@, @"wiki"@, @"users"@. Plugins are kept when the policy narrows,
-- and run again when it widens.
data ExternalPolicy = ExternalOff | ExternalWiki | ExternalUsers
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | A policy's name, in JSON and in the setting that keeps it.
externalPolicyName :: ExternalPolicy -> Text
externalPolicyName = \case
  ExternalOff -> "off"
  ExternalWiki -> "wiki"
  ExternalUsers -> "users"

instance ToJSON ExternalPolicy where
  toJSON = String . externalPolicyName

instance FromJSON ExternalPolicy where
  parseJSON = withText "ExternalPolicy" $ \t ->
    maybe (fail ("not a policy of external plugins (off, wiki or users): " <> toS t)) pure (find (\p -> externalPolicyName p == t) [minBound .. maxBound])

-- | 'ExternalPolicyChange' is the body that sets the policy
-- (@PUT api\/v1\/external-plugins\/policy@).
newtype ExternalPolicyChange = ExternalPolicyChange {policy :: ExternalPolicy}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- What is installed

-- | 'ExternalPlugin' is an installed external plugin: its manifest, the
-- SHA-256 of its bundle in lowercase hex (the blob it is kept as), the
-- bundle's size in bytes, whether it is the asker's own ('mine') or the
-- wiki's, installed by the administrator for everyone, and when it was
-- installed or last replaced.
data ExternalPlugin = ExternalPlugin
  { manifest :: ExternalManifest
  , bundle :: Text
  , size :: Int
  , mine :: Bool
  , installedAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | 'ExternalPlugins' is what @GET api\/v1\/external-plugins@ answers:
-- the policy, and the plugins the asker may run, the wiki's first, then
-- their own, each in the order of their ids. The wiki's are listed while
-- the policy is not 'ExternalOff', and always to an administrator, who
-- manages them; a user's own while it is 'ExternalUsers'. Someone not
-- signed in gets the policy and no plugins. A user's own plugin with the
-- id of one of the wiki's is listed, and its frame is not served: the
-- wiki's runs in its place.
data ExternalPlugins = ExternalPlugins
  { policy :: ExternalPolicy
  , external :: [ExternalPlugin]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | The most plugins the wiki installs for everyone.
maxWikiPlugins :: Int
maxWikiPlugins = 50

-- | The most plugins of their own a user adds.
maxOwnPlugins :: Int
maxOwnPlugins = 20

-- | The first segment of a plugin frame's address.
frameSegment :: Text
frameSegment = "plugin-frame"

-- | The last segment of the address of a frame that runs a plugin's
-- diagnostics.
diagnosticsSegment :: Text
diagnosticsSegment = "diagnostics"

-- | The address of the frame that runs a plugin's diagnostics, relative to
-- the wiki's root:
-- @plugin-frame\/\<wiki|mine>\/\<id>\/\<bundle's hash>\/diagnostics@. The
-- hash is the bundle's, so a frame of a version replaced since is not
-- served ("Wiki.Server.PluginFrame").
framePath :: ExternalPlugin -> Text
framePath p = T.intercalate "/" [frameSegment, if p.mine then "mine" else "wiki", p.manifest.id.text, p.bundle, diagnosticsSegment]
