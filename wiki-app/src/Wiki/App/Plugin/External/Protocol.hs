-- | The plugins' protocol, version 1, as the editor speaks it with an
-- external plugin's code (WP5.84 of docs/PLAN.md, decision 69). Messages
-- are strings of JSON, sent over the 'MessagePort' the editor gave the
-- plugin's frame ("Wiki.App.Plugin.External.Frame"). The wiki's runtime in
-- the frame (@web\/plugin\/runtime.js@) passes them to and from the
-- plugin's code, which runs in a worker and calls @wikiPlugin.plugin@.
-- A message from the plugin is at most 'maxFrameMessage' characters, and
-- the plugin sends one message once connected
-- (its @ready@) and one for each message the host sends: the runtime
-- passes on nothing else, and the host refuses anything else whatever the
-- runtime did, stopping the plugin.
--
-- The host sends one message:
--
-- * @{"type": "diagnose", "id": n, "text": t}@: the text of the page being
--   edited, as last checked, under a number of the host's.
--
-- The plugin sends:
--
-- * @{"type": "ready"}@, once its code has registered what it does;
-- * @{"type": "problems", "re": n, "problems": [p]}@, the problems it found
--   in the text sent under the number @n@, each
--   @{"from": a, "to": b, "severity": "error" | "warning", "message": m}@;
-- * @{"type": "failed", "message": m}@, when its code failed (the runtime
--   says this when the worker cannot start or throws).
--
-- @from@ and @to@ are offsets into the text sent, in UTF-16 code units, as
-- JavaScript counts a string: @from@ the first unit marked, @to@ the unit
-- after the last. The editor marks whole lines, so a problem stands for
-- the lines from @from@'s to the last marked unit's. An answer that breaks
-- these rules is refused whole, never clamped: an offset outside the text
-- or inside a character written with two units, more than 'maxProblems'
-- problems, a message that is empty or longer than 'maxMessageLength'.
module Wiki.App.Plugin.External.Protocol
  ( -- * Messages
    FromPlugin (..)
  , Reported (..)
  , fromPlugin
  , diagnose

    -- * Problems
  , problemsIn
  , utf16Length

    -- * Limits
  , maxProblems
  , maxMessageLength
  , maxFrameMessage
  , startWithin
  , answerWithin
  )
where

import Control.Monad.Fail (fail)
import Data.Aeson (Object, Value, object, withObject, (.:), (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (Parser, parseEither)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (NominalDiffTime)
import Protolude
import Wiki.Core.Markdown.Problems (Problem (..), ProblemKind (..), Severity (..))

-- | 'FromPlugin' is a message from a plugin, as 'fromPlugin' read it.
data FromPlugin
  = -- | The plugin's code has registered what it does: it may be asked.
    Ready
  | -- | The problems found in the text sent under this number, as the
    -- plugin reported them.
    Problems Int [Reported]
  | -- | The plugin's code failed, with the browser's or its own message.
    Failed Text
  deriving stock (Eq, Show)

-- | 'Reported' is a problem as a plugin reports it: UTF-16 offsets into the
-- text it was sent, how much it matters, and its message.
data Reported = Reported
  { from :: Int
  , to :: Int
  , severity :: Severity
  , message :: Text
  }
  deriving stock (Eq, Show)

-- | The message a JSON value from a plugin is, or why it is none of the
-- protocol's. Fields the protocol does not name are ignored.
fromPlugin :: Value -> Either Text FromPlugin
fromPlugin = first (\why -> "it sent a message the wiki cannot read (" <> toS why <> ").") . parseEither message
  where
    message = withObject "a message" $ \o ->
      (o .: "type" :: Parser Text) >>= \case
        "ready" -> pure Ready
        "problems" -> Problems <$> number o "re" <*> (o .: "problems" >>= traverse reported)
        "failed" -> Failed <$> o .: "message"
        other -> fail ("no message has the type " <> toS other)
    reported = withObject "a problem" $ \o ->
      Reported
        <$> number o "from"
        <*> number o "to"
        <*> (o .: "severity" >>= severityOf)
        <*> o .: "message"
    severityOf :: Text -> Parser Severity
    severityOf = \case
      "error" -> pure Error
      "warning" -> pure Warning
      other -> fail ("a problem's severity is error or warning, not " <> toS other)
    number :: Object -> Text -> Parser Int
    number o k = o .: Key.fromText k

-- | The message that asks a plugin for the problems of a text, under a
-- number of the host's.
diagnose :: Int -> Text -> Value
diagnose n text = object ["type" .= ("diagnose" :: Text), "id" .= n, "text" .= text]

-- | The most problems a plugin reports in one answer.
maxProblems :: Int
maxProblems = 500

-- | The most characters of a problem's message.
maxMessageLength :: Int
maxMessageLength = 300

-- | The longest message a plugin may send, in characters (UTF-16 units in
-- the JavaScript, which checks it first; characters in the program, never
-- more), 1 MiB. Five hundred problems of the longest message fit, each
-- character escaped, with room to spare.
maxFrameMessage :: Int
maxFrameMessage = 1024 * 1024

-- | How long a plugin has to say it is ready, in seconds: its frame's
-- document is loaded, and its code started in a worker.
startWithin :: NominalDiffTime
startWithin = 20

-- | How long a plugin has to answer an ask, in seconds.
answerWithin :: NominalDiffTime
answerWithin = 10

-- | The length of a text in UTF-16 code units, as JavaScript counts it.
utf16Length :: Text -> Int
utf16Length = T.foldl' (\n c -> n + units c) 0

units :: Char -> Int
units c = if ord c > 0xFFFF then 2 else 1

-- | The problems a plugin's report stands for in the text it was sent,
-- each on the lines it marks, its message after the plugin's name; or why
-- the report is refused.
problemsIn :: Text -> Text -> [Reported] -> Either Text [Problem]
problemsIn name text reported = do
  when (length reported > maxProblems) $
    Left ("it reported more than " <> show maxProblems <> " problems.")
  for_ reported $ \r -> do
    when (T.null (T.strip r.message) || T.length r.message > maxMessageLength) $
      Left ("it reported a problem whose message is not 1 to " <> show maxMessageLength <> " characters.")
    when (r.from < 0 || r.to < r.from) $
      Left ("it reported a problem from " <> show r.from <> " to " <> show r.to <> ", which is no range of the text.")
  places <- placesOf text (concat [[r.from, r.to] | r <- reported])
  pure
    [ Problem {from = opening.line, to = lastLine, kind = FromPlugin, severity = r.severity, message = name <> ": " <> r.message}
    | r <- reported
    , let opening = Map.findWithDefault (Place 1 False) r.from places
          end = Map.findWithDefault (Place 1 False) r.to places
          -- The line of the last unit marked: the line before the end's
          -- when the last unit is a line break.
          lastLine = if r.to > r.from && end.afterBreak then end.line - 1 else end.line
    ]

-- | 'Place' is where an offset is: the line it is on (from 1), and
-- whether the unit before it is a line break.
data Place = Place {line :: Int, afterBreak :: Bool}

-- | The place of each offset, in one walk through the text; or why an
-- offset is not one of the text's: past its end, or between the two units
-- of a character written with two.
placesOf :: Text -> [Int] -> Either Text (Map Int Place)
placesOf text offsets = go (sort (ordNub offsets)) 0 (Place 1 False) (toS text)
  where
    go :: [Int] -> Int -> Place -> [Char] -> Either Text (Map Int Place)
    go [] _ _ _ = Right mempty
    go wanted@(o : rest) at here cs
      | o == at = Map.insert o here <$> go rest at here cs
      | otherwise = case cs of
          [] -> Left ("it reported the offset " <> show o <> ", past the end of the text, " <> show at <> " units long.")
          c : more
            | o < at + units c -> Left ("it reported the offset " <> show o <> ", inside a character written with two UTF-16 units.")
            | otherwise -> go wanted (at + units c) (if c == '\n' then Place (here.line + 1) True else Place here.line False) more
