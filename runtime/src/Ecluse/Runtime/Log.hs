-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The structured-logging pipeline.

Écluse sits in the install path of someone else's build, so when it refuses a
package or runs slow the operator must see /why/ from the logs alone. This module
stands up a @katip@ 'LogEnv' (the single log stream every layer attaches context
to), chooses its on-the-wire shape, and sets the severity it admits:

* __'JsonLog'__ writes __one compact JSON object per line__ to stdout (JSONL): the
  whole physical line /is/ the JSON, with no pretty-printing and no level or
  timestamp prefix outside the object, and any newline inside a field escaped as
  @\\n@ so a record never spans two lines. This is the in-container default, the
  shape a log collector's stdout JSON autodiscovery consumes directly.
* __'ConsoleLog'__ writes the human-readable bracketed form for local development.

A 'LogEnv' built here carries no colour codes even on a terminal, so a captured
JSON line is always valid JSON. The format and the 'LogLevel' are parsed from
@ECLUSE_OBSERVABILITY__LOG_FORMAT@ and @ECLUSE_OBSERVABILITY__LOG_LEVEL@ at the
configuration boundary (@Ecluse.Config@) and the resulting 'LogEnv' is held in the
composition root ("Ecluse.Runtime.Env").

== The JSON line

'JsonLog' renders the shape a Datadog-class collector reads without a custom
pipeline, using that vendor's reserved log attributes ('jsonLine'):

* @timestamp@ (RFC 3339 UTC), @status@ (@debug@ \/ @info@ \/ @warn@ \/ @error@,
  mapped from the @katip@ severity by 'severityStatus'), and @message@;
* @service@, @env@, and @version@: the unified-service identity, resolved once at
  boot ("Ecluse.Runtime.Telemetry.Resolve") and handed to the formatter, so the
  identity stamps every line rather than only the lines raised inside a request;
* @dd.trace_id@ and @dd.span_id@ when a span is in scope, read from the log site's
  own @dd@ payload ('ddField') in the id format Datadog correlates on
  ('formatDdTraceId');
* @data@: the per-call structured payload, unchanged;
* @katip@: the emitter's namespace, application, host, process, thread, and source
  location.

== Secrets

A bearer token is carried as the redacted @Secret@ of "Ecluse.Core.Credential", whose
'Show' renders only a placeholder, so token material cannot reach a log field
through any structured payload or message built from it (see
@docs\/architecture\/observability.md@). A URL is reduced to its host and port
before it names anything in a log line or a span
('Ecluse.Core.Security.Authority.authorityLabel'), so userinfo and a pre-signed
query string cannot ride a location into the stream. This module adds no field that
would defeat either.

The model is described in @docs\/architecture\/observability.md@ → "Logs".
-}
module Ecluse.Runtime.Log (
    -- * Log format
    LogFormat (..),
    parseLogFormat,

    -- * Log level
    LogLevel (..),
    parseLogLevel,
    severityFloor,
    severityStatus,

    -- * Pipeline construction
    newLogEnv,
    newScribe,
    formatterFor,

    -- * Structured context
    moduleField,

    -- * Datadog trace correlation
    DdContext (..),
    DdSpan (..),
    ddField,
    ddObject,
    formatDdTraceId,
    formatDdSpanId,
) where

import Data.Aeson (Value (Object, String), object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Text (encodeToLazyText)
import Data.ByteString qualified as BS
import Data.Text.Lazy.Builder qualified as TB
import Katip (
    ColorStrategy (ColorLog),
    Environment,
    Item (..),
    LogEnv,
    LogItem,
    Namespace (Namespace),
    Scribe,
    Severity (AlertS, CriticalS, DebugS, EmergencyS, ErrorS, InfoS, NoticeS, WarningS),
    SimpleLogPayload,
    Verbosity (V2),
    defaultScribeSettings,
    initLogEnv,
    itemJson,
    permitItem,
    registerScribe,
    sl,
    unLogStr,
 )
import Katip.Scribes.Handle (ItemFormatter, bracketFormat, mkHandleScribeWithFormatter)

import Ecluse.Core.Wire (WireVocab (..), parseWire)

{- | The on-the-wire shape of the log stream, selected by configuration. A sum
type rather than a 'Bool' so each case names its intent and a new shape is a new
constructor, not a second flag.
-}
data LogFormat
    = {- | One compact JSON object per line to stdout (JSONL) -- the in-container
      default a log collector's stdout JSON parsing consumes.
      -}
      JsonLog
    | -- | The human-readable bracketed form, for local development.
      ConsoleLog
    deriving stock (Eq, Show)

-- The wire vocabulary of a 'LogFormat': the single source both 'parseWire' and
-- the accepted-set message derive from for this type.
instance WireVocab LogFormat where
    wireKind = "log format"
    wireTable =
        (JsonLog, "json")
            :| [(ConsoleLog, "console")]

{- | Parse a 'LogFormat' from its wire name, naming the accepted set on failure.
The same strict, fail-loud style as the other configuration enums
(@Ecluse.Config@).

>>> parseLogFormat "json"
Right JsonLog

>>> parseLogFormat "yaml"
Left "unknown log format \"yaml\" (expected one of: json, console)"
-}
parseLogFormat :: Text -> Either Text LogFormat
parseLogFormat = parseWire

{- | The lowest severity the stream keeps, selected by configuration. The four
values are the ones the rendered @status@ field speaks ('severityStatus'), so the
level an operator sets and the value they filter on in a log backend are the same
vocabulary.
-}
data LogLevel
    = -- | Keep everything, the per-decision diagnostics included.
      DebugLevel
    | -- | The default: normal runtime conditions and worse.
      InfoLevel
    | -- | Warnings and worse.
      WarnLevel
    | -- | Errors alone.
      ErrorLevel
    deriving stock (Eq, Ord, Show)

-- The wire vocabulary of a 'LogLevel', listed from most to least verbose so the
-- accepted-set message reads as a ladder.
instance WireVocab LogLevel where
    wireKind = "log level"
    wireTable =
        (DebugLevel, "debug")
            :| [ (InfoLevel, "info")
               , (WarnLevel, "warn")
               , (ErrorLevel, "error")
               ]

{- | Parse a 'LogLevel' from its wire name, naming the accepted set on failure, the
same strict style as 'parseLogFormat'.

>>> parseLogLevel "warn"
Right WarnLevel

>>> parseLogLevel "trace"
Left "unknown log level \"trace\" (expected one of: debug, info, warn, error)"
-}
parseLogLevel :: Text -> Either Text LogLevel
parseLogLevel = parseWire

{- | The @katip@ 'Severity' floor a 'LogLevel' admits: the scribe keeps an item at or
above it. @katip@ orders its severities, so 'WarnLevel' keeps 'WarningS' and every
severity above it, and 'InfoLevel' keeps 'NoticeS' along with 'InfoS'.
-}
severityFloor :: LogLevel -> Severity
severityFloor = \case
    DebugLevel -> DebugS
    InfoLevel -> InfoS
    WarnLevel -> WarningS
    ErrorLevel -> ErrorS

{- | The @status@ a @katip@ 'Severity' renders as. @katip@ carries the eight syslog
severities; a log backend's status facet reads the four an operator acts on, so
'NoticeS' folds into @info@ and everything above 'ErrorS' folds into @error@.
-}
severityStatus :: Severity -> Text
severityStatus = \case
    DebugS -> "debug"
    InfoS -> "info"
    NoticeS -> "info"
    WarningS -> "warn"
    ErrorS -> "error"
    CriticalS -> "error"
    AlertS -> "error"
    EmergencyS -> "error"

{- | Build the application 'LogEnv': a @katip@ environment under the @ecluse@
namespace with a single stdout scribe in the chosen 'LogFormat', keeping every item
at or above @level@. This is the value the composition root holds and every later
layer logs through.

@logIdentity@ is the span-less @dd@ identity
('Ecluse.Runtime.Telemetry.Correlation.ddIdentity'); the JSON formatter stamps it on
every line, so a line raised outside any request scope still carries the service it
came from.
-}
newLogEnv :: LogFormat -> LogLevel -> DdContext -> Environment -> IO LogEnv
newLogEnv format level logIdentity environment = do
    scribe <- newScribe format level logIdentity
    base <- initLogEnv (Namespace ["ecluse"]) environment
    registerScribe "stdout" scribe defaultScribeSettings base

{- | Build the stdout 'Scribe' for a 'LogFormat' at a 'LogLevel'. Colour is forced
__off__ ('ColorLog' 'False') so a captured 'JsonLog' line is always valid JSON -- no
ANSI escapes leak into the object even when stdout is a terminal. The handle scribe
writes each item as exactly one line (the formatter output plus a single trailing
newline), which is what makes 'JsonLog' a true JSONL stream.
-}
newScribe :: LogFormat -> LogLevel -> DdContext -> IO Scribe
newScribe format level logIdentity =
    mkHandleScribeWithFormatter
        (formatterFor format logIdentity)
        (ColorLog False)
        stdout
        (permitItem (severityFloor level))
        V2

{- | The @katip@ 'ItemFormatter' a 'LogFormat' wires into its scribe: the one-line
JSON encoder for 'JsonLog', the bracketed human form for 'ConsoleLog'. The @dd@
identity is the one the JSON line stamps; 'ConsoleLog' does not render it.
-}
formatterFor :: (LogItem a) => LogFormat -> DdContext -> ItemFormatter a
formatterFor format logIdentity = case format of
    JsonLog -> jsonLineFormat logIdentity
    ConsoleLog -> bracketFormat

{- The JSONL encoder: one compact JSON object, no trailing newline (the handle scribe
adds it). The colourise flag is ignored because 'newScribe' forces colour off, and
wrapping the object in ANSI escapes would make the line invalid JSON. -}
jsonLineFormat :: (LogItem a) => DdContext -> ItemFormatter a
jsonLineFormat logIdentity _colourise verb logItem =
    TB.fromLazyText (encodeToLazyText (jsonLine logIdentity verb logItem))

{- The rendered JSON log line. The reserved attributes a log backend reads without a
custom pipeline sit at the top level; the emitter's own @katip@ fields are nested
under @katip@ so they cannot collide with one. The per-call payload is carried under
@data@ with its @dd@ object lifted out: the identity is already top level, and only
the span ids remain to render. -}
jsonLine :: (LogItem a) => DdContext -> Verbosity -> Item a -> Value
jsonLine logIdentity verb logItem = Object (KeyMap.fromList (reserved <> whenPresent))
  where
    katipObject :: KeyMap.KeyMap Value
    katipObject = case itemJson verb logItem of
        Object o -> o
        _ -> KeyMap.empty

    structured :: KeyMap.KeyMap Value
    structured = case KeyMap.lookup "data" katipObject of
        Just (Object o) -> o
        _ -> KeyMap.empty

    -- The identity, with the log site's own active span filled in when it installed one.
    context :: DdContext
    context = logIdentity{ddSpan = payloadSpan structured <|> ddSpan logIdentity}

    reserved :: [(Key, Value)]
    reserved =
        [ ("timestamp", toJSON (_itemTime logItem))
        , ("status", toJSON (severityStatus (_itemSeverity logItem)))
        , ("message", toJSON (TB.toLazyText (unLogStr (_itemMessage logItem))))
        , ("service", toJSON (ddService context))
        , ("env", maybe (toJSON (_itemEnv logItem)) toJSON (ddEnv context))
        , ("data", Object (KeyMap.delete "dd" structured))
        , ("katip", Object (KeyMap.filterWithKey (\key _ -> key `notElem` promoted) katipObject))
        ]

    whenPresent :: [(Key, Value)]
    whenPresent =
        catMaybes
            [ ("version",) . toJSON <$> ddVersion context
            , ("dd",) . spanObject <$> ddSpan context
            ]

    -- The @katip@ keys this line renders itself, so the nested block does not repeat them.
    promoted :: [Key]
    promoted = ["at", "data", "env", "msg", "sev"]

    spanObject :: DdSpan -> Value
    spanObject theSpan = object ["trace_id" .= ddTraceId theSpan, "span_id" .= ddSpanId theSpan]

{- The active span's ids from a log site's own @dd@ payload ('ddField'). Absent
outside a span scope, and absent for a payload whose @dd@ object carries no ids, so a
line never renders a half-filled correlation pair. -}
payloadSpan :: KeyMap.KeyMap Value -> Maybe DdSpan
payloadSpan structured = case KeyMap.lookup "dd" structured of
    Just (Object dd) -> DdSpan <$> textAt "trace_id" dd <*> textAt "span_id" dd
    _ -> Nothing
  where
    textAt :: Key -> KeyMap.KeyMap Value -> Maybe Text
    textAt key o = case KeyMap.lookup key o of
        Just (String t) -> Just t
        _ -> Nothing

{- | The structured context naming the __source module__ a log line was emitted
from, so every JSON record carries a @module@ field (e.g.
@"module":"Ecluse.Runtime.Server.Pipeline"@). Compose it into a log site's payload alongside
the event's own fields, so the stream can be filtered by emitter without leaning on
the @katip@ namespace. @katip@ renders the key into the line's @data@ object. This is
the standard tag for a log raised off the 'Handler' reader (a plain-'IO' path that
opens its own context through the composition-root 'LogEnv').
-}
moduleField :: Text -> SimpleLogPayload
moduleField = sl "module"

{- | The unified-service identity stamped onto every log line, plus the active span's
ids when one is in scope. @service@\/@env@\/@version@ come from the same resolved
telemetry identity as the traces ("Ecluse.Runtime.Telemetry.Resolve"), so logs and
traces share one identity; the trace\/span ids are present only when a span is active
(filled by "Ecluse.Runtime.Telemetry.Correlation" off the OpenTelemetry context).
-}
data DdContext = DdContext
    { ddService :: Text
    -- ^ @service@ -- the resolved service name.
    , ddEnv :: Maybe Text
    -- ^ @env@ -- the deployment environment, when configured.
    , ddVersion :: Maybe Text
    -- ^ @version@ -- the service version.
    , ddSpan :: Maybe DdSpan
    -- ^ The active span's correlation ids, when a span is in scope.
    }
    deriving stock (Eq, Show)

{- | The active span's ids, __already in the id format Datadog expects__ (see
'formatDdTraceId' \/ 'formatDdSpanId'). Held as rendered 'Text' so this type stays free
of any OpenTelemetry dependency.
-}
data DdSpan = DdSpan
    { ddTraceId :: Text
    -- ^ @dd.trace_id@ -- the trace id in Datadog form.
    , ddSpanId :: Text
    -- ^ @dd.span_id@ -- the span id in Datadog form.
    }
    deriving stock (Eq, Show)

{- | The @dd@ object as JSON: @service@ always, @env@\/@version@ when configured, and
@trace_id@\/@span_id@ only when a span is active. A log site installs it as context
('ddField'); the rendered line lifts the ids to its own @dd@ object and carries the
identity at the top level.
-}
ddObject :: DdContext -> Value
ddObject ctx =
    object $
        catMaybes
            [ Just ("service" .= ddService ctx)
            , ("env" .=) <$> ddEnv ctx
            , ("version" .=) <$> ddVersion ctx
            , ("trace_id" .=) . ddTraceId <$> ddSpan ctx
            , ("span_id" .=) . ddSpanId <$> ddSpan ctx
            ]

{- | The @dd@ object as a @katip@ structured payload, nested under the @dd@ key. Compose
it into a log site's payload, or install it as the initial context of a
request\/worker scope, so the rendered line carries that scope's active span.
-}
ddField :: DdContext -> SimpleLogPayload
ddField = sl "dd" . ddObject

{- | Render a raw 16-byte trace id into the id format Datadog correlates on: the
__unsigned decimal of the low 64 bits__. Datadog's log↔trace correlation matches
@dd.trace_id@ as a decimal 64-bit value (the low half of an OpenTelemetry 128-bit id);
the full-128-bit-hex form is a separate opt-in not used here. Reads the last eight bytes
big-endian, so a shorter id is taken whole and a longer one is truncated to its low 64
bits -- never a partial-byte misread.
-}
formatDdTraceId :: ByteString -> Text
formatDdTraceId = show . low64Bits

{- | Render a raw 8-byte span id into the Datadog form: the __unsigned decimal__ of the
64-bit id (read big-endian), matching @dd.span_id@.
-}
formatDdSpanId :: ByteString -> Text
formatDdSpanId = show . low64Bits

-- The unsigned 64-bit value of the last (up to) eight bytes, big-endian. Shared by the
-- trace-id low-64 truncation and the span-id read so both decode identically.
low64Bits :: ByteString -> Word64
low64Bits = BS.foldl' (\acc byte -> acc * 256 + fromIntegral byte) 0 . lastBytes 8
  where
    lastBytes :: Int -> ByteString -> ByteString
    lastBytes n bytes = BS.drop (max 0 (BS.length bytes - n)) bytes
