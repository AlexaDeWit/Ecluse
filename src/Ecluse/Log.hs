{- | The structured-logging pipeline.

Écluse sits in the install path of someone else's build, so when it refuses a
package or runs slow the operator must see /why/ from the logs alone. This module
stands up a @katip@ 'LogEnv' — the single log stream every layer attaches context
to — and chooses its on-the-wire shape:

* __'JsonLog'__ writes __one compact JSON object per line__ to stdout (JSONL): the
  whole physical line /is/ the JSON, with no pretty-printing and no level or
  timestamp prefix outside the object, and any newline inside a field escaped as
  @\\n@ so a record never spans two lines. This is the in-container default, the
  shape a log collector's stdout JSON autodiscovery consumes directly.
* __'ConsoleLog'__ writes the human-readable bracketed form for local development.

A 'LogEnv' built here carries no colour codes even on a terminal, so a captured
JSON line is always valid JSON. The selected format is parsed from @ECLUSE_LOG_FORMAT@
at the configuration boundary ("Ecluse.Config") and the resulting 'LogEnv' is held
in the composition root ("Ecluse.Env").

Trace-ID correlation rides this stream as the @dd@ object ('ddField'): @service@\/
@env@\/@version@ from the resolved telemetry identity, plus the active span's
@trace_id@\/@span_id@ in the id format Datadog expects ('formatDdTraceId'). The object
is built here but stays free of any OpenTelemetry dependency — the active span is read
and the ids rendered by "Ecluse.Telemetry.Correlation", which composes 'ddField' into a
log site's payload.

== Secrets

A bearer token is carried as the redacted @Secret@ of "Ecluse.Core.Credential", whose
'Show' renders only a placeholder, so token material cannot reach a log field
through any structured payload or message built from it (see
@docs\/architecture\/observability.md@). This module adds no field that would
defeat that redaction.

The model is described in @docs\/architecture\/observability.md@ → "Logs".
-}
module Ecluse.Log (
    -- * Log format
    LogFormat (..),
    parseLogFormat,
    renderLogFormat,

    -- * Pipeline construction
    newLogEnv,
    newScribe,
    formatterFor,

    -- * Structured context
    auditContext,
    moduleField,

    -- * Datadog trace correlation
    DdContext (..),
    DdSpan (..),
    ddField,
    ddObject,
    formatDdTraceId,
    formatDdSpanId,

    -- * Rendering (for serialise-and-assert)
    renderLogLine,
) where

import Data.Aeson (Value, object, (.=))
import Data.ByteString qualified as BS
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Katip (
    ColorStrategy (ColorLog),
    Environment,
    Item,
    LogEnv,
    LogItem,
    Namespace (Namespace),
    Scribe,
    Severity (DebugS),
    SimpleLogPayload,
    Verbosity (V2),
    defaultScribeSettings,
    initLogEnv,
    permitItem,
    registerScribe,
    sl,
 )
import Katip.Scribes.Handle (ItemFormatter, bracketFormat, jsonFormat, mkHandleScribeWithFormatter)

import Ecluse.Core.Wire (WireVocab (..), parseWire, renderWire)

-- ── log format ───────────────────────────────────────────────────────────────

{- | The on-the-wire shape of the log stream, selected by configuration. A sum
type rather than a 'Bool' so each case names its intent and a new shape is a new
constructor, not a second flag.
-}
data LogFormat
    = {- | One compact JSON object per line to stdout (JSONL) — the in-container
      default a log collector's stdout JSON parsing consumes.
      -}
      JsonLog
    | -- | The human-readable bracketed form, for local development.
      ConsoleLog
    deriving stock (Eq, Show)

-- The wire vocabulary of a 'LogFormat': the single source both 'parseWire' and
-- 'renderWire' derive from for this type.
instance WireVocab LogFormat where
    wireKind = "log format"
    wireTable =
        (JsonLog, "json")
            :| [(ConsoleLog, "console")]

{- | Parse a 'LogFormat' from its wire name, naming the accepted set on failure.
The same strict, fail-loud style as the other configuration enums
("Ecluse.Config").

>>> parseLogFormat "json"
Right JsonLog

>>> parseLogFormat "yaml"
Left "unknown log format \"yaml\" (expected one of: json, console)"
-}
parseLogFormat :: Text -> Either Text LogFormat
parseLogFormat = parseWire

-- | The wire name of a 'LogFormat' (the inverse of 'parseLogFormat').
renderLogFormat :: LogFormat -> Text
renderLogFormat = renderWire

-- ── pipeline construction ────────────────────────────────────────────────────

{- | Build the application 'LogEnv': a @katip@ environment under the @ecluse@
namespace with a single stdout scribe in the chosen 'LogFormat'. This is the value
the composition root holds and every later layer logs through.

The scribe admits every severity ('DebugS' upward); a deployment narrows what it
keeps through @katip@'s own verbosity controls rather than by rebuilding the
environment.
-}
newLogEnv :: LogFormat -> Environment -> IO LogEnv
newLogEnv format environment = do
    scribe <- newScribe format
    base <- initLogEnv (Namespace ["ecluse"]) environment
    registerScribe "stdout" scribe defaultScribeSettings base

{- | Build the stdout 'Scribe' for a 'LogFormat'. Colour is forced __off__
('ColorLog' 'False') so a captured 'JsonLog' line is always valid JSON — no ANSI
escapes leak into the object even when stdout is a terminal. The handle scribe
writes each item as exactly one line (the formatter output plus a single trailing
newline), which is what makes 'JsonLog' a true JSONL stream.
-}
newScribe :: LogFormat -> IO Scribe
newScribe format =
    mkHandleScribeWithFormatter
        (formatterFor format)
        (ColorLog False)
        stdout
        (permitItem DebugS)
        V2

{- | The @katip@ 'ItemFormatter' a 'LogFormat' wires into its scribe: the compact
one-line JSON encoder for 'JsonLog', the bracketed human form for 'ConsoleLog'.

Exposed so a test can render an item through the exact formatter the scribe uses,
asserting on the serialised line without writing to stdout (see 'renderLogLine').
-}
formatterFor :: (LogItem a) => LogFormat -> ItemFormatter a
formatterFor = \case
    JsonLog -> jsonFormat
    ConsoleLog -> bracketFormat

-- ── structured context ───────────────────────────────────────────────────────

{- | The structured context for an audit event — a denial or other rule decision —
carrying the @package@, @version@, and @rule@ the operator needs to explain a 403
from the log line alone. These are the high-cardinality identifiers that belong on
the log line, never on a metric label (see
@docs\/architecture\/observability.md@ → "Cardinality and attributes").

Attach it to a log call as the structured payload; @katip@ renders the three keys
into the line's @data@ object.
-}
auditContext ::
    -- | The package the decision concerns.
    Text ->
    -- | The package version.
    Text ->
    -- | The name of the rule that decided.
    Text ->
    SimpleLogPayload
auditContext package version rule =
    sl "package" package <> sl "version" version <> sl "rule" rule

{- | The structured context naming the __source module__ a log line was emitted
from, so every JSON record carries a @module@ field (e.g.
@"module":"Ecluse.Server.Pipeline"@). Compose it into a log site's payload alongside
the event's own fields, so the stream can be filtered by emitter without leaning on
the @katip@ namespace. @katip@ renders the key into the line's @data@ object. This is
the standard tag for a log raised off the 'Handler' reader (a plain-'IO' path that
opens its own context through the composition-root 'LogEnv').
-}
moduleField :: Text -> SimpleLogPayload
moduleField = sl "module"

-- ── Datadog trace correlation ──────────────────────────────────────────────────

{- | The unified-service identity stamped onto every log line as the @dd@ object, plus
the active span's ids when one is in scope. @service@\/@env@\/@version@ come from the
same resolved telemetry identity as the traces ("Ecluse.Telemetry.Resolve"), so logs
and traces share one identity; the trace\/span ids are present only when a span is
active (filled by "Ecluse.Telemetry.Correlation" off the OpenTelemetry context).
-}
data DdContext = DdContext
    { ddService :: Text
    -- ^ @dd.service@ — the resolved service name.
    , ddEnv :: Maybe Text
    -- ^ @dd.env@ — the deployment environment, when configured.
    , ddVersion :: Maybe Text
    -- ^ @dd.version@ — the service version, when configured.
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
    -- ^ @dd.trace_id@ — the trace id in Datadog form.
    , ddSpanId :: Text
    -- ^ @dd.span_id@ — the span id in Datadog form.
    }
    deriving stock (Eq, Show)

{- | The @dd@ object as JSON: @service@ always, @env@\/@version@ when configured, and
@trace_id@\/@span_id@ only when a span is active. This is the object a log collector's
unified-service tagging and trace-to-log correlation read.
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
it into a log site's payload so the rendered JSON line carries
@"dd":{"service":…,"trace_id":…}@ for trace-to-log correlation.
-}
ddField :: DdContext -> SimpleLogPayload
ddField = sl "dd" . ddObject

{- | Render a raw 16-byte trace id into the id format Datadog correlates on: the
__unsigned decimal of the low 64 bits__. Datadog's log↔trace correlation matches
@dd.trace_id@ as a decimal 64-bit value (the low half of an OpenTelemetry 128-bit id);
the full-128-bit-hex form is a separate opt-in not used here. Reads the last eight bytes
big-endian, so a shorter id is taken whole and a longer one is truncated to its low 64
bits — never a partial-byte misread.
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

-- ── rendering ────────────────────────────────────────────────────────────────

{- | Render a single log 'Item' to the exact text the scribe for this 'LogFormat'
writes for it — the formatter output for one item, without the trailing newline
the handle scribe appends to separate physical lines.

This is what the unit tests assert on: it reproduces the scribe's serialisation
with no stdout dependency, so a 'JsonLog' line can be checked for being a single
compact object with escaped newlines, and a 'ConsoleLog' line for the
human-readable form.
-}
renderLogLine :: (LogItem a) => LogFormat -> Item a -> Text
renderLogLine format item =
    TL.toStrict (TB.toLazyText (formatterFor format False V2 item))
