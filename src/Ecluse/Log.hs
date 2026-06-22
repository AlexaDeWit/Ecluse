{- | The structured-logging pipeline.

Écluse is an inline dependency in someone else's build path, so when it refuses a
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
JSON line is always valid JSON. The selected format is parsed from @PROXY_LOG_FORMAT@
at the configuration boundary ("Ecluse.Config") and the resulting 'LogEnv' is held
in the composition root ("Ecluse.Env").

Trace-ID correlation (the @dd@ object that stitches a line to its span) is __not__
added here; it layers on top of this stream once the tracing substrate exists.
The scribe is kept additive precisely so that correlation can be attached without
reworking the format.

== Secrets

A bearer token is carried as the redacted @Secret@ of "Ecluse.Credential", whose
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

    -- * Rendering (for serialise-and-assert)
    renderLogLine,
) where

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

{- | Parse a 'LogFormat' from its wire name, naming the accepted set on failure.
The same strict, fail-loud style as the other configuration enums
("Ecluse.Config").

>>> parseLogFormat "json"
Right JsonLog

>>> parseLogFormat "yaml"
Left "unknown log format \"yaml\" (expected one of: json, console)"
-}
parseLogFormat :: Text -> Either Text LogFormat
parseLogFormat = \case
    "json" -> Right JsonLog
    "console" -> Right ConsoleLog
    other ->
        Left
            ( "unknown log format \""
                <> other
                <> "\" (expected one of: json, console)"
            )

-- | The wire name of a 'LogFormat' (the inverse of 'parseLogFormat').
renderLogFormat :: LogFormat -> Text
renderLogFormat = \case
    JsonLog -> "json"
    ConsoleLog -> "console"

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
