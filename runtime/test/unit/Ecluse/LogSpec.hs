-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.LogSpec (spec) where

import Data.Aeson (Object, Value (Object), decodeStrict, eitherDecodeStrict, object, (.:), (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseMaybe)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Lazy.Builder qualified as TB
import Data.Time (UTCTime (..), fromGregorian)
import GHC.IO.Handle (hClose, hDuplicate, hDuplicateTo)
import Katip (
    Environment (Environment),
    Item (..),
    Namespace (Namespace),
    Severity (AlertS, CriticalS, DebugS, EmergencyS, ErrorS, InfoS, NoticeS, WarningS),
    SimpleLogPayload,
    ThreadIdText (ThreadIdText),
    Verbosity (V2),
    closeScribes,
    logF,
    logStr,
    runKatipT,
    sl,
 )
import Test.Hspec
import UnliftIO (bracket, evaluate)
import UnliftIO.Temporary (withSystemTempFile)

import Ecluse.Core.Credential (mkSecret)
import Ecluse.Runtime.Log (
    DdContext (..),
    DdSpan (..),
    LogFormat (..),
    LogLevel (..),
    ddField,
    ddObject,
    formatDdSpanId,
    formatDdTraceId,
    formatterFor,
    newLogEnv,
    newScribe,
    parseLogFormat,
    parseLogLevel,
    severityFloor,
    severityStatus,
 )

-- | A fixed instant, so a rendered line is deterministic across runs.
fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2026 6 22) 0

{- | The boot-resolved identity the formatter stamps on every line: a deployment that
named its environment and version.
-}
testIdentity :: DdContext
testIdentity = DdContext "ecluse" (Just "prod") (Just "1.4.2") Nothing

{- | Build a log 'Item' with the given structured payload and message, holding
every other field fixed. This is the unit the scribe serialises; rendering it
through the production formatter asserts on the emitted line with no stdout
dependency.
-}
item :: SimpleLogPayload -> Text -> Item SimpleLogPayload
item = itemAt WarningS

-- | 'item' at a chosen severity, for the status-mapping cases.
itemAt :: Severity -> SimpleLogPayload -> Text -> Item SimpleLogPayload
itemAt severity payload message =
    Item
        { _itemApp = Namespace ["ecluse"]
        , _itemEnv = Environment "test"
        , _itemSeverity = severity
        , _itemThread = ThreadIdText "ThreadId 1"
        , _itemHost = "test-host"
        , _itemProcess = 1
        , _itemPayload = payload
        , _itemMessage = logStr message
        , _itemTime = fixedTime
        , _itemNamespace = Namespace ["ecluse"]
        , _itemLoc = Nothing
        }

{- | The physical JSONL line an item renders to through the production formatter, at
the scribe's own verbosity and with colour off (as 'newScribe' forces it).
-}
renderLine :: DdContext -> Item SimpleLogPayload -> Text
renderLine logIdentity logItem =
    toText (TB.toLazyText (formatterFor JsonLog logIdentity False V2 logItem))

-- | The decoded object of a rendered line, for asserting on structure.
lineObject :: DdContext -> Item SimpleLogPayload -> Maybe Object
lineObject logIdentity = decodeStrict . encodeUtf8 . renderLine logIdentity

-- | A top-level string field of a rendered line.
topField :: Text -> Item SimpleLogPayload -> Maybe Text
topField key logItem = lineObject testIdentity logItem >>= parseMaybe (\o -> o .: Key.fromText key)

-- | A field of the rendered line's @data@ object (the per-call structured payload).
dataField :: Text -> Item SimpleLogPayload -> Maybe Text
dataField key logItem = do
    o <- lineObject testIdentity logItem
    dat <- parseMaybe (.: "data") o
    parseMaybe (\d -> d .: Key.fromText key) dat

-- | The structured-context fields the audit trail attaches to a denial.
deniedContext :: SimpleLogPayload
deniedContext =
    sl "package" ("@evil/pkg" :: Text)
        <> sl "version" ("1.0.0" :: Text)
        <> sl "rule" ("DenyInstallTimeExecution" :: Text)

-- | The rendered line's top-level @dd@ correlation object.
ddObjectOf :: DdContext -> Item SimpleLogPayload -> Maybe Object
ddObjectOf logIdentity logItem = lineObject logIdentity logItem >>= parseMaybe (.: "dd")

-- | A string field of a @dd@ object.
ddStr :: Text -> Object -> Maybe Text
ddStr key = parseMaybe (\ob -> ob .: Key.fromText key)

{- | Run an 'IO' action with the process 'stdout' redirected to a temporary file,
returning everything written. The original 'stdout' is restored on every exit
path. This lets a test capture what a scribe (which writes to the real 'stdout')
actually emits, without a network or any other dependency.
-}
captureStdout :: IO () -> IO Text
captureStdout act =
    withSystemTempFile "ecluse-log-capture.txt" $ \path tmpHandle ->
        bracket (hDuplicate stdout) restore $ \_saved -> do
            hFlush stdout
            hDuplicateTo tmpHandle stdout
            act
            hFlush stdout
            hClose tmpHandle
            decodeUtf8 <$> readFileBS path
  where
    restore saved = do
        hFlush stdout
        hDuplicateTo saved stdout
        hClose saved

{- | Emit one event through a real 'LogEnv' at the given level, capturing what the
scribe wrote to stdout. The whole admission decision lives in the scribe, so this is
how a level's floor is asserted.
-}
emitAt :: LogLevel -> Severity -> Text -> IO Text
emitAt level severity message =
    captureStdout $ do
        logEnv <- newLogEnv JsonLog level testIdentity (Environment "test")
        runKatipT logEnv $ logF (mempty :: SimpleLogPayload) (Namespace ["serve"]) severity (logStr message)
        void (closeScribes logEnv)

spec :: Spec
spec = do
    describe "parseLogFormat" $ do
        it "parses the two accepted wire names" $ do
            parseLogFormat "json" `shouldBe` Right JsonLog
            parseLogFormat "console" `shouldBe` Right ConsoleLog

        it "rejects an unknown format, naming the accepted set" $
            parseLogFormat "yaml"
                `shouldBe` Left "unknown log format \"yaml\" (expected one of: json, console)"

    describe "parseLogLevel" $ do
        it "parses the four accepted wire names" $ do
            parseLogLevel "debug" `shouldBe` Right DebugLevel
            parseLogLevel "info" `shouldBe` Right InfoLevel
            parseLogLevel "warn" `shouldBe` Right WarnLevel
            parseLogLevel "error" `shouldBe` Right ErrorLevel

        it "rejects an unknown level, naming the accepted set" $
            parseLogLevel "trace"
                `shouldBe` Left "unknown log level \"trace\" (expected one of: debug, info, warn, error)"

        it "maps each level onto the katip severity floor it admits" $ do
            severityFloor DebugLevel `shouldBe` DebugS
            severityFloor InfoLevel `shouldBe` InfoS
            severityFloor WarnLevel `shouldBe` WarningS
            severityFloor ErrorLevel `shouldBe` ErrorS

    describe "severityStatus (the four statuses a backend facets on)" $
        it "folds the eight katip severities onto debug/info/warn/error" $ do
            severityStatus DebugS `shouldBe` "debug"
            severityStatus InfoS `shouldBe` "info"
            severityStatus NoticeS `shouldBe` "info"
            severityStatus WarningS `shouldBe` "warn"
            severityStatus ErrorS `shouldBe` "error"
            severityStatus CriticalS `shouldBe` "error"
            severityStatus AlertS `shouldBe` "error"
            severityStatus EmergencyS `shouldBe` "error"

    describe "the rendered JSON line" $ do
        it "carries exactly the contracted top-level keys" $ do
            let logItem = item deniedContext "denied"
            fmap (sort . map Key.toText . KeyMap.keys) (lineObject testIdentity logItem)
                `shouldBe` Just ["data", "env", "katip", "message", "service", "status", "timestamp", "version"]

        it "renders the timestamp as RFC 3339 UTC and the status from the severity" $ do
            let logItem = item deniedContext "denied"
            topField "timestamp" logItem `shouldBe` Just "2026-06-22T00:00:00Z"
            topField "status" logItem `shouldBe` Just "warn"
            topField "message" logItem `shouldBe` Just "denied"

        it "stamps the resolved service/env/version identity" $ do
            let logItem = item deniedContext "denied"
            topField "service" logItem `shouldBe` Just "ecluse"
            topField "env" logItem `shouldBe` Just "prod"
            topField "version" logItem `shouldBe` Just "1.4.2"

        it "falls the env back to the katip environment when none was configured" $ do
            -- A deployment that names no DD_ENV / deployment.environment still stamps
            -- an env, so the field is never absent from a line.
            let bare = DdContext "ecluse" Nothing Nothing Nothing
            (lineObject bare (item mempty "boot") >>= parseMaybe (.: "env"))
                `shouldBe` Just ("test" :: Text)

        it "carries every katip severity through as its mapped status" $
            for_ [DebugS, InfoS, NoticeS, WarningS, ErrorS, CriticalS, AlertS, EmergencyS] $ \severity ->
                topField "status" (itemAt severity mempty "event")
                    `shouldBe` Just (severityStatus severity)

        it "preserves the per-call structured payload under data" $ do
            let logItem = item deniedContext "denied"
            dataField "package" logItem `shouldBe` Just "@evil/pkg"
            dataField "version" logItem `shouldBe` Just "1.0.0"
            dataField "rule" logItem `shouldBe` Just "DenyInstallTimeExecution"

        it "keeps the katip emitter fields under one nested key" $ do
            let logItem = item deniedContext "denied"
                katip = lineObject testIdentity logItem >>= parseMaybe (.: "katip")
            (katip >>= parseMaybe (.: "host")) `shouldBe` Just ("test-host" :: Text)
            (katip >>= parseMaybe (.: "thread")) `shouldBe` Just ("ThreadId 1" :: Text)
            fmap (sort . map Key.toText . KeyMap.keys) katip
                `shouldBe` Just ["app", "host", "loc", "ns", "pid", "thread"]

    describe "dd trace correlation on the line" $ do
        it "lifts the log site's active-span ids to a top-level dd object" $ do
            let logItem =
                    item
                        (ddField (DdContext "ecluse" (Just "prod") (Just "1.4.2") (Just (DdSpan "42" "7"))))
                        "denied"
                dd = ddObjectOf testIdentity logItem
            (dd >>= ddStr "trace_id") `shouldBe` Just "42"
            (dd >>= ddStr "span_id") `shouldBe` Just "7"

        it "does not repeat the identity inside data.dd" $ do
            -- The identity is already top level; the payload's dd object is consumed
            -- for its ids alone, so the line never carries the same service twice.
            let logItem =
                    item
                        (ddField (DdContext "ecluse" (Just "prod") (Just "1.4.2") (Just (DdSpan "42" "7"))))
                        "denied"
                dat :: Maybe Object
                dat = lineObject testIdentity logItem >>= parseMaybe (.: "data")
            fmap KeyMap.toList dat `shouldBe` Just []

        it "omits dd entirely outside any span scope" $
            ddObjectOf testIdentity (item deniedContext "denied") `shouldBe` Nothing

        it "omits dd when the payload's dd object carries no ids" $
            -- A line raised under the identity context but outside a span: the object
            -- is installed, the ids are not, so no half-filled correlation pair renders.
            ddObjectOf testIdentity (item (ddField testIdentity) "denied") `shouldBe` Nothing

    describe "log level admission (the scribe's floor)" $ do
        it "suppresses a Debug event at the default info level" $ do
            captured <- emitAt InfoLevel DebugS "diagnostic"
            captured `shouldBe` ""

        it "admits a Debug event at the debug level" $ do
            captured <- emitAt DebugLevel DebugS "diagnostic"
            captured `shouldSatisfy` T.isInfixOf "\"status\":\"debug\""

        it "suppresses an Info event at the warn level" $ do
            captured <- emitAt WarnLevel InfoS "routine"
            captured `shouldBe` ""

        it "admits a Warning event at the warn level" $ do
            captured <- emitAt WarnLevel WarningS "trouble"
            captured `shouldSatisfy` T.isInfixOf "\"status\":\"warn\""

        it "admits an Error event at the error level and suppresses a Warning" $ do
            admitted <- emitAt ErrorLevel ErrorS "broken"
            admitted `shouldSatisfy` T.isInfixOf "\"status\":\"error\""
            suppressed <- emitAt ErrorLevel WarningS "trouble"
            suppressed `shouldBe` ""

    describe "JsonLog stays one physical line (embedded newlines escaped)" $
        for_ escapeCases $ \(label, raw) ->
            it ("keeps one physical line for: " <> toString label) $ do
                captured <- emitAt InfoLevel WarningS raw
                -- The scribe terminates each event with one trailing newline, so a message
                -- carrying embedded newlines still emits as a single physical JSONL line,
                -- its newline escaped to the two characters '\' 'n' inside the JSON string.
                case filter (not . T.null) (T.lines captured) of
                    [line] -> line `shouldSatisfy` T.isInfixOf "\\n"
                    other -> expectationFailure ("expected exactly one JSON log line, got " <> show (length other))

    describe "secrets never reach a log field" $ do
        it "a Secret embedded in a payload renders only its redaction, never the token" $ do
            -- The realistic leak path: code logs a value built from a Secret. The
            -- Secret's Show is a fixed placeholder, so the token text cannot reach
            -- a structured field. This is the load-bearing redaction
            -- (observability.md: token material must never reach a log), asserted
            -- through the real scribe's emitted output.
            let token = "super-secret-token"
                leaky = sl "credential" (T.pack (show (mkSecret token)))
            captured <- captureStdout $ do
                logEnv <- newLogEnv JsonLog InfoLevel testIdentity (Environment "test")
                runKatipT logEnv $ logF leaky (Namespace ["serve"]) WarningS (logStr ("using credential" :: Text))
                void (closeScribes logEnv)
            captured `shouldSatisfy` (not . T.isInfixOf token)
            captured `shouldSatisfy` T.isInfixOf "REDACTED"

        it "holds for the console format too" $ do
            let token = "another-secret"
                leaky = sl "credential" (T.pack (show (mkSecret token)))
            captured <- captureStdout $ do
                logEnv <- newLogEnv ConsoleLog InfoLevel testIdentity (Environment "test")
                runKatipT logEnv $ logF leaky (Namespace ["serve"]) WarningS (logStr ("using credential" :: Text))
                void (closeScribes logEnv)
            captured `shouldSatisfy` (not . T.isInfixOf token)

    describe "newScribe" $
        it "constructs a scribe for each format without throwing" $ do
            -- A 'Scribe' is opaque, so constructing it (the format switch and
            -- scribe wiring) and forcing it to weak-head normal form is the
            -- assertion: the pipeline assembles for both shapes.
            _ <- newScribe JsonLog InfoLevel testIdentity >>= evaluate
            _ <- newScribe ConsoleLog InfoLevel testIdentity >>= evaluate
            pure () :: Expectation

    describe "newLogEnv (end-to-end through the real scribe)" $ do
        it "writes a JsonLog event as exactly one compact JSON line to stdout" $ do
            captured <- captureStdout $ do
                logEnv <- newLogEnv JsonLog InfoLevel testIdentity (Environment "test")
                runKatipT logEnv $
                    logF deniedContext (Namespace ["serve"]) WarningS (logStr ("denied" :: Text))
                void (closeScribes logEnv)
            -- The scribe terminates each event with a newline, so a single event
            -- is one non-empty physical line; that line is a complete JSON object
            -- carrying the structured data.
            let physicalLines = filter (not . T.null) (T.lines captured)
            length physicalLines `shouldBe` 1
            case physicalLines of
                [line] -> do
                    eitherDecodeStrict (encodeUtf8 line) `shouldSatisfy` isObjectValue
                    line `shouldSatisfy` T.isInfixOf "DenyInstallTimeExecution"
                _ -> expectationFailure "expected exactly one JSON log line"

        it "round-trips a newline-bearing message: the decoded message equals the exact original" $ do
            let original = "denied\nfor cause" :: Text
            captured <- emitAt InfoLevel WarningS original
            -- The escaped newline in the single physical JSONL line decodes back to the
            -- exact newline-bearing message: the JSON string escaping is lossless, not
            -- merely one-line-safe.
            case filter (not . T.null) (T.lines captured) of
                [line] -> lineMessage line `shouldBe` Just original
                other -> expectationFailure ("expected exactly one JSON log line, got " <> show (length other))

        it "writes a ConsoleLog event in the human-readable bracketed form" $ do
            captured <- captureStdout $ do
                logEnv <- newLogEnv ConsoleLog InfoLevel testIdentity (Environment "test")
                runKatipT logEnv $
                    logF deniedContext (Namespace ["serve"]) WarningS (logStr ("denied" :: Text))
                void (closeScribes logEnv)
            captured `shouldSatisfy` T.isInfixOf "[Warning]"
            captured `shouldSatisfy` T.isInfixOf "denied"

    describe "the Datadog id format" $ do
        it "renders a trace id as the unsigned decimal of its low 64 bits (high bits ignored)" $ do
            formatDdTraceId (BS.pack (replicate 8 0xFF <> [0, 0, 0, 0, 0, 0, 0, 42])) `shouldBe` "42"
            formatDdTraceId (BS.pack (replicate 8 0x00 <> [0, 0, 0, 0, 0, 0, 1, 0])) `shouldBe` "256"

        it "renders a span id as the unsigned decimal of its 64 bits (big-endian)" $ do
            formatDdSpanId (BS.pack [0, 0, 0, 0, 0, 0, 0, 1]) `shouldBe` "1"
            formatDdSpanId (BS.pack [1, 0, 0, 0, 0, 0, 0, 0]) `shouldBe` "72057594037927936"

        it "builds the dd context object: service always, env/version when set, ids only with a span" $ do
            ddObject (DdContext "ecluse" (Just "prod") (Just "1.4.2") (Just (DdSpan "42" "7")))
                `shouldBe` object
                    [ "service" .= ("ecluse" :: Text)
                    , "env" .= ("prod" :: Text)
                    , "version" .= ("1.4.2" :: Text)
                    , "trace_id" .= ("42" :: Text)
                    , "span_id" .= ("7" :: Text)
                    ]
            ddObject (DdContext "ecluse" Nothing Nothing Nothing)
                `shouldBe` object ["service" .= ("ecluse" :: Text)]
  where
    -- Whether a decoded JSON result is a single object (the JSONL contract).
    isObjectValue :: Either String Value -> Bool
    isObjectValue = \case
        Right (Object _) -> True
        _ -> False

    -- The top-level message field decoded back from a serialised JSON log line.
    lineMessage :: Text -> Maybe Text
    lineMessage line = case eitherDecodeStrict (encodeUtf8 line) of
        Right o -> parseMaybe (.: "message") (o :: Object)
        Left _ -> Nothing

    -- Newline-bearing messages whose escaping the JSONL line must preserve.
    escapeCases :: [(Text, Text)]
    escapeCases =
        [ ("a trailing newline", "denied\n")
        , ("an interior newline", "denied\nfor cause")
        , ("a multi-line message", "line one\nline two\nline three")
        , ("a carriage return and newline", "denied\r\nfor cause")
        ]
