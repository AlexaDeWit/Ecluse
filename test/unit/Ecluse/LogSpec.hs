module Ecluse.LogSpec (spec) where

import Data.Aeson (Object, Value (Object), eitherDecodeStrict, object, withObject, (.:), (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (parseMaybe)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import GHC.IO.Handle (hClose, hDuplicate, hDuplicateTo)
import Katip (
    Environment (Environment),
    Item (..),
    Namespace (Namespace),
    Severity (WarningS),
    SimpleLogPayload,
    ThreadIdText (ThreadIdText),
    Verbosity (V2),
    closeScribes,
    itemJson,
    logF,
    logStr,
    runKatipT,
    sl,
 )
import Test.Hspec
import UnliftIO (bracket, evaluate)
import UnliftIO.Temporary (withSystemTempFile)

import Ecluse.Core.Credential (mkSecret)
import Ecluse.Log (
    DdContext (..),
    DdSpan (..),
    LogFormat (..),
    auditContext,
    ddField,
    ddObject,
    formatDdSpanId,
    formatDdTraceId,
    low64Bits,
    newLogEnv,
    newScribe,
    parseLogFormat,
    renderLogFormat,
    renderLogLine,
 )

-- | A fixed instant, so a rendered line is deterministic across runs.
fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2026 6 22) 0

{- | Build a log 'Item' with the given structured payload and message, holding
every other field fixed. This is the unit the scribe serialises; rendering it
through 'renderLogLine' reproduces exactly what the scribe would write, with no
stdout dependency.
-}
item :: SimpleLogPayload -> Text -> Item SimpleLogPayload
item payload message =
    Item
        { _itemApp = Namespace ["ecluse"]
        , _itemEnv = Environment "test"
        , _itemSeverity = WarningS
        , _itemThread = ThreadIdText "ThreadId 1"
        , _itemHost = "test-host"
        , _itemProcess = 1
        , _itemPayload = payload
        , _itemMessage = logStr message
        , _itemTime = fixedTime
        , _itemNamespace = Namespace ["ecluse"]
        , _itemLoc = Nothing
        }

{- | The decoded JSON object an item serialises to at the scribe's verbosity, for
asserting on structure rather than brittle substrings.
-}
itemObject :: Item SimpleLogPayload -> Maybe Object
itemObject = parseMaybe (withObject "item" pure) . itemJson V2

-- | A top-level string field of a serialised item.
topField :: Text -> Item SimpleLogPayload -> Maybe Text
topField key logItem = itemObject logItem >>= parseMaybe (\o -> o .: Key.fromText key)

-- | A field of the item's nested @data@ object (the structured payload).
dataField :: Text -> Item SimpleLogPayload -> Maybe Text
dataField key logItem = do
    o <- itemObject logItem
    dat <- parseMaybe (.: "data") o
    parseMaybe (\d -> d .: Key.fromText key) dat

-- | The structured-context fields the audit trail attaches to a denial.
deniedContext :: SimpleLogPayload
deniedContext = auditContext "@evil/pkg" "1.0.0" "DenyInstallTimeExecution"

-- | The nested @dd@ correlation object of a serialised item's @data@ payload.
ddObjectOf :: Item SimpleLogPayload -> Maybe Object
ddObjectOf logItem = do
    o <- itemObject logItem
    dat <- parseMaybe (.: "data") o
    parseMaybe (.: "dd") dat

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

spec :: Spec
spec = do
    describe "parseLogFormat" $ do
        it "parses the two accepted wire names" $ do
            parseLogFormat "json" `shouldBe` Right JsonLog
            parseLogFormat "console" `shouldBe` Right ConsoleLog

        it "rejects an unknown format, naming the accepted set" $
            parseLogFormat "yaml"
                `shouldBe` Left "unknown log format \"yaml\" (expected one of: json, console)"

        it "round-trips through renderLogFormat" $ do
            renderLogFormat JsonLog `shouldBe` "json"
            renderLogFormat ConsoleLog `shouldBe` "console"
            parseLogFormat (renderLogFormat JsonLog) `shouldBe` Right JsonLog
            parseLogFormat (renderLogFormat ConsoleLog) `shouldBe` Right ConsoleLog

    describe "renderLogLine (format selection)" $ do
        it "JsonLog emits a single compact JSON object (one line, no pretty-print)" $ do
            let line = renderLogLine JsonLog (item deniedContext "denied")
            -- One JSON object: a single '{'…'}' with nothing outside it, and no
            -- embedded physical newline -- the JSONL one-line guarantee.
            T.isPrefixOf "{" line `shouldBe` True
            T.isSuffixOf "}" line `shouldBe` True
            T.count "\n" line `shouldBe` 0
            -- It is valid JSON that round-trips to the expected payload fields.
            dataField "rule" (item deniedContext "denied") `shouldBe` Just "DenyInstallTimeExecution"

        it "ConsoleLog emits the human-readable bracketed form, not JSON" $ do
            let line = renderLogLine ConsoleLog (item deniedContext "denied")
            T.isPrefixOf "[" line `shouldBe` True
            line `shouldSatisfy` T.isInfixOf "denied"
            -- The console form is not a JSON object.
            T.isPrefixOf "{" line `shouldBe` False

    describe "renderLogLine (JSONL one-line / escaping, table-driven)" $
        for_ escapeCases $ \(label, raw) ->
            it ("escapes an embedded newline in: " <> toString label) $ do
                let line = renderLogLine JsonLog (item mempty raw)
                -- The message spans no physical line: any embedded newline is
                -- escaped to the two characters '\' 'n', so the record stays one
                -- line for a line-delimited collector.
                T.count "\n" line `shouldBe` 0
                line `shouldSatisfy` T.isInfixOf "\\n"
                -- And the line is still one well-formed JSON object whose decoded
                -- "msg" recovers the original text with its real newline.
                topField "msg" (item mempty raw) `shouldBe` Just raw

    describe "renderLogLine (expected keys)" $
        it "carries the standard katip keys and the structured data object" $ do
            let it' = item deniedContext "denied"
            topField "msg" it' `shouldBe` Just "denied"
            topField "sev" it' `shouldBe` Just "Warning"
            isJust (itemObject it') `shouldBe` True
            dataField "package" it' `shouldBe` Just "@evil/pkg"
            dataField "version" it' `shouldBe` Just "1.0.0"
            dataField "rule" it' `shouldBe` Just "DenyInstallTimeExecution"

    describe "auditContext" $
        it "attaches package, version, and rule under the data object" $ do
            let it' = item (auditContext "left-pad" "1.3.0" "AllowScope") "admitted"
            dataField "package" it' `shouldBe` Just "left-pad"
            dataField "version" it' `shouldBe` Just "1.3.0"
            dataField "rule" it' `shouldBe` Just "AllowScope"

    describe "secrets never reach a log field" $ do
        it "a Secret embedded in a payload renders only its redaction, never the token" $ do
            -- The realistic leak path: code logs a value built from a Secret. The
            -- Secret's Show is a fixed placeholder, so the token text cannot reach
            -- a structured field. This is the load-bearing redaction
            -- (observability.md: token material must never reach a log).
            let token = "super-secret-token"
                leaky = sl "credential" (T.pack (show (mkSecret token)))
                line = renderLogLine JsonLog (item leaky "using credential")
            line `shouldSatisfy` (not . T.isInfixOf token)
            line `shouldSatisfy` T.isInfixOf "REDACTED"

        it "holds for the console format too" $ do
            let token = "another-secret"
                leaky = sl "credential" (T.pack (show (mkSecret token)))
                line = renderLogLine ConsoleLog (item leaky "using credential")
            line `shouldSatisfy` (not . T.isInfixOf token)

    describe "newScribe" $
        it "constructs a scribe for each format without throwing" $ do
            -- A 'Scribe' is opaque, so constructing it (the format switch and
            -- scribe wiring) and forcing it to weak-head normal form is the
            -- assertion: the pipeline assembles for both shapes.
            _ <- newScribe JsonLog >>= evaluate
            _ <- newScribe ConsoleLog >>= evaluate
            pure () :: Expectation

    describe "newLogEnv (end-to-end through the real scribe)" $ do
        it "writes a JsonLog event as exactly one compact JSON line to stdout" $ do
            captured <- captureStdout $ do
                logEnv <- newLogEnv JsonLog (Environment "test")
                runKatipT logEnv $
                    logF deniedContext (Namespace ["serve"]) WarningS (logStr ("denied" :: Text))
                _ <- closeScribes logEnv
                pure ()
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

        it "writes a ConsoleLog event in the human-readable bracketed form" $ do
            captured <- captureStdout $ do
                logEnv <- newLogEnv ConsoleLog (Environment "test")
                runKatipT logEnv $
                    logF deniedContext (Namespace ["serve"]) WarningS (logStr ("denied" :: Text))
                _ <- closeScribes logEnv
                pure ()
            captured `shouldSatisfy` T.isInfixOf "[Warning]"
            captured `shouldSatisfy` T.isInfixOf "denied"

    describe "dd trace correlation (Datadog id format + dd object)" $ do
        it "renders a trace id as the unsigned decimal of its low 64 bits (high bits ignored)" $ do
            formatDdTraceId (BS.pack (replicate 8 0xFF <> [0, 0, 0, 0, 0, 0, 0, 42])) `shouldBe` "42"
            formatDdTraceId (BS.pack (replicate 8 0x00 <> [0, 0, 0, 0, 0, 0, 1, 0])) `shouldBe` "256"

        it "renders a span id as the unsigned decimal of its 64 bits (big-endian)" $ do
            formatDdSpanId (BS.pack [0, 0, 0, 0, 0, 0, 0, 1]) `shouldBe` "1"
            formatDdSpanId (BS.pack [1, 0, 0, 0, 0, 0, 0, 0]) `shouldBe` "72057594037927936"

        it "builds the dd object: service always, env/version when set, ids only with a span" $ do
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

        it "carries the dd object on one compact JSON line under data.dd" $ do
            let logItem =
                    item
                        (ddField (DdContext "ecluse" (Just "prod") (Just "1.4.2") (Just (DdSpan "42" "7"))))
                        "denied"
                rendered = renderLogLine JsonLog logItem
                dd = ddObjectOf logItem
            length (T.lines rendered) `shouldBe` 1
            (dd >>= ddStr "service") `shouldBe` Just "ecluse"
            (dd >>= ddStr "env") `shouldBe` Just "prod"
            (dd >>= ddStr "version") `shouldBe` Just "1.4.2"
            (dd >>= ddStr "trace_id") `shouldBe` Just "42"
            (dd >>= ddStr "span_id") `shouldBe` Just "7"

    describe "low64Bits" $ do
        it "extracts the 64-bit value from a valid 8-byte input" $ do
            low64Bits "000000000000002a" `shouldBe` 42
            low64Bits "0000000000000100" `shouldBe` 256
            low64Bits "ffffffffffffffff" `shouldBe` 18446744073709551615

        it "pads inputs shorter than 8 bytes with leading zeros" $ do
            low64Bits "2a" `shouldBe` 42
            low64Bits "0100" `shouldBe` 256
            low64Bits "" `shouldBe` 0

        it "truncates inputs longer than 8 bytes, taking only the low 64 bits (the end)" $ do
            low64Bits "ffff000000000000002a" `shouldBe` 42
            low64Bits "010000000000000000" `shouldBe` 0

        it "returns 0 for non-hex strings" $ do
            low64Bits "hello" `shouldBe` 0

  where
    -- Whether a decoded JSON result is a single object (the JSONL contract).
    isObjectValue :: Either String Value -> Bool
    isObjectValue = \case
        Right (Object _) -> True
        _ -> False

    -- Newline-bearing messages whose escaping the JSONL line must preserve.
    escapeCases :: [(Text, Text)]
    escapeCases =
        [ ("a trailing newline", "denied\n")
        , ("an interior newline", "denied\nfor cause")
        , ("a multi-line message", "line one\nline two\nline three")
        , ("a carriage return and newline", "denied\r\nfor cause")
        ]
