-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

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
import Ecluse.Runtime.Log (
    DdContext (..),
    DdSpan (..),
    LogFormat (..),
    ddField,
    ddObject,
    formatDdSpanId,
    formatDdTraceId,
    newLogEnv,
    newScribe,
    parseLogFormat,
 )

-- | A fixed instant, so a rendered line is deterministic across runs.
fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2026 6 22) 0

{- | Build a log 'Item' with the given structured payload and message, holding
every other field fixed. This is the unit the scribe serialises; decoding it
through 'itemJson' asserts on the serialised structure with no stdout dependency.
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
deniedContext =
    sl "package" ("@evil/pkg" :: Text)
        <> sl "version" ("1.0.0" :: Text)
        <> sl "rule" ("DenyInstallTimeExecution" :: Text)

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

    describe "JsonLog stays one physical line (embedded newlines escaped)" $
        for_ escapeCases $ \(label, raw) ->
            it ("keeps one physical line for: " <> toString label) $ do
                captured <- captureStdout $ do
                    logEnv <- newLogEnv JsonLog (Environment "test")
                    runKatipT logEnv $ logF (mempty :: SimpleLogPayload) (Namespace ["serve"]) WarningS (logStr raw)
                    _ <- closeScribes logEnv
                    pure ()
                -- The scribe terminates each event with one trailing newline, so a message
                -- carrying embedded newlines still emits as a single physical JSONL line,
                -- its newline escaped to the two characters '\' 'n' inside the JSON string.
                case filter (not . T.null) (T.lines captured) of
                    [line] -> line `shouldSatisfy` T.isInfixOf "\\n"
                    other -> expectationFailure ("expected exactly one JSON log line, got " <> show (length other))

    describe "serialised item (expected keys)" $
        it "carries the standard katip keys and the structured data object" $ do
            let it' = item deniedContext "denied"
            topField "msg" it' `shouldBe` Just "denied"
            topField "sev" it' `shouldBe` Just "Warning"
            isJust (itemObject it') `shouldBe` True
            dataField "package" it' `shouldBe` Just "@evil/pkg"
            dataField "version" it' `shouldBe` Just "1.0.0"
            dataField "rule" it' `shouldBe` Just "DenyInstallTimeExecution"

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
                logEnv <- newLogEnv JsonLog (Environment "test")
                runKatipT logEnv $ logF leaky (Namespace ["serve"]) WarningS (logStr ("using credential" :: Text))
                _ <- closeScribes logEnv
                pure ()
            captured `shouldSatisfy` (not . T.isInfixOf token)
            captured `shouldSatisfy` T.isInfixOf "REDACTED"

        it "holds for the console format too" $ do
            let token = "another-secret"
                leaky = sl "credential" (T.pack (show (mkSecret token)))
            captured <- captureStdout $ do
                logEnv <- newLogEnv ConsoleLog (Environment "test")
                runKatipT logEnv $ logF leaky (Namespace ["serve"]) WarningS (logStr ("using credential" :: Text))
                _ <- closeScribes logEnv
                pure ()
            captured `shouldSatisfy` (not . T.isInfixOf token)

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

        it "round-trips a newline-bearing message: the decoded msg equals the exact original" $ do
            let original = "denied\nfor cause" :: Text
            captured <- captureStdout $ do
                logEnv <- newLogEnv JsonLog (Environment "test")
                runKatipT logEnv $
                    logF deniedContext (Namespace ["serve"]) WarningS (logStr original)
                _ <- closeScribes logEnv
                pure ()
            -- The escaped newline in the single physical JSONL line decodes back to the
            -- exact newline-bearing message: the JSON string escaping is lossless, not
            -- merely one-line-safe.
            case filter (not . T.null) (T.lines captured) of
                [line] -> lineMsg line `shouldBe` Just original
                other -> expectationFailure ("expected exactly one JSON log line, got " <> show (length other))

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

        it "carries the dd object under data.dd" $ do
            let logItem =
                    item
                        (ddField (DdContext "ecluse" (Just "prod") (Just "1.4.2") (Just (DdSpan "42" "7"))))
                        "denied"
                dd = ddObjectOf logItem
            (dd >>= ddStr "service") `shouldBe` Just "ecluse"
            (dd >>= ddStr "env") `shouldBe` Just "prod"
            (dd >>= ddStr "version") `shouldBe` Just "1.4.2"
            (dd >>= ddStr "trace_id") `shouldBe` Just "42"
            (dd >>= ddStr "span_id") `shouldBe` Just "7"
  where
    -- Whether a decoded JSON result is a single object (the JSONL contract).
    isObjectValue :: Either String Value -> Bool
    isObjectValue = \case
        Right (Object _) -> True
        _ -> False

    -- The top-level msg field decoded back from a serialised JSON log line.
    lineMsg :: Text -> Maybe Text
    lineMsg line = case eitherDecodeStrict (encodeUtf8 line) of
        Right o -> parseMaybe (.: "msg") (o :: Object)
        Left _ -> Nothing

    -- Newline-bearing messages whose escaping the JSONL line must preserve.
    escapeCases :: [(Text, Text)]
    escapeCases =
        [ ("a trailing newline", "denied\n")
        , ("an interior newline", "denied\nfor cause")
        , ("a multi-line message", "line one\nline two\nline three")
        , ("a carriage return and newline", "denied\r\nfor cause")
        ]
