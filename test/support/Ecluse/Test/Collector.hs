-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Collector fixture configuration and evidence from its detailed debug export.
module Ecluse.Test.Collector (
    collectorConfig,
    ExportedSpan (..),
    AttributeValue (..),
    exportedSpans,
    PrintedSpan (..),
    printedSpans,
    datadogIds,
    loggedDatadogIds,
    spanFor,
    linkedMirrorTraces,
    privateArtifactAnswers,
    privateLookupAnswers,
) where

import Data.Aeson (Key)
import Data.Char (isHexDigit)
import Data.List (lookup)
import Data.Set qualified as Set
import Data.Text qualified as T
import Numeric (readHex)

import Ecluse.Test.Log (lineField)

-- | Flow-style YAML for the @env:@ provider, which needs no file or shell in the collector image.
collectorConfig :: [Text] -> Text
collectorConfig pipelines =
    "{receivers: {otlp: {protocols: {http: {endpoint: \"0.0.0.0:4318\"}}}}, "
        <> "exporters: {debug: {verbosity: detailed}}, "
        <> "service: {pipelines: {"
        <> T.intercalate ", " (map debugPipeline pipelines)
        <> "}}}"

debugPipeline :: Text -> Text
debugPipeline name = name <> ": {receivers: [otlp], exporters: [debug]}"

-- | One span as the debug exporter printed it.
data ExportedSpan = ExportedSpan
    { esName :: Text
    , esAttributes :: [(Text, AttributeValue)]
    }
    deriving stock (Eq, Show)

-- | An attribute value as the debug exporter prints it: a type tag, then the value in brackets.
data AttributeValue
    = TextValue Text
    | IntValue Int
    | -- | A value of a type no case reads, kept as printed.
      OtherValue Text
    deriving stock (Eq, Ord, Show)

-- | Every span in the collector's output, in the order the collector printed them.
exportedSpans :: Text -> [ExportedSpan]
exportedSpans = map psSpan . printedSpans

-- | One printed span with the identifiers a correlation check reads.
data PrintedSpan = PrintedSpan
    { psSpan :: ExportedSpan
    , psTraceId :: Maybe Text
    -- ^ The 32-hex trace ID, when the span printed a valid one.
    , psSpanId :: Maybe Text
    -- ^ The 16-hex span ID, when the span printed a valid one.
    , psParentId :: Maybe Text
    , psKind :: Maybe Text
    , psLinks :: [(Text, Text)]
    -- ^ The trace and span IDs each of the span's links names, in printed order.
    }

-- | Every span in the collector's output with its identifiers and links, in printed order.
printedSpans :: Text -> [PrintedSpan]
printedSpans = spansIn . lines
  where
    spansIn printed = case dropWhile (not . T.isPrefixOf "Span #") printed of
        [] -> []
        _ : rest ->
            let (own, later) = span ownLine rest
             in [ PrintedSpan
                    { psSpan = ExportedSpan name (mapMaybe attribute own)
                    , psTraceId = validId 32 =<< uniqueField "Trace ID" own
                    , psSpanId = validId 16 =<< uniqueField "ID" own
                    , psParentId = uniqueField "Parent ID" own
                    , psKind = uniqueField "Kind" own
                    , psLinks = spanLinks (takeWhile detailLine later)
                    }
                | Just name <- [fieldNamed "Name" own]
                ]
                    <> spansIn later

    -- The exporter indents a span's fields and attributes under an unindented @Attributes:@ label.
    -- Any other line starts the span's events or links, the next span, or another signal.
    ownLine line = " " `T.isPrefixOf` line || T.stripEnd line == "Attributes:"

    -- The span's events, then its links, each under an unindented label and numbered header.
    detailLine line = " " `T.isPrefixOf` line || any (`T.isPrefixOf` line) ["Events:", "SpanEvent #", "Links:", "SpanLink #"]

    fieldNamed field own =
        listToMaybe [T.strip (T.drop 1 value) | (key, value) <- map (T.breakOn ":") own, T.strip key == field]

    uniqueField field own = case [T.strip (T.drop 1 value) | (key, value) <- map (T.breakOn ":") own, T.strip key == field] of
        [value] -> Just value
        _ -> Nothing

    -- An all-zero ID is the exporter's rendering of an absent one.
    validId size value = value <$ guard (T.length value == size && T.all isHexDigit value && T.any (/= '0') value)

    -- Links print last, so every @SpanLink #n@ after the @Links:@ label is one of this span's.
    spanLinks details = mapMaybe linkOf (linkBlocks (drop 1 (dropWhile ((/= "Links:") . T.stripEnd) details)))

    linkBlocks = \case
        header : rest
            | "SpanLink #" `T.isPrefixOf` header ->
                let (fields, more) = break (T.isPrefixOf "SpanLink #") rest
                 in fields : linkBlocks more
        _ -> []

    linkOf fields = (,) <$> (validId 32 =<< linkField "Trace ID" fields) <*> (validId 16 =<< linkField "ID" fields)

    linkField field fields = case [T.strip value | Just entry <- map (T.stripPrefix "-> " . T.strip) fields, Just value <- [T.stripPrefix (field <> ": ") entry]] of
        [value] -> Just value
        _ -> Nothing

    attribute line = do
        entry <- T.stripPrefix "-> " (T.strip line)
        let (key, printed) = T.breakOn ": " entry
        value <- T.stripPrefix ": " printed
        pure (key, attributeValue value)

{- | A printed span's IDs as Datadog logs them: the low 64 bits of the trace ID and the span ID,
each as an unsigned decimal.
-}
datadogIds :: PrintedSpan -> Maybe (Text, Text)
datadogIds printed = (,) <$> (decimal . T.takeEnd 16 =<< psTraceId printed) <*> (decimal =<< psSpanId printed)
  where
    decimal hex = case readHex (toString hex) of
        [(value, "")] -> Just (show (value :: Word64))
        _ -> Nothing

-- | The trace and span IDs one JSON log line carries in the @dd@ object at the key path, for 'datadogIds' to match.
loggedDatadogIds :: [Key] -> Text -> Maybe (Text, Text)
loggedDatadogIds path line = (,) <$> lineField (path <> ["dd", "trace_id"]) line <*> lineField (path <> ["dd", "span_id"]) line

attributeValue :: Text -> AttributeValue
attributeValue printed =
    fromMaybe (OtherValue printed) $
        (TextValue <$> tagged "Str") <|> (IntValue <$> (tagged "Int" >>= readMaybe @Integer . toString >>= toIntegralSized))
  where
    tagged tag = T.stripPrefix (tag <> "(") printed >>= T.stripSuffix ")"

-- Whether a span carries every one of the given attributes.
spanCarries :: [(Text, AttributeValue)] -> ExportedSpan -> Bool
spanCarries attributes exported = all (`elem` esAttributes exported) attributes

-- | Match a domain span only when its own attributes identify the required coordinate.
spanFor :: Text -> Text -> Text -> ExportedSpan -> Bool
spanFor package version name exported =
    esName exported == name
        && spanCarries [("ecluse.package", TextValue package), ("ecluse.version", TextValue version)] exported

{- | The traces in which a coordinate's rule evaluation and mirror enqueue ran together, and whose
enqueue span the coordinate's mirror job links back to.
-}
linkedMirrorTraces :: Text -> Text -> Text -> [Text]
linkedMirrorTraces package version output =
    ordNub
        [ traceKey
        | enqueue <- named "ecluse.mirror.enqueue"
        , Just traceKey <- [psTraceId enqueue]
        , Just traceKey `elem` map psTraceId (named "ecluse.rule.eval")
        , Just spanKey <- [psSpanId enqueue]
        , any (elem (traceKey, spanKey) . psLinks) (named "ecluse.mirror.job")
        ]
  where
    named name = filter (spanFor package version name . psSpan) (printedSpans output)

-- | Statuses of GETs to one private artifact, in collector output order.
privateArtifactAnswers :: Text -> Text -> [ExportedSpan] -> [AttributeValue]
privateArtifactAnswers host target =
    mapMaybe (lookup "http.status_code" . esAttributes) . filter (spanCarries fetch)
  where
    fetch =
        [ ("http.method", TextValue "GET")
        , ("http.host", TextValue host)
        , ("http.target", TextValue target)
        ]

-- | Statuses of native index GETs in serving traces, excluding consumer work and missing trace context.
privateLookupAnswers :: Text -> Text -> Text -> [AttributeValue]
privateLookupAnswers host target output = privateArtifactAnswers host target [psSpan item | item <- observed, servingClient item]
  where
    observed = printedSpans output
    serving = Set.fromList [traceKey | item <- observed, psKind item == Just "Server", psParentId item `elem` [Just "", Just "0000000000000000"], Just traceKey <- [psTraceId item]]
    workers = Set.fromList [traceKey | item <- observed, psKind item == Just "Consumer", Just traceKey <- [psTraceId item]]
    servingClient item = psKind item == Just "Client" && maybe False (\traceKey -> Set.member traceKey serving && Set.notMember traceKey workers) (psTraceId item)
