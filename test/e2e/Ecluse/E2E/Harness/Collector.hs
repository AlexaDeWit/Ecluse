-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Reads of the OTLP collector's debug-exporter output, which prints every signal it receives at
detailed verbosity. A case that needs one span's own attributes reads the output as spans, because
an attribute is evidence only beside the name and the other attributes of the span that carries it.
-}
module Ecluse.E2E.Harness.Collector (
    awaitCollectorLog,
    awaitCollectorSpans,

    -- * Printed spans
    ExportedSpan (..),
    AttributeValue (..),
    exportedSpans,
    spanCarries,
) where

import Data.Text qualified as T

import Ecluse.E2E.Harness.Docker (awaitContainerLog, containerLogs)
import Ecluse.E2E.Harness.Types
import Ecluse.Test.Poll (pollUntil)

{- | Poll the OTLP collector's debug-exporter output until the predicate holds. It fails loudly when
the environment booted without a collector, which only @ecCollector = True@ provides.
-}
awaitCollectorLog :: E2E -> (Text -> Bool) -> Int -> IO Bool
awaitCollectorLog e2e matches attempts = do
    collector <- collectorContainer e2e
    awaitContainerLog collector matches attempts

{- | Poll the collector's printed spans until the predicate holds, at the pace of
'awaitCollectorLog'. It yields the spans it last read, so a failure shows what the collector held.
-}
awaitCollectorSpans :: E2E -> ([ExportedSpan] -> Bool) -> Int -> IO [ExportedSpan]
awaitCollectorSpans e2e matches attempts = do
    collector <- collectorContainer e2e
    pollUntil attempts 250000 matches (exportedSpans <$> containerLogs collector)

collectorContainer :: E2E -> IO String
collectorContainer =
    maybe (fail "this environment was booted without a collector") pure . e2eCollectorContainer

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
exportedSpans = spansIn . lines
  where
    spansIn printed = case dropWhile (not . T.isPrefixOf "Span #") printed of
        [] -> []
        _ : rest ->
            let (own, later) = span ownLine rest
             in [ExportedSpan name (mapMaybe attribute own) | Just name <- [fieldNamed "Name" own]] <> spansIn later

    -- The exporter indents a span's fields and attributes under an unindented @Attributes:@ label.
    -- Any other line starts the span's events or links, the next span, or another signal.
    ownLine line = " " `T.isPrefixOf` line || T.stripEnd line == "Attributes:"

    fieldNamed field own =
        listToMaybe [T.strip (T.drop 1 value) | (key, value) <- map (T.breakOn ":") own, T.strip key == field]

    attribute line = do
        entry <- T.stripPrefix "-> " (T.strip line)
        let (key, printed) = T.breakOn ": " entry
        value <- T.stripPrefix ": " printed
        pure (key, attributeValue value)

attributeValue :: Text -> AttributeValue
attributeValue printed =
    fromMaybe (OtherValue printed) $
        (TextValue <$> tagged "Str") <|> (IntValue <$> (tagged "Int" >>= readMaybe . toString))
  where
    tagged tag = T.stripPrefix (tag <> "(") printed >>= T.stripSuffix ")"

-- | Whether a span carries every one of the given attributes.
spanCarries :: [(Text, AttributeValue)] -> ExportedSpan -> Bool
spanCarries attributes exported = all (`elem` esAttributes exported) attributes
