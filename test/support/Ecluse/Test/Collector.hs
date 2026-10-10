-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Span-local evidence from the collector's detailed debug export, shared by unit and E2E tests.
module Ecluse.Test.Collector (
    ExportedSpan (..),
    AttributeValue (..),
    exportedSpans,
    spanCarries,
    spanFor,
    privateArtifactAnswers,
) where

import Data.List (lookup)
import Data.Text qualified as T

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

-- | Match a domain span only when its own attributes identify the required coordinate.
spanFor :: Text -> Text -> Text -> ExportedSpan -> Bool
spanFor package version name exported =
    esName exported == name
        && spanCarries [("ecluse.package", TextValue package), ("ecluse.version", TextValue version)] exported

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
