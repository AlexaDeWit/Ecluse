-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE DeriveAnyClass #-}

{- | The proxy's Prometheus scrape, read as samples. The harness samples the admission gauges
during a window and reads cache outcomes after a replay. A line it cannot read is skipped, so a
series added later never breaks a report.
-}
module Ecluse.BenchLoad.Exposition (
    -- * Samples
    Sample (..),
    parseExposition,
    seriesTotal,
    commonLabels,
    renderSample,

    -- * Gauge summaries
    GaugeSummary (..),
    summariseGauge,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T

-- | One exposition line: the metric name, its labels, and its value.
data Sample = Sample
    { sampleName :: Text
    , sampleLabels :: [(Text, Text)]
    , sampleValue :: Double
    }
    deriving stock (Eq, Show)

-- | Parse a text exposition, skipping comments, blank lines, and lines it cannot read.
parseExposition :: Text -> [Sample]
parseExposition = mapMaybe sampleLine . lines

sampleLine :: Text -> Maybe Sample
sampleLine line
    | T.null stripped || "#" `T.isPrefixOf` stripped = Nothing
    | otherwise = do
        let (name, rest) = T.break (\c -> c == '{' || c == ' ') stripped
        guard (not (T.null name))
        (labels, afterLabels) <- case T.uncons rest of
            Just ('{', inner) -> labelSet inner []
            _ -> Just ([], rest)
        valueText <- listToMaybe (words afterLabels)
        Sample name labels <$> sampleNumber valueText
  where
    stripped = T.strip line

-- Labels are @key="value"@ pairs, where a value escapes backslash, quote, and newline.
labelSet :: Text -> [(Text, Text)] -> Maybe ([(Text, Text)], Text)
labelSet input acc = case T.uncons (T.dropWhile (\c -> c == ',' || c == ' ') input) of
    Just ('}', rest) -> Just (reverse acc, rest)
    Just _ -> do
        let (key, afterKey) = T.breakOn "=\"" (T.dropWhile (\c -> c == ',' || c == ' ') input)
        guard (not (T.null key) && not (T.null afterKey))
        (value, rest) <- quoted (T.drop 2 afterKey) ""
        labelSet rest ((T.strip key, value) : acc)
    Nothing -> Nothing

quoted :: Text -> Text -> Maybe (Text, Text)
quoted input acc = case T.uncons input of
    Just ('"', rest) -> Just (acc, rest)
    Just ('\\', rest) -> case T.uncons rest of
        Just ('n', more) -> quoted more (T.snoc acc '\n')
        Just (c, more) -> quoted more (T.snoc acc c)
        Nothing -> Nothing
    Just (c, rest) -> quoted rest (T.snoc acc c)
    Nothing -> Nothing

sampleNumber :: Text -> Maybe Double
sampleNumber = \case
    "+Inf" -> Just (1 / 0)
    "-Inf" -> Just (negate (1 / 0))
    other -> readMaybe (toString other)

{- | The sum over every series of one metric whose labels include all the given pairs. 'Nothing'
when no series matches, which keeps an absent metric apart from a zero one.
-}
seriesTotal :: Text -> [(Text, Text)] -> [Sample] -> Maybe Double
seriesTotal name wanted samples = case matching of
    [] -> Nothing
    found -> Just (sum (map sampleValue found))
  where
    matching = [s | s <- samples, sampleName s == name, all (`elem` sampleLabels s) wanted]

-- | The label pairs every sample carries: the resource labels the exporter repeats on each series.
commonLabels :: [Sample] -> [Text]
commonLabels = \case
    [] -> []
    sample : rest -> [key | pair@(key, _) <- sampleLabels sample, all (elem pair . sampleLabels) rest]

-- | One sample as a report line, without the given labels.
renderSample :: [Text] -> Sample -> Text
renderSample resourceLabels s =
    sampleName s <> labelPart <> " " <> show (sampleValue s)
  where
    kept = [(k, v) | (k, v) <- sampleLabels s, k `notElem` resourceLabels]
    labelPart
        | null kept = ""
        | otherwise = "{" <> T.intercalate "," [k <> "=" <> v | (k, v) <- kept] <> "}"

-- | A gauge sampled across a window. A missed sample is a scrape that failed or timed out.
data GaugeSummary = GaugeSummary
    { gsSamples :: Int
    , gsMissed :: Int
    , gsMax :: Maybe Double
    , gsMean :: Maybe Double
    , gsLast :: Maybe Double
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Summarise samples in the order taken, 'Nothing' marking a miss.
summariseGauge :: [Maybe Double] -> GaugeSummary
summariseGauge readings =
    GaugeSummary
        { gsSamples = length taken
        , gsMissed = length readings - length taken
        , gsMax = case taken of
            [] -> Nothing
            highest : rest -> Just (foldl' max highest rest)
        , gsMean = if null taken then Nothing else Just (sum taken / fromIntegral (length taken))
        , gsLast = listToMaybe (reverse taken)
        }
  where
    taken = catMaybes readings
