-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Prometheus samples shared by load reports and strict product-counter assertions.
module Ecluse.Test.Exposition (
    Sample (..),
    parseExposition,
    parseExpositionStrict,
) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isSpace)
import Data.Text qualified as T

-- | One series value, independent of any exemplar that annotates it.
data Sample = Sample
    { sampleName :: Text
    , sampleLabels :: [(Text, Text)]
    , sampleValue :: Double
    }
    deriving stock (Eq, Show)

data SampleSyntax = PrefixSamples | CompleteSamples

-- | Read sample prefixes, skipping comments, blank lines, and unreadable lines for load reports.
parseExposition :: Text -> [Sample]
parseExposition = mapMaybe (fmap fst . samplePrefix PrefixSamples) . lines

-- | Read complete samples, including exemplar annotations, or report the first malformed line.
parseExpositionStrict :: Text -> Either Text [Sample]
parseExpositionStrict = fmap catMaybes . traverse sampleLine . zip [1 :: Int ..] . lines
  where
    sampleLine (number, line)
        | ignorable line = Right Nothing
        | otherwise = case samplePrefix CompleteSamples line of
            Just (sample, rest) | isJust (sampleTail rest) -> Right (Just sample)
            _ -> Left ("metrics scrape contains a malformed sample at line " <> show number <> ": " <> line)

ignorable :: Text -> Bool
ignorable line = T.null stripped || "#" `T.isPrefixOf` stripped
  where
    stripped = T.strip line

samplePrefix :: SampleSyntax -> Text -> Maybe (Sample, Text)
samplePrefix syntax line
    | ignorable line = Nothing
    | otherwise = do
        let (name, rest) = T.break nameEnd stripped
        guard (not (T.null name))
        case syntax of
            PrefixSamples -> pure ()
            CompleteSamples -> guard (identifier True name)
        (labels, afterLabels) <- case T.uncons rest of
            Just ('{', inner) -> labelSet syntax inner []
            _ -> Just ([], rest)
        case syntax of
            PrefixSamples -> pure ()
            CompleteSamples -> guard (maybe False (isSpace . fst) (T.uncons afterLabels))
        let (valueText, afterValue) = T.break isSpace (T.stripStart afterLabels)
        value <- sampleNumber syntax valueText
        pure (Sample name labels value, afterValue)
  where
    stripped = T.strip line
    nameEnd c =
        c == '{' || case syntax of
            PrefixSamples -> c == ' '
            CompleteSamples -> isSpace c

labelSet :: SampleSyntax -> Text -> [(Text, Text)] -> Maybe ([(Text, Text)], Text)
labelSet syntax input acc = case T.uncons start of
    Just ('}', rest) -> Just (reverse acc, rest)
    Just _ -> do
        let (key, afterKey) = T.breakOn "=\"" start
        guard (not (T.null key) && not (T.null afterKey))
        case syntax of
            PrefixSamples -> pure ()
            CompleteSamples -> do
                guard (identifier False (T.strip key))
                guard (all ((/= T.strip key) . fst) acc)
        (value, rest) <- quoted syntax (T.drop 2 afterKey) ""
        let labels = (T.strip key, value) : acc
        case syntax of
            PrefixSamples -> labelSet syntax rest labels
            CompleteSamples -> case T.uncons (T.stripStart rest) of
                Just (',', more) -> labelSet syntax more labels
                Just ('}', more) -> Just (reverse labels, more)
                _ -> Nothing
    Nothing -> Nothing
  where
    start = T.dropWhile separator input
    separator c = case syntax of
        PrefixSamples -> c == ',' || c == ' '
        CompleteSamples -> isSpace c

quoted :: SampleSyntax -> Text -> Text -> Maybe (Text, Text)
quoted syntax input acc = case T.uncons input of
    Just ('"', rest) -> Just (acc, rest)
    Just ('\\', rest) -> case T.uncons rest of
        Just ('n', more) -> quoted syntax more (T.snoc acc '\n')
        Just (c, more) -> case syntax of
            PrefixSamples -> quoted syntax more (T.snoc acc c)
            CompleteSamples | c == '\\' || c == '"' -> quoted syntax more (T.snoc acc c)
            CompleteSamples -> Nothing
        Nothing -> Nothing
    Just (c, rest) -> quoted syntax rest (T.snoc acc c)
    Nothing -> Nothing

sampleNumber :: SampleSyntax -> Text -> Maybe Double
sampleNumber syntax = \case
    "+Inf" -> Just (1 / 0)
    "-Inf" -> Just (negate (1 / 0))
    "NaN" -> case syntax of
        CompleteSamples -> Just (0 / 0)
        PrefixSamples -> readMaybe "NaN"
    other -> case syntax of
        PrefixSamples -> readMaybe (toString other)
        CompleteSamples -> do
            guard (T.all (\c -> isDigit c || c `elem` (".eE+-" :: String)) other)
            guard (not ("+-" `T.isPrefixOf` other))
            readMaybe (toString (fromMaybe other (T.stripPrefix "+" other)))

identifier :: Bool -> Text -> Bool
identifier metric input = case T.uncons input of
    Just (first, rest) -> initial first && T.all (\c -> initial c || isDigit c) rest
    Nothing -> False
  where
    initial c = isAsciiLower c || isAsciiUpper c || c == '_' || (metric && c == ':')

sampleTail :: Text -> Maybe ()
sampleTail input = case T.uncons (T.stripStart input) of
    Nothing -> Just ()
    Just ('#', rest) -> exemplar rest
    Just _ -> do
        let (timestamp, rest) = T.break isSpace (T.stripStart input)
        guard (T.all (\c -> isDigit c || c == '-' || c == '+') timestamp)
        _ <- readMaybe (toString timestamp) :: Maybe Integer
        case T.uncons (T.stripStart rest) of
            Nothing -> Just ()
            Just ('#', more) -> exemplar more
            _ -> Nothing

exemplar :: Text -> Maybe ()
exemplar input = do
    inner <- T.stripPrefix "{" (T.stripStart input)
    (labels, afterLabels) <- labelSet CompleteSamples inner []
    guard (not (null labels))
    guard (maybe False (isSpace . fst) (T.uncons afterLabels))
    case words afterLabels of
        [value] -> void (sampleNumber CompleteSamples value)
        [value, timestamp] -> do
            _ <- sampleNumber CompleteSamples value
            time <- sampleNumber CompleteSamples timestamp
            guard (not (isNaN time || isInfinite time))
        _ -> Nothing
