-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Independent Compact Index observations retain physical rows and opaque native fields.
module Ecluse.Test.Registry.RubyGems.Reference (
    RubyInfoRow (..),
    referenceInfo,
) where

import Data.Text qualified as T
import Data.Text.Encoding qualified as Text

-- | Raw versions and platforms remain distinct from comparison keys.
data RubyInfoRow = RubyInfoRow
    { rowPosition :: Int
    , rowVersion :: Text
    , rowPlatform :: Text
    , rowDependencies :: [(Text, [Text])]
    , rowFields :: [(Text, Text)]
    }
    deriving stock (Eq, Show)

-- | This test reader shares no parser or coordinate helper with the production adapter.
referenceInfo :: ByteString -> Either Text [RubyInfoRow]
referenceInfo bytes = do
    input <- first (toText . show) (Text.decodeUtf8' bytes)
    let physical = zip [0 ..] (T.splitOn "\n" input)
    case dropWhile ((/= "---") . snd) physical of
        [] -> Left "Compact Index header delimiter is absent"
        _ : rows -> traverse readRow (filter (not . T.null . snd) rows)
  where
    readRow (position, raw) = do
        let (coordinate, rest) = T.breakOn " " raw
            (version, platformSuffix) = T.breakOn "-" coordinate
            platform = if T.null platformSuffix then "ruby" else T.drop 1 platformSuffix
        when (T.null coordinate || T.null rest || T.null platform) (Left "Compact Index coordinate is incomplete")
        let (dependencies, attributes) = T.breakOn "|" (T.drop 1 rest)
        when (T.null attributes) (Left "Compact Index metadata delimiter is absent")
        parsedDependencies <- traverse dependency (nonemptyParts "," dependencies)
        fields <- traverse attribute (nonemptyParts "," (T.drop 1 attributes))
        pure (RubyInfoRow position version platform parsedDependencies fields)
    dependency value = do
        (name, requirements) <- separated ":" value
        pure (name, T.splitOn "&" requirements)
    attribute = separated ":"
    separated delimiter value = case T.breakOn delimiter value of
        (key, remainder)
            | not (T.null key), not (T.null remainder) -> Right (key, T.drop (T.length delimiter) remainder)
        _ -> Left "Compact Index field is incomplete"
    nonemptyParts delimiter = filter (not . T.null) . T.splitOn delimiter
