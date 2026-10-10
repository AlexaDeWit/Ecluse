-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Markdown tables as the performance reports print them: the writer of one row, and a
reader that finds each table in a report with its header and the heading above it. The
reader takes a report that another commit rendered, so it looks columns up by header name.
-}
module Ecluse.BenchReport.Markdown (
    -- * Writing
    cells,
    fixed,

    -- * Reading
    Table (..),
    tables,
    hasColumns,
    records,
    linkText,
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Numeric (showFFloat)

-- | One table row from its cells.
cells :: [Text] -> Text
cells xs = "| " <> T.intercalate " | " xs <> " |"

-- | A number to a fixed count of decimal places.
fixed :: Int -> Double -> Text
fixed places x = toText (showFFloat (Just places) x "")

-- | A table as a report holds it.
data Table = Table
    { tableHeading :: Maybe Text
    -- ^ The last heading above the table, without its @#@ marks.
    , tableHeader :: [Text]
    , tableRows :: [[Text]]
    -- ^ The rows with as many cells as the header. The reader drops any other row.
    }
    deriving stock (Eq, Show)

-- | Every table of a document, in order. Lines that belong to no table are skipped.
tables :: Text -> [Table]
tables = go Nothing . map T.strip . T.lines
  where
    go heading = \case
        [] -> []
        remaining@(line : rest)
            | Just title <- headingText line -> go (Just title) rest
            | isRow line ->
                let (block, after) = span isRow remaining
                 in maybeToList (tableOf heading (map splitRow block)) <> go heading after
            | otherwise -> go heading rest
    isRow = T.isPrefixOf "|"

tableOf :: Maybe Text -> [[Text]] -> Maybe Table
tableOf heading = \case
    [] -> Nothing
    header : rest ->
        Just (Table heading header (filter ((== length header) . length) (filter (not . isRule) rest)))
  where
    isRule row = all (T.all (`elem` ['-', ':', ' '])) row && any (T.any (== '-')) row

headingText :: Text -> Maybe Text
headingText line = case T.span (== '#') line of
    (marks, rest) | not (T.null marks) -> T.strip <$> T.stripPrefix " " rest
    _ -> Nothing

splitRow :: Text -> [Text]
splitRow line = map T.strip (T.splitOn "|" inner)
  where
    opened = T.drop 1 line
    inner = fromMaybe opened (T.stripSuffix "|" opened)

-- | Whether the header holds every named column. Names compare without case.
hasColumns :: [Text] -> Table -> Bool
hasColumns names table = all (`elem` map T.toLower (tableHeader table)) names

-- | Each row as a lookup from its lower-cased header cell to its value.
records :: Table -> [Map Text Text]
records table = map (Map.fromList . zip (map T.toLower (tableHeader table))) (tableRows table)

-- | The text of a cell that is one Markdown link, and any other cell unchanged.
linkText :: Text -> Text
linkText cell = fromMaybe cell $ do
    (label, target) <- T.breakOn "](" <$> T.stripPrefix "[" cell
    guard (not (T.null target) && T.isSuffixOf ")" target)
    pure label
