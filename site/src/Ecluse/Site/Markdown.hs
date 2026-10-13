-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | CommonMark fragments with Zola heading attributes for the generated OpenAPI reference.
module Ecluse.Site.Markdown (
    -- * Blocks
    heading,
    attributedHeading,
    table,

    -- * Inlines
    bold,
    code,
    link,

    -- * Text preparation
    escapeCell,
    slugify,
) where

import Data.Char (isAlphaNum)
import Data.Text qualified as T
import Ecluse.Report.Markdown (cells)

-- | A heading at the given level, which is clamped to at least one.
heading :: Int -> Text -> Text
heading level text = T.replicate (max 1 level) "#" <> " " <> text

-- | A heading carrying an anchor id and CSS classes.
attributedHeading :: Int -> Text -> [Text] -> Text -> Text
attributedHeading level anchor classes text =
    heading level text <> " {#" <> anchor <> foldMap (" ." <>) classes <> "}"

-- | A left-aligned table: the header cells, then one line per row.
table :: [Text] -> [[Text]] -> [Text]
table header rows = cells header : cells (":--" <$ header) : map cells rows

-- | Bold inline text.
bold :: Text -> Text
bold text = "**" <> text <> "**"

-- | Inline code.
code :: Text -> Text
code text = "`" <> text <> "`"

-- | An inline link.
link :: Text -> Text -> Text
link text target = "[" <> text <> "](" <> target <> ")"

{- | Make free prose safe inside a table cell. A pipe would end the cell, and any
line break would end the row.
-}
escapeCell :: Text -> Text
escapeCell = T.replace "|" "\\|" . T.unwords . T.words

-- | The anchor form of a name: lowercase, with each run of other characters a hyphen.
slugify :: Text -> Text
slugify = T.intercalate "-" . filter (not . T.null) . T.split (not . isAlphaNum) . T.toLower
