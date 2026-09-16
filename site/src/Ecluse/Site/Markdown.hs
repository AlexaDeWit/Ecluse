-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The Markdown vocabulary of the generated OpenAPI reference: headings that carry
an anchor and CSS classes, tables, and the inline forms a table cell needs.

A fragment is embedded in a Zola page, so the emitted text is CommonMark plus the
heading-attribute extension and inline HTML. An attribute block only takes effect
at the end of a heading line.
-}
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

-- | A heading at the given level, which is clamped to at least one.
heading :: Int -> Text -> Text
heading level text = T.replicate (max 1 level) "#" <> " " <> text

-- | A heading carrying an anchor id and CSS classes.
attributedHeading :: Int -> Text -> [Text] -> Text -> Text
attributedHeading level anchor classes text =
    heading level text <> " {#" <> anchor <> foldMap (" ." <>) classes <> "}"

-- | A left-aligned table: the header cells, then one line per row.
table :: [Text] -> [[Text]] -> [Text]
table header rows = tableRow header : tableRow (":--" <$ header) : map tableRow rows

tableRow :: [Text] -> Text
tableRow cells = "| " <> T.intercalate " | " cells <> " |"

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
