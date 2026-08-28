-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The Markdown vocabulary the generated site fragments share: headings that
carry an anchor and CSS classes, aligned tables, and the inline forms a table cell
needs.

A fragment is embedded in a Zola page, so the emitted text is CommonMark plus the
heading-attribute extension and inline HTML. An attribute block only takes effect
at the end of a heading line.
-}
module Ecluse.Site.Markdown (
    -- * Blocks
    Alignment (AlignLeft, AlignRight),
    heading,
    attributedHeading,
    table,

    -- * Inlines
    bold,
    code,
    htmlSpan,
    link,

    -- * Text preparation
    escapeCell,
    slugify,
) where

import Data.Char (isAlphaNum)
import Data.Text qualified as T

-- | How a table column's contents sit under its header.
data Alignment
    = AlignLeft
    | AlignRight
    deriving stock (Eq, Show)

-- | A heading at the given level, which is clamped to at least one.
heading :: Int -> Text -> Text
heading level text = T.replicate (max 1 level) "#" <> " " <> text

-- | A heading carrying an anchor id and CSS classes.
attributedHeading :: Int -> Text -> [Text] -> Text -> Text
attributedHeading level anchor classes text =
    heading level text <> " {#" <> anchor <> foldMap (" ." <>) classes <> "}"

-- | A table: the header cells with their alignments, then one line per row.
table :: [(Alignment, Text)] -> [[Text]] -> [Text]
table header rows = tableRow (map snd header) : tableRow (map (marker . fst) header) : map tableRow rows
  where
    marker = \case
        AlignLeft -> ":--"
        AlignRight -> "--:"

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

-- | An inline HTML span carrying CSS classes, for styling Markdown cannot express.
htmlSpan :: [Text] -> Text -> Text
htmlSpan classes text = "<span class=\"" <> T.unwords classes <> "\">" <> text <> "</span>"

{- | Make free prose safe inside a table cell. A pipe would end the cell, and any
line break would end the row.
-}
escapeCell :: Text -> Text
escapeCell = T.replace "|" "\\|" . T.unwords . T.words

-- | The anchor form of a name: lowercase, with each run of other characters a hyphen.
slugify :: Text -> Text
slugify = T.intercalate "-" . filter (not . T.null) . T.split (not . isAlphaNum) . T.toLower
