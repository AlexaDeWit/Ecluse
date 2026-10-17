-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pagefind title fallbacks for generated Haddock HTML.
module Ecluse.Site.Search (prepareSearchMetadata) where

import Data.Text qualified as T

{- | Retain generated titles, or use the source filename when Haddock omits the title.
Pagefind gives automatic and explicit metadata precedence over this fallback.
-}
prepareSearchMetadata :: Text -> Text -> Text
prepareSearchMetadata sourceName html
    | "<title>" `T.isInfixOf` html = T.replace "<title>" titleOpen html
    | "<title" `T.isInfixOf` html = html
    | otherwise = T.replace "</head>" (titleOpen <> escapeTitle sourceName <> "</title></head>") html
  where
    titleOpen = "<title data-pagefind-default-meta=\"title\">"

escapeTitle :: Text -> Text
escapeTitle = T.replace ">" "&gt;" . T.replace "<" "&lt;" . T.replace "&" "&amp;"
