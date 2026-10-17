-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pagefind title fallbacks for generated Haddock HTML.
module Ecluse.Site.Search (prepareSearchMetadata) where

import Data.Text qualified as T

{- | Annotate the generated, attribute-free title element without changing its escaped text.
Pagefind uses this fallback only when automatic or explicit title metadata is absent.
-}
prepareSearchMetadata :: Text -> Text
prepareSearchMetadata = T.replace "<title>" "<title data-pagefind-default-meta=\"title\">"
