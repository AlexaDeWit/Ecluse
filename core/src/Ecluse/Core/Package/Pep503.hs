-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The PEP 503 name grammar (PyPI): the canonical key a package name matches on.

'Ecluse.Core.Package.canonicalise' dispatches here on the ecosystem tag, and every other caller
reads the grammar through that dispatch.
-}
module Ecluse.Core.Package.Pep503 (
    normalisePyPI,
) where

import Data.Text qualified as T

{- | PEP 503 name normalisation: lower-case, then collapse each run of @-@, @_@ or @.@ to a
single @-@, so two spellings of one distribution share a canonical key.
-}
normalisePyPI :: Text -> Text
normalisePyPI t =
    T.intercalate "-"
        . filter (not . T.null)
        . T.splitOn "-"
        $ T.map (\c -> if c == '_' || c == '.' then '-' else c) (T.toLower t)
