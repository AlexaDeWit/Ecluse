-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The PEP 503 name grammar (PyPI).

'normalisePyPI' produces the canonical key a PyPI package name matches on.
"Ecluse.Core.Package" dispatches to it on the ecosystem tag from
'Ecluse.Core.Package.canonicalise', the way "Ecluse.Core.Version" dispatches to its
per-ecosystem version grammars. Every other caller reads it through that dispatch.
-}
module Ecluse.Core.Package.Pep503 (
    normalisePyPI,
) where

import Data.Text qualified as T

{- | PEP 503 name normalisation: lower-case, and collapse each run of @\'-\'@\/@\'_\'@\/@\'.\'@
to a single @\'-\'@, so two spellings of one distribution share a canonical key.
-}
normalisePyPI :: Text -> Text
normalisePyPI t =
    T.intercalate "-"
        . filter (not . T.null)
        . T.splitOn "-"
        $ T.map (\c -> if c == '_' || c == '.' then '-' else c) (T.toLower t)
