-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | PyPI name normalisation, reached through 'Ecluse.Core.Package.canonicalise'.
module Ecluse.Core.Package.Pep503 (
    normalisePyPI,
) where

import Data.Text qualified as T

import Ecluse.Core.Text (toLowerUnlessAsciiUnchanged)

-- | Lowercase and collapse @-_.@ runs for PyPI keys, dropping edge separators without validating names.
normalisePyPI :: Text -> Text
normalisePyPI t
    | hasCanonicalSeparators lowered = lowered
    | otherwise =
        T.intercalate "-"
            . filter (not . T.null)
            . T.splitOn "-"
            $ T.map (\c -> if c == '_' || c == '.' then '-' else c) lowered
  where
    lowered = toLowerUnlessAsciiUnchanged t

hasCanonicalSeparators :: Text -> Bool
hasCanonicalSeparators t =
    not (T.any (\c -> c == '_' || c == '.') t)
        && not (T.isPrefixOf "-" t)
        && not (T.isSuffixOf "-" t)
        && not (T.isInfixOf "--" t)
