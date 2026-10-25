-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Cargo name keys, without PyPI separator rules or Unicode case conversion.
module Ecluse.Core.Package.Cargo (
    cargoNameKey,
    cargoCollisionText,
) where

import Data.Char (isAsciiUpper, toLower)
import Data.Text qualified as T

-- | Lowercase ASCII letters for index lookup, preserving hyphens and underscores.
cargoNameKey :: Text -> Text
cargoNameKey = T.map lowerAscii

-- | Fold case and hyphen/underscore aliases without changing separator counts.
cargoCollisionText :: Text -> Text
cargoCollisionText = T.map collisionChar
  where
    collisionChar '_' = '-'
    collisionChar c = lowerAscii c

lowerAscii :: Char -> Char
lowerAscii c = if isAsciiUpper c then toLower c else c
