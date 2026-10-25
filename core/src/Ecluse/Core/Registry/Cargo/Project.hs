-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Cargo identities for the supported crates.io-compatible ASCII name contract.
module Ecluse.Core.Registry.Cargo.Project (
    projectName,
    cargoNameKey,
    cargoCollisionKey,
) where

import Data.Char (isAsciiLower, isAsciiUpper)
import Data.Text qualified as T

import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.Core.Package (PackageName, mkPackageName, pkgEcosystem, pkgNamespace, renderPackageName)
import Ecluse.Core.Package.Cargo (cargoCollisionText, cargoNameKey)
import Ecluse.Core.Registry (ParseError)
import Ecluse.Core.Registry.WireSupport (nameComponentWith, withinNameLimit)
import Ecluse.Core.Text (isAsciiAlphaNum)

-- | Parse a native name, retaining its published case and separator spelling.
projectName :: Text -> Either ParseError PackageName
projectName raw = do
    withinNameLimit "Cargo package name" 64 raw
    mkPackageName Cargo Nothing <$> nameComponentWith "Cargo package name" usableName raw

usableName :: Text -> Bool
usableName raw =
    maybe False (alphabetic . fst) (T.uncons raw)
        && T.all nameChar raw
  where
    alphabetic c = isAsciiLower c || isAsciiUpper c
    nameChar c = isAsciiAlphaNum c || c == '-' || c == '_'

-- | Match ownership aliases only for an unscoped Cargo identity.
cargoCollisionKey :: PackageName -> Maybe Text
cargoCollisionKey name = do
    guard (pkgEcosystem name == Cargo && isNothing (pkgNamespace name))
    pure (cargoCollisionText (renderPackageName name))
