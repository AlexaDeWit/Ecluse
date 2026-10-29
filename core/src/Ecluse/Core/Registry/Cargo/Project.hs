-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Cargo identities for the supported crates.io-compatible ASCII name contract.
module Ecluse.Core.Registry.Cargo.Project (
    projectName,
    cargoNameKey,
    cargoNameChars,
    cargoCollisionKey,
    cargoArchiveStem,
    cargoArchiveFilename,
) where

import Data.Char (isAsciiLower, isAsciiUpper)
import Data.Text qualified as T

import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.Core.Package (PackageName, mkPackageName, pkgEcosystem, pkgNamespace, renderPackageName)
import Ecluse.Core.Package.Cargo (cargoCollisionText, cargoNameKey)
import Ecluse.Core.Registry (ParseError)
import Ecluse.Core.Registry.WireSupport (nameComponentWith, withinNameLimit)
import Ecluse.Core.Text (isAsciiAlphaNum)
import Ecluse.Core.Version (Version, renderVersion)

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

-- | Native name characters also cover every level of CodeArtifact prefix subdivision.
cargoNameChars :: [Char]
cargoNameChars = filter nameChar ['\0' .. '\127']

nameChar :: Char -> Bool
nameChar c = isAsciiAlphaNum c || c == '-' || c == '_'

-- | Match ownership aliases only for an unscoped Cargo identity.
cargoCollisionKey :: PackageName -> Maybe Text
cargoCollisionKey name = do
    guard (pkgEcosystem name == Cargo && isNothing (pkgNamespace name))
    pure (cargoCollisionText (renderPackageName name))

-- | The archive root directory: the published name and raw version joined by a hyphen.
cargoArchiveStem :: Text -> Text -> Text
cargoArchiveStem name version = name <> "-" <> version

-- | The logical @.crate@ filename for a published name and raw version.
cargoArchiveFilename :: Text -> Version -> Text
cargoArchiveFilename name version = cargoArchiveStem name (renderVersion version) <> ".crate"
