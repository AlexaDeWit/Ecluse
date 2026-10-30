-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Retained RubyGems installation metadata stays separate from grouped policy facts.
module Ecluse.Core.Registry.RubyGems.Document (
    GemRequirement,
    GemDependency (..),
    GemRow (..),
    GemDocument (..),
    restrictGemDocument,
    selectGemVersion,
    gemDocumentBytes,
) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time (UTCTime)

import Ecluse.Core.Package (Hash, PackageName, hashValue, renderPackageName)
import Ecluse.Core.Package.Entry (EntryKey)
import Ecluse.Core.Registry.RubyGems.Project (GemCoordinate, coordinateFilename, coordinatePlatform, coordinateVersion, unGemPlatform)
import Ecluse.Core.Server.Path (unFilename)
import Ecluse.Core.Text (textStorageBytes)
import Ecluse.Core.Version (Version, renderVersion)

-- | Conjunctive native clauses retain their exact operators and version spelling.
type GemRequirement = NonEmpty Text

-- | A runtime dependency and every conjunctive requirement on that dependency.
data GemDependency = GemDependency
    { gdName :: PackageName
    , gdRequirement :: GemRequirement
    }
    deriving stock (Eq, Show)

-- | One artifact retains its original physical position and independently owned source bytes.
data GemRow = GemRow
    { grEntryKey :: EntryKey
    , grCoordinate :: GemCoordinate
    , grDependencies :: [GemDependency]
    , grRequiredRuby :: GemRequirement
    , grRequiredRubyGems :: GemRequirement
    , grChecksum :: Hash
    , grPublishedAt :: Maybe UTCTime
    , grExtras :: Map Text Text
    , grSource :: ByteString
    }
    deriving stock (Eq, Show)

-- | Ordered per-package rows preserve platform distinctions without fetching compact specifications.
data GemDocument = GemDocument
    { gemName :: PackageName
    , gemHeader :: ByteString
    , gemRows :: [GemRow]
    }
    deriving stock (Eq, Show)

-- | Keep only artifacts authorised by their original source positions.
restrictGemDocument :: Set EntryKey -> GemDocument -> GemDocument
restrictGemDocument admitted document = document{gemRows = filter ((`Set.member` admitted) . grEntryKey) (gemRows document)}

-- | Retain every platform row for one exact raw version, without comparison-key aliases.
selectGemVersion :: Version -> GemDocument -> Maybe GemDocument
selectGemVersion version document = do
    let rows = filter ((== renderVersion version) . renderVersion . coordinateVersion . grCoordinate) (gemRows document)
    guard (not (null rows))
    pure document{gemRows = rows}

{- | Cache units for a retained document. Every row carries the document's gem name, so the name is
measured once and counted per row.
-}
gemDocumentBytes :: GemDocument -> Int64
gemDocumentBytes document = 64 + nameBytes + fromIntegral (BS.length (gemHeader document)) + sum (map rowBytes (gemRows document))
  where
    nameBytes = nativeTextBytes (renderPackageName (gemName document))
    rowBytes row =
        let coordinate = grCoordinate row
            strings = [renderVersion (coordinateVersion coordinate), unGemPlatform (coordinatePlatform coordinate), unFilename (coordinateFilename coordinate), hashValue (grChecksum row)]
            requirementBytes values = sum [32 + nativeTextBytes clause | clause <- toList values]
         in 256
                + nameBytes
                + sum (map nativeTextBytes strings)
                + fromIntegral (BS.length (grSource row))
                + requirementBytes (grRequiredRuby row)
                + requirementBytes (grRequiredRubyGems row)
                + sum [64 + nativeTextBytes (renderPackageName (gdName dependency)) + requirementBytes (gdRequirement dependency) | dependency <- grDependencies row]
                + sum [64 + nativeTextBytes key + nativeTextBytes value | (key, value) <- Map.toList (grExtras row)]

nativeTextBytes :: Text -> Int64
nativeTextBytes = fromIntegral . textStorageBytes
