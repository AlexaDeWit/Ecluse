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
) where

import Data.Set qualified as Set
import Data.Time (UTCTime)

import Ecluse.Core.Package (Hash, PackageName)
import Ecluse.Core.Package.Entry (EntryKey)
import Ecluse.Core.Registry.RubyGems.Project (GemCoordinate, coordinateVersion)
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
