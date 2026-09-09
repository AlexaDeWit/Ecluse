-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared metadata validation for full documents and selective adapter reads.
Ecosystem callbacks own wire projection and package-name parsing.
-}
module Ecluse.Core.Registry.Metadata.Projection (
    projectMetadata,
    validateReportedName,
    projectionResult,
    selectiveError,
) where

import Data.Aeson (Value, eitherDecodeStrict, parseJSON)
import Data.Aeson.Types (parseMaybe)

import Ecluse.Core.Json.Selective (SelectiveError (SelectiveTooDeeplyNested, SelectiveUndecodable))
import Ecluse.Core.Package (PackageInfo, PackageName)
import Ecluse.Core.Registry (ParseError)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataBoundExceeded, MetadataNameMismatch, MetadataUndecodable))
import Ecluse.Core.Registry.WireSupport (Projection (NameMismatch, Projected))
import Ecluse.Core.Security (
    LimitError (TooDeeplyNested),
    Limits,
    checkArtifactCount,
    checkNestingDepth,
    checkVersionCount,
    maxNestingDepth,
 )

-- | Check depth before projection and name agreement before counts, retaining the decoded document.
projectMetadata :: (Value -> Either ParseError (Projection PackageInfo)) -> Limits -> ByteString -> Either MetadataError (PackageInfo, Value)
projectMetadata project limits body = do
    value <- first (const MetadataUndecodable) (eitherDecodeStrict body)
    bounded <- first MetadataBoundExceeded (checkNestingDepth limits value)
    info <- first (const MetadataUndecodable) (project bounded) >>= projectionResult
    versionBounded <- first MetadataBoundExceeded (checkVersionCount limits info)
    boundedInfo <- first MetadataBoundExceeded (checkArtifactCount limits versionBounded)
    pure (boundedInfo, bounded)

-- | An absent, non-string, or rejected name is an undecodable document.
validateReportedName :: (Text -> Either ParseError PackageName) -> Maybe Value -> Either MetadataError PackageName
validateReportedName parseName = \case
    Nothing -> Left MetadataUndecodable
    Just nameValue -> case parseMaybe parseJSON nameValue of
        Nothing -> Left MetadataUndecodable
        Just raw -> first (const MetadataUndecodable) (parseName raw)

-- | Preserve the reported name when refusing a mismatched origin.
projectionResult :: Projection a -> Either MetadataError a
projectionResult = \case
    NameMismatch reported -> Left (MetadataNameMismatch reported)
    Projected projected -> Right projected

-- | Selective reads report the same decode and depth failures as full reads.
selectiveError :: Limits -> SelectiveError -> MetadataError
selectiveError limits = \case
    SelectiveUndecodable -> MetadataUndecodable
    SelectiveTooDeeplyNested -> MetadataBoundExceeded (TooDeeplyNested (maxNestingDepth limits))
