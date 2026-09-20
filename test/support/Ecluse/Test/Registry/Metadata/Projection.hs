-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Whole-tree reference projection for matched historical measurements.
module Ecluse.Test.Registry.Metadata.Projection (projectMetadata) where

import Data.Aeson (Value, eitherDecodeStrict)
import Ecluse.Core.Package (PackageInfo)
import Ecluse.Core.Registry (ParseError)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataBoundExceeded, MetadataUndecodable))
import Ecluse.Core.Registry.Metadata.Projection (projectionResult)
import Ecluse.Core.Registry.WireSupport (Projection)
import Ecluse.Core.Security (Limits, checkArtifactCount)
import Ecluse.Test.Security.Limits (checkNestingDepth, checkVersionCount)

-- | Check depth before projection and name agreement before counts, retaining the decoded document.
projectMetadata :: (Value -> Either ParseError (Projection PackageInfo)) -> Limits -> ByteString -> Either MetadataError (PackageInfo, Value)
projectMetadata project limits body = do
    value <- first (const MetadataUndecodable) (eitherDecodeStrict body)
    bounded <- first MetadataBoundExceeded (checkNestingDepth limits value)
    info <- first (const MetadataUndecodable) (project bounded) >>= projectionResult
    versionBounded <- first MetadataBoundExceeded (checkVersionCount limits info)
    boundedInfo <- first MetadataBoundExceeded (checkArtifactCount limits versionBounded)
    pure (boundedInfo, bounded)
