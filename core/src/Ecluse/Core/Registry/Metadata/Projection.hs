-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared metadata validation for full documents and selective adapter reads.
Ecosystem callbacks own wire projection and package-name parsing.
-}
module Ecluse.Core.Registry.Metadata.Projection (
    validateReportedName,
    projectionResult,
    streamError,
) where

import Data.Aeson (Value, parseJSON)
import Data.Aeson.Types (parseMaybe)

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (ParseError (ParseError))
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataBoundExceeded, MetadataNameMismatch, MetadataUndecodable))
import Ecluse.Core.Registry.WireSupport (Projection (NameMismatch, Projected))
import Ecluse.Core.Security (
    LimitError (TooDeeplyNested),
    Limits,
    maxNestingDepth,
 )

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

-- | Translate incremental parser bounds without requiring validity of skipped data.
streamError :: Limits -> ParseError -> MetadataError
streamError limits = \case
    ParseError "retained JSON nesting limit" -> MetadataBoundExceeded (TooDeeplyNested (maxNestingDepth limits))
    _ -> MetadataUndecodable
