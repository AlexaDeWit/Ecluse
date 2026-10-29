-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared metadata validation for full documents and selective adapter reads.
Ecosystem callbacks own wire projection and package-name parsing.
-}
module Ecluse.Core.Registry.Metadata.Projection (
    validateReportedName,
    validateNameAgreement,
    projectionResult,
    streamError,
    finishSelectedVersion,
) where

import Data.Aeson (Value, parseJSON)
import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict qualified as Map

import Ecluse.Core.Package (PackageInfo (infoVersions), PackageName)
import Ecluse.Core.Registry (ParseError)
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.JsonStream (StreamResult (..), isNestingLimit)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataBoundExceeded, MetadataNameMismatch, MetadataUndecodable), VersionDoc (..), VersionRead (..))
import Ecluse.Core.Registry.WireSupport (Projection (NameMismatch, Projected), checkNameAgreement)
import Ecluse.Core.Security (
    LimitError (TooDeeplyNested),
    Limits,
    maxNestingDepth,
 )
import Ecluse.Core.Version (Version, renderVersion)

-- | An absent, non-string, or rejected name is an undecodable document.
validateReportedName :: (Text -> Either ParseError PackageName) -> Maybe Value -> Either MetadataError PackageName
validateReportedName parseName = \case
    Nothing -> Left MetadataUndecodable
    Just nameValue -> case parseMaybe parseJSON nameValue of
        Nothing -> Left MetadataUndecodable
        Just raw -> first (const MetadataUndecodable) (parseName raw)

-- | Validate the native name and preserve it when refusing a mismatched origin.
validateNameAgreement :: (Text -> Either ParseError PackageName) -> PackageName -> Maybe Value -> Either MetadataError PackageName
validateNameAgreement parseName requested value = do
    reported <- validateReportedName parseName value
    projectionResult (checkNameAgreement requested reported reported)

-- | Preserve the reported name when refusing a mismatched origin.
projectionResult :: Projection a -> Either MetadataError a
projectionResult = \case
    NameMismatch reported -> Left (MetadataNameMismatch reported)
    Projected projected -> Right projected

-- | Translate incremental parser bounds without requiring validity of skipped data.
streamError :: Limits -> ParseError -> MetadataError
streamError limits err =
    if isNestingLimit err
        then MetadataBoundExceeded (TooDeeplyNested (maxNestingDepth limits))
        else MetadataUndecodable

{- | A selected walk's version: its typed details by raw version and the native document @select@
keeps for it. No upstream latest tag is reported.
-}
finishSelectedVersion :: (a -> PackageInfo) -> (Version -> a -> Maybe CachedDoc) -> Limits -> Version -> StreamResult a -> Either MetadataError VersionRead
finishSelectedVersion info select limits version streamed = do
    result <- first (streamError limits) (streamValue streamed)
    let selected = do
            details <- Map.lookup (renderVersion version) (infoVersions (info result))
            raw <- select version result
            pure VersionDoc{vdDetails = details, vdRaw = Just raw}
    pure VersionRead{vrVersion = selected, vrBodyBytes = streamBytes streamed, vrUpstreamLatest = Nothing}
