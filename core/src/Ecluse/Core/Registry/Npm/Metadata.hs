-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
-- The reads specialise here. Full laziness would float each member's rarely taken continuation out
-- of the element's continuation, and every member of a read would allocate it.
{-# OPTIONS_GHC -fno-full-laziness #-}

{- | npm's part of a metadata read, which "Ecluse.Core.Registry.Metadata.Fetch" drives.
Both reads fetch the full packument because publish-age rules need its @time@ map.
Selective reads materialise only the requested version and timestamp. A version read pairs the
typed projection with the selected version object, which the mirror write republishes.
-}
module Ecluse.Core.Registry.Npm.Metadata (
    -- * What the read driver runs
    npmRead,

    -- * The memory budget
    npmChargeFactors,

    -- * Walking a packument
    npmPackumentWalk,
    NpmFullRead,
    npmFullWalk,
    npmFullTable,

    -- * Pure projection
    projectNpmStream,
    projectNpmPacked,
    selectNpmRead,
    selectNpmVersionDoc,
) where

import Control.Monad.ST (ST, stToIO)
import Data.Aeson (Value (Object))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.JsonStream.TokenParser (TokenResult)
import Data.Map.Strict qualified as Map

import Ecluse.Core.Package (PackageInfo (..), PackageName, renderPackageName)
import Ecluse.Core.Package.Filter (enforceArtifactLocations, enforceArtifactLocationsOf)
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmCached, npmPacked)
import Ecluse.Core.Registry.Json.Intern (InternTable, Interned (..), decodedName, internName, tableTexts)
import Ecluse.Core.Registry.Json.Packed (docTable)
import Ecluse.Core.Registry.Json.Shape (Trees (..))
import Ecluse.Core.Registry.Json.Walk (Step, Steps, Walked (..), pureStep, readJsonWalk, readJsonWalkST)
import Ecluse.Core.Registry.Json.Writer (Writer, newWriter)
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Registry.Metadata (MetadataError, VersionDoc (..), VersionRead (..))
import Ecluse.Core.Registry.Metadata.Fetch.Types (DocumentWalk, EcosystemRead (..))
import Ecluse.Core.Registry.Metadata.Projection (streamError)
import Ecluse.Core.Registry.Npm.Document (PackedPackument)
import Ecluse.Core.Registry.Npm.Reader (PackumentRead (..), npmWalk, releaseUniqueFields)
import Ecluse.Core.Registry.Npm.Request (MetadataForm (Full), metadataRequest, npmArtifactHosts, packageUrl)
import Ecluse.Core.Registry.Npm.StreamingProjection (PackedRead, TreeRead, emptyPackedRead, emptyTreeRead, finishPacked, finishTree, keepsPackedRelease, keepsTreeRelease, packedStep, treeStep)
import Ecluse.Core.Registry.ServedDocument (objectField)
import Ecluse.Core.Security (AllowedHostPorts, Limits, ecosystemArtifactAuthorities, maxNestingDepth)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (..))
import Ecluse.Core.Version (Version, renderVersion)

-- | What the read driver needs to read a packument: the request, both walks, and their finishes.
npmRead :: EcosystemRead
npmRead =
    EcosystemRead
        { erRequest = \base token -> metadataRequest base token Full
        , erUniqueFields = releaseUniqueFields
        , erWalkFull = readNpmFull
        , erFinishFull = finishNpmFull
        , erWalkSelected = \limits name version -> readNpmPackument limits name (OneRelease (renderVersion version))
        , erFinishSelected = finishNpmVersion
        }

{- | npm's charges cover read peaks of 1.071 bytes per source byte and realistic output working sets
of 1.524 bytes per basis byte from one meter step up. Calibration is recorded in @docs/testing.md@.
-}
npmChargeFactors :: ChargeFactors
npmChargeFactors = ChargeFactors{cfFullReadPermille = 1200, cfOutputPermille = 2000}

-- Walk a packument's chunks into aeson's tree with the production field policy.
readNpmPackument :: Limits -> PackageName -> PackumentRead -> DocumentWalk (Walked TreeRead)
readNpmPackument limits name mode bound table = readJsonWalk bound (npmPackumentWalk limits name mode table)

-- | The production packument walk into aeson's tree over a caller's intern table.
npmPackumentWalk :: Limits -> PackageName -> PackumentRead -> InternTable -> TokenResult -> Step (Walked TreeRead)
npmPackumentWalk limits name mode table = npmWalk Trees (maxNestingDepth limits) mode (pureStep (treeStep limits name)) keepsTreeRelease table emptyTreeRead

-- | What a full read finishes with: the read's table, and its typed facts and packed releases.
type NpmFullRead = Walked PackedRead

-- Walk a whole packument's chunks into its packed form, given the origin's base URL.
readNpmFull :: Limits -> PackageName -> Text -> DocumentWalk NpmFullRead
readNpmFull limits name base bound table0 readChunk = do
    (table, writer) <- stToIO (npmFullTable base name table0)
    readJsonWalkST stToIO bound (npmFullWalk writer limits name table) readChunk

{- | A full read's table with the source author pointer every served release holds, and the writer
that packs each release against it.
-}
npmFullTable :: Text -> PackageName -> InternTable -> ST st (InternTable, Writer st)
npmFullTable base name table0 = (seeded,) <$> newWriter (Just (authorKey, pointer))
  where
    Interned authorKey withKey = internName (decodedName "author") table0
    Interned pointer seeded = internName (decodedName (authorPointer base name)) withKey

-- | The production full-read walk: each kept release packed by the writer against the caller's table.
npmFullWalk :: Writer st -> Limits -> PackageName -> InternTable -> TokenResult -> ST st (Steps (ST st) NpmFullRead)
npmFullWalk writer limits name table = npmWalk writer (maxNestingDepth limits) WholePackument (packedStep writer limits name) keepsPackedRelease table emptyPackedRead
{-# INLINE npmFullWalk #-}

finishNpmFull :: Limits -> PackageName -> Text -> StreamResult NpmFullRead -> Either MetadataError (PackageInfo, CachedDoc)
finishNpmFull limits name base streamed =
    bimap (enforceArtifactLocations npmArtifactAuthorities base) (fst npmPacked) <$> projectNpmPacked limits name base streamed

-- | Finish a full read over its sealed table, keeping its typed error classification.
projectNpmPacked :: Limits -> PackageName -> Text -> StreamResult NpmFullRead -> Either MetadataError (PackageInfo, PackedPackument)
projectNpmPacked limits name base streamed = do
    Walked table acc <- first (streamError limits) (streamValue streamed)
    finishPacked limits name (authorPointer base name) (docTable (tableTexts table)) acc

finishNpmVersion :: Limits -> PackageName -> Text -> Version -> StreamResult (Walked TreeRead) -> Either MetadataError VersionRead
finishNpmVersion limits name base version streamed = do
    projected <- projectNpmStream limits name base streamed
    let selected = selectNpmRead version (streamBytes streamed) projected
    pure selected{vrVersion = vrVersion selected >>= locationCheckedDoc base}

-- | Finish a streamed source, keeping its typed error classification.
projectNpmStream :: Limits -> PackageName -> Text -> StreamResult (Walked TreeRead) -> Either MetadataError (PackageInfo, Value)
projectNpmStream limits name base streamed = do
    Walked _ acc <- first (streamError limits) (streamValue streamed)
    finishTree limits name (authorPointer base name) acc

-- | Pair one release with its compact source object and the same document's latest tag.
selectNpmRead :: Version -> Int -> (PackageInfo, Value) -> VersionRead
selectNpmRead version bodyBytes (info, raw) =
    VersionRead
        { vrVersion = do
            details <- Map.lookup (renderVersion version) (infoVersions info)
            selected <- selectNpmVersionDoc version (fst npmCached raw)
            pure VersionDoc{vdDetails = details, vdRaw = Just selected}
        , vrBodyBytes = bodyBytes
        , vrUpstreamLatest = Map.lookup "latest" (infoDistTags info)
        }

authorPointer :: Text -> PackageName -> Text
authorPointer base name = "See " <> fromRight (base <> "/" <> renderPackageName name) (packageUrl base name)

locationCheckedDoc :: Text -> VersionDoc -> Maybe VersionDoc
locationCheckedDoc upstreamBaseUrl doc =
    (\details -> doc{vdDetails = details})
        <$> enforceArtifactLocationsOf npmArtifactAuthorities upstreamBaseUrl (vdDetails doc)

npmArtifactAuthorities :: AllowedHostPorts
npmArtifactAuthorities = ecosystemArtifactAuthorities npmArtifactHosts

-- | Select the object paired with a warm full projection under the same rendered version key.
selectNpmVersionDoc :: Version -> CachedDoc -> Maybe CachedDoc
selectNpmVersionDoc version doc = do
    Object packument <- snd npmCached doc
    versions <- objectField "versions" packument
    fst npmCached <$> KeyMap.lookup (Key.fromText (renderVersion version)) versions
