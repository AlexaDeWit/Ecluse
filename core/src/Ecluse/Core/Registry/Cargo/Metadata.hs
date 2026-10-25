-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Prepared sparse metadata reads capture checked discovery and keep all finishing operations pure.
module Ecluse.Core.Registry.Cargo.Metadata (
    cargoRead,
    prepareCargoRead,
    projectCargoIndex,
    selectCargoVersionDoc,
) where

import Data.Aeson (Value (Object))
import Data.Map.Strict qualified as Map

import Ecluse.Core.Package (PackageInfo (infoVersions), PackageName)
import Ecluse.Core.Registry (UrlFormationError (UnparseableUrl))
import Ecluse.Core.Registry.CachedDocument (CachedDoc, cargoCached)
import Ecluse.Core.Registry.Cargo.Discovery (CargoDiscovery, cargoDiscovery, discoveryIndex)
import Ecluse.Core.Registry.Cargo.Document (cargoDocument, cargoRecords)
import Ecluse.Core.Registry.Cargo.Reader (CargoIndexResult (..), CargoRead (..), readCargoIndex, readCargoIndexCharged, readCargoIndexChunks)
import Ecluse.Core.Registry.Cargo.Request (cargoIndexRequest)
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataBoundExceeded, MetadataUndecodable), VersionDoc (..), VersionRead (..))
import Ecluse.Core.Registry.Metadata.Fetch.Preparation (PrepareRead (..))
import Ecluse.Core.Registry.Metadata.Fetch.Types (EcosystemRead (..))
import Ecluse.Core.Registry.Metadata.Projection (streamError)
import Ecluse.Core.Registry.Origin (OriginClient (ocChargeSelectedRead))
import Ecluse.Core.Registry.Request (baseRequestUrl)
import Ecluse.Core.Registry.ServedDocument (stringField)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), Limits (maxMetadataBytes))
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Version (Version, renderVersion)

-- | Prepare after the selected origin receives its caller or service credential. Boot performs no discovery.
prepareCargoRead :: PrepareRead
prepareCargoRead = PrepareRead (\tracing origin -> fmap (cargoReadWith (ocChargeSelectedRead origin)) <$> cargoDiscovery tracing origin)

-- | Full and selected reads scan through EOF under one captured discovery response.
cargoRead :: CargoDiscovery -> EcosystemRead
cargoRead = cargoReadWith (const pass)

cargoReadWith :: (Int -> IO ()) -> CargoDiscovery -> EcosystemRead
cargoReadWith pay discovery =
    EcosystemRead
        { erRequest = \base token name -> do
            unless (registryUrlText (baseRequestUrl base) == discoveryIndex discovery) (Left (UnparseableUrl "Cargo discovery belongs to another index root"))
            cargoIndexRequest base token name
        , erUniqueFields = ["vers", "cksum"]
        , erWalkFull = \limits name _ -> readCargoIndex discovery limits name FullRead
        , erFinishFull = \limits _ base -> finishFull discovery limits base
        , erWalkSelected = \limits name version -> readCargoIndexCharged pay discovery limits name (SelectedRead version)
        , erFinishSelected = \limits name base version streamed -> do
            unless (base == discoveryIndex discovery) (Left MetadataUndecodable)
            finishSelected limits name base version streamed
        }

-- | Project held bytes with production framing, limits, discovery and full-read finishing.
projectCargoIndex :: CargoDiscovery -> Limits -> PackageName -> ByteString -> Either MetadataError (PackageInfo, CachedDoc)
projectCargoIndex discovery limits name bytes = do
    streamed <- first MetadataBoundExceeded (readCargoIndexChunks discovery limits name FullRead (MetadataBodyLimit (maxMetadataBytes limits)) [bytes])
    finishFull discovery limits (discoveryIndex discovery) streamed

finishFull :: CargoDiscovery -> Limits -> Text -> StreamResult CargoIndexResult -> Either MetadataError (PackageInfo, CachedDoc)
finishFull discovery limits base streamed = do
    unless (base == discoveryIndex discovery) (Left MetadataUndecodable)
    result <- first (streamError limits) (streamValue streamed)
    pure (cargoIndexInfo result, cargoIndexDocument result)

finishSelected :: Limits -> PackageName -> Text -> Version -> StreamResult CargoIndexResult -> Either MetadataError VersionRead
finishSelected limits _ _ version streamed = do
    result <- first (streamError limits) (streamValue streamed)
    let selected = do
            details <- Map.lookup (renderVersion version) (infoVersions (cargoIndexInfo result))
            raw <- selectCargoVersionDoc version (cargoIndexDocument result)
            pure VersionDoc{vdDetails = details, vdRaw = Just raw}
    pure VersionRead{vrVersion = selected, vrBodyBytes = streamBytes streamed, vrUpstreamLatest = Nothing}

-- | Keep the exact selected source row and its original physical coordinate.
selectCargoVersionDoc :: Version -> CachedDoc -> Maybe CachedDoc
selectCargoVersionDoc version document = do
    source <- snd cargoCached document
    let selected = filter (hasVersion . snd) (cargoRecords source)
    case selected of
        [row] -> Just (fst cargoCached (cargoDocument [row]))
        _ -> Nothing
  where
    hasVersion value = recordVersion value == Just (renderVersion version)

recordVersion :: Value -> Maybe Text
recordVersion = \case
    Object fields -> stringField "vers" fields
    _ -> Nothing
