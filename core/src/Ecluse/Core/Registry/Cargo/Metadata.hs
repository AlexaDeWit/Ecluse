-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Prepared sparse metadata reads capture checked discovery and keep all finishing operations pure.
module Ecluse.Core.Registry.Cargo.Metadata (
    cargoReadWith,
    cargoChargeFactors,
    prepareCargoRead,
    prepareCargoReadAs,
    finishCargoFull,
    selectCargoVersionDoc,
) where

import Data.Aeson (Value (Object))

import Ecluse.Core.Package (PackageInfo)
import Ecluse.Core.Registry (UrlFormationError (UnparseableUrl))
import Ecluse.Core.Registry.CachedDocument (CachedDoc, cargoCached)
import Ecluse.Core.Registry.Cargo.Credential (cargoCredential)
import Ecluse.Core.Registry.Cargo.Discovery (CargoDiscovery, cargoDiscovery, discoveryIndex)
import Ecluse.Core.Registry.Cargo.Document (cargoDocument, cargoRecords)
import Ecluse.Core.Registry.Cargo.Reader (CargoIndexResult (..), CargoRead (..), readCargoIndexCharged)
import Ecluse.Core.Registry.Cargo.Request (cargoIndexRequest)
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Registry.Metadata (MetadataError)
import Ecluse.Core.Registry.Metadata.Fetch.Preparation (PrepareRead (..), bindToOrigin, presentedRead)
import Ecluse.Core.Registry.Metadata.Fetch.Types (EcosystemRead (..))
import Ecluse.Core.Registry.Metadata.Projection (finishSelectedVersion, streamError)
import Ecluse.Core.Registry.Origin (OriginClient (ocChargeFullReadWorkspace, ocChargeSelectedRead))
import Ecluse.Core.Registry.Request (CredentialMapping)
import Ecluse.Core.Registry.ServedDocument (stringField)
import Ecluse.Core.Security (Limits)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (..))
import Ecluse.Core.Version (Version, renderVersion)

-- | Per-byte charges from native corpus read, retained-entry and output measurements.
cargoChargeFactors :: ChargeFactors
cargoChargeFactors = ChargeFactors{cfFullReadPermille = 1700, cfKeptPermille = 1500, cfOutputPermille = 2600}

-- | Prepare after the selected origin receives its caller or service credential. Boot performs no discovery.
prepareCargoRead :: PrepareRead
prepareCargoRead = readAfterDiscovery cargoCredential

-- | 'prepareCargoRead' with discovery and index reads presenting the credential under @mapping@.
prepareCargoReadAs :: CredentialMapping -> PrepareRead
prepareCargoReadAs mapping = presentedRead mapping (readAfterDiscovery mapping)

readAfterDiscovery :: CredentialMapping -> PrepareRead
readAfterDiscovery mapping = PrepareRead (\origin -> fmap (cargoReadWith (ocChargeFullReadWorkspace origin) (ocChargeSelectedRead origin)) <$> cargoDiscovery mapping origin)

-- | Full and selected reads reserve row workspace through separate absolute callbacks.
cargoReadWith :: (Int -> IO ()) -> (Int -> IO ()) -> CargoDiscovery -> EcosystemRead
cargoReadWith fullPay selectedPay discovery =
    bindToOrigin
        (UnparseableUrl "Cargo discovery belongs to another index root")
        (discoveryIndex discovery)
        EcosystemRead
            { erRequest = cargoIndexRequest
            , erUniqueFields = []
            , erWalkFull = \limits name _ -> readCargoIndexCharged fullPay discovery limits name FullRead
            , erFinishFull = \limits _ _ -> finishCargoFull limits
            , erWalkSelected = \limits name version -> readCargoIndexCharged selectedPay discovery limits name (SelectedRead version)
            , erFinishSelected = \limits _ _ -> finishSelectedVersion cargoIndexInfo (\version result -> selectCargoVersionDoc version (cargoIndexDocument result)) limits
            }

-- | Finish a complete native index walk.
finishCargoFull :: Limits -> StreamResult CargoIndexResult -> Either MetadataError (PackageInfo, CachedDoc)
finishCargoFull limits streamed = do
    result <- first (streamError limits) (streamValue streamed)
    pure (cargoIndexInfo result, cargoIndexDocument result)

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
