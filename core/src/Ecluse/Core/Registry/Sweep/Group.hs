-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Bounded inventory joins for a mount's mirror and private-cache preview.
Each package retains the observations that actually listed it.
-}
module Ecluse.Core.Registry.Sweep.Group (
    groupAlphabet,
    collectGroupBucket,
    boundedVersions,
) where

import Control.Monad (foldM)
import Data.Conduit (fuseUpstream)
import Data.Conduit.List qualified as CL
import Data.Map.Strict qualified as Map

import Ecluse.Core.Fault (tfCause, tfDetail, transportFault)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Maintenance (
    NameAlphabet,
    NamePrefix,
    StoreFacts (factBackend, factNameAlphabet),
    StoreFault (faultTransport),
    StoreObservation (obFacts, obListPackagesIn),
    StoredVersion (storedVersion),
    noNameAlphabet,
    protocolFault,
 )
import Ecluse.Core.Registry.Sweep.Walk (BucketNames, collectBucketWith, insertInventory)
import Ecluse.Core.Version (renderVersion)

-- | Use a common partition only when both backends declare the same alphabet.
groupAlphabet :: StoreObservation -> StoreObservation -> NameAlphabet
groupAlphabet mirror cache
    | alphabet mirror == alphabet cache = alphabet mirror
    | otherwise = noNameAlphabet
  where
    alphabet = factNameAlphabet . obFacts

-- | Join actual package presence under the shared bucket budget, retaining each location.
collectGroupBucket :: NameAlphabet -> NamePrefix -> StoreObservation -> StoreObservation -> IO (BucketNames (PackageName, [StoreObservation]))
collectGroupBucket alphabet prefix mirror cache =
    fmap (second Map.elems) <$> collectBucketWith alphabet prefix Map.union source
  where
    source = do
        fault <- locatedPages False mirror
        maybe (locatedPages True cache) (pure . Just) fault
    locatedPages slot store =
        fmap (locatedFault store)
            <$> fuseUpstream
                (obListPackagesIn store prefix)
                (CL.map (map (,Map.singleton slot store)))

locatedFault :: StoreObservation -> StoreFault -> StoreFault
locatedFault store fault =
    fault
        { faultTransport = transportFault (tfCause transport) (factBackend (obFacts store) <> ": " <> tfDetail transport)
        }
  where
    transport = faultTransport fault

-- | Reject an oversized combined version inventory and deduplicate identities within each location.
boundedVersions :: Int -> [(StoreObservation, [StoredVersion])] -> Either StoreFault [(StoreObservation, [StoredVersion])]
boundedVersions limit locations = do
    combined <- maybeToRight overflow (foldM addLocation Map.empty indexed)
    pure [(store, Map.elems (Map.mapMaybe (Map.lookup index) combined)) | (index, (store, _)) <- indexed]
  where
    indexed = zip [0 :: Int ..] locations
    addLocation held (index, (_, versions)) = foldM (addVersion index) held versions
    addVersion index held version =
        insertInventory limit Map.union held (renderVersion (storedVersion version), Map.singleton index version)
    overflow = protocolFault "the combined inventory crossed limits.maxVersionCount"
