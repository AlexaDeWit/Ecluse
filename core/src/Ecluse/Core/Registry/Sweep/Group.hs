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

import Data.Conduit (yield)
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
import Ecluse.Core.Registry.Sweep.Walk (BucketNames (..), collectBucket)
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
collectGroupBucket alphabet prefix mirror cache = do
    left <- collectBucket alphabet prefix (obListPackagesIn mirror prefix)
    right <- collectBucket alphabet prefix (obListPackagesIn cache prefix)
    case (left, right) of
        (BucketRead leftNames, BucketRead rightNames) -> do
            combined <- collectBucket alphabet prefix (yield leftNames >> yield rightNames $> Nothing)
            pure (locate leftNames rightNames combined)
        (BucketFaulted fault, _) -> pure (BucketFaulted (locatedFault mirror fault))
        (_, BucketFaulted fault) -> pure (BucketFaulted (locatedFault cache fault))
        (BucketUnsplittable, _) -> pure BucketUnsplittable
        (_, BucketUnsplittable) -> pure BucketUnsplittable
        (BucketOverflowed narrower, _) -> pure (BucketOverflowed narrower)
        (_, BucketOverflowed narrower) -> pure (BucketOverflowed narrower)
  where
    locate leftNames rightNames = \case
        BucketRead _ ->
            let locations =
                    Map.unionWith
                        (<>)
                        (Map.fromList [(name, [mirror]) | name <- leftNames])
                        (Map.fromList [(name, [cache]) | name <- rightNames])
             in BucketRead (Map.toAscList locations)
        BucketFaulted fault -> BucketFaulted fault
        BucketUnsplittable -> BucketUnsplittable
        BucketOverflowed narrower -> BucketOverflowed narrower

locatedFault :: StoreObservation -> StoreFault -> StoreFault
locatedFault store fault =
    fault
        { faultTransport = transportFault (tfCause transport) (factBackend (obFacts store) <> ": " <> tfDetail transport)
        }
  where
    transport = faultTransport fault

-- | Reject an oversized combined version inventory and deduplicate identities within each location.
boundedVersions :: Int -> [(StoreObservation, [StoredVersion])] -> Either StoreFault [(StoreObservation, [StoredVersion])]
boundedVersions limit locations
    | length identities > max 0 limit = Left (protocolFault "the combined inventory crossed limits.maxVersionCount")
    | otherwise = Right [(store, deduplicate versions) | (store, versions) <- locations]
  where
    identities = ordNub [renderVersion (storedVersion version) | (_, versions) <- locations, version <- versions]
    deduplicate = Map.elems . Map.fromList . map (\version -> (renderVersion (storedVersion version), version))
