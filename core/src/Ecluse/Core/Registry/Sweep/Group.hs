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

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Maintenance (
    StoreFacts (factNameAlphabet),
    StoreFault,
    StoreObservation (obFacts, obListPackagesIn),
    StoredVersion (storedVersion),
    protocolFault,
 )
import Ecluse.Core.Registry.Maintenance.NameSpace (
    NameAlphabet,
    NamePrefix,
    noNameAlphabet,
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
collectGroupBucket :: NameAlphabet -> NamePrefix -> StoreObservation -> StoreObservation -> IO (BucketNames (StoreObservation, StoreFault) (PackageName, [Bool]))
collectGroupBucket alphabet prefix mirror cache =
    fmap (second Map.elems) <$> collectBucketWith alphabet prefix Map.union source
  where
    source = do
        fault <- locatedPages False mirror
        maybe (locatedPages True cache) (pure . Just) fault
    -- The slot keys its own entry, so a name a location lists on several pages joins once.
    locatedPages slot store =
        fmap (store,)
            <$> fuseUpstream
                (obListPackagesIn store prefix)
                (CL.map (map (,Map.singleton slot slot)))

-- | Reject an oversized combined version inventory and deduplicate identities within each location.
boundedVersions :: Int -> [(a, [StoredVersion])] -> Either StoreFault [(a, [StoredVersion])]
boundedVersions limit locations = do
    combined <- maybeToRight overflow (foldM addLocation Map.empty indexed)
    pure [(store, Map.elems (Map.mapMaybe (Map.lookup index) combined)) | (index, (store, _)) <- indexed]
  where
    indexed = zip [0 :: Int ..] locations
    addLocation held (index, (_, versions)) = foldM (addVersion index) held versions
    addVersion index held version =
        insertInventory limit Map.union held (renderVersion (storedVersion version), Map.singleton index version)
    overflow = protocolFault "the combined inventory crossed limits.maxVersionCount"
