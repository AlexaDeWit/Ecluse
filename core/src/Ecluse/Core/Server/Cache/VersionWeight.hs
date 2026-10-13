-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Conservative accounting for selectively decoded releases.
The cache charges backing allocations and repeated structures without deduplicating sharing.
A retained raw version object is charged on the shared wire-to-resident model, without encoding it.
-}
module Ecluse.Core.Server.Cache.VersionWeight (weighVersion, weighEntryKey) where

import Ecluse.Core.Registry.Metadata (VersionDoc (vdDetails, vdRaw), VersionRead (vrUpstreamLatest, vrVersion))
import Ecluse.Core.Server.Cache.VersionWeight.Fields
import Ecluse.Core.Version (renderVersion)

-- | Estimate retained release bytes. 'maxBound' marks an uncacheable saturated estimate.
weighVersion :: VersionRead -> Int
weighVersion versionRead =
    fromInteger (min (toInteger (maxBound :: Int)) (versionPart + latestPart))
  where
    versionPart = maybe 1024 pairWeight (vrVersion versionRead)
    latestPart = maybe 0 (textWeight . renderVersion) (vrUpstreamLatest versionRead)

pairWeight :: VersionDoc -> Integer
pairWeight doc = 16 * 1024 + detailsFieldsWeight (vdDetails doc) + maybe 0 rawWeight (vdRaw doc)
