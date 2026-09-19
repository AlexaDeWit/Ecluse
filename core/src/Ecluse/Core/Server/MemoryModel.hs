-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The one wire-to-resident memory model every byte budget shares.

A fetched metadata document costs far more resident than its wire size: the parsed
structure, the retained raw 'Data.Aeson.Value', and their spines expand a compact encoding
by a near-constant factor. Every consumer that budgets bytes against that expansion must use
this factor, or the budgets drift apart. It sits at the high end of the measured ratio, so an
estimate upper-bounds resident bytes and a leaner document is only over-evicted.
-}
module Ecluse.Core.Server.MemoryModel (
    expandWireBytes,
    contractResidentBytes,
    packumentOriginFanout,
    mirrorJobEstimatedBytes,
) where

{- | Scale a wire (compact-encoded) byte count to its estimated resident footprint: the 7.5x
high-end ratio, applied as a halved integer to stay in 'Int' arithmetic.
-}
expandWireBytes :: Int -> Int
expandWireBytes wireBytes = wireBytes * residentRatioNumerator `div` residentRatioDenominator

{- | Invert 'expandWireBytes': scale a resident-byte budget back to the wire (compact-encoded)
byte count it can hold, by the same ratio, so the two can never drift apart.
-}
contractResidentBytes :: Int -> Int
contractResidentBytes residentBytes = residentBytes * residentRatioDenominator `div` residentRatioNumerator

residentRatioNumerator :: Int
residentRatioNumerator = 15

residentRatioDenominator :: Int
residentRatioDenominator = 2

{- | How many origins one admitted materialisation holds at once. The encode and the cache
residency are covered elsewhere, by the material margin and the cache tenant.
-}
packumentOriginFanout :: Int
packumentOriginFanout = 2

{- | The estimated resident footprint of one queued mirror job (a name, a version,
an artifact URL): what the in-memory queue's depth cap charges per slot.
-}
mirrorJobEstimatedBytes :: Int
mirrorJobEstimatedBytes = 1024
