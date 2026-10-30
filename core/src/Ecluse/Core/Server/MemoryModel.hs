-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Retained-document accounting and fixed resource estimates.
Compact representation charges differ from original source bytes. The retained estimate
does not bound active input, parser, policy, merge, or output memory.
-}
module Ecluse.Core.Server.MemoryModel (
    expandWireBytes,
    expandWireBytesInteger,
    chargeForResident,
    mirrorJobEstimatedBytes,
    bufferedChunkCharge,
    decodedDocumentCharge,
    saturatingCharge,
) where

import Data.ByteString qualified as BS

{- | Estimate retained bytes from a compact representation charge using the 7.5 factor.
This factor is independent of source-byte regression envelopes and active-work admission.
-}
expandWireBytes :: Int -> Int
expandWireBytes = expandWireCharge

-- | Expand retained-state terms before their final saturating conversion to Int.
expandWireBytesInteger :: Integer -> Integer
expandWireBytesInteger = expandWireCharge

expandWireCharge :: (Integral a) => a -> a
expandWireCharge wireBytes = wireBytes * fromIntegral residentRatioNumerator `div` fromIntegral residentRatioDenominator

-- | The smallest compact charge that 'expandWireBytes' expands to at least the given heap bytes.
chargeForResident :: Int -> Int
chargeForResident resident = (resident * residentRatioDenominator + residentRatioNumerator - 1) `div` residentRatioNumerator

residentRatioNumerator :: Int
residentRatioNumerator = 15

residentRatioDenominator :: Int
residentRatioDenominator = 2

-- | The resident-byte allowance per in-memory mirror queue slot.
mirrorJobEstimatedBytes :: Int
mirrorJobEstimatedBytes = 1024

-- | Allow four overlapping byte copies and builder/list cells before retaining a nonempty chunk.
bufferedChunkCharge :: Int -> Int
bufferedChunkCharge = saturatingCharge . bufferedChargeInteger . toInteger

bufferedChargeInteger :: Integer -> Integer
bufferedChargeInteger bytes
    | bytes <= 0 = 0
    | otherwise = 4 * bytes + 128

-- | Estimate scalar copies and tree/domain slots from actual JSON or XML bytes before decoding.
decodedDocumentCharge :: ByteString -> Int
decodedDocumentCharge bytes
    | BS.null bytes = 0
    | otherwise = saturatingCharge (4096 + 8 * toInteger (BS.length bytes) + 512 * markers)
  where
    markers = BS.foldl' (\count byte -> if byte `elem` [34, 44, 58, 60, 62, 91, 93, 123, 125] then count + 1 else count) 0 bytes

-- | Clamp an Integer charge into the Int admission interface: below zero to zero, past 'maxBound' to 'maxBound'.
saturatingCharge :: Integer -> Int
saturatingCharge = fromInteger . max 0 . min (toInteger (maxBound :: Int))
