-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Digest authority shared by admission and worker verification.
The public floor cannot fall below SHA-256. The trusted floor can, because an
operator may trust a private source that carries legacy digests.
See @docs/architecture/security.md@ for the trust assumptions.
-}
module Ecluse.Core.Package.Integrity (
    -- * Algorithm strength
    assertedAlg,

    -- * The authoritative digest of a set
    authoritativeDigest,

    -- * Integrity floors
    IntegrityFloor (..),
    meetsFloor,
    partitionByFloor,

    -- ** The public-integrity floor (hard-floored at SHA-256)
    MinIntegrity,
    mkMinIntegrity,
    parseMinIntegrity,
    unMinIntegrity,

    -- ** The trusted-integrity floor (loosenable below SHA-256)
    MinTrustedIntegrity,
    mkMinTrustedIntegrity,
    parseMinTrustedIntegrity,
    unMinTrustedIntegrity,

    -- * Version admissibility
    VersionIntegrity (..),
    classifyDigests,
) where

import Data.Foldable (maximumBy)
import Data.List.NonEmpty qualified as NE

import Ecluse.Core.Package.Hash (
    Hash,
    HashAlg (SHA256, SRI),
    hashAlg,
    hashValue,
    isComputable,
    parseHashAlg,
    renderHashAlg,
    sriAlgorithm,
 )

-- | Resolve an SRI prefix or a raw algorithm tag. An unknown prefix clears no floor.
assertedAlg :: Hash -> Maybe HashAlg
assertedAlg h = case hashAlg h of
    SRI -> sriAlgorithm (hashValue h)
    alg -> Just alg

-- | Select by asserted algorithm, then computability, retaining the last equal-ranked hash.
authoritativeDigest :: NonEmpty Hash -> Hash
authoritativeDigest = maximumBy (comparing digestAuthority)
  where
    digestAuthority :: Hash -> (HashAlg, Bool)
    digestAuthority h = case assertedAlg h of
        Nothing -> (SHA256, False)
        Just alg -> (alg, isComputable alg)

-- | Read a floor's minimum algorithm. Each smart constructor owns its floor's restrictions.
class IntegrityFloor floor where
    -- | The minimum algorithm this floor requires.
    floorAlgorithm :: floor -> HashAlg

-- | A public admission floor that cannot fall below SHA-256.
newtype MinIntegrity = MinIntegrity HashAlg
    deriving stock (Eq, Show)

-- | Reject algorithms below SHA-256, whose collisions permit substitution of public bytes.
mkMinIntegrity :: HashAlg -> Either Text MinIntegrity
mkMinIntegrity alg
    | alg >= SHA256 = Right (MinIntegrity alg)
    | otherwise =
        Left
            ( "the minimum public integrity algorithm must be SHA-256 or stronger, not "
                <> renderHashAlg alg
            )

-- | Parse an algorithm name, distinguishing unknown names from a floor below SHA-256.
parseMinIntegrity :: Text -> Either Text MinIntegrity
parseMinIntegrity raw = parseHashAlg raw >>= mkMinIntegrity

-- | The floor algorithm.
unMinIntegrity :: MinIntegrity -> HashAlg
unMinIntegrity (MinIntegrity alg) = alg

instance IntegrityFloor MinIntegrity where
    floorAlgorithm = unMinIntegrity

-- | A trusted admission floor that may fall below SHA-256 for an operator's private source.
newtype MinTrustedIntegrity = MinTrustedIntegrity HashAlg
    deriving stock (Eq, Show)

{- | Build a 'MinTrustedIntegrity'. It accepts any known algorithm, including the broken
SHA-1 and MD5, and rejects the bare 'SRI' wrapper, which names no algorithm of its own.
-}
mkMinTrustedIntegrity :: HashAlg -> Either Text MinTrustedIntegrity
mkMinTrustedIntegrity SRI =
    Left "the minimum trusted integrity algorithm must name a concrete algorithm, not a bare SRI"
mkMinTrustedIntegrity alg = Right (MinTrustedIntegrity alg)

-- | Parse an algorithm name. Unlike 'parseMinIntegrity' it accepts a sub-SHA-256 one.
parseMinTrustedIntegrity :: Text -> Either Text MinTrustedIntegrity
parseMinTrustedIntegrity raw = parseHashAlg raw >>= mkMinTrustedIntegrity

-- | The trusted floor algorithm.
unMinTrustedIntegrity :: MinTrustedIntegrity -> HashAlg
unMinTrustedIntegrity (MinTrustedIntegrity alg) = alg

instance IntegrityFloor MinTrustedIntegrity where
    floorAlgorithm = unMinTrustedIntegrity

{- | Whether an algorithm meets a floor: at least as strong as the floor's minimum, by
'HashAlg' 'Ord'. Pass a resolved algorithm from 'assertedAlg', never a bare 'SRI'.
-}
meetsFloor :: (IntegrityFloor floor) => floor -> HashAlg -> Bool
meetsFloor flr alg = alg >= floorAlgorithm flr

-- | Whether a set of digests holds any that clears an admission floor.
data VersionIntegrity
    = -- | At least one digest asserts an algorithm at or above the floor: admissible.
      MeetsFloor
    | -- | Digests are present, but none clears the floor.
      BelowFloor
    | -- | The set holds no digest.
      NoIntegrity
    deriving stock (Eq, Show)

-- | Keep the files whose own digests clear a floor, so a release loses only the files it cannot verify.
partitionByFloor :: (IntegrityFloor floor) => floor -> (file -> [Hash]) -> NonEmpty file -> Either VersionIntegrity (NonEmpty file)
-- Inlined, so a caller's projection is a field read in its own loop.
{-# INLINE partitionByFloor #-}
partitionByFloor flr digestsOf files = case nonEmpty (NE.filter (digestsMeetFloor flr . digestsOf) files) of
    Just survivors -> Right survivors
    Nothing -> Left (integrityOf False (all (null . digestsOf) files))

digestsMeetFloor :: (IntegrityFloor floor) => floor -> [Hash] -> Bool
digestsMeetFloor flr = any (maybe False (meetsFloor flr) . assertedAlg)

-- | Read a set of digests against a floor: one file's, or every file's of a version together.
classifyDigests :: (IntegrityFloor floor) => floor -> [Hash] -> VersionIntegrity
classifyDigests flr digests = integrityOf (digestsMeetFloor flr digests) (null digests)

-- Whether a digest clears the floor, then whether there is no digest at all.
integrityOf :: Bool -> Bool -> VersionIntegrity
integrityOf clears none
    | clears = MeetsFloor
    | none = NoIntegrity
    | otherwise = BelowFloor
