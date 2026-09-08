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
    classifyArtifacts,
) where

import Data.Foldable (maximumBy)
import Data.List.NonEmpty qualified as NE

import Ecluse.Core.Package (Artifact (artHashes))
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

{- | Select by asserted algorithm, then computability, retaining the last equal-ranked hash.
The worker also considers the selected hash's same-algorithm SRI alternatives.
-}
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

{- | Parse a 'MinTrustedIntegrity' from an algorithm name (e.g. @"sha256"@, @"md5"@), case-
and separator-insensitive. Unlike 'parseMinIntegrity' it accepts a sub-SHA-256 name.
-}
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

-- | Whether a version carries any digest that clears an admission floor.
data VersionIntegrity
    = -- | At least one digest asserts an algorithm at or above the floor: admissible.
      MeetsFloor
    | -- | Digests are present, but none clears the floor.
      BelowFloor
    | -- | No artifact carries a digest.
      NoIntegrity
    deriving stock (Eq, Show)

{- | Partition a version's artifacts against a floor, so a release loses the files that clear no
tamper-evident fingerprint rather than disappearing whole.
-}
partitionByFloor :: (IntegrityFloor floor) => floor -> NonEmpty Artifact -> Either VersionIntegrity (NonEmpty Artifact)
partitionByFloor flr arts = case nonEmpty (NE.filter (artifactMeetsFloor flr) arts) of
    Just survivors -> Right survivors
    Nothing -> Left (classifyArtifacts flr arts)

artifactMeetsFloor :: (IntegrityFloor floor) => floor -> Artifact -> Bool
artifactMeetsFloor flr art = any (maybe False (meetsFloor flr) . assertedAlg) (artHashes art)

-- | Distinguish a floor-clearing version from one carrying only weak digests or none.
classifyArtifacts :: (IntegrityFloor floor) => floor -> NonEmpty Artifact -> VersionIntegrity
classifyArtifacts flr arts
    | any (artifactMeetsFloor flr) arts = MeetsFloor
    | all (null . artHashes) arts = NoIntegrity
    | otherwise = BelowFloor
