-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Integrity-algorithm strength and the admission integrity floors.

Écluse trusts a digest only as far as its algorithm is collision-resistant. Both
contexts, the /untrusted/ public upstream and the /trusted/ private upstream, default
to requiring a SHA-256-or-stronger digest, but their floors are __asymmetric__. The
public floor is a __hard__ SHA-256 boundary: raisable, never lowerable. The trusted
floor is __operator-loosenable__ below SHA-256 for a legacy private mirror, where trust
in the operator's own vetted source substitutes for cryptographic strength. This module
applies the 'HashAlg' ordering that ranks algorithms by checksum authority, and it decides
what clears a floor. The worker's tamper gate and the serve layer's two admission gates
therefore share one notion of "strong enough" rather than each re-encoding the ranking.

== The strength ranking

'HashAlg' 'Ord' is the operational total ordering. MD5 and SHA-1 rank below the SHA-256
floor. SHA-256 and the modern long digests rank at or above it. SHA-512 ranks above
Blake2b as the npm/SRI-native top digest. 'assertedAlg' resolves what a 'Hash' /claims/:
its tag directly, or for a Subresource-Integrity string the algorithm named in its
@\<alg\>-\<base64\>@ prefix. An SRI therefore ranks and floors by the algorithm it
embeds. The 'IntegrityFloor' class abstracts "the minimum algorithm a floor requires",
so 'meetsFloor' and 'classifyArtifacts' rank candidates against either floor through
this one ordering.

== The public-integrity floor

A 'MinIntegrity' is the configured minimum algorithm a __public__ (untrusted) version's
digest must meet to be admitted. It is opaque and __hard-floored at SHA-256__. An
operator can /raise/ it to SHA-512 or Blake2b, as cryptanalysis ages an algorithm.
Nothing can lower it below SHA-256, because a collision can substitute the bytes of a
public version admitted on a SHA-1 digest. There is no escape hatch. 'mkMinIntegrity'
\/ 'parseMinIntegrity' reject a sub-SHA-256 value at construction, so no config or
constructor path can lower this floor.

== The trusted-integrity floor

A 'MinTrustedIntegrity' is the configured minimum algorithm a __trusted__ (private)
version's digest must meet to be served. It also defaults to SHA-256, but it is __not
hard-floored__. An operator may loosen it to SHA-1 or MD5 for a legacy private mirror
(see @docs\/architecture\/security.md@ → "Asymmetric integrity trust"). It still rejects
an unknown algorithm name. This loosening is the /only/ way Écluse serves a sub-SHA-256
digest, and only on the operator's own trusted source, never on untrusted public bytes.
-}
module Ecluse.Core.Package.Integrity (
    -- * Algorithm strength
    assertedAlg,

    -- * Algorithm names and SRI strings
    renderHashAlg,
    parseHashAlg,
    sriAlgorithm,
    sriPrefix,
    sriBody,

    -- * The authoritative digest of a set
    authoritativeDigest,

    -- * Integrity floors
    IntegrityFloor (..),
    meetsFloor,

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
    sriBody,
    sriPrefix,
 )

{- | The algorithm a 'Hash' asserts: its tag directly, or, for an 'SRI' string, the
algorithm named in its @\<alg\>-\<base64\>@ prefix. The SRI prefixes resolved are
@sha256@, @sha384@ and @sha512@ (every long digest the model represents and a registry
serves). An unrecognised or malformed prefix yields 'Nothing', so it asserts no
algorithm and clears no floor: the fail-closed reading.

>>> import Ecluse.Core.Package (mkHash, HashAlg (SHA1, SRI))
>>> assertedAlg <$> mkHash SRI "sha512-z4PhNX7vuL3xVChQ1m2AB9Yg5AULVxXcg/SpIdNs6c5H0NE8XYXysP+DGNKHfuwvY7kxvUdBeoGlODJ6+SfaPg=="
Right (Just SHA512)

>>> assertedAlg <$> mkHash SHA1 "da39a3ee5e6b4b0d3255bfef95601890afd80709"
Right (Just SHA1)

>>> assertedAlg <$> mkHash SRI "sha384-OLBgp1GsljhM2TJ+sbHjaiH9txEUvgdDTAzHv2P24donTt6/529l+9Ua0vFImLlb"
Right (Just SHA384)
-}
assertedAlg :: Hash -> Maybe HashAlg
assertedAlg h = case hashAlg h of
    SRI -> sriAlgorithm (hashValue h)
    alg -> Just alg

{- | The __most authoritative__ digest of a set, ranked by the algorithm each digest
asserts ('assertedAlg', under which an SRI ranks as its embedded algorithm). The
ranking uses the same 'HashAlg' 'Ord' the admission floors rank candidates by. The
worker's tamper gate and the serve-side floor therefore agree on one authority order,
rather than each re-encoding it. This is the digest the worker verifies fetched bytes
against, never a weaker one while a stronger is present. A match on a weaker algorithm
cannot rescue a failed strong one.

A digest asserting no algorithm sits at the SHA-256 floor tier. That is an unresolvable
SRI, unconstructable today because 'Ecluse.Core.Package.mkHash' resolves every
component, but ranked defensively. Inside an equal algorithm authority, a digest Écluse
can recompute ('Ecluse.Core.Package.isComputable') wins over one it cannot. A tie
therefore never over-rejects an artifact that a co-present verifiable digest could
prove. The selection never drops __below__ the strongest tier to a weaker computable
algorithm. The final tie-break is 'maximumBy''s keep-latest, deterministic over the
artifact's wire order.
-}
authoritativeDigest :: NonEmpty Hash -> Hash
authoritativeDigest = maximumBy (comparing digestAuthority)
  where
    -- The two-level authority key: the asserted algorithm first (by the operational
    -- 'HashAlg' ordering), then recomputability inside an equal algorithm.
    digestAuthority :: Hash -> (HashAlg, Bool)
    digestAuthority h = case assertedAlg h of
        Nothing -> (SHA256, False)
        Just alg -> (alg, isComputable alg)

{- | The shared interface of an integrity floor: the minimum algorithm it requires. Both
the hard-floored public 'MinIntegrity' and the loosenable trusted 'MinTrustedIntegrity'
are floors. 'meetsFloor' and 'classifyArtifacts' therefore rank candidates against either
through this one class, backed by the same 'HashAlg' ordering the worker's tamper gate
also consults. The class only /reads/ a floor's algorithm. A newtype's construction
invariant, the public hard-floor and the trusted loosenability, lives in its smart
constructors, never here.
-}
class IntegrityFloor floor where
    -- | The minimum algorithm this floor requires.
    floorAlgorithm :: floor -> HashAlg

{- | The configured minimum integrity algorithm a __public__ (untrusted) version's
digest must meet to be admitted. The type is opaque and __hard-floored at SHA-256__.
Build it only through 'mkMinIntegrity' \/ 'parseMinIntegrity', which reject anything
weaker. A value of this type therefore carries the proof that the floor is itself
collision-resistant. There is deliberately no loosenable variant of /this/ floor:
Écluse never admits untrusted public bytes on a sub-SHA-256 digest.
-}
newtype MinIntegrity = MinIntegrity HashAlg
    deriving stock (Eq, Show)

{- | Build a 'MinIntegrity', rejecting any algorithm weaker than SHA-256 (the hard
floor). A weak floor is a configuration error, never a silent clamp. A collision could
substitute the bytes of a public version admitted on a SHA-1 digest, and that defeats the
gate.
-}
mkMinIntegrity :: HashAlg -> Either Text MinIntegrity
mkMinIntegrity alg
    | alg >= SHA256 = Right (MinIntegrity alg)
    | otherwise =
        Left
            ( "the minimum public integrity algorithm must be SHA-256 or stronger, not "
                <> renderHashAlg alg
            )

{- | Parse a 'MinIntegrity' from an algorithm name (e.g. @"sha256"@, @"sha512"@,
@"blake2b"@), case- and separator-insensitive. An unrecognised name and an
algorithm below the SHA-256 floor are distinct errors, so the report names the
misconfiguration precisely.
-}
parseMinIntegrity :: Text -> Either Text MinIntegrity
parseMinIntegrity raw = parseHashAlg raw >>= mkMinIntegrity

-- | The floor algorithm.
unMinIntegrity :: MinIntegrity -> HashAlg
unMinIntegrity (MinIntegrity alg) = alg

instance IntegrityFloor MinIntegrity where
    floorAlgorithm = unMinIntegrity

{- | The configured minimum integrity algorithm a __trusted__ (private) version's digest
must meet to be served. Like 'MinIntegrity' it defaults to SHA-256, but it carries __no
hard floor__. An operator may loosen it to SHA-1 or MD5 for a legacy private mirror.
There, trust in their own vetted source substitutes for cryptographic strength. Build it
only through 'mkMinTrustedIntegrity' \/ 'parseMinTrustedIntegrity', which still reject an
unknown algorithm name (and the bare 'SRI' wrapper, which names no algorithm). Loosening
this floor is the /only/ path by which Écluse serves a sub-SHA-256 digest, and only on the
operator's own trusted source.
-}
newtype MinTrustedIntegrity = MinTrustedIntegrity HashAlg
    deriving stock (Eq, Show)

{- | Build a 'MinTrustedIntegrity'. It accepts any /known/ algorithm, including the
broken SHA-1 and MD5, which an operator may deliberately loosen the trusted floor to. It
rejects the bare 'SRI' wrapper, which asserts no algorithm of its own and so could never
be a meaningful floor. There is intentionally no SHA-256 hard minimum here: that is the
one behavioural difference from 'mkMinIntegrity'.
-}
mkMinTrustedIntegrity :: HashAlg -> Either Text MinTrustedIntegrity
mkMinTrustedIntegrity SRI =
    Left "the minimum trusted integrity algorithm must name a concrete algorithm, not a bare SRI"
mkMinTrustedIntegrity alg = Right (MinTrustedIntegrity alg)

{- | Parse a 'MinTrustedIntegrity' from an algorithm name (e.g. @"sha256"@, @"sha1"@,
@"md5"@), case- and separator-insensitive. It rejects an unrecognised name. Unlike
'parseMinIntegrity', it /accepts/ a sub-SHA-256 name, because the trusted floor is
loosenable.
-}
parseMinTrustedIntegrity :: Text -> Either Text MinTrustedIntegrity
parseMinTrustedIntegrity raw = parseHashAlg raw >>= mkMinTrustedIntegrity

-- | The trusted floor algorithm.
unMinTrustedIntegrity :: MinTrustedIntegrity -> HashAlg
unMinTrustedIntegrity (MinTrustedIntegrity alg) = alg

instance IntegrityFloor MinTrustedIntegrity where
    floorAlgorithm = unMinTrustedIntegrity

{- | Whether an algorithm meets a floor: at least as strong as the floor's configured
minimum, by 'HashAlg' 'Ord'. The candidate algorithm is a
/resolved/ one (from 'assertedAlg'), never a bare 'SRI'.
-}
meetsFloor :: (IntegrityFloor floor) => floor -> HashAlg -> Bool
meetsFloor flr alg = alg >= floorAlgorithm flr

{- | How a version's artifacts stand against an integrity floor: the three-way verdict
an admission gate (public or trusted) acts on.
-}
data VersionIntegrity
    = -- | At least one digest asserts an algorithm at or above the floor: admissible.
      MeetsFloor
    | {- | The version carries an integrity digest, but none meets the floor (e.g. a
      legacy SHA-1 shasum only under a SHA-256 floor). Inadmissible, and distinct from
      carrying no digest at all, so the refusal can say which.
      -}
      BelowFloor
    | {- | The version carries no integrity digest of any kind: inadmissible (no floor
      can be met without a digest).
      -}
      NoIntegrity
    deriving stock (Eq, Show)

{- | Classify a version's artifacts against a floor (public or trusted). A version
'MeetsFloor' iff any of its digests, across all of its artifacts, asserts a
floor-clearing algorithm. Failing that, it is 'NoIntegrity' when no artifact carries any
digest at all, and 'BelowFloor' otherwise. An npm version has one artifact, but the check
spans the whole 'NonEmpty', so it holds for a multi-artifact ecosystem too.
-}
classifyArtifacts :: (IntegrityFloor floor) => floor -> NonEmpty Artifact -> VersionIntegrity
classifyArtifacts flr arts
    | any meetsFloorArtifact arts = MeetsFloor
    | all (null . artHashes) arts = NoIntegrity
    | otherwise = BelowFloor
  where
    meetsFloorArtifact art = any hashMeetsFloor (artHashes art)
    hashMeetsFloor h = maybe False (meetsFloor flr) (assertedAlg h)

-- The algorithm vocabulary (the wire name renderer\/parser and the SRI splitter\/resolver)
-- lives in "Ecluse.Core.Package", the lowest layer. The export list above re-exports it, so
-- this module's callers, the worker, and SQS all import it from here.
