{- | Integrity-algorithm strength and the public-admission integrity floor.

Écluse trusts a digest only as far as its algorithm is collision-resistant, and it
trusts the two upstreams __asymmetrically__: a /trusted/ private upstream's versions
are vetted out of band, so a weak (legacy SHA-1) digest there is acceptable, but an
/untrusted/ public upstream's bytes are tamper-evident only through a strong digest.
This module is the one place that ranks algorithms by strength and decides what
clears the floor, so the worker's tamper gate and the serve layer's admission gate
share a single notion of "strong enough" rather than each re-encoding the ranking.

== The strength ranking

'integrityStrength' orders algorithms by collision resistance: the broken ones
(MD5, SHA-1) rank below the SHA-256 floor; SHA-256 and the modern long digests rank
at or above it. 'assertedAlg' resolves what a 'Hash' /claims/ — its tag directly, or
for a Subresource-Integrity string the algorithm named in its @\<alg\>-\<base64\>@
prefix — so an SRI is ranked and floored by the algorithm it embeds.

== The public-integrity floor

A 'MinIntegrity' is the configured minimum algorithm a __public__ version's digest
must meet to be admitted. It is opaque and __hard-floored at SHA-256__: it can be
/raised/ (to SHA-512 or Blake2b, as cryptanalysis ages an algorithm) but never set
below SHA-256, because admitting a public version on a SHA-1 digest would let a
collision substitute its bytes. The trusted private path never consults the floor —
trust substitutes for crypto strength there (see
@docs\/architecture\/security.md@ → "Asymmetric integrity trust").
-}
module Ecluse.Package.Integrity (
    -- * Algorithm strength
    Strength,
    integrityStrength,
    assertedAlg,

    -- * Algorithm names and SRI strings
    renderHashAlg,
    parseHashAlg,
    sriAlgorithm,
    sriPrefix,
    sriBody,

    -- * The public-integrity floor
    MinIntegrity,
    defaultMinIntegrity,
    mkMinIntegrity,
    parseMinIntegrity,
    unMinIntegrity,
    renderMinIntegrity,
    meetsFloor,

    -- * Version admissibility
    VersionIntegrity (..),
    classifyArtifacts,
) where

import Ecluse.Package (
    Artifact (artHashes),
    Hash,
    HashAlg (Blake2b, MD5, SHA1, SHA256, SHA512, SRI),
    hashAlg,
    hashValue,
    parseHashAlg,
    renderHashAlg,
    sriAlgorithm,
    sriBody,
    sriPrefix,
 )

-- ── algorithm strength ───────────────────────────────────────────────────────

{- | The collision-resistance tier of a hash algorithm, with constructors ordered
__weakest to strongest__ so the derived 'Ord' /is/ the strength ranking: two tiers
compare by collision resistance, and equal-strength algorithms share a tier (so they
compare 'EQ'). This is the one named ranking the worker's tamper gate and the serve
layer's admission floor both consult.
-}
data Strength
    = {- | A bare 'SRI' wrapper asserts no algorithm at all — below every real
      digest, so an unresolved SRI never wins a strongest-digest comparison.
      -}
      Unasserted
    | -- | MD5: practical collisions; the weakest real algorithm.
      Weakest
    | -- | SHA-1: practical collisions.
      Weak
    | -- A future weak-but-not-broken algorithm would slot a new tier here, between
      -- the broken algorithms and the SHA-256 floor — an enum needs no renumbering,
      -- unlike the old Int ranking, which reserved a numeric gap for exactly this.

      -- | SHA-256: collision-resistant; the public-integrity floor.
      Floor
    | -- | The modern long digests SHA-512 and Blake2b — equal strength, the top tier.
      Strongest
    deriving stock (Eq, Ord, Show)

{- | The collision-resistance 'Strength' tier of an algorithm; __a stronger algorithm
ranks higher__ under 'Strength''s 'Ord'.

The broken algorithms rank below the SHA-256 floor (@'integrityStrength' 'SHA256'@):
MD5 and SHA-1 have practical collisions, so a match on one cannot prove the bytes
were not substituted. SHA-256 and the modern long digests rank at or above the floor,
with SHA-512 and Blake2b sharing the top tier (equal strength). A bare 'SRI' ranks
lowest of all — it is a wrapper, not an algorithm, so resolve it with 'assertedAlg'
before ranking; ranking below every real algorithm, an unresolved SRI never wins a
strongest-digest comparison.

>>> integrityStrength SHA512 > integrityStrength SHA256
True

>>> integrityStrength SHA1 >= integrityStrength SHA256
False
-}
integrityStrength :: HashAlg -> Strength
integrityStrength = \case
    SRI -> Unasserted
    MD5 -> Weakest
    SHA1 -> Weak
    SHA256 -> Floor
    SHA512 -> Strongest
    Blake2b -> Strongest

{- | The algorithm a 'Hash' asserts: its tag directly, or — for an 'SRI' string — the
algorithm named in its @\<alg\>-\<base64\>@ prefix. The SRI prefixes resolved are
@sha256@ and @sha512@ (the long digests the model represents and a registry serves);
an unrecognised or malformed prefix yields 'Nothing', so it asserts no algorithm and
clears no floor (the fail-closed reading).

>>> import Ecluse.Package (mkHash, HashAlg (SHA1, SRI))
>>> assertedAlg <$> mkHash SRI "sha512-z4PhNX7vuL3xVChQ1m2AB9Yg5AULVxXcg/SpIdNs6c5H0NE8XYXysP+DGNKHfuwvY7kxvUdBeoGlODJ6+SfaPg=="
Right (Just SHA512)

>>> assertedAlg <$> mkHash SHA1 "da39a3ee5e6b4b0d3255bfef95601890afd80709"
Right (Just SHA1)

>>> assertedAlg <$> mkHash SRI "sha384-OLBgp1GsljhM2TJ+sbHjaiH9txEUvgdDTAzHv2P24donTt6/529l+9Ua0vFImLlb"
Right Nothing
-}
assertedAlg :: Hash -> Maybe HashAlg
assertedAlg h = case hashAlg h of
    SRI -> sriAlgorithm (hashValue h)
    alg -> Just alg

-- ── the public-integrity floor ───────────────────────────────────────────────

{- | The configured minimum integrity algorithm a __public__ (untrusted) version's
digest must meet to be admitted. Opaque and __hard-floored at SHA-256__: build it
only through 'mkMinIntegrity' \/ 'parseMinIntegrity', which reject anything weaker, so
a value of this type carries the proof that the floor is itself collision-resistant.
-}
newtype MinIntegrity = MinIntegrity HashAlg
    deriving stock (Eq, Show)

{- | The default public-integrity floor: SHA-256, which is also the __hard minimum__
the floor may never be set below.
-}
defaultMinIntegrity :: MinIntegrity
defaultMinIntegrity = MinIntegrity SHA256

{- | Build a 'MinIntegrity', rejecting any algorithm weaker than SHA-256 (the hard
floor). A weak floor is a configuration error, never a silent clamp: a public version
admitted on a SHA-1 digest could be substituted by a collision, defeating the gate.
-}
mkMinIntegrity :: HashAlg -> Either Text MinIntegrity
mkMinIntegrity alg
    | integrityStrength alg >= integrityStrength SHA256 = Right (MinIntegrity alg)
    | otherwise =
        Left
            ( "the minimum public integrity algorithm must be SHA-256 or stronger, not "
                <> renderHashAlg alg
            )

{- | Parse a 'MinIntegrity' from an algorithm name (e.g. @"sha256"@, @"sha512"@,
@"blake2b"@), case- and separator-insensitive. An unrecognised name and an
algorithm below the SHA-256 floor are distinct errors, so a misconfiguration is
reported precisely.
-}
parseMinIntegrity :: Text -> Either Text MinIntegrity
parseMinIntegrity raw = parseHashAlg raw >>= mkMinIntegrity

-- | The floor algorithm.
unMinIntegrity :: MinIntegrity -> HashAlg
unMinIntegrity (MinIntegrity alg) = alg

-- | Render a 'MinIntegrity' as its lower-case algorithm name (round-trips 'parseMinIntegrity').
renderMinIntegrity :: MinIntegrity -> Text
renderMinIntegrity = renderHashAlg . unMinIntegrity

{- | Whether an algorithm meets the floor: at least as strong as the configured
minimum. The candidate algorithm is a /resolved/ one (from 'assertedAlg'), never a
bare 'SRI'.
-}
meetsFloor :: MinIntegrity -> HashAlg -> Bool
meetsFloor (MinIntegrity floorAlg) alg = integrityStrength alg >= integrityStrength floorAlg

-- ── version admissibility ────────────────────────────────────────────────────

{- | How a version's artifacts stand against the public-integrity floor — the
three-way verdict the public admission gate acts on.
-}
data VersionIntegrity
    = -- | At least one digest asserts an algorithm at or above the floor: admissible.
      MeetsFloor
    | {- | The version carries an integrity digest, but none meets the floor (e.g. a
      legacy SHA-1 shasum only). Inadmissible from a public upstream — distinct from
      carrying no digest at all, so the refusal can say which.
      -}
      BelowFloor
    | -- | The version carries no integrity digest of any kind: inadmissible.
      NoIntegrity
    deriving stock (Eq, Show)

{- | Classify a version's artifacts against the floor. A version 'MeetsFloor' iff any
of its digests (across all of its artifacts) asserts a floor-clearing algorithm;
failing that, it is 'NoIntegrity' when no artifact carries any digest at all, else
'BelowFloor'. npm publishes one artifact per version, but the check spans the whole
'NonEmpty' so it holds for a multi-artifact ecosystem too.
-}
classifyArtifacts :: MinIntegrity -> NonEmpty Artifact -> VersionIntegrity
classifyArtifacts minIntegrity arts
    | any meetsFloorArtifact arts = MeetsFloor
    | all (null . artHashes) arts = NoIntegrity
    | otherwise = BelowFloor
  where
    meetsFloorArtifact art = any hashMeetsFloor (artHashes art)
    hashMeetsFloor h = maybe False (meetsFloor minIntegrity) (assertedAlg h)

-- The algorithm vocabulary (the wire name renderer\/parser and the SRI splitter\/resolver)
-- lives in "Ecluse.Package", the lowest layer, and is re-exported above so this module's
-- callers (and the worker and SQS) keep importing it from here.
