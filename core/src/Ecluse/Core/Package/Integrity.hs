{- | Integrity-algorithm strength and the admission integrity floors.

Écluse trusts a digest only as far as its algorithm is collision-resistant. Both
contexts — the /untrusted/ public upstream and the /trusted/ private upstream — default
to requiring a SHA-256-or-stronger digest, but they are floored __asymmetrically__: the
public floor is a __hard__ SHA-256 boundary (raisable, never lowerable), while the
trusted floor is __operator-loosenable__ below SHA-256 for a legacy private mirror, where
trust in the operator's own vetted source substitutes for cryptographic strength. This
module is the one place that ranks algorithms by strength and decides what clears a
floor, so the worker's tamper gate and the serve layer's two admission gates share a
single notion of "strong enough" rather than each re-encoding the ranking.

== The strength ranking

'integrityStrength' orders algorithms by collision resistance: the broken ones
(MD5, SHA-1) rank below the SHA-256 floor; SHA-256 and the modern long digests rank
at or above it. 'assertedAlg' resolves what a 'Hash' /claims/ — its tag directly, or
for a Subresource-Integrity string the algorithm named in its @\<alg\>-\<base64\>@
prefix — so an SRI is ranked and floored by the algorithm it embeds. The 'IntegrityFloor'
class abstracts "the minimum algorithm a floor requires", so 'meetsFloor' and
'classifyArtifacts' rank candidates against either floor through this one ranking.

== The public-integrity floor

A 'MinIntegrity' is the configured minimum algorithm a __public__ (untrusted) version's
digest must meet to be admitted. It is opaque and __hard-floored at SHA-256__: it can be
/raised/ (to SHA-512 or Blake2b, as cryptanalysis ages an algorithm) but never set below
SHA-256, because admitting a public version on a SHA-1 digest would let a collision
substitute its bytes. There is no escape-hatch: 'mkMinIntegrity' \/ 'parseMinIntegrity'
reject a sub-SHA-256 value at construction, so no config or constructor path can lower
this floor.

== The trusted-integrity floor

A 'MinTrustedIntegrity' is the configured minimum algorithm a __trusted__ (private)
version's digest must meet to be served. It also defaults to SHA-256, but is __not
hard-floored__: an operator may loosen it to SHA-1 or MD5 for a legacy private mirror
(see @docs\/architecture\/security.md@ → "Asymmetric integrity trust"). It still rejects
an unknown algorithm name. This loosening is the /only/ way Écluse will serve a sub-SHA-256
digest, and only on the operator's own trusted source — never on untrusted public bytes.
-}
module Ecluse.Core.Package.Integrity (
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

    -- * Integrity floors
    IntegrityFloor (..),
    meetsFloor,

    -- ** The public-integrity floor (hard-floored at SHA-256)
    MinIntegrity,
    defaultMinIntegrity,
    mkMinIntegrity,
    parseMinIntegrity,
    unMinIntegrity,
    renderMinIntegrity,

    -- ** The trusted-integrity floor (loosenable below SHA-256)
    MinTrustedIntegrity,
    defaultMinTrustedIntegrity,
    mkMinTrustedIntegrity,
    parseMinTrustedIntegrity,
    unMinTrustedIntegrity,
    renderMinTrustedIntegrity,

    -- * Version admissibility
    VersionIntegrity (..),
    classifyArtifacts,
) where

import Ecluse.Core.Package (
    Artifact (artHashes),
    Hash,
    HashAlg (Blake2b, MD5, SHA1, SHA256, SHA384, SHA512, SRI),
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
    | {- | SHA-384: above the floor but below the top tier. It is SHA-512 truncated, so
      its 192-bit collision resistance sits below SHA-512's 256-bit and above SHA-256's
      128-bit — a tier of its own.
      -}
      Strong
    | -- | The modern long digests SHA-512 and Blake2b — equal strength, the top tier.
      Strongest
    deriving stock (Eq, Ord, Show)

{- | The collision-resistance 'Strength' tier of an algorithm; __a stronger algorithm
ranks higher__ under 'Strength''s 'Ord'.

The broken algorithms rank below the SHA-256 floor (@'integrityStrength' 'SHA256'@):
MD5 and SHA-1 have practical collisions, so a match on one cannot prove the bytes
were not substituted. SHA-256 and the longer digests rank at or above the floor:
SHA-384 above it in a tier of its own, then SHA-512 and Blake2b sharing the top tier
(equal strength). A bare 'SRI' ranks lowest of all — it is a wrapper, not an algorithm,
so resolve it with 'assertedAlg' before ranking; ranking below every real algorithm, an
unresolved SRI never wins a strongest-digest comparison.

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
    SHA384 -> Strong
    SHA512 -> Strongest
    Blake2b -> Strongest

{- | The algorithm a 'Hash' asserts: its tag directly, or — for an 'SRI' string — the
algorithm named in its @\<alg\>-\<base64\>@ prefix. The SRI prefixes resolved are
@sha256@, @sha384@ and @sha512@ (every long digest the model represents and a registry
serves); an unrecognised or malformed prefix yields 'Nothing', so it asserts no
algorithm and clears no floor (the fail-closed reading).

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

-- ── the integrity floors ─────────────────────────────────────────────────────

{- | The shared interface of an integrity floor: the minimum algorithm it requires. Both
the hard-floored public 'MinIntegrity' and the loosenable trusted 'MinTrustedIntegrity'
are floors, so 'meetsFloor' and 'classifyArtifacts' rank candidates against either through
this one class — backed by the single 'integrityStrength' ranking the worker's tamper gate
also consults. The class only /reads/ a floor's algorithm; a newtype's construction
invariant (the public hard-floor, the trusted loosenability) lives in its smart
constructors, never here.
-}
class IntegrityFloor floor where
    -- | The minimum algorithm this floor requires.
    floorAlgorithm :: floor -> HashAlg

{- | The configured minimum integrity algorithm a __public__ (untrusted) version's
digest must meet to be admitted. Opaque and __hard-floored at SHA-256__: build it
only through 'mkMinIntegrity' \/ 'parseMinIntegrity', which reject anything weaker, so
a value of this type carries the proof that the floor is itself collision-resistant.
There is deliberately no loosenable variant of /this/ floor: untrusted public bytes are
never admitted on a sub-SHA-256 digest.
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

instance IntegrityFloor MinIntegrity where
    floorAlgorithm = unMinIntegrity

{- | The configured minimum integrity algorithm a __trusted__ (private) version's digest
must meet to be served. Like 'MinIntegrity' it defaults to SHA-256, but unlike it carries
__no hard floor__: an operator may loosen it to SHA-1 or MD5 for a legacy private mirror,
where trust in their own vetted source substitutes for cryptographic strength. Build it
only through 'mkMinTrustedIntegrity' \/ 'parseMinTrustedIntegrity', which still reject an
unknown algorithm name (and the bare 'SRI' wrapper, which names no algorithm). Loosening
this floor is the /only/ path by which Écluse serves a sub-SHA-256 digest, and only on the
operator's own trusted source.
-}
newtype MinTrustedIntegrity = MinTrustedIntegrity HashAlg
    deriving stock (Eq, Show)

-- | The default trusted-integrity floor: SHA-256, the same secure default as the public floor.
defaultMinTrustedIntegrity :: MinTrustedIntegrity
defaultMinTrustedIntegrity = MinTrustedIntegrity SHA256

{- | Build a 'MinTrustedIntegrity'. Any /known/ algorithm is accepted — including the
broken SHA-1 and MD5, which an operator may deliberately loosen the trusted floor to —
but the bare 'SRI' wrapper, which asserts no algorithm of its own, is rejected (it could
never be a meaningful floor). There is intentionally no SHA-256 hard minimum here: that is
the one behavioural difference from 'mkMinIntegrity'.
-}
mkMinTrustedIntegrity :: HashAlg -> Either Text MinTrustedIntegrity
mkMinTrustedIntegrity SRI =
    Left "the minimum trusted integrity algorithm must name a concrete algorithm, not a bare SRI"
mkMinTrustedIntegrity alg = Right (MinTrustedIntegrity alg)

{- | Parse a 'MinTrustedIntegrity' from an algorithm name (e.g. @"sha256"@, @"sha1"@,
@"md5"@), case- and separator-insensitive. An unrecognised name is rejected; unlike
'parseMinIntegrity', a sub-SHA-256 name is /accepted/ — the trusted floor is loosenable.
-}
parseMinTrustedIntegrity :: Text -> Either Text MinTrustedIntegrity
parseMinTrustedIntegrity raw = parseHashAlg raw >>= mkMinTrustedIntegrity

-- | The trusted floor algorithm.
unMinTrustedIntegrity :: MinTrustedIntegrity -> HashAlg
unMinTrustedIntegrity (MinTrustedIntegrity alg) = alg

-- | Render a 'MinTrustedIntegrity' as its lower-case algorithm name (round-trips 'parseMinTrustedIntegrity').
renderMinTrustedIntegrity :: MinTrustedIntegrity -> Text
renderMinTrustedIntegrity = renderHashAlg . unMinTrustedIntegrity

instance IntegrityFloor MinTrustedIntegrity where
    floorAlgorithm = unMinTrustedIntegrity

{- | Whether an algorithm meets a floor: at least as strong as the floor's configured
minimum, by the shared 'integrityStrength' ranking. The candidate algorithm is a
/resolved/ one (from 'assertedAlg'), never a bare 'SRI'.
-}
meetsFloor :: (IntegrityFloor floor) => floor -> HashAlg -> Bool
meetsFloor flr alg = integrityStrength alg >= integrityStrength (floorAlgorithm flr)

-- ── version admissibility ────────────────────────────────────────────────────

{- | How a version's artifacts stand against an integrity floor — the three-way verdict
an admission gate (public or trusted) acts on.
-}
data VersionIntegrity
    = -- | At least one digest asserts an algorithm at or above the floor: admissible.
      MeetsFloor
    | {- | The version carries an integrity digest, but none meets the floor (e.g. a
      legacy SHA-1 shasum only under a SHA-256 floor). Inadmissible — distinct from
      carrying no digest at all, so the refusal can say which.
      -}
      BelowFloor
    | {- | The version carries no integrity digest of any kind: inadmissible (no floor
      can be met without a digest).
      -}
      NoIntegrity
    deriving stock (Eq, Show)

{- | Classify a version's artifacts against a floor (public or trusted). A version
'MeetsFloor' iff any of its digests (across all of its artifacts) asserts a floor-clearing
algorithm; failing that, it is 'NoIntegrity' when no artifact carries any digest at all,
else 'BelowFloor'. npm publishes one artifact per version, but the check spans the whole
'NonEmpty' so it holds for a multi-artifact ecosystem too.
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
-- lives in "Ecluse.Core.Package", the lowest layer, and is re-exported above so this module's
-- callers (and the worker and SQS) keep importing it from here.
