{- | Merging several upstream packuments into the one document Écluse serves.

A packument is the /set of available versions/ of a package, and that set is
spread across upstreams: a trusted private upstream holds what has been vetted,
while a gated public upstream holds the full history — including versions not yet
mirrored. Serving only the private document would hide those, so Écluse serves
their __union__ rather than short-circuiting on a private hit. This module is the
pure, ecosystem-agnostic fold that builds that union over the
'Ecluse.Package.PackageInfo' domain model — it lives above the registry handle,
written once and reused by every ecosystem, and never imports a registry adapter.

The trust split is the __caller's__, expressed as a 'Provenance' tag on each
input and applied /before/ the merge: 'TrustedSource' (private) versions are
admitted as-is; 'GatedSource' (public) versions are the already-rule-filtered set.
This module
does not run rules — it unions exactly what it is handed (see
@docs\/architecture\/rules-engine.md@ → "Applying verdicts to a packument").

Two things make the merge more than a map union, and both are
__supply-chain signals, not silent reconciliations__:

* __Collision.__ When the same version key comes from both a 'TrustedSource' and
  a 'GatedSource', the trusted copy wins (it is the authority).
* __Divergence.__ If the colliding copies carry /differing artifact integrity/,
  that is exactly the tampering Écluse exists to catch. The trusted copy still
  wins the merge, but the divergence is __reported__ in the 'MergeResult'; whether
  to additionally drop the version (fail-closed) is a policy decision left to the
  caller, so this module stays pure.

See @docs\/architecture\/registry-model.md@ → "Packument merge across upstreams".
-}
module Ecluse.Package.Merge (
    -- * Provenance
    Provenance (..),

    -- * Merging
    MergeResult (..),
    Divergence (..),
    IntegrityFingerprint,
    integrityHashes,
    mergePackuments,
) where

import Data.Map.Strict qualified as Map
import Data.Time (UTCTime)

import Ecluse.Package (
    Artifact (..),
    Hash (..),
    HashAlg,
    PackageDetails (..),
    PackageInfo (..),
 )
import Ecluse.Version (Version, compareVersions, unVersion)

{- | The trust provenance of an upstream's contribution to the merge. The split
is decided by the caller — by /which/ upstream a document came from — and applied
before merging, never derived here.

The constructors are named @\*Source@ rather than the bare @Trusted@\/@Gated@
because "Ecluse.Package" already exports a 'Ecluse.Package.Trust' constructor
named @Trusted@; a bare name would collide for the many callers that import
"Ecluse.Package" openly.
-}
data Provenance
    = -- | A private-upstream document. Its versions are already vetted, so they
      -- enter the union unfiltered and win any collision.
      TrustedSource
    | -- | A public-upstream document. Its versions are the set that already
      -- survived the rules engine; the merge unions them but never re-filters.
      GatedSource
    deriving stock (Eq, Ord, Show)

{- | A detected integrity conflict: a version key present in more than one source
whose artifact integrity does /not/ agree. The trusted copy wins the merge; this
record preserves both fingerprints so the caller can log, meter, and decide policy
(serve-with-private-winning vs fail-closed). It is the merge's supply-chain
signal — surfaced, never silently reconciled.
-}
data Divergence = Divergence
    { divVersion :: Text
    -- ^ The raw version-string key the conflict was found at (the
    -- 'Ecluse.Package.infoVersions' key).
    , divWinning :: IntegrityFingerprint
    -- ^ Integrity of the copy that won the merge (the higher-precedence source).
    , divLosing :: IntegrityFingerprint
    -- ^ Integrity of the copy that lost — kept so the conflict is auditable.
    }
    deriving stock (Eq, Show)

{- | The outcome of merging a set of upstream packuments: the one document Écluse
serves, plus every integrity divergence detected while building it.
-}
data MergeResult = MergeResult
    { mergedInfo :: PackageInfo
    -- ^ The merged packument: the union of surviving versions, with @dist-tags@
    -- and @time@ reconciled over that union.
    , mergeDivergences :: [Divergence]
    -- ^ Every same-version integrity conflict found, in the order the merge
    -- encountered them. Empty when no two sources disagreed on a shared version's
    -- integrity.
    }
    deriving stock (Eq, Show)

{- | An order-independent fingerprint of a version's artifact integrity: the
sorted multiset of @(algorithm, digest)@ pairs across all of the version's
artifacts. Two versions /diverge/ exactly when their fingerprints differ, so the
comparison ignores artifact ordering and non-integrity fields (filename, URL,
size) that legitimately vary between mirrors of the same bytes.

Opaque so the equality used for divergence detection cannot be sidestepped; read
the pairs back with 'integrityHashes' when logging or metering a 'Divergence'.
-}
newtype IntegrityFingerprint = IntegrityFingerprint [(HashAlg, Text)]
    deriving stock (Eq, Show)

-- | The @(algorithm, digest)@ pairs of a fingerprint, sorted, for an audit trail.
integrityHashes :: IntegrityFingerprint -> [(HashAlg, Text)]
integrityHashes (IntegrityFingerprint hs) = hs

{- | Merge several upstream packuments, by 'Provenance', into the single document
Écluse serves, reporting every integrity divergence found. Pure and total.

The merge is a fold with the __degenerate identity at one input__: merging a
single packument yields that packument (its versions, tags, and times) with no
divergences, so 0\/1-upstream deployments need no special case. The model:

* __Union by version key__, with __'TrustedSource' winning__ a collision over
  'GatedSource' (the private upstream is the authority). A collision whose integrity
  differs is recorded as a 'Divergence'; the winner is still kept.
* __'dist-tags' reconciled over the union.__ @latest@ is repointed to the highest
  /surviving/ version across all sources (by 'compareVersions'); any other tag
  pointing at a version absent from the union is dropped.
* __@time@ restricted to the union__ — publish times for versions that did not
  survive are dropped.

The merged document's identity ('Ecluse.Package.infoName') is taken from the
first input; callers fetch one package across its upstreams, so all inputs share
it. An empty input list yields an empty document for that name's absence — there
is nothing to serve — represented by 'Nothing'.
-}
mergePackuments :: [(Provenance, PackageInfo)] -> Maybe MergeResult
mergePackuments [] = Nothing
mergePackuments inputs@((_, firstInfo) : _) =
    Just
        MergeResult
            { mergedInfo =
                PackageInfo
                    { infoName = infoName firstInfo
                    , infoVersions = mergedVersions
                    , infoDistTags = reconciledTags
                    , infoPublishedAt = reconciledTimes
                    }
            , mergeDivergences = divergences
            }
  where
    -- Fold the versions of every source into one map, trusted winning on a key
    -- collision, collecting any integrity divergence as we go.
    (mergedVersions, divergences) =
        foldl' mergeSource (Map.empty, []) inputs

    mergeSource ::
        (Map Text PackageDetails, [Divergence]) ->
        (Provenance, PackageInfo) ->
        (Map Text PackageDetails, [Divergence])
    mergeSource acc (prov, info) =
        foldl' (mergeVersion prov) acc (Map.toList (infoVersions info))

    mergeVersion ::
        Provenance ->
        (Map Text PackageDetails, [Divergence]) ->
        (Text, PackageDetails) ->
        (Map Text PackageDetails, [Divergence])
    mergeVersion prov (versions, divs) (key, incoming) =
        case Map.lookup key versions of
            Nothing -> (Map.insert key incoming versions, divs)
            Just existing ->
                let (winner, divs') = resolveCollision key prov existing incoming
                 in (Map.insert key winner versions, divs <> divs')

    -- The accumulator already holds a value for this key; @existing@ won an
    -- earlier round (so it is at least as high-precedence as anything before),
    -- and @incoming@ is the new contender at provenance @prov@. The higher
    -- precedence wins, and a differing integrity is a divergence either way.
    resolveCollision ::
        Text ->
        Provenance ->
        PackageDetails ->
        PackageDetails ->
        (PackageDetails, [Divergence])
    resolveCollision key prov existing incoming =
        let winner = if prov == TrustedSource then incoming else existing
            loser = if prov == TrustedSource then existing else incoming
            diverges = fingerprint existing /= fingerprint incoming
            divs =
                [ Divergence
                    { divVersion = key
                    , divWinning = fingerprint winner
                    , divLosing = fingerprint loser
                    }
                | diverges
                ]
         in (winner, divs)

    survives :: Text -> Bool
    survives key = Map.member key mergedVersions

    -- @dist-tags@ reconciled over the union: @latest@ to the highest surviving
    -- version across all sources, every other surviving-target tag carried, and
    -- absent-target tags dropped. Built from the lowest-precedence source first
    -- so higher-precedence sources' tag targets win.
    reconciledTags :: Map Text Version
    reconciledTags =
        let carried =
                Map.filter (survives . unVersion) $
                    Map.unions (map (infoDistTags . snd) (reverse inputs))
         in case latestSurviving of
                Nothing -> Map.delete "latest" carried
                Just v -> Map.insert "latest" v carried

    -- The highest surviving version across the union, by 'compareVersions'.
    -- Versions whose raw text does not parse abstain from ordering, so they
    -- cannot become @latest@ (mirroring the version engine's "unknown" posture).
    latestSurviving :: Maybe Version
    latestSurviving =
        foldl' higher Nothing (map pkgVersion (Map.elems mergedVersions))
      where
        higher acc v = case acc of
            Nothing -> Just v
            Just best -> if compareVersions v best == Just GT then Just v else acc

    -- @time@ over the union: each source's publish times, restricted to surviving
    -- versions, with higher-precedence sources winning a per-version collision.
    reconciledTimes :: Map Text UTCTime
    reconciledTimes =
        Map.filterWithKey (\k _ -> survives k) $
            Map.unions (map (infoPublishedAt . snd) (reverse inputs))

-- The order-independent integrity fingerprint of a version: every artifact's
-- @(algorithm, digest)@ pairs, gathered across all artifacts and sorted, so the
-- comparison is stable regardless of artifact or hash ordering on the wire.
fingerprint :: PackageDetails -> IntegrityFingerprint
fingerprint =
    IntegrityFingerprint
        . sort
        . concatMap artHashPairs
        . toList
        . pkgArtifacts
  where
    artHashPairs art = [(hashAlg h, hashValue h) | h <- artHashes art]
