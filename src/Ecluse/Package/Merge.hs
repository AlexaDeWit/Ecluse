{- | Merging several upstream packuments into the one document Écluse serves.

A packument is the /set of available versions/ of a package, and that set is
spread across upstreams: a trusted private upstream holds what has been vetted,
while a gated public upstream holds the full history — including versions not yet
mirrored. Serving only the private document would hide those, so Écluse serves
their __union__ rather than short-circuiting on a private hit. This module is the
pure, ecosystem-agnostic fold that reasons over that union on the
'Ecluse.Package.PackageInfo' domain model — it lives above the registry handle,
written once and reused by every ecosystem, and never imports a registry adapter.

__Decision surface, not served surface.__ This module reasons over the /typed/
'PackageInfo' but does __not__ emit a finished, re-serialisable 'PackageInfo'.
The document Écluse serves is the raw upstream JSON (@Value@), edited in place by
the serve layer, so that every unmodeled wire key survives. The typed model
is lossy, so re-encoding it would drop those keys. This module therefore emits a
'MergePlan' — exactly which versions survive, which input each survivor came from,
the reconciled @dist-tags@\/@time@, and the detected divergences — that the serve
layer __replays onto the raw @Value@s__. See @docs\/architecture\/registry-model.md@
→ "Decision surface vs served surface".

The trust split is the __caller's__, expressed as a 'Provenance' tag on each
input and applied /before/ the merge: 'TrustedSource' (private) versions are
admitted as-is; 'GatedSource' (public) versions are the already-rule-filtered set.
This module does not run rules — it reasons over exactly what it is handed (see
@docs\/architecture\/rules-engine.md@ → "Applying verdicts to a packument").

Two things make the merge more than a map union, and both are
__supply-chain signals, not silent reconciliations__:

* __Collision.__ When the same version key comes from both a 'TrustedSource' and
  a 'GatedSource', the trusted copy wins (it is the authority) — recorded in the
  plan as the survivor's winning 'SourceId'.
* __Divergence.__ If the colliding copies carry /differing artifact integrity/,
  that is exactly the tampering Écluse exists to catch. The trusted copy still
  wins the merge, but the divergence is __reported__ in the 'MergePlan'; whether
  to additionally drop the version (fail-closed) is a policy decision left to the
  caller, so this module stays pure.

See @docs\/architecture\/registry-model.md@ → "Packument merge across upstreams".
-}
module Ecluse.Package.Merge (
    -- * Provenance
    Provenance (..),

    -- * Merging
    SourceId,
    MergePlan (..),
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
    PackageName,
 )
import Ecluse.Version (Version, selectLatest, unVersion)

{- | The trust provenance of an upstream's contribution to the merge. The split
is decided by the caller — by /which/ upstream a document came from — and applied
before merging, never derived here.

The constructors are named @\*Source@ rather than the bare @Trusted@\/@Gated@
because "Ecluse.Package" already exports a 'Ecluse.Package.Trust' constructor
named @Trusted@; a bare name would collide for the many callers that import
"Ecluse.Package" openly.
-}
data Provenance
    = {- | A private-upstream document. Its versions are already vetted, so they
      enter the union unfiltered and win any collision.
      -}
      TrustedSource
    | {- | A public-upstream document. Its versions are the set that already
      survived the rules engine; the merge unions them but never re-filters.
      -}
      GatedSource
    deriving stock (Eq, Ord, Show)

{- | A stable identifier for one input to a single 'mergePackuments' call: the
__0-based index of that @(Provenance, PackageInfo)@ in the input list__.

The serve layer needs to take a surviving version's object from the /raw/
@Value@ of whichever source won it, so the plan must name that source. 'Provenance'
alone is /not/ enough: it identifies a source only while there is exactly one
input per provenance (the npm topology today — one trusted, one gated). The
input index stays unambiguous even when several inputs share a provenance (e.g. an
aggregating private upstream plus a first-party source, both 'TrustedSource'),
which keeps the plan robust for the multi-source case without a new type. The
caller pairs each 'SourceId' back to the raw @Value@ it passed at that position.
-}
type SourceId = Int

{- | A detected integrity conflict: a version key present in more than one source
whose artifact integrity does /not/ agree. The trusted copy wins the merge; this
record preserves both fingerprints so the caller can log, meter, and decide policy
(serve-with-private-winning vs fail-closed). It is the merge's supply-chain
signal — surfaced, never silently reconciled.
-}
data Divergence = Divergence
    { divVersion :: Text
    {- ^ The raw version-string key the conflict was found at (the
    'Ecluse.Package.infoVersions' key).
    -}
    , divWinning :: IntegrityFingerprint
    -- ^ Integrity of the copy that won the merge (the higher-precedence source).
    , divLosing :: IntegrityFingerprint
    -- ^ Integrity of the copy that lost — kept so the conflict is auditable.
    }
    deriving stock (Eq, Show)

{- | The outcome of reasoning over a set of upstream packuments: a __plan__ the
serve layer replays onto the raw upstream @Value@s to assemble the lossless
served body. It carries exactly the decisions the merge owns — never a finished,
re-serialisable document (see this module's header, "Decision surface, not served
surface").
-}
data MergePlan = MergePlan
    { mpName :: PackageName
    {- ^ The package identity, taken from the first input (all inputs are the same
    package fetched across its upstreams).
    -}
    , mpSurvivors :: Map Text SourceId
    {- ^ Each surviving version key mapped to the 'SourceId' of the input that won
    it, so the serve layer takes that version's object from the right source's
    raw @Value@. Trusted wins a collision; absent versions are not keys here.
    -}
    , mpDistTags :: Map Text Version
    {- ^ @dist-tags@ reconciled over the surviving union — @latest@ resolved by the
    shared selector, every other surviving-target tag carried, absent-target
    tags dropped.
    -}
    , mpTime :: Map Text UTCTime
    {- ^ The @time@ union restricted to surviving versions; publish times for
    versions that did not survive are dropped.
    -}
    , mpDivergences :: [Divergence]
    {- ^ Every same-version integrity conflict found, in the order the merge
    encountered them. Empty when no two sources disagreed on a shared version's
    integrity.
    -}
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

{- | Reason over several upstream packuments, by 'Provenance', and emit the
'MergePlan' the serve layer replays onto the raw @Value@s. Pure and total.

The merge is a fold with the __degenerate identity at one input__: a single
packument yields a plan whose survivors are all of its versions (all won by source
@0@), with its tags and times reconciled and no divergences, so 0\/1-upstream
deployments need no special case. The model:

* __Union by version key__, with __'TrustedSource' winning__ a collision over
  'GatedSource' (the private upstream is the authority). The winning input's
  'SourceId' is recorded for the survivor. A collision whose integrity differs is
  recorded as a 'Divergence'; the winner is still kept.
* __'dist-tags' reconciled over the union.__ @latest@ is resolved by
  'Ecluse.Version.selectLatest' — keep-unless-denied, stable-preferring, and
  unparseable-safe — from the precedence-winning source's tagged @latest@ and the
  surviving versions; any other tag pointing at a version absent from the union is
  dropped. Collisions on the same tag are resolved __by provenance__ (trusted
  wins), consistent with the version fold, so the plan does not depend on caller
  input order.
* __@time@ restricted to the union__, with per-version collisions also resolved by
  provenance — publish times for versions that did not survive are dropped.

The plan's identity ('mpName') is taken from the first input; callers fetch one
package across its upstreams, so all inputs share it. An empty input list yields
'Nothing' — there is nothing to serve.
-}
mergePackuments :: [(Provenance, PackageInfo)] -> Maybe MergePlan
mergePackuments [] = Nothing
mergePackuments inputs@((_, firstInfo) : _) =
    Just
        MergePlan
            { mpName = infoName firstInfo
            , mpSurvivors = Map.map snd mergedVersions
            , mpDistTags = reconciledTags
            , mpTime = reconciledTimes
            , mpDivergences = divergences
            }
  where
    -- Each input tagged with its provenance and its stable 'SourceId' (list index).
    indexed :: [(SourceId, Provenance, PackageInfo)]
    indexed = [(i, prov, info) | (i, (prov, info)) <- zip [0 ..] inputs]

    -- Fold every source's versions into one map, trusted winning a key collision,
    -- recording for each survivor the details that won and the source that won it,
    -- and collecting any integrity divergence as we go.
    (mergedVersions, reversedDivs) =
        foldl' mergeSource (Map.empty, []) indexed

    -- The fold prepends each divergence (O(1)) rather than appending, so it stays
    -- O(k) for k divergences; reverse once here to restore encounter order, which
    -- 'mpDivergences' documents.
    divergences = reverse reversedDivs

    mergeSource ::
        (Map Text (PackageDetails, SourceId), [Divergence]) ->
        (SourceId, Provenance, PackageInfo) ->
        (Map Text (PackageDetails, SourceId), [Divergence])
    mergeSource acc (sid, prov, info) =
        -- Fold the tree directly with 'foldlWithKey'' rather than over
        -- 'Map.toList': the list form allocates a cons cell and a (key, value)
        -- tuple per version (up to 'maxVersionCount') only to be torn apart at
        -- once. 'mergeVersion' takes the key and value unpaired to match.
        Map.foldlWithKey' (mergeVersion sid prov) acc (infoVersions info)

    mergeVersion ::
        SourceId ->
        Provenance ->
        (Map Text (PackageDetails, SourceId), [Divergence]) ->
        Text ->
        PackageDetails ->
        (Map Text (PackageDetails, SourceId), [Divergence])
    mergeVersion sid prov (versions, divs) key incoming =
        case Map.lookup key versions of
            Nothing -> (Map.insert key (incoming, sid) versions, divs)
            Just (existing, existingSid) ->
                -- Prepend, not append: 'divs <> divs'' would re-traverse the growing
                -- accumulator each step, making the fold O(k²) in the number of
                -- divergences. 'mergePackuments' reverses once at the end to recover
                -- the encounter order 'mpDivergences' documents.
                let (winner, divs') =
                        resolveCollision key prov (existing, existingSid) (incoming, sid)
                 in (Map.insert key winner versions, divs' ++ divs)

    -- The accumulator already holds a value for this key; @existing@ won an
    -- earlier round (so it is at least as high-precedence as anything before),
    -- and @incoming@ is the new contender at provenance @prov@. The higher
    -- precedence wins — keeping its 'SourceId' — and a differing integrity is a
    -- divergence either way.
    resolveCollision ::
        Text ->
        Provenance ->
        (PackageDetails, SourceId) ->
        (PackageDetails, SourceId) ->
        ((PackageDetails, SourceId), [Divergence])
    resolveCollision key prov existing incoming =
        let (winner, loser)
                | prov == TrustedSource = (incoming, existing)
                | otherwise = (existing, incoming)
            diverges = fingerprint (fst existing) /= fingerprint (fst incoming)
            divs =
                [ Divergence
                    { divVersion = key
                    , divWinning = fingerprint (fst winner)
                    , divLosing = fingerprint (fst loser)
                    }
                | diverges
                ]
         in (winner, divs)

    survives :: Text -> Bool
    survives key = Map.member key mergedVersions

    -- The surviving version objects (the details that won each key).
    survivingDetails :: [PackageDetails]
    survivingDetails = map fst (Map.elems mergedVersions)

    -- @dist-tags@ from every source with same-tag collisions resolved __by
    -- provenance__ (trusted wins); computed once and shared by the carried tags
    -- and @latest@ resolution below.
    distTagsByProvenance :: Map Text Version
    distTagsByProvenance = byProvenance infoDistTags

    -- @dist-tags@ reconciled over the union: every surviving-target tag carried,
    -- and @latest@ resolved by the shared selector. Built so the plan never depends
    -- on the order the caller happened to pass the inputs.
    reconciledTags :: Map Text Version
    reconciledTags =
        let carried = Map.filter (survives . unVersion) distTagsByProvenance
         in case resolvedLatest of
                Nothing -> Map.delete "latest" carried
                Just v -> Map.insert "latest" v carried

    -- @latest@ via the shared resolver: keep the precedence-winning source's
    -- tagged @latest@ if it survives, else repoint (stable-preferring,
    -- unparseable-safe) among survivors. @chosen@ is picked by provenance — the
    -- trusted source's @latest@ when one is tagged — consistent with the version
    -- and dist-tag folds, so it does not depend on caller input order.
    resolvedLatest :: Maybe Version
    resolvedLatest =
        selectLatest chosenLatest (map pkgVersion survivingDetails)

    chosenLatest :: Maybe Version
    chosenLatest = Map.lookup "latest" distTagsByProvenance

    -- @time@ over the union: each source's publish times restricted to surviving
    -- versions, with per-version collisions resolved by provenance (trusted wins).
    reconciledTimes :: Map Text UTCTime
    reconciledTimes =
        Map.filterWithKey (\k _ -> survives k) (byProvenance infoPublishedAt)

    {- Combine a per-source map across all inputs, resolving same-key collisions
    __by provenance__: a 'TrustedSource' entry wins over a 'GatedSource' one,
    independent of the order the inputs were passed. A left-biased union over the
    inputs sorted so trusted sources come first achieves this; 'Data.Map.Strict'
    is stable so a later trusted source never displaces an earlier one needlessly,
    but any two values that could collide across the trust split are decided by the
    split, not by position. Consults the once-sorted 'inputsByProvenance'. -}
    byProvenance :: (PackageInfo -> Map Text a) -> Map Text a
    byProvenance f =
        Map.unions [f info | (_, _, info) <- inputsByProvenance]

    -- The inputs sorted so trusted sources precede gated, computed once and shared
    -- by every 'byProvenance' call rather than re-sorted per call. Trusted ranks
    -- before gated, so it wins the left-biased union; 'sortOn' is stable, so
    -- same-provenance inputs keep their original input order.
    inputsByProvenance :: [(SourceId, Provenance, PackageInfo)]
    inputsByProvenance = sortOn provenanceRank indexed
      where
        provenanceRank (_, prov, _) = prov == GatedSource

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
