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
* __Divergence.__ When the colliding copies __contradict on a shared integrity
  algorithm__ — an algorithm both expose carries /disagreeing/ digests — that is
  exactly the tampering Écluse exists to catch. Copies that merely expose
  /different/ algorithm sets without contradicting on a shared one (one mirror also
  carrying a legacy digest the other omits) describe the same bytes and are __not__ a
  divergence. The trusted copy still wins the merge, but a real contradiction is
  __reported__ in the 'MergePlan'; whether to additionally drop the version
  (fail-closed) is a policy decision left to the caller, so this module stays pure.

__The merge is a lawful 'Monoid'.__ The fold is realised over a 'Merge'
accumulator with a lawful 'Semigroup' \/ 'Monoid': 'mempty' is the empty merge
(the degenerate identity at zero inputs) and @(<>)@ is the trusted-wins union with
order-independent divergence detection. 'mergePackuments' assigns each input a
'SourceId' by list position, @foldMap@s the contributions into the accumulator,
and projects to a 'MergePlan'. See the 'Semigroup' instance for the exact law
domain (associative + identity, intentionally __not__ commutative).

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

    -- * The merge accumulator
    -- $accumulator
    Merge,
    contribute,
    planFrom,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time (UTCTime)

import Ecluse.Package (
    Artifact (..),
    HashAlg,
    PackageDetails (..),
    PackageInfo (..),
    PackageName,
    hashAlg,
    hashValue,
 )
import Ecluse.Version (Version, selectLatest, unVersion)

{- | The trust provenance of an upstream's contribution to the merge. The split
is decided by the caller — by /which/ upstream a document came from — and applied
before merging, never derived here.

The constructors are named @\*Source@ rather than the bare @Trusted@\/@Gated@
because "Ecluse.Package" already exports a 'Ecluse.Package.Trust' constructor
named @Trusted@; a bare name would collide for the many callers that import
"Ecluse.Package" openly.

The 'Ord' instance is the trust order itself — 'TrustedSource' compares __less
than__ 'GatedSource' so that "smallest wins" gives trusted precedence; the merge's
resolution leans on this directly (see 'mergePackuments').
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
whose copies __contradict on a shared algorithm__ — an algorithm both expose carries
disagreeing digests. The trusted copy wins the merge; this record preserves both
fingerprints so the caller can log, meter, and decide policy (serve-with-private-winning
vs fail-closed). It is the merge's supply-chain signal — surfaced, never silently
reconciled.

'Ord' is derived purely to let 'MergePlan' carry divergences as a 'Set': the
ordering is structural (over the version key and the two fingerprints) and has no
meaning beyond deduplication and a stable presentation.
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
    deriving stock (Eq, Ord, Show)

{- | The outcome of reasoning over a set of upstream packuments: a __plan__ the
serve layer replays onto the raw upstream @Value@s to assemble the lossless
served body. It carries exactly the decisions the merge owns — never a finished,
re-serialisable document (see this module's header, "Decision surface, not served
surface").
-}
data MergePlan = MergePlan
    { mpName :: PackageName
    {- ^ The package identity, carried from the contributions. Every contribution that
    reaches the merge has had its self-reported name validated against the requested
    one upstream of here (a disagreeing origin is dropped before the merge), so all
    inputs carry the same identity and it is never a substituted or manufactured value
    — only one an upstream genuinely reported.
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
    , mpDivergences :: Set Divergence
    {- ^ Every distinct same-version integrity conflict found. A 'Set' because
    divergence is a property of the /set/ of distinct integrity fingerprints
    contributed for a version key, not of any pairwise fold step: the winner's
    fingerprint is recorded against /each distinct fingerprint that contradicts it on
    a shared algorithm/, which is order-independent and deduplicating by construction.
    Empty when no two copies of a shared version contradict on a shared algorithm —
    including when they merely expose different algorithm sets without disagreeing on
    one they share.
    -}
    }
    deriving stock (Eq, Show)

{- | An order-independent fingerprint of a version's artifact integrity: the
sorted multiset of @(algorithm, digest)@ pairs across all of the version's
artifacts. The comparison ignores artifact ordering and non-integrity fields
(filename, URL, size) that legitimately vary between mirrors of the same bytes.

Two copies /diverge/ when they __contradict on a shared algorithm__: an algorithm
present in both carries disagreeing digests. An /asymmetric/ pair — one copy
exposing an algorithm the other omits — does __not__ diverge on that account; only a
shared algorithm whose digests disagree does. So a mirror serving a modern digest
alongside a legacy one agrees with a mirror serving only the modern digest, as long
as that shared digest matches.

Opaque so the comparison used for divergence detection cannot be sidestepped; read
the pairs back with 'integrityHashes' when logging or metering a 'Divergence'. 'Ord'
is derived (structurally, over the sorted pairs) only so a 'Divergence' may live in a
'Set'; it carries no domain meaning beyond that, and in particular is __not__ the
divergence test (which is the shared-algorithm contradiction above, never structural
inequality of the whole set).
-}
newtype IntegrityFingerprint = IntegrityFingerprint [(HashAlg, Text)]
    deriving stock (Eq, Ord, Show)

-- | The @(algorithm, digest)@ pairs of a fingerprint, sorted, for an audit trail.
integrityHashes :: IntegrityFingerprint -> [(HashAlg, Text)]
integrityHashes (IntegrityFingerprint hs) = hs

{- | The trust-then-position rank of a contribution, the strict total order the
whole merge resolves by: 'TrustedSource' outranks 'GatedSource', and ties — only
possible between two inputs of the /same/ provenance — are broken by the __lower
'SourceId'__ (the earlier input position). 'SourceId' is unique per input, so this
is a strict total order and the smallest-ranked contribution is a deterministic,
order-independent winner. With one trusted and one gated source (the topology
today) the trusted rank always wins outright and the positional tiebreak never
fires, so behaviour is preserved exactly.

The order is over a @(Provenance, SourceId)@ pair, exploiting that 'TrustedSource'
'<' 'GatedSource' and that smaller 'SourceId' is earlier; @minimumBy (comparing
rank)@ then picks the winner.
-}
rank :: Provenance -> SourceId -> (Provenance, SourceId)
rank prov sid = (prov, sid)

{- | One source's contribution to a single version key: the input that offered it
(by 'Provenance' and 'SourceId') together with the integrity fingerprint and the
typed details it carried. Equality and order are structural over all four fields,
so a 'Set' of these deduplicates identical contributions while keeping distinct
ones (e.g. two sources at the same key but differing integrity).
-}
data Candidate = Candidate
    { candProvenance :: Provenance
    , candSourceId :: SourceId
    , candFingerprint :: IntegrityFingerprint
    , candDetails :: PackageDetails
    }
    deriving stock (Show)

-- The identity of a candidate, for 'Eq' and 'Ord' alike: its resolution rank
-- ('TrustedSource' first, then lower 'SourceId') and its integrity fingerprint.
-- 'candDetails' is deliberately /not/ part of the identity — two contributions
-- that agree on rank and integrity are the same candidate for the merge — and
-- 'SourceId' is unique per call, so this never collapses two real inputs. Keeping
-- 'Eq' and 'Ord' on the same fields satisfies their consistency law (so a 'Set'
-- membership test agrees with structural equality).
candKey :: Candidate -> ((Provenance, SourceId), IntegrityFingerprint)
candKey c = (rank (candProvenance c) (candSourceId c), candFingerprint c)

instance Eq Candidate where
    a == b = candKey a == candKey b

instance Ord Candidate where
    compare a b = compare (candKey a) (candKey b)

{- | One source's tagged value (a @dist-tags@ target or a @time@ instant) for a
single key, paired with the rank of the source that offered it, so the projection
can pick the precedence-winning value the same way version collisions are
resolved. Ordered by rank alone — the winner is the minimum — so a left-biased
@Map.unions@ of singletons resolves the collision by provenance, not position.
-}
data Ranked a = Ranked
    { rankedRank :: (Provenance, SourceId)
    , rankedValue :: a
    }
    deriving stock (Eq, Show)

instance (Eq a) => Ord (Ranked a) where
    compare a b = compare (rankedRank a) (rankedRank b)

-- Combine two ranked values for the same key by keeping the higher-precedence
-- one (the smaller rank). Associative and commutative, so a 'Map.unionWith' over
-- it resolves a key's collision by provenance independent of input order.
keepBetter :: Ranked a -> Ranked a -> Ranked a
keepBetter x y = if rankedRank x <= rankedRank y then x else y

{- $accumulator
The merge is realised as a fold into a lawful 'Monoid'. 'contribute' turns one
@(Provenance, PackageInfo)@ input into a 'Merge'; @(<>)@ combines two merges
(trusted-wins union, with order-independent divergence kept unresolved until the
projection); 'mempty' is the empty merge (the degenerate identity). 'planFrom'
projects a folded 'Merge' to a 'MergePlan'. 'mergePackuments' is exactly
@'planFrom' . 'foldMap' ('uncurry' 'contribute')@. The 'Merge' type is opaque —
build it only through 'contribute' and 'mempty' — so a 'SourceId' always names a
real input position. See the 'Semigroup' instance for the law domain (associative
+ identity, intentionally __not__ commutative, and why).
-}

{- | The monoidal accumulator the merge folds into. It holds, /unresolved/, every
candidate offered for every version key, plus the ranked @dist-tags@ and @time@
contributions; resolution to a single winner per key, and the divergence set,
happens once in 'planFrom'. Keeping candidates unresolved is what makes @(<>)@
associative: a pairwise winner-vs-loser decision taken /during/ the fold is not
associative once three or more copies of a key collide, because divergence is a
property of the whole /set/ of distinct fingerprints, not of any one step.

Each accumulator also carries the count of inputs it represents, so that @(<>)@
can __re-index the right operand's 'SourceId's by the left operand's input
count__. This positional re-indexing is what makes a 'SourceId' name an input's
list position after a @foldMap@ of single-input contributions — and it is the sole
reason the instance is non-commutative (see the 'Semigroup' instance).
-}
data Merge = Merge
    { mergeCount :: Int
    -- ^ How many inputs this accumulator represents (the next free 'SourceId').
    , mergeVersions :: Map Text (Set Candidate)
    -- ^ Every candidate offered for each version key, unresolved.
    , mergeDistTags :: Map Text (Ranked Version)
    -- ^ The precedence-winning @dist-tags@ target offered for each tag.
    , mergeTime :: Map Text (Ranked UTCTime)
    -- ^ The precedence-winning publish instant offered for each version key.
    , mergeName :: Maybe PackageName
    {- ^ The package identity. Every contribution carries the same name — each has been
    validated against the requested name before reaching the merge — so the left-biased
    @(<>)@ choice selects that one shared identity rather than arbitrating between
    possibly-divergent self-reports. 'Nothing' only for 'mempty' (the empty merge), so
    the @(<|>)@ over 'Maybe' encodes purely the degenerate "no inputs yet" identity.
    -}
    }
    deriving stock (Eq, Show)

{- | The merge's 'Semigroup' has a deliberately narrow law domain, and the
narrowing is load-bearing, not an accident:

* __Associative__ — @(a '<>' b) '<>' c@ '==' @a '<>' (b '<>' c)@. The 'SourceId'
  re-indexing offsets compose additively, and every per-key combiner (set union
  for candidates, "keep the smaller rank" for tags\/time, "left name wins" for the
  identity) is itself associative, so the whole is.
* __Identity__ — 'mempty' (the empty merge) is both a left and a right unit.
* __Intentionally NOT commutative__ — @a '<>' b@ '/=' @b '<>' a@ in general. @(<>)@
  re-indexes the /right/ operand's 'SourceId's by the /left/ operand's input
  count, because a 'SourceId' must name the input's __position__ in the caller's
  list — the index the serve layer pairs back to a raw @Value@. Swapping the
  operands swaps those positions, so the 'SourceId' /labels/ differ.

The order-independence guarantee, stated precisely (and the reason commutativity is
the wrong law): precedence is resolved __by provenance__, so the surviving key set
and the winning /provenance/ per key are invariant under any permutation of the
inputs, and the value-level reconciliations (the survivor a key resolves to, the
divergence fingerprint-pairs, the @dist-tags@\/@time@ targets) are invariant under
any permutation that keeps each collision __cross-provenance__ — which the npm
topology (exactly one trusted, one gated upstream) always does, so every observable
decision is order-independent there. The /sole/ residual order-dependence is the
positional tiebreak between two inputs of the __same__ provenance: provenance cannot
break that tie, so the lower 'SourceId' (earlier input) wins it, and which copy is
the divergence winner then tracks order. That positional tiebreak is exactly why
'SourceId' exists and why the instance is non-commutative.
-}
instance Semigroup Merge where
    a <> b =
        Merge
            { mergeCount = mergeCount a + mergeCount b
            , mergeVersions =
                Map.unionWith Set.union (mergeVersions a) (shiftVersions (mergeVersions b))
            , mergeDistTags =
                Map.unionWith keepBetter (mergeDistTags a) (shiftRanked <$> mergeDistTags b)
            , mergeTime =
                Map.unionWith keepBetter (mergeTime a) (shiftRanked <$> mergeTime b)
            , mergeName = mergeName a <|> mergeName b
            }
      where
        -- Re-index the right operand's SourceIds into positions after the left
        -- operand's inputs, so a fold of single-input contributions lands each at
        -- its list index. This offset is what makes (<>) non-commutative.
        offset = mergeCount a
        shiftVersions = fmap (Set.map shiftCandidate)
        shiftCandidate c = c{candSourceId = candSourceId c + offset}
        shiftRanked (Ranked (prov, sid) v) = Ranked (prov, sid + offset) v

instance Monoid Merge where
    mempty =
        Merge
            { mergeCount = 0
            , mergeVersions = Map.empty
            , mergeDistTags = Map.empty
            , mergeTime = Map.empty
            , mergeName = Nothing
            }

{- | One input's contribution to the accumulator, at local 'SourceId' @0@: every
version becomes a candidate, every @dist-tags@ target and @time@ instant a ranked
value at this input's provenance, and the package name is offered as the identity.
@foldMap contribute@ over the inputs then re-indexes each to its list position via
the 'Semigroup' offset, so the absolute 'SourceId' of a single-input contribution
is its index in the @foldMap@.
-}
contribute :: Provenance -> PackageInfo -> Merge
contribute prov info =
    Merge
        { mergeCount = 1
        , mergeVersions = Map.map candidateFor (infoVersions info)
        , mergeDistTags = Map.map (Ranked here) (infoDistTags info)
        , mergeTime = Map.map (Ranked here) (infoPublishedAt info)
        , mergeName = Just (infoName info)
        }
  where
    -- Local SourceId 0; the Semigroup offset re-indexes it to the input position.
    here = (prov, 0)
    candidateFor details =
        Set.singleton
            Candidate
                { candProvenance = prov
                , candSourceId = 0
                , candFingerprint = fingerprint details
                , candDetails = details
                }

{- | Reason over several upstream packuments, by 'Provenance', and emit the
'MergePlan' the serve layer replays onto the raw @Value@s. Pure and total.

The merge is a fold with the __degenerate identity at one input__: a single
packument yields a plan whose survivors are all of its versions (all won by source
@0@), with its tags and times reconciled and no divergences, so 0\/1-upstream
deployments need no special case. It is realised as a 'foldMap' of each input's
'contribute' into the lawful 'Merge' 'Monoid', projected by 'planFrom'. The model:

* __Union by version key__, with __'TrustedSource' winning__ a collision over
  'GatedSource' (the private upstream is the authority). The winning input's
  'SourceId' is recorded for the survivor. A collision whose copies contradict on a
  shared integrity algorithm is recorded as a 'Divergence'; the winner is still kept.
* __'dist-tags' reconciled over the union.__ @latest@ is resolved by
  'Ecluse.Version.selectLatest' — keep-unless-denied, stable-preferring, and
  unparseable-safe — from the precedence-winning source's tagged @latest@ and the
  surviving versions; any other tag pointing at a version absent from the union is
  dropped. Collisions on the same tag are resolved __by provenance__ (trusted
  wins), consistent with the version fold, so the plan does not depend on caller
  input order.
* __@time@ restricted to the union__, with per-version collisions also resolved by
  provenance — publish times for versions that did not survive are dropped.

The plan's identity ('mpName') is carried from the contributions; callers fetch one
package across its upstreams and each contribution's name has been validated against
the requested one before reaching here, so all inputs share that one identity and it
is never a substituted value. An empty input list yields 'Nothing' — there is nothing
to serve.
-}
mergePackuments :: [(Provenance, PackageInfo)] -> Maybe MergePlan
mergePackuments [] = Nothing
mergePackuments inputs = planFrom (foldMap (uncurry contribute) inputs)

{- | Project the resolved 'MergePlan' from a folded 'Merge'. Resolves each version
key to its precedence winner, derives the divergence 'Set' from the shared-algorithm
contradictions among each key's distinct fingerprints, and reconciles
@dist-tags@\/@time@ over the survivors. Returns
'Nothing' only for the empty merge ('mempty'), which has no name and so nothing to
serve — equivalently, the empty input list.
-}
planFrom :: Merge -> Maybe MergePlan
planFrom acc = do
    name <- mergeName acc
    pure
        MergePlan
            { mpName = name
            , mpSurvivors = Map.map (candSourceId . winnerOf) (mergeVersions acc)
            , mpDistTags = reconciledTags
            , mpTime = reconciledTimes
            , mpDivergences = divergences
            }
  where
    -- The precedence winner among a key's candidates: the minimum by rank
    -- ('TrustedSource' first, then lower 'SourceId'). A key always has at least
    -- one candidate, so 'Set.findMin' is total here.
    winnerOf :: Set Candidate -> Candidate
    winnerOf = Set.findMin

    survives :: Text -> Bool
    survives key = Map.member key (mergeVersions acc)

    -- The surviving version objects (the details that won each key).
    survivingDetails :: [PackageDetails]
    survivingDetails =
        [candDetails (winnerOf cs) | cs <- Map.elems (mergeVersions acc)]

    -- Divergence is a property of the /set/ of distinct integrity fingerprints a key
    -- was offered, never of a pairwise fold step — which is what keeps it
    -- order-independent and associative for 3+ copies of a key. For each version key,
    -- record the winner's fingerprint against each distinct fingerprint that
    -- 'contradicts' it on a shared algorithm; a fingerprint that only adds or omits an
    -- algorithm relative to the winner, without disagreeing on one they share, is not a
    -- divergence (and the winner never contradicts itself, so it is excluded too). With
    -- the two-source topology this is a single winner-vs-loser pair; with three or more
    -- it is the full fan-out, deduplicated by the 'Set'.
    divergences :: Set Divergence
    divergences =
        Set.fromList
            [ Divergence{divVersion = key, divWinning = win, divLosing = lose}
            | (key, cs) <- Map.toList (mergeVersions acc)
            , let win = candFingerprint (winnerOf cs)
            , let distinct = Set.fromList [candFingerprint c | c <- Set.toList cs]
            , lose <- Set.toList distinct
            , contradicts win lose
            ]

    -- @dist-tags@ reconciled over the union: every surviving-target tag carried,
    -- and @latest@ resolved by the shared selector. Same-tag collisions are
    -- already resolved by provenance in the accumulator, so this never depends on
    -- the order the caller passed the inputs.
    reconciledTags :: Map Text Version
    reconciledTags =
        let carried = Map.filter (survives . unVersion) (Map.map rankedValue (mergeDistTags acc))
         in case resolvedLatest of
                Nothing -> Map.delete "latest" carried
                Just v -> Map.insert "latest" v carried

    -- @latest@ via the shared resolver: keep the precedence-winning source's
    -- tagged @latest@ if it survives, else repoint (stable-preferring,
    -- unparseable-safe) among survivors. @chosen@ is the provenance-winning
    -- source's @latest@, consistent with the version and dist-tag folds.
    resolvedLatest :: Maybe Version
    resolvedLatest =
        selectLatest chosenLatest (map pkgVersion survivingDetails)

    chosenLatest :: Maybe Version
    chosenLatest = rankedValue <$> Map.lookup "latest" (mergeDistTags acc)

    -- @time@ over the union: each key's publish instant (collisions already
    -- resolved by provenance in the accumulator) restricted to surviving versions.
    reconciledTimes :: Map Text UTCTime
    reconciledTimes =
        Map.filterWithKey (\k _ -> survives k) (Map.map rankedValue (mergeTime acc))

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

-- Whether two fingerprints contradict: some algorithm carried by /both/ has
-- disagreeing digests. This is the divergence test. Only a shared algorithm whose
-- digests disagree counts — an asymmetric pair that merely adds or omits an algorithm
-- one side lacks does not contradict, because the same bytes can be described by
-- different sets of digests (an older mirror serving only a legacy shasum, a newer one
-- serving that shasum alongside a modern SRI). The comparison is per algorithm over the
-- set of digests offered for it, so it is symmetric and ignores algorithms present on
-- only one side; a weak digest agreeing therefore never suppresses a contradicting
-- strong one, and a strong digest agreeing makes the asymmetric weak one irrelevant.
contradicts :: IntegrityFingerprint -> IntegrityFingerprint -> Bool
contradicts a b =
    or (Map.intersectionWith (/=) (digestsByAlg a) (digestsByAlg b))
  where
    digestsByAlg :: IntegrityFingerprint -> Map HashAlg (Set Text)
    digestsByAlg (IntegrityFingerprint pairs) =
        Map.fromListWith Set.union [(alg, Set.singleton digest) | (alg, digest) <- pairs]
