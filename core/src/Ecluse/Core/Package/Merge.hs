-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Merging several upstream packuments into the one document Écluse serves.

A packument is the /set of available versions/ of a package, and that set is spread
across upstreams. A trusted private upstream holds the vetted set, while a gated public
upstream holds the full history, including versions not yet mirrored.
Serving only the private document would hide those, so Écluse serves their __union__
rather than short-circuiting on a private hit. This module is the pure,
ecosystem-agnostic fold that reasons over that union on the
'Ecluse.Core.Package.PackageInfo' domain model. It lives above the registry handle,
written once and reused by every ecosystem, and it never imports a registry adapter.

__Decision surface, not served surface.__ This module reasons over the /typed/
'PackageInfo' but does __not__ emit a finished, re-serialisable 'PackageInfo'. The
document Écluse serves is the raw upstream document, rebuilt from the winning sources
so that every unmodeled wire key survives. The typed model is lossy, so re-encoding it
would drop those keys. The serve layer holds that raw document opaquely (as a
'Ecluse.Core.Registry.CachedDocument.CachedDoc') and never reads it, because the
rebuild runs through an injected adapter capability. This module therefore emits a
'MergePlan': exactly which versions survive, which input each survivor came from, the
reconciled @dist-tags@\/@time@, and the detected divergences. The serve layer
__replays that plan onto the raw documents__ through the same capability. See
@docs\/architecture\/registry-model.md@ → "Decision surface vs served surface".

The trust split is the __caller's__. It rides on each input as a 'Provenance' tag and
applies /before/ the merge. 'TrustedSource' (private) versions enter as-is.
'GatedSource' (public) versions are the already-rule-filtered set. This module does not
run rules: it reasons over exactly what it is handed (see
@docs\/architecture\/rules-engine.md@ → "Applying verdicts to a packument").

Two things make the merge more than a map union, and both are
__supply-chain signals, not silent reconciliations__:

* __Collision__. When the same version key comes from both a 'TrustedSource' and
  a 'GatedSource', the trusted copy wins, because it is the authority. The plan
  records it as the survivor's winning 'SourceId'.
* __Divergence__. The colliding copies __contradict on a shared integrity algorithm__
  when an algorithm both expose carries /disagreeing/ digests. That is exactly the
  tampering Écluse exists to catch. Copies may expose /different/ algorithm sets without
  contradicting on a shared one, as when one mirror also carries a legacy digest the
  other omits. Those copies describe the same bytes and are __not__ a divergence.
  The trusted copy still wins the merge, and the 'MergePlan' __reports__ a real
  contradiction. Whether to drop the version as well (fail-closed) is a policy decision
  left to the caller, so this module stays pure.

__The merge is a lawful 'Monoid'.__ The fold runs over a 'Merge' accumulator with a
lawful 'Semigroup' \/ 'Monoid'. 'mempty' is the empty merge, the degenerate identity at
zero inputs, and @(<>)@ is the trusted-wins union with order-independent divergence
detection. 'mergePackuments' assigns each input a 'SourceId' by list position,
@foldMap@s the contributions into the accumulator, and projects to a 'MergePlan'. See
the 'Semigroup' instance for the exact law domain (associative and identity,
intentionally __not__ commutative).

See @docs\/architecture\/registry-model.md@ → "Packument merge across upstreams".
-}
module Ecluse.Core.Package.Merge (
    -- * Provenance
    Provenance (..),

    -- * Merging
    SourceId,
    MergePlan (..),
    Divergence (..),
    IntegrityFingerprint,
    integrityHashes,
    mergePackuments,

    -- * Divergence policy (a post-plan projection)
    DivergencePolicy (..),
    parseDivergencePolicy,
    applyDivergencePolicy,

    -- * The merge accumulator
    -- $accumulator
    Merge,
    contribute,
    planFrom,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (UTCTime)

import Ecluse.Core.Package (
    Artifact (..),
    Hash,
    HashAlg (SRI),
    PackageDetails (..),
    PackageInfo (..),
    PackageName,
    hashAlg,
    hashValue,
    sriBody,
 )
import Ecluse.Core.Package.Integrity (assertedAlg)
import Ecluse.Core.Version (Version, selectLatest, unVersion)

{- | The trust provenance of an upstream's contribution to the merge. The caller
decides the split, by /which/ upstream a document came from, and applies it before
merging. Nothing here derives it.

The constructors are named @\*Source@ rather than the bare @Trusted@\/@Gated@
because "Ecluse.Core.Package" already exports a 'Ecluse.Core.Package.Trust' constructor
named @Trusted@. A bare name would collide for the many callers that import
"Ecluse.Core.Package" openly.

The 'Ord' instance is the trust order itself: 'TrustedSource' compares __less than__
'GatedSource', so "smallest wins" gives trusted precedence. The merge's resolution
leans on this directly (see 'mergePackuments').
-}
data Provenance
    = {- | A private-upstream document. Its versions are already vetted, so they
      enter the union unfiltered and win any collision.
      -}
      TrustedSource
    | {- | A public-upstream document. Its versions are the set that already
      survived the rules engine. The merge unions them but never re-filters.
      -}
      GatedSource
    deriving stock (Eq, Ord, Show)

{- | A stable identifier for one input to a single 'mergePackuments' call: the
__0-based index of that @(Provenance, PackageInfo)@ in the input list__.

The serve layer takes a surviving version's object from the /raw/ @Value@ of whichever
source won it. The plan must therefore name that source. 'Provenance' alone is /not/
enough. It identifies a source only while there is exactly one input per provenance.
That is the npm topology today: one trusted and one gated. The input index stays
unambiguous even when several inputs share a provenance, for example an aggregating
private upstream plus a first-party source, both 'TrustedSource'. That keeps the plan
correct for the multi-source case without a new type. The caller pairs each 'SourceId'
back to the raw @Value@ it passed at that position.
-}
type SourceId = Int

{- | A detected integrity conflict: a version key present in more than one source whose
copies __contradict on a shared algorithm__. An algorithm both copies expose carries
disagreeing digests. The trusted copy wins the merge, and this record preserves both
fingerprints so the caller can log, meter, and decide policy (serve-with-private-winning
against fail-closed). It is the merge's supply-chain signal: surfaced, never silently
reconciled.

'Ord' is derived purely to let 'MergePlan' carry divergences as a 'Set'. The ordering is
structural, over the version key and the two fingerprints, and has no meaning beyond
deduplication and a stable presentation.
-}
data Divergence = Divergence
    { divVersion :: Text
    {- ^ The raw version-string key the conflict was found at (the
    'Ecluse.Core.Package.infoVersions' key).
    -}
    , divWinning :: IntegrityFingerprint
    -- ^ Integrity of the copy that won the merge (the higher-precedence source).
    , divLosing :: IntegrityFingerprint
    -- ^ Integrity of the copy that lost, kept so the conflict is auditable.
    }
    deriving stock (Eq, Ord, Show)

{- | The outcome of reasoning over a set of upstream packuments. It is a __plan__ the
serve layer replays onto the raw upstream @Value@s to assemble the lossless
served body. It carries exactly the decisions the merge owns, never a finished,
re-serialisable document (see this module's header, "Decision surface, not served
surface").
-}
data MergePlan = MergePlan
    { mpName :: PackageName
    {- ^ The package identity, carried from the contributions. A check upstream of the
    merge validates every contribution's self-reported name against the requested one
    and drops a disagreeing origin. All inputs therefore carry the same identity, and it
    is never a substituted or manufactured value, only one an upstream genuinely
    reported.
    -}
    , mpSurvivors :: Map Text SourceId
    {- ^ Each surviving version key mapped to the 'SourceId' of the input that won
    it. The serve layer then takes that version's object from the right source's
    raw @Value@. Trusted wins a collision. An absent version is not a key here.
    -}
    , mpDistTags :: Map Text Version
    {- ^ @dist-tags@ reconciled over the surviving union: the shared selector resolves
    @latest@, the plan carries every other surviving-target tag, and it drops an
    absent-target tag.
    -}
    , mpTime :: Map Text UTCTime
    {- ^ The served @time@ map, __reconstructed from the survivors__. Each surviving
    version's publish instant comes from the /same/ winning candidate whose manifest is
    served. A version's served time therefore always comes from the source that won its
    manifest, and it is never fabricated from a different source. A winner with no known publish
    time contributes no entry, so this map is keyed by a subset of the survivors.
    -}
    , mpDivergences :: Set Divergence
    {- ^ Every distinct same-version integrity conflict found. It is a 'Set' because
    divergence is a property of the /set/ of distinct integrity fingerprints contributed
    for a version key. No pairwise fold step decides it. The projection records the
    winner's fingerprint against /each distinct fingerprint that contradicts it on a
    shared algorithm/, which is order-independent and deduplicating by construction.
    Empty when no two copies of a shared version contradict on a shared algorithm. That
    includes copies that merely expose different algorithm sets without disagreeing on
    one they share.
    -}
    }
    deriving stock (Eq, Show)

{- | The operator's policy for a served version a cross-upstream integrity 'Divergence'
was detected on. The signal itself fires under __both__ policies: the @WARNING@ log line
and the @ecluse.registry.merge.divergence@ counter. The policy decides only whether the
contested version is also withheld from the served listing.

The merge never consults this. 'mergePackuments' always records the divergence and lets
the trusted copy win, and 'applyDivergencePolicy' is a separate projection the serve
layer runs over the finished plan. That keeps the merge a pure, policy-free function and
leaves the availability-against-strictness trade with the operator
(@ECLUSE_INTEGRITY__DIVERGENCE_POLICY@).
-}
data DivergencePolicy
    = {- | Serve the trusted (winning) copy and rely on the divergence signal alone (the
      default). The contested version stays in the listing.
      -}
      Warn
    | {- | Withhold every version a divergence was detected on from the served listing: it
      is dropped from the survivors, its @time@ entry removed, and any @dist-tag@
      (including @latest@) that pointed at it dropped. A resolver pinned to that exact
      version then fails to resolve it rather than receive a contested copy.
      -}
      FailClosed
    deriving stock (Eq, Ord, Show)

{- | Parse an operator-supplied divergence policy (the @ECLUSE_INTEGRITY__DIVERGENCE_POLICY@ value):
@warn@ or @fail-closed@, case-insensitively and tolerant of @fail_closed@\/@failclosed@
spellings. Any other value is a 'Left' naming the offending input.
-}
parseDivergencePolicy :: Text -> Either Text DivergencePolicy
parseDivergencePolicy raw =
    case T.toLower (T.strip raw) of
        "warn" -> Right Warn
        "fail-closed" -> Right FailClosed
        "fail_closed" -> Right FailClosed
        "failclosed" -> Right FailClosed
        other -> Left ("unknown divergence policy '" <> other <> "' (expected 'warn' or 'fail-closed')")

{- | Apply a 'DivergencePolicy' to a finished 'MergePlan': a projection the serve layer
runs __after__ it logs and meters the plan's divergences. 'Warn' is the identity.
'FailClosed' drops every version key a 'Divergence' was detected on from the served
surface, coherently. It removes the key from 'mpSurvivors' and 'mpTime'. It also removes
any 'mpDistTags' target (including @latest@) that resolved to a dropped version. The
projected plan therefore still assembles a self-consistent document. It leaves 'mpDivergences'
intact, because that is the audit record the caller's log and metric have already
actioned. Dropping every surviving version leaves an empty 'mpSurvivors', and the caller
takes the same no-survivors terminal 'mergePackuments' already guards for.
-}
applyDivergencePolicy :: DivergencePolicy -> MergePlan -> MergePlan
applyDivergencePolicy Warn plan = plan
applyDivergencePolicy FailClosed plan =
    plan
        { mpSurvivors = Map.withoutKeys (mpSurvivors plan) dropped
        , mpTime = Map.withoutKeys (mpTime plan) dropped
        , mpDistTags = Map.filter (\target -> not (unVersion target `Set.member` dropped)) (mpDistTags plan)
        }
  where
    dropped = Set.map divVersion (mpDivergences plan)

{- | An order-independent fingerprint of a version's artifact integrity. It is the sorted
multiset of @(artifact filename, resolved algorithm, comparable digest body)@ triples
across all of the version's artifacts. Each digest keys first by the __artifact__ it
fingerprints, its filename, which is the stable identity a registry serves a file under.
It keys next by the algorithm it /asserts/, __not__ by its raw 'HashAlg' wrapper tag.
'Ecluse.Core.Package.Integrity.assertedAlg' reads that algorithm: a hex
'Ecluse.Core.Package.Hash''s tag, or the algorithm an SRI string embeds. So an
@sha256-…@ SRI and a hex SHA-256 digest bucket together under 'SHA256', while an
@sha256-…@ and an @sha512-…@ SRI bucket apart. A digest that asserts no algorithm (a bare
or malformed SRI) keys under 'Nothing', its own bucket. An unknown digest therefore never
merges with a real algorithm: the fail-closed reading. The /body/ is the comparable digest, an
SRI's base64 body (without its @\<alg\>-@ prefix) or a hex digest's raw value. That form
is uniform within any shared resolved algorithm, so comparing bodies is sound. The
comparison ignores artifact ordering and the non-identity fields (URL, size) that
legitimately vary between mirrors of the same bytes.

Two copies /diverge/ when they __contradict on a shared artifact under a shared resolved
algorithm__. A file both serve, under an algorithm both assert for it, carries
disagreeing bodies. An /asymmetric/ pair does __not__ diverge on that account. One copy
may assert an algorithm the other omits, including a mirror that recomputed integrity
under a /different/ algorithm. One copy may serve a file the other does not carry. A
multi-artifact ecosystem's private mirror may hold fewer wheels than the public index,
or a file may be renamed between mirrors. Only a shared file's shared algorithm
disagreeing is the tampering signal. A differing file /set/ describes availability, not
substituted bytes. A mirror may serve a modern digest alongside a legacy one. It agrees
with a mirror serving only the modern digest, as long as the shared digest matches. A
mirror carrying a subset of a version's files agrees with the full index on exactly the
files it carries.

Opaque, so nothing can sidestep the comparison divergence detection uses. Read the
triples back with 'integrityHashes' when logging or metering a 'Divergence'. 'Ord' is
derived structurally, over the sorted triples, only so a 'Divergence' may live in a
'Set'. It carries no domain meaning beyond that. In particular it is __not__ the
divergence test, which is the shared-key contradiction above, never structural
inequality of the whole set.
-}
newtype IntegrityFingerprint = IntegrityFingerprint [(Text, Maybe HashAlg, Text)]
    deriving stock (Eq, Ord, Show)

{- | The @(artifact filename, resolved algorithm, comparable digest body)@ triples of a
fingerprint, sorted, for an audit trail. The algorithm is the one each digest /asserts/,
or 'Nothing' when it asserts none. The body is its comparable form: an SRI's base64 body,
or a hex digest's raw value.
-}
integrityHashes :: IntegrityFingerprint -> [(Text, Maybe HashAlg, Text)]
integrityHashes (IntegrityFingerprint hs) = hs

{- | The trust-then-position rank of a contribution, the strict total order the whole
merge resolves by. 'TrustedSource' outranks 'GatedSource'. A tie, possible only between
two inputs of the /same/ provenance, goes to the __lower 'SourceId'__, the earlier input
position. 'SourceId' is unique per input, so this is a strict total order and the
smallest-ranked contribution is a deterministic, order-independent winner. With one
trusted and one gated source, the topology today, the trusted rank always wins outright
and the positional tiebreak never fires.

The order is over a @(Provenance, SourceId)@ pair, exploiting that 'TrustedSource' '<'
'GatedSource' and that a smaller 'SourceId' is earlier. Then @minimumBy (comparing rank)@
picks the winner.
-}
rank :: Provenance -> SourceId -> (Provenance, SourceId)
rank prov sid = (prov, sid)

{- | One source's contribution to a single version key. It names the input that offered
the key, by 'Provenance' and 'SourceId', together with the integrity fingerprint and the
typed details it carried. Equality and order are structural over all four fields. A 'Set'
of these therefore deduplicates identical contributions and keeps distinct ones, for
example two sources at the same key but differing integrity.
-}
data Candidate = Candidate
    { candProvenance :: Provenance
    , candSourceId :: SourceId
    , candFingerprint :: ~IntegrityFingerprint
    {- ^ __Deliberately lazy__: the @~@ opts out of StrictData. The merge consults the
    fingerprint only when a version key genuinely collides across sources. The common
    cold-path merge collides on a handful of keys out of thousands. Computing every
    version's fingerprint eagerly is therefore almost entirely wasted work. Candidate
    ordering compares rank before fingerprint, and ranks are unique per input, so 'Set'
    operations never force it. Only the divergence derivation over a multi-candidate key
    does.
    -}
    , candDetails :: PackageDetails
    }
    deriving stock (Show)

-- The identity of a candidate, for 'Eq' and 'Ord' alike: its resolution rank
-- ('TrustedSource' first, then lower 'SourceId') and its integrity fingerprint.
-- 'candDetails' is deliberately /not/ part of the identity, because two contributions
-- that agree on rank and integrity are the same candidate for the merge. 'SourceId' is
-- unique per call, so this never collapses two real inputs. Keeping 'Eq' and 'Ord' on
-- the same fields satisfies their consistency law, so a 'Set' membership test agrees
-- with structural equality.
candKey :: Candidate -> ((Provenance, SourceId), IntegrityFingerprint)
candKey c = (rank (candProvenance c) (candSourceId c), candFingerprint c)

instance Eq Candidate where
    a == b = candKey a == candKey b

instance Ord Candidate where
    compare a b = compare (candKey a) (candKey b)

{- | One source's tagged @dist-tags@ target for a single tag, paired with the rank of
the source that offered it. The projection can then pick the precedence-winning target
the same way it resolves version collisions. Ordered by rank alone, with the winner the
minimum, so a left-biased @Map.unions@ of singletons resolves the collision by
provenance, not position. Per-version time needs no parallel ranked axis: it rides
inside each version 'Candidate', read off the /same/ winner the manifest comes from.
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
The merge is a fold into a lawful 'Monoid'. 'contribute' turns one
@(Provenance, PackageInfo)@ input into a 'Merge'. @(<>)@ combines two merges: a
trusted-wins union that keeps order-independent divergence unresolved until the
projection. 'mempty' is the empty merge, the degenerate identity. 'planFrom' projects a
folded 'Merge' to a 'MergePlan'. 'mergePackuments' is exactly
@'planFrom' . 'foldMap' ('uncurry' 'contribute')@. The 'Merge' type is opaque, built
only through 'contribute' and 'mempty', so a 'SourceId' always names a real input
position. See the 'Semigroup' instance for the law domain: associative and identity,
intentionally __not__ commutative, and why.
-}

{- | The monoidal accumulator the merge folds into. It holds, /unresolved/, every
candidate offered for every version key, plus the ranked @dist-tags@ contributions.
Resolution to a single winner per key, and the divergence set, happens once in
'planFrom'. The served @time@ map needs no axis here. Each version's publish instant
rides inside its 'Candidate' (on 'candDetails'), so 'planFrom' reads it off the same
winner the manifest comes from. Keeping candidates unresolved is what makes @(<>)@
associative. A pairwise winner-against-loser decision taken /during/ the fold is not
associative once three or more copies of a key collide. Divergence is a property of the
whole /set/ of distinct fingerprints, not of any one step.

Each accumulator also carries the count of inputs it represents, so that @(<>)@ can
__re-index the right operand's 'SourceId's by the left operand's input count__. This
positional re-indexing is what makes a 'SourceId' name an input's list position after a
@foldMap@ of single-input contributions. It is also the sole reason the instance is
non-commutative (see the 'Semigroup' instance).
-}
data Merge = Merge
    { mergeCount :: Int
    -- ^ How many inputs this accumulator represents (the next free 'SourceId').
    , mergeVersions :: Map Text (Set Candidate)
    -- ^ Every candidate offered for each version key, unresolved.
    , mergeDistTags :: Map Text (Ranked Version)
    -- ^ The precedence-winning @dist-tags@ target offered for each tag.
    , mergeName :: Maybe PackageName
    {- ^ The package identity. Every contribution carries the same name, because a check
    upstream of the merge validates each one against the requested name. The left-biased
    @(<>)@ choice therefore selects that one shared identity, rather than arbitrating
    between possibly-divergent self-reports. 'Nothing' only for 'mempty', the empty
    merge, so the @(<|>)@ over 'Maybe' encodes purely the degenerate "no inputs yet"
    identity.
    -}
    }
    deriving stock (Eq, Show)

{- | The merge's 'Semigroup' has a deliberately narrow law domain, and the narrowing is
load-bearing, not an accident.

* Associative: @(a '<>' b) '<>' c@ '==' @a '<>' (b '<>' c)@. The 'SourceId' re-indexing
  offsets compose additively. Every per-key combiner is itself associative: set union
  for candidates, "keep the smaller rank" for tags, and "left name wins" for the
  identity.
* Identity: 'mempty', the empty merge, is both a left and a right unit.
* Intentionally not commutative: @a '<>' b@ '/=' @b '<>' a@ in general. The @(<>)@
  operator re-indexes the /right/ operand's 'SourceId's by the /left/ operand's input
  count. A 'SourceId' must name the input's __position__ in the caller's list, which is
  the index the serve layer pairs back to a raw @Value@. Swapping the operands swaps
  those positions, so the 'SourceId' /labels/ differ.

The order-independence guarantee, stated precisely, is also the reason commutativity is
the wrong law. Precedence resolves __by provenance__, so the surviving key set and the
winning /provenance/ per key are invariant under any permutation of the inputs. The
value-level reconciliations are invariant under any permutation that keeps each
collision __cross-provenance__. Those reconciliations are the survivor a key resolves to,
the divergence fingerprint-pairs, the @dist-tags@ targets, and the served @time@ read off
each survivor. The npm topology (exactly one trusted, one gated upstream) always keeps a
collision cross-provenance, so every observable decision is order-independent there.

The /sole/ residual order-dependence is the positional tiebreak between two inputs of
the __same__ provenance. Provenance cannot break that tie, so the lower 'SourceId', the
earlier input, wins it, and which copy is the divergence winner then tracks order. That
positional tiebreak is exactly why 'SourceId' exists and why the instance is
non-commutative.
-}
instance Semigroup Merge where
    a <> b =
        Merge
            { mergeCount = mergeCount a + mergeCount b
            , mergeVersions =
                Map.unionWith Set.union (mergeVersions a) (shiftVersions (mergeVersions b))
            , mergeDistTags =
                Map.unionWith keepBetter (mergeDistTags a) (shiftRanked <$> mergeDistTags b)
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
            , mergeName = Nothing
            }

{- | One input's contribution to the accumulator, at local 'SourceId' @0@. Every version
becomes a candidate, carrying its own publish time on 'candDetails'. Every @dist-tags@
target becomes a ranked value at this input's provenance. The package name becomes the
offered identity. A @foldMap contribute@ over the inputs then re-indexes each to its list
position via the 'Semigroup' offset. The absolute 'SourceId' of a single-input
contribution is therefore its index in the @foldMap@.
-}
contribute :: Provenance -> PackageInfo -> Merge
contribute prov info =
    Merge
        { mergeCount = 1
        , mergeVersions = Map.map candidateFor (infoVersions info)
        , mergeDistTags = Map.map (Ranked here) (infoDistTags info)
        , mergeName = Just (infoName info)
        }
  where
    -- Local SourceId 0. The Semigroup offset re-indexes it to the input position.
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

The merge is a fold with the __degenerate identity at one input__. A single packument
yields a plan whose survivors are all of its versions, all won by source @0@. The plan
reconciles its tags and times, and it records no divergences. A 0-upstream or 1-upstream
deployment therefore needs no special case. The fold is a 'foldMap' of each input's
'contribute' into the lawful 'Merge' 'Monoid', projected by 'planFrom'. The model:

* The merge unions the version keys, and __'TrustedSource' wins__ a collision over
  'GatedSource', because the private upstream is the authority. The plan records the
  winning input's 'SourceId' for the survivor. A collision whose copies contradict on a
  shared integrity algorithm becomes a 'Divergence', and the winner is still kept.
* The merge reconciles @dist-tags@ over the union.
  'Ecluse.Core.Version.selectLatest' resolves @latest@ (keep-unless-denied,
  stable-preferring, and unparseable-safe) from the precedence-winning source's tagged
  @latest@ and the surviving versions. It drops any other tag pointing at a version
  absent from the union. Collisions on the same tag resolve __by provenance__ (trusted
  wins), consistent with the version fold. The plan therefore does not depend on caller
  input order.
* The merge reconstructs @time@ from the survivors. Each survivor's publish instant
  comes off the /same/ winning candidate whose manifest is served. A version's served
  time therefore always comes from the source that won its manifest, and it is never
  fabricated from a different source. A winner with no known publish time contributes
  no entry.

The plan carries its identity ('mpName') from the contributions. A caller fetches one
package across its upstreams, and a check upstream of the merge validates each
contribution's name against the requested one. All inputs therefore share that one
identity, and it is never a substituted value. An empty input list yields 'Nothing':
there is nothing to serve.
-}
mergePackuments :: [(Provenance, PackageInfo)] -> Maybe MergePlan
mergePackuments [] = Nothing
mergePackuments inputs = planFrom (foldMap (uncurry contribute) inputs)

{- | Project the resolved 'MergePlan' from a folded 'Merge'. It resolves each version key
to its precedence winner. It derives the divergence 'Set' from the shared-algorithm
contradictions among each key's distinct fingerprints. It reconciles @dist-tags@ over the
survivors, and reconstructs the served @time@ map from each survivor's winning
candidate. Returns 'Nothing' only for the empty merge ('mempty'), which has no name and
so nothing to serve. That is equivalently the empty input list.
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
    -- was offered, never of a pairwise fold step. That is what keeps it
    -- order-independent and associative for 3+ copies of a key. For each version key,
    -- record the winner's fingerprint against each distinct fingerprint that
    -- 'contradicts' it on a shared algorithm. A fingerprint that only adds or omits an
    -- algorithm relative to the winner, without disagreeing on one they share, is not a
    -- divergence. The winner never contradicts itself, so it drops out too. With
    -- the two-source topology this is a single winner-against-loser pair. With three or
    -- more it is the full fan-out, deduplicated by the 'Set'.
    divergences :: Set Divergence
    divergences =
        Set.fromList
            [ Divergence{divVersion = key, divWinning = win, divLosing = lose}
            | (key, cs) <- Map.toList (mergeVersions acc)
            , -- A key offered by one source alone cannot diverge, because the winner
            -- never contradicts itself, so nothing computes its fingerprints. This is
            -- the guard that keeps the lazy 'candFingerprint' unforced across the
            -- overwhelmingly common collision-free merge.
            Set.size cs > 1
            , let win = candFingerprint (winnerOf cs)
            , let distinct = Set.fromList [candFingerprint c | c <- Set.toList cs]
            , lose <- Set.toList distinct
            , contradicts win lose
            ]

    -- @dist-tags@ reconciled over the union: it carries every surviving-target tag,
    -- and the shared selector resolves @latest@. The accumulator has already resolved
    -- same-tag collisions by provenance, so this never depends on the order the caller
    -- passed the inputs.
    reconciledTags :: Map Text Version
    reconciledTags =
        let carried = Map.filter (survives . unVersion) (Map.map rankedValue (mergeDistTags acc))
         in case resolvedLatest of
                Nothing -> Map.delete "latest" carried
                Just v -> Map.insert "latest" v carried

    -- @latest@ via the shared resolver: keep the precedence-winning source's
    -- tagged @latest@ if it survives, else repoint (stable-preferring,
    -- unparseable-safe) among survivors. The @chosen@ argument is the
    -- provenance-winning source's @latest@, consistent with the version and dist-tag
    -- folds.
    resolvedLatest :: Maybe Version
    resolvedLatest =
        selectLatest chosenLatest (map pkgVersion survivingDetails)

    chosenLatest :: Maybe Version
    chosenLatest = rankedValue <$> Map.lookup "latest" (mergeDistTags acc)

    -- @time@ reconstructed from the survivors. Each surviving version's publish instant
    -- comes from the /same/ winning candidate whose manifest is served. The manifest and
    -- its timestamp therefore always come from one authoritative source, and no
    -- timestamp is fabricated from a source other than the manifest's. A winner with no
    -- known publish time drops out, so this map is keyed by a subset of the survivors.
    reconciledTimes :: Map Text UTCTime
    reconciledTimes =
        Map.mapMaybe (pkgPublishedAt . candDetails . winnerOf) (mergeVersions acc)

-- The order-independent integrity fingerprint of a version. It gathers every artifact's
-- @(filename, resolved algorithm, comparable digest body)@ triples across all artifacts
-- and sorts them. The comparison is then stable regardless of artifact or hash ordering
-- on the wire. Each digest keys by the artifact it fingerprints (its filename) and the
-- algorithm it asserts ('assertedAlg'), not its raw wrapper tag. It compares by its body
-- ('comparableBody'). The divergence test therefore reasons over what each digest
-- actually claims about each file. It never reasons over the opaque SRI tag, or over the
-- version's file set as a whole.
fingerprint :: PackageDetails -> IntegrityFingerprint
fingerprint =
    IntegrityFingerprint
        . sort
        . concatMap artHashPairs
        . toList
        . pkgArtifacts
  where
    artHashPairs art = [(artFilename art, assertedAlg h, comparableBody h) | h <- artHashes art]

-- The comparable digest body for keying: an SRI's base64 body (the bytes its algorithm
-- digests, without the @\<alg\>-@ prefix) or a hex digest's raw value. Within a shared
-- resolved algorithm the encoding is uniform in practice: sha1 hex on both sides,
-- sha256\/sha512 SRI base64 on both sides. Comparing bodies is therefore correct.
comparableBody :: Hash -> Text
comparableBody h = case hashAlg h of
    SRI -> sriBody (hashValue h)
    _ -> hashValue h

-- Whether two fingerprints contradict: some /(artifact, resolved algorithm)/ carried
-- by /both/ has disagreeing digest bodies. This is the divergence test. The key pairs
-- the artifact's filename with the algorithm each digest asserts ('assertedAlg'). One
-- digest therefore compares only against another that fingerprints the same file under
-- the same claimed algorithm. 'Nothing', a digest asserting no algorithm, is its own
-- algorithm key. An unknown digest therefore only ever compares with another unknown
-- one, and never collapses onto a real algorithm. Only a shared file's shared resolved
-- algorithm with disagreeing bodies counts. An asymmetric pair does not contradict. One
-- copy may add or omit an algorithm the other side lacks, including a mirror that
-- recomputed integrity under a different algorithm. One copy may carry a file the other
-- lacks, as a multi-artifact ecosystem's mirror carries fewer files than the index.
-- Different digest sets can describe the same bytes, and a version's file set can
-- legitimately differ between mirrors without any byte being substituted. The comparison
-- runs per (file, algorithm) over the set of bodies offered for it. It is therefore
-- symmetric, and it ignores keys present on only one side. A weak digest agreeing never
-- suppresses a contradicting strong one, and a strong digest agreeing makes the
-- asymmetric weak one irrelevant.
contradicts :: IntegrityFingerprint -> IntegrityFingerprint -> Bool
contradicts a b =
    or (Map.intersectionWith (/=) (digestsByKey a) (digestsByKey b))
  where
    digestsByKey :: IntegrityFingerprint -> Map (Text, Maybe HashAlg) (Set Text)
    digestsByKey (IntegrityFingerprint triples) =
        Map.fromListWith Set.union [((file, alg), Set.singleton digest) | (file, alg, digest) <- triples]
