{- | The ecosystem-agnostic filtering /decision/ for a single public-upstream
packument: which versions survive a rule set, which version @dist-tags.latest@
resolves to, and the per-version decisions a no-survivors outcome must report.

This mirrors "Ecluse.Core.Package.Merge" -- the pure fold above the registry handle that
emits a __plan__ rather than a finished document. It reasons over the typed
'Ecluse.Core.Package.PackageInfo' domain model only; it never touches a registry's wire
format. The per-ecosystem adapter __replays__ this plan onto the raw upstream
document, so unmodeled wire keys survive (the typed model is lossy, so re-encoding
it would drop them). See @docs\/architecture\/registry-model.md@ → "Decision
surface vs served surface".

__Decision, not served surface.__ A 'FilterPlan' carries exactly the decisions the
filter owns:

* __Survivors.__ A version key survives iff the rules engine 'Admitted' it; every
  other verdict -- a denial, deny-by-default, or an undecidable outcome -- drops it.
  Presence in the served packument /is/ availability (see
  @docs\/research\/reverse-engineering\/npm.md@ §8), so a non-approved version is
  removed rather than flagged.

* __Resolved @latest@.__ The surviving @dist-tags.latest@ under the shared
  __keep-unless-denied, stable-preferring__ rule ('Ecluse.Core.Version.selectLatest'):
  the upstream @latest@ is kept untouched while it survives, and only repointed --
  to the highest /stable/ survivor -- when it was itself denied. This is the
  @latest@ /within the public set/, which the cross-upstream merge then re-resolves
  over the union; it is not the final served @latest@.

* __Decisions.__ Every version's 'Decision', in version-key order, so a
  no-survivors outcome can render each denial and choose a status.

What the plan deliberately omits is any "dropped tags" list: a stale tag -- one
whose target did not survive -- is droppable __structurally__ from the survivor set
alone (a tag is kept iff its target is in 'fpSurvivors'), so the replay needs no
extra field to find them. The plan stays minimal: the decisions the filter owns,
nothing the replay can recompute.

This filters a __single public packument__ (the gated set). Combining it with the
trusted /private/ set is the cross-upstream merge ("Ecluse.Core.Package.Merge").
-}
module Ecluse.Core.Package.Filter (
    FilterPlan (..),
    filterPlan,
    filterPlanFromDecisions,
    restrictToSurvivors,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Ecluse.Core.Package (PackageInfo (infoDistTags, infoVersions), pkgVersion)
import Ecluse.Core.Rules (RuleDeps, evalRules, prepare)
import Ecluse.Core.Rules.Types (Decision (Admitted), EvalContext, PrecededRule)
import Ecluse.Core.Version (Version, renderVersion, selectLatest, unVersion)

{- | The decisions filtering a single public packument owns, for the adapter to
replay onto the raw upstream @Value@. Carries only what the filter decides over the
typed model -- never a finished, re-serialisable document (see this module's
header). The replay derives everything else (which stale tags to drop, which
@time@ entries to prune) from these fields.
-}
data FilterPlan = FilterPlan
    { fpSurvivors :: Set Text
    {- ^ The surviving version keys (the raw 'Ecluse.Core.Package.infoVersions' keys):
    exactly those the rules engine approved. Empty when no version survived.
    -}
    , fpLatest :: Maybe Version
    {- ^ @dist-tags.latest@ resolved over the survivors by the shared selector --
    kept as published while it survives, else repointed (stable-preferring) to the
    highest survivor. 'Nothing' when nothing survives. When present it is always one
    of 'fpSurvivors', so the replay can point @latest@ at a key that is served.
    -}
    , fpDecisions :: [Decision]
    {- ^ Every version's 'Decision', in version-key order, for the no-survivors
    status and denial body. Carried for every version (not only the denied ones) so
    the adapter can zip them back onto the same-ordered versions.
    -}
    }
    deriving stock (Eq, Show)

{- | Decide a single public packument against a rule set: which versions survive,
where @latest@ resolves, and every version's decision. 'IO' and total -- it reasons
over the typed 'PackageInfo' alone, with no registry wire format in sight, deciding
each version through the one engine ('Ecluse.Core.Rules.evalRules', over the
'prepare'd policy), so the filter's per-version decision and the serve path share it.

A version survives iff the engine 'Admitted' it; every other verdict drops it.
@latest@ is resolved by 'Ecluse.Core.Version.selectLatest' from the upstream-tagged
@latest@ (looked up among the versions, so a tag aimed at an absent version
contributes nothing) and the surviving versions -- kept while it survives, else
repointed downward to the highest stable survivor. The decisions are returned for
__every__ version in key order, so the adapter has each denial's reason when
nothing survives.
-}
filterPlan :: RuleDeps -> EvalContext -> [PrecededRule] -> PackageInfo -> IO FilterPlan
filterPlan deps ctx rules info = do
    prepared <- prepare deps rules
    decisions <- traverse (evalRules ctx prepared) (infoVersions info)
    pure (filterPlanFromDecisions decisions info)

{- | Build a 'FilterPlan' from per-version 'Decision's already taken, rather than
evaluating the pure tier here. This is the path the __effectful__ tier feeds: it
decides each version in IO (see "Ecluse.Core.Rules.Effectful"), then hands the decisions
here for the same pure survivor\/@latest@ resolution 'filterPlan' performs. The
decision map is keyed by raw version string and __must__ cover exactly the
packument's versions; a version with no decision is treated as not surviving.

A version survives iff its decision is an 'Admitted'; every other
verdict -- denial, deny-by-default, or 'Ecluse.Core.Rules.Types.Undecidable' -- drops it,
so a fail-closed undecidable version is filtered out exactly like a denial, while its
decision is still carried in 'fpDecisions' for the no-survivors status.
-}
filterPlanFromDecisions :: Map Text Decision -> PackageInfo -> FilterPlan
filterPlanFromDecisions decisions info =
    FilterPlan
        { fpSurvivors = survivors
        , fpLatest = selectLatest chosen survivingVersions
        , fpDecisions = Map.elems decisions
        }
  where
    -- A version survives only on an explicit approval; every other outcome (deny,
    -- deny-by-default, undecidable) drops it.
    survivors :: Set Text
    survivors = Map.keysSet (Map.filter isApproved decisions)

    isApproved :: Decision -> Bool
    isApproved = \case
        Admitted{} -> True
        _ -> False

    -- The parsed 'Version' a raw key projects to, if present in the packument.
    -- Used both to map surviving keys to 'Version's and to resolve @latest@.
    versionOf :: Text -> Maybe Version
    versionOf raw = pkgVersion <$> Map.lookup raw (infoVersions info)

    -- 'selectLatest'\'s @chosen@: the upstream @latest@ tag's target as a 'Version'
    -- (the tag's raw string looked up among the versions). It decides /survival/
    -- itself, so the version need only be present, not surviving.
    chosen :: Maybe Version
    chosen = Map.lookup "latest" (infoDistTags info) >>= versionOf . unVersion

    -- 'selectLatest'\'s @survivors@: the surviving versions' parsed 'Version's.
    survivingVersions :: [Version]
    survivingVersions = mapMaybe versionOf (Set.toList survivors)

{- | Restrict a 'PackageInfo' to the version keys that survived filtering -- the
'FilterPlan'\'s own 'fpSurvivors' -- so the typed view handed to the cross-upstream
merge carries exactly the gated set ('Ecluse.Core.Package.Merge.mergePackuments'
treats a gated source as already filtered and never re-filters). @dist-tags@ is
pruned to the surviving keys likewise (the merge reconciles tags over the union);
@dist-tags@ targets absent from the survivors are dropped. Each surviving version
carries its own publish time, so restricting the versions carries the times with it
(the merge reconstructs the served @time@ from the survivors).
-}
restrictToSurvivors :: Set Text -> PackageInfo -> PackageInfo
restrictToSurvivors survivors info =
    info
        { infoVersions = Map.restrictKeys (infoVersions info) survivors
        , infoDistTags = Map.filter ((`Set.member` survivors) . renderVersion) (infoDistTags info)
        }
