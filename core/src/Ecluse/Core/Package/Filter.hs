-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

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

== Egress-scheme enforcement

The module also owns the other ecosystem-agnostic reduction a fetched packument needs
before serve: normalising every served artifact URL against the https-only egress policy
('Ecluse.Core.Security.Egress.resolveTarballUrl'), parameterised by the upstream base URL
the packument was served from. Like the filter above, it reasons over the domain model and
the agnostic egress policy alone (no wire format in sight), so it is the projection
post-step every ecosystem shares rather than copies: a divergent copy of an egress-policy
application is exactly the drift the policy's correct-by-construction design exists to
prevent, and the foreign-host artifact locations of PyPI and RubyGems make it matter there
even more than for npm. An https artifact URL is kept, a same-host @http@ URL is upgraded to
https, and a version whose artifact is @http@ on a foreign host (or any non-http(s) URL) is
dropped from the served set and recorded as an 'Ecluse.Core.Package.InvalidVersionManifest'.
-}
module Ecluse.Core.Package.Filter (
    -- * Rule-filter plan
    FilterPlan (..),
    filterPlanFromDecisions,
    restrictToSurvivors,

    -- * Egress-scheme enforcement
    enforceArtifactScheme,
    enforceArtifactSchemeDetails,
) where

import Data.Aeson (Value (String))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T

import Ecluse.Core.Package (
    Artifact (artUrl),
    InvalidEntry (..),
    InvalidEntryKind (InvalidVersionManifest),
    PackageDetails (pkgArtifacts),
    PackageInfo (infoDistTags, infoInvalidEntries, infoVersions),
    pkgVersion,
 )
import Ecluse.Core.Rules.Types (Decision (Admitted))
import Ecluse.Core.Security (hostAddress)
import Ecluse.Core.Security.Egress (registryUrlText, resolveTarballUrl)
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

{- | Build a 'FilterPlan' from per-version 'Decision's already taken. This is the
path the __effectful__ tier feeds: it decides each version in IO (see
"Ecluse.Core.Rules"), then hands the decisions here for the pure
survivor\/@latest@ resolution. @latest@ is resolved by
'Ecluse.Core.Version.selectLatest' from the upstream-tagged @latest@ (looked up
among the versions, so a tag aimed at an absent version contributes nothing) and
the surviving versions -- kept while it survives, else repointed downward to the
highest stable survivor. The decision map is keyed by raw version string and
__must__ cover exactly the packument's versions; a version with no decision is
treated as not surviving.

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

{- | Normalise every served version's artifact URL scheme against the https-only egress
policy ('Ecluse.Core.Security.Egress.resolveTarballUrl'), given the @upstreamBaseUrl@ the
packument was served from. An https artifact URL is kept, a same-host @http@ URL is
__upgraded__ to https, and a version whose artifact is @http@ on a foreign host (or any
non-http(s) URL) is __dropped__ from the served set and recorded as an
'Ecluse.Core.Package.InvalidVersionManifest' carrying the offending URL (the #486
drop-and-record contract), so the version is never dialled in plaintext and the drop is
observable.

The enforcement applies only when the upstream is __https__ (in production every configured
upstream is https by construction). A non-https upstream is the test\/dev loopback opt-in,
whose artifact URLs are left untouched. Applied as a projection post-step at the fetch
boundary, where the upstream URL is known, so each ecosystem's projection stays context-free
and shares this one fold rather than copying it.
-}
enforceArtifactScheme :: Text -> PackageInfo -> PackageInfo
enforceArtifactScheme upstreamBaseUrl info =
    case httpsUpstreamHost upstreamBaseUrl of
        Nothing -> info
        Just upstreamHost ->
            let (kept, drops) = Map.foldrWithKey (step upstreamHost) (Map.empty, []) (infoVersions info)
             in info{infoVersions = kept, infoInvalidEntries = infoInvalidEntries info <> drops}
  where
    step upstreamHost rawVersion details (keptAcc, dropAcc) =
        case resolveDetails upstreamHost details of
            Right ok -> (Map.insert rawVersion ok keptAcc, dropAcc)
            Left (reason, badUrl) ->
                (keptAcc, InvalidEntry InvalidVersionManifest rawVersion (String badUrl) reason : dropAcc)

{- | The single-version form of 'enforceArtifactScheme' for the selective decode path:
'Nothing' drops the version (its artifact URL is non-https and not upgradeable), a 'Just'
carries the version with each artifact's URL normalised to https. A non-https (test\/dev
loopback) upstream leaves the version untouched.
-}
enforceArtifactSchemeDetails :: Text -> PackageDetails -> Maybe PackageDetails
enforceArtifactSchemeDetails upstreamBaseUrl details =
    case httpsUpstreamHost upstreamBaseUrl of
        Nothing -> Just details
        Just upstreamHost -> rightToMaybe (resolveDetails upstreamHost details)

-- The bare host of an @https@ upstream base URL, or 'Nothing' for a non-https (test/dev
-- loopback) upstream whose artifact URLs the scheme enforcement leaves untouched.
httpsUpstreamHost :: Text -> Maybe Text
httpsUpstreamHost baseUrl
    | "https://" `T.isPrefixOf` T.toLower baseUrl = Just (hostAddress baseUrl)
    | otherwise = Nothing

-- Resolve every artifact of a version against the egress policy: 'Right' the version
-- with each @artUrl@ normalised to https, or 'Left' the drop reason and the first
-- offending URL.
resolveDetails :: Text -> PackageDetails -> Either (Text, Text) PackageDetails
resolveDetails upstreamHost details =
    (\arts -> details{pkgArtifacts = arts}) <$> traverse (resolveArtifact upstreamHost) (pkgArtifacts details)

-- Normalise one artifact's URL: keep https, upgrade a same-host http, drop otherwise.
resolveArtifact :: Text -> Artifact -> Either (Text, Text) Artifact
resolveArtifact upstreamHost art =
    case resolveTarballUrl upstreamHost (artUrl art) of
        Right resolved -> Right art{artUrl = registryUrlText resolved}
        Left reason -> Left (reason, artUrl art)
