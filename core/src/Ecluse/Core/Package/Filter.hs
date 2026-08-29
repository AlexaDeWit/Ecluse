-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The ecosystem-agnostic filtering /decision/ for a single public-upstream
packument. It says which versions survive a rule set, which version @dist-tags.latest@
resolves to, and the per-version decisions a no-survivors outcome must report.

This mirrors "Ecluse.Core.Package.Merge": the pure fold above the registry handle that
emits a __plan__ rather than a finished document. It reasons over the typed
'Ecluse.Core.Package.PackageInfo' domain model only, and never touches a registry's wire
format. The per-ecosystem adapter __replays__ this plan onto the raw upstream
document, so unmodeled wire keys survive. The typed model is lossy, so re-encoding it
would drop them. See @docs\/architecture\/registry-model.md@ → "Decision surface vs
served surface".

__Decision, not served surface.__ A 'FilterPlan' carries exactly the decisions the
filter owns:

* __Survivors.__ A version key survives iff the rules engine 'Admitted' it. Every
  other verdict drops it: a denial, deny-by-default, or an undecidable outcome.
  Presence in the served packument /is/ availability, so the filter removes a
  non-approved version rather than flagging it.

* __Resolved @latest@.__ The surviving @dist-tags.latest@ under the shared
  __keep-unless-denied, stable-preferring__ rule ('Ecluse.Core.Version.selectLatest').
  The upstream @latest@ stays untouched while it survives. The filter repoints it to
  the highest /stable/ survivor only when the tagged version was itself denied. This is
  the @latest@ /within the public set/, which the cross-upstream merge then re-resolves
  over the union. It is not the final served @latest@.

* __Decisions.__ Every version's 'Decision', in version-key order, so a
  no-survivors outcome can render each denial and choose a status.

The plan deliberately omits any "dropped tags" list. A stale tag is one whose target
did not survive. The survivor set alone drops it __structurally__: a tag survives iff
its target is in 'fpSurvivors'. The replay therefore needs no extra field to find
one. The plan stays minimal: the decisions the filter owns, nothing the replay
can recompute.

This filters a __single public packument__ (the gated set). Combining it with the
trusted /private/ set is the cross-upstream merge ("Ecluse.Core.Package.Merge").

== Egress-scheme enforcement

The module also owns the other ecosystem-agnostic reduction a fetched packument needs
before serve. It normalises every served artifact URL against the https-only egress policy
('Ecluse.Core.Security.Egress.resolveTarballUrl'). The upstream base URL that served the
packument parameterises that normalisation. Like the filter above, it reasons over the
domain model and the agnostic egress policy alone, with no wire format in sight. It is
therefore the projection post-step every ecosystem shares rather than copies. A divergent
copy of an egress-policy application is exactly the drift the policy's
correct-by-construction design exists to prevent. PyPI and RubyGems put artifacts on
foreign hosts, so that matters there even more than for npm. It keeps an https artifact
URL and upgrades a same-host @http@ URL to https. It drops a version whose artifact is
@http@ on a foreign host, or on any non-http(s) URL, from the served set. It records each
drop as an 'Ecluse.Core.Package.InvalidVersionManifest'.
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
import Ecluse.Core.Security (authorityLabel, hostAddress)
import Ecluse.Core.Security.Egress (registryUrlText, resolveTarballUrl)
import Ecluse.Core.Version (Version, renderVersion, selectLatest)

{- | The filtering decisions for one public packument, for the adapter to replay onto the raw
upstream @Value@. It carries only decisions, never a finished, re-serialisable document.
-}
data FilterPlan = FilterPlan
    { fpSurvivors :: Set Text
    {- ^ The surviving version keys (the raw 'Ecluse.Core.Package.infoVersions' keys):
    exactly those the rules engine approved. Empty when no version survived.
    -}
    , fpLatest :: Maybe Version
    {- ^ @dist-tags.latest@ resolved over the survivors: kept while it survives, else repointed
    (stable-preferring) to the highest survivor. Always one of 'fpSurvivors', or 'Nothing'.
    -}
    , fpDecisions :: [Decision]
    {- ^ Every version's 'Decision', admitted ones included, in version-key order so the adapter can
    zip them back onto the same-ordered versions. Feeds the no-survivors status and denial body.
    -}
    }
    deriving stock (Eq, Show)

{- | Build a 'FilterPlan' from per-version 'Decision's already taken. The decision map __must__
cover every version of the packument, because an undecided version does not survive.
A version survives iff its decision is 'Admitted'. Every other verdict drops it, fail-closed.
-}
filterPlanFromDecisions :: Map Text Decision -> PackageInfo -> FilterPlan
filterPlanFromDecisions decisions info =
    FilterPlan
        { fpSurvivors = survivors
        , fpLatest = selectLatest chosen survivingVersions
        , fpDecisions = Map.elems decisions
        }
  where
    -- A version survives only on an explicit approval. Every other outcome drops
    -- it: deny, deny-by-default, undecidable.
    survivors :: Set Text
    survivors = Map.keysSet (Map.filter isApproved decisions)

    isApproved :: Decision -> Bool
    isApproved = \case
        Admitted{} -> True
        _ -> False

    -- The parsed 'Version' a raw key projects to, if present in the packument. It
    -- both maps surviving keys to 'Version's and resolves @latest@.
    versionOf :: Text -> Maybe Version
    versionOf raw = pkgVersion <$> Map.lookup raw (infoVersions info)

    -- The upstream @latest@ tag's target. 'selectLatest' decides survival itself, so this version
    -- need only be present, not surviving.
    chosen :: Maybe Version
    chosen = Map.lookup "latest" (infoDistTags info) >>= versionOf . renderVersion

    -- 'selectLatest'\'s @survivors@: the surviving versions' parsed 'Version's.
    survivingVersions :: [Version]
    survivingVersions = mapMaybe versionOf (Set.toList survivors)

{- | Restrict a 'PackageInfo' to the surviving version keys, pruning @dist-tags@ to targets
that survive. 'Ecluse.Core.Package.Merge.mergePackuments' treats the result as already gated.
-}
restrictToSurvivors :: Set Text -> PackageInfo -> PackageInfo
restrictToSurvivors survivors info =
    info
        { infoVersions = Map.restrictKeys (infoVersions info) survivors
        , infoDistTags = Map.filter ((`Set.member` survivors) . renderVersion) (infoDistTags info)
        }

{- | Normalise each served artifact URL against the https-only egress policy
('Ecluse.Core.Security.Egress.resolveTarballUrl'): upgrade a same-host @http@ URL, drop and record
every other non-https version. Only an https upstream triggers this, sparing a test\/dev loopback.
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
                -- The offending URL is upstream-supplied and reaches a log line, so the record
                -- keeps only its authority ('authorityLabel').
                (keptAcc, InvalidEntry InvalidVersionManifest rawVersion (String (authorityLabel badUrl)) reason : dropAcc)

{- | The single-version form of 'enforceArtifactScheme', for the selective decode path.
'Nothing' means the artifact URL is non-https and not upgradeable, so the version drops.
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

-- Resolve every artifact of a version against the egress policy. 'Right' is the version
-- with each @artUrl@ normalised to https. 'Left' is the drop reason and the first
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
