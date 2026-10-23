-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Ecosystem-independent listing decisions and artifact-location admission.
Adapters replay the surviving versions and files onto their raw documents.
"Ecluse.Core.Package.Filter.Internal" holds the location check and its drop records.
-}
module Ecluse.Core.Package.Filter (
    -- * Rule-filter plan
    FilterPlan (..),
    filterPlanFromDecisions,
    restrictToSurvivors,

    -- * Served-location enforcement
    enforceArtifactLocations,
    enforceArtifactLocationsOf,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Ecluse.Core.Package (
    Artifact,
    InvalidEntry,
    PackageDetails (pkgArtifacts),
    PackageInfo (infoDistTags, infoInvalidEntries, infoVersions),
    pkgVersion,
 )
import Ecluse.Core.Package.Filter.Internal (ArtifactOrigin, ArtifactRefusal, AuthorityVerdict, artifactOrigin, noAuthorityVerdict, partitionArtifacts, resolveArtifact)
import Ecluse.Core.Rules.Types (Decision (Admitted))
import Ecluse.Core.Security (AllowedHostPorts)
import Ecluse.Core.Strict (strictElements)
import Ecluse.Core.Version (renderVersion)

{- | The filtering decisions for one public packument, for the adapter to replay onto the raw
upstream @Value@. It carries only decisions, never a finished, re-serialisable document.
-}
data FilterPlan = FilterPlan
    { fpSurvivors :: Set Text
    {- ^ The surviving version keys (the raw 'Ecluse.Core.Package.infoVersions' keys):
    exactly those the rules engine approved. Empty when no version survived.
    -}
    , fpDecisions :: [Decision]
    {- ^ Every version's 'Decision', admitted ones included, in version-key order so the adapter can
    zip them back onto the same-ordered versions. Feeds the no-survivors status and denial body.
    -}
    }
    deriving stock (Eq, Show)

{- | Build a 'FilterPlan' from per-version 'Decision's already taken. A version survives iff its
decision is 'Admitted', so an undecided one drops fail-closed.
-}
filterPlanFromDecisions :: Map Text Decision -> FilterPlan
filterPlanFromDecisions decisions =
    FilterPlan
        { fpSurvivors = Map.keysSet (Map.filter isApproved decisions)
        , fpDecisions = Map.elems decisions
        }

-- A version survives only on an explicit approval. Deny, deny-by-default and undecidable drop.
isApproved :: Decision -> Bool
isApproved = \case
    Admitted{} -> True
    _ -> False

{- | Restrict a 'PackageInfo' to the surviving version keys, pruning @dist-tags@ to targets
that survive. 'Ecluse.Core.Package.Merge.mergePackuments' treats the result as already gated.
-}
restrictToSurvivors :: Set Text -> PackageInfo -> PackageInfo
restrictToSurvivors survivors info =
    info
        { infoVersions = Map.restrictKeys (infoVersions info) survivors
        , infoDistTags = Map.filter ((`Set.member` survivors) . renderVersion) (infoDistTags info)
        }

{- | Drop artifacts with refused filenames, schemes, or authorities, and versions left with none.
Retain drop records for operator reporting.
-}
enforceArtifactLocations :: AllowedHostPorts -> Text -> PackageInfo -> PackageInfo
enforceArtifactLocations ecosystemHosts upstreamBaseUrl info =
    info{infoVersions = kept, infoInvalidEntries = strictElements (infoInvalidEntries info <> drops)}
  where
    origin = artifactOrigin ecosystemHosts upstreamBaseUrl
    Located kept drops _ = Map.foldrWithKey' step (Located Map.empty [] noAuthorityVerdict) (infoVersions info)

    step rawVersion details (Located keptAcc dropAcc previous) =
        case locatedVersion origin previous rawVersion details of
            (verdict, Just located, fileDrops) -> Located (Map.insert rawVersion located keptAcc) (fileDrops <> dropAcc) verdict
            (verdict, Nothing, emptied) -> Located keptAcc (emptied <> dropAcc) verdict

-- The versions kept so far, their drop records in version-key order, and the verdict the last URL left.
data Located = Located (Map Text PackageDetails) [InvalidEntry] AuthorityVerdict

{- | The single-version form of 'enforceArtifactLocations', for the selective decode path.
'Nothing' means no artifact of the version survived, so the version drops.
-}
enforceArtifactLocationsOf :: AllowedHostPorts -> Text -> PackageDetails -> Maybe PackageDetails
enforceArtifactLocationsOf ecosystemHosts upstreamBaseUrl details = located
  where
    (_, located, _) = locatedVersion (artifactOrigin ecosystemHosts upstreamBaseUrl) noAuthorityVerdict (renderVersion (pkgVersion details)) details

-- A version with only its located artifacts, 'Nothing' when it has none, beside its drop records and the verdict its last URL left.
locatedVersion :: ArtifactOrigin -> AuthorityVerdict -> Text -> PackageDetails -> (AuthorityVerdict, Maybe PackageDetails, [InvalidEntry])
locatedVersion origin previous rawVersion details =
    case partitionArtifacts rawVersion checked of
        (Just survivors, drops) -> (verdict, Just details{pkgArtifacts = strictElements survivors}, drops)
        (Nothing, drops) -> (verdict, Nothing, drops)
  where
    (verdict, checked) = resolvedInOrder origin previous (toList (pkgArtifacts details))

-- 'mapAccumL' over 'resolveArtifact', written strict, since 'mapAccumL' builds lazy pairs for every file of a read.
resolvedInOrder :: ArtifactOrigin -> AuthorityVerdict -> [Artifact] -> (AuthorityVerdict, [Either ArtifactRefusal Artifact])
resolvedInOrder origin = go
  where
    go previous = \case
        [] -> (previous, [])
        art : rest -> case resolveArtifact origin previous art of
            (decided, checked) -> case go decided rest of
                (final, checkedRest) -> (final, checked : checkedRest)
