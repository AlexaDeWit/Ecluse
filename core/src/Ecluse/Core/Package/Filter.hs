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
    InvalidEntry,
    PackageDetails (pkgArtifacts),
    PackageInfo (infoDistTags, infoInvalidEntries, infoVersions),
    pkgVersion,
 )
import Ecluse.Core.Package.Filter.Internal (ArtifactOrigin, artifactOrigin, partitionArtifacts, resolveArtifact)
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
    (kept, drops) = Map.foldrWithKey step (Map.empty, []) (infoVersions info)

    step rawVersion details (keptAcc, dropAcc) =
        case locatedVersion origin rawVersion details of
            (Just located, fileDrops) -> (Map.insert rawVersion located keptAcc, fileDrops <> dropAcc)
            (Nothing, emptied) -> (keptAcc, emptied <> dropAcc)

{- | The single-version form of 'enforceArtifactLocations', for the selective decode path.
'Nothing' means no artifact of the version survived, so the version drops.
-}
enforceArtifactLocationsOf :: AllowedHostPorts -> Text -> PackageDetails -> Maybe PackageDetails
enforceArtifactLocationsOf ecosystemHosts upstreamBaseUrl details =
    fst (locatedVersion (artifactOrigin ecosystemHosts upstreamBaseUrl) (renderVersion (pkgVersion details)) details)

-- A version with only its located artifacts, 'Nothing' when it has none, beside its drop records.
locatedVersion :: ArtifactOrigin -> Text -> PackageDetails -> (Maybe PackageDetails, [InvalidEntry])
locatedVersion origin rawVersion details =
    case partitionArtifacts rawVersion (map (resolveArtifact origin) (toList (pkgArtifacts details))) of
        (Just survivors, drops) -> (Just details{pkgArtifacts = strictElements survivors}, drops)
        (Nothing, drops) -> (Nothing, drops)
