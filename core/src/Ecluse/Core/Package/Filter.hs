-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Ecosystem-independent listing decisions and artifact-location admission.
Adapters replay the surviving versions and files onto their raw documents.
-}
module Ecluse.Core.Package.Filter (
    -- * Rule-filter plan
    FilterPlan (..),
    filterPlanFromDecisions,
    restrictToSurvivors,

    -- * Served-location enforcement
    enforceArtifactLocations,
    enforceArtifactLocationsOf,

    -- * The per-artifact check (exported for its unit spec)
    ArtifactOrigin,
    artifactOrigin,
    ArtifactRefusal (..),
    resolveArtifact,
) where

import Data.Aeson (Value (String))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T

import Ecluse.Core.Package (
    Artifact (artFilename, artUrl),
    InvalidEntry,
    InvalidEntryKind (InvalidIndexFile, InvalidVersionManifest),
    PackageDetails (pkgArtifacts),
    PackageInfo (infoDistTags, infoInvalidEntries, infoVersions),
    mkInvalidEntry,
    pkgVersion,
 )
import Ecluse.Core.Rules.Types (Decision (Admitted))
import Ecluse.Core.Security (AllowedHostPorts, HostPort, artifactAuthorityHonoured, authorityLabel, hostAddress, hostPortAddress)
import Ecluse.Core.Security.Egress (registryUrlText, resolveTarballUrl)
import Ecluse.Core.Strict (strictElements)
import Ecluse.Core.Text (urlFilename)
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
        case partitionArtifacts origin rawVersion details of
            (Just survivors, fileDrops) -> (Map.insert rawVersion survivors keptAcc, fileDrops <> dropAcc)
            (Nothing, emptied) -> (keptAcc, emptied <> dropAcc)

{- | The single-version form of 'enforceArtifactLocations', for the selective decode path.
'Nothing' means no artifact of the version survived, so the version drops.
-}
enforceArtifactLocationsOf :: AllowedHostPorts -> Text -> PackageDetails -> Maybe PackageDetails
enforceArtifactLocationsOf ecosystemHosts upstreamBaseUrl details =
    fst (partitionArtifacts (artifactOrigin ecosystemHosts upstreamBaseUrl) (renderVersion (pkgVersion details)) details)

-- | The inputs every artifact of one document is checked against, derived once from its upstream.
data ArtifactOrigin = ArtifactOrigin
    { originHosts :: AllowedHostPorts
    , originAuthority :: Maybe HostPort
    , originHttpsHost :: Maybe Text
    }

-- | Derive a document's 'ArtifactOrigin' from its ecosystem's artifact hosts and upstream base URL.
artifactOrigin :: AllowedHostPorts -> Text -> ArtifactOrigin
artifactOrigin ecosystemHosts upstreamBaseUrl =
    ArtifactOrigin
        { originHosts = ecosystemHosts
        , originAuthority = hostPortAddress upstreamBaseUrl
        , originHttpsHost = httpsUpstreamHost upstreamBaseUrl
        }

-- | Why one artifact was refused, for the drop record that reports it.
data ArtifactRefusal = ArtifactRefusal
    { refusedFile :: Text
    , refusedReason :: Text
    , refusedUrl :: Text
    -- ^ The URL the failing check read. A drop record reduces it to its authority.
    }
    deriving stock (Eq, Show)

-- 'Nothing' survivors means the version itself drops, recorded once under its version key
-- rather than once per file, so an emptied version reads as one loss.
partitionArtifacts :: ArtifactOrigin -> Text -> PackageDetails -> (Maybe PackageDetails, [InvalidEntry])
partitionArtifacts origin rawVersion details =
    case nonEmpty (rights resolved) of
        Just survivors -> (Just details{pkgArtifacts = strictElements survivors}, map fileDrop refusals)
        Nothing -> (Nothing, map (versionDrop rawVersion) (take 1 refusals))
  where
    resolved = map (resolveArtifact origin) (toList (pkgArtifacts details))
    refusals = lefts resolved

-- Record one dropped file under its own name. 'mkInvalidEntry' reduces only a scheme-bearing
-- string, so the URL is reduced here, whatever its spelling.
fileDrop :: ArtifactRefusal -> InvalidEntry
fileDrop refusal =
    mkInvalidEntry InvalidIndexFile (refusedFile refusal) (String (authorityLabel (refusedUrl refusal))) (refusedReason refusal)

-- Record a version whose every artifact was refused, keyed by its raw version string.
versionDrop :: Text -> ArtifactRefusal -> InvalidEntry
versionDrop rawVersion refusal =
    mkInvalidEntry InvalidVersionManifest rawVersion (String (authorityLabel (refusedUrl refusal))) (refusedReason refusal)

{- | Check an artifact's filename, its https normalisation, then its authority. A non-https (loopback)
upstream skips normalisation, but not the authority check, which the download gate also applies.
-}
resolveArtifact :: ArtifactOrigin -> Artifact -> Either ArtifactRefusal Artifact
resolveArtifact origin art = do
    checkFilename url
    normalised <- normaliseScheme
    -- Text equal to the original has passed the filename check already.
    located <-
        if normalised == url
            then Right art
            else art{artUrl = normalised} <$ checkFilename normalised
    if artifactAuthorityHonoured (originHosts origin) (originAuthority origin) (hostPortAddress normalised)
        then Right located
        else Left (refusal "artifact authority is neither the serving upstream nor a declared artifact host" normalised)
  where
    url = artUrl art

    checkFilename candidate =
        when (isNothing (urlFilename candidate)) $
            Left (refusal "artifact URL has no safe filename" candidate)

    normaliseScheme = case originHttpsHost origin of
        Nothing -> Right url
        Just upstreamHost -> bimap (`refusal` url) registryUrlText (resolveTarballUrl upstreamHost url)

    refusal reason candidate = ArtifactRefusal{refusedFile = artFilename art, refusedReason = reason, refusedUrl = candidate}

-- The bare host of an @https@ upstream base URL, or 'Nothing' for a non-https (test/dev
-- loopback) upstream whose artifact URLs the scheme normalisation leaves untouched.
httpsUpstreamHost :: Text -> Maybe Text
httpsUpstreamHost baseUrl
    | "https://" `T.isPrefixOf` T.toLower baseUrl = Just (hostAddress baseUrl)
    | otherwise = Nothing
