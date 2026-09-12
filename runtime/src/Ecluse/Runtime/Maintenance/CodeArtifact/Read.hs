-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The read-only half of the CodeArtifact maintenance leaf: the calls that only observe, and
the evidence one observed version carries. A 'ReadPlane' holds no deletion, no cursor write, and
no publication, so holding one confers no authority over the repository, and an observation is
evidence a later decision reads rather than a permission it acts on. The requests and verdicts
the whole leaf shares live in "Ecluse.Runtime.Maintenance.CodeArtifact.Decide".
-}
module Ecluse.Runtime.Maintenance.CodeArtifact.Read (
    -- * The calls that only observe
    ReadPlane (..),

    -- * Where an observation was made
    RepositoryIdentity (..),
    identityOfStore,

    -- * What one observation preserves
    VersionObservation (..),
    VersionOrigin (..),

    -- * One listing page
    observationsOfPage,
    versionsOfPage,
    storedOfObservation,
) where

import Amazonka.CodeArtifact qualified as CA
import Amazonka.CodeArtifact.Lens qualified as CAL
import Lens.Micro ((^.))

import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Maintenance (
    StoreFault,
    StoredVersion (..),
    VersionPresence,
 )
import Ecluse.Core.Text (nonBlank)
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (
    CodeArtifactStore (..),
    formatEcosystem,
    presenceOf,
 )

-- | The CodeArtifact calls that observe an inventory, held apart from every call that changes it.
data ReadPlane = ReadPlane
    { rpListPackages :: CA.ListPackages -> IO (Either StoreFault CA.ListPackagesResponse)
    , rpListVersions :: CA.ListPackageVersions -> IO (Either StoreFault CA.ListPackageVersionsResponse)
    , rpDescribeRepository :: CA.DescribeRepository -> IO (Either StoreFault CA.DescribeRepositoryResponse)
    , rpListTags :: CA.ListTagsForResource -> IO (Either StoreFault CA.ListTagsForResourceResponse)
    }

{- | The exact repository an observation was made in. The region is the store's own, which is
also the one the leaf binds its @amazonka@ environment to.
-}
data RepositoryIdentity = RepositoryIdentity
    { ridDomainOwner :: Text
    -- ^ The 12-digit account number that owns the domain.
    , ridRegion :: Text
    , ridDomain :: Text
    , ridRepository :: Text
    , ridEcosystem :: Ecosystem
    }
    deriving stock (Eq, Show)

-- | The repository a store's coordinates name.
identityOfStore :: CodeArtifactStore -> RepositoryIdentity
identityOfStore store =
    RepositoryIdentity
        { ridDomainOwner = casDomainOwner store
        , ridRegion = casRegion store
        , ridDomain = casDomain store
        , ridRepository = casRepository store
        , ridEcosystem = formatEcosystem (casFormat store)
        }

{- | How a version entered the domain, as the store reported it. Each field stays optional,
because one the store omitted is not one it denied.
-}
data VersionOrigin = VersionOrigin
    { vorType :: Maybe CA.PackageVersionOriginType
    -- ^ CodeArtifact's own token, verbatim, so a token this build does not know survives.
    , vorEntryRepository :: Maybe Text
    -- ^ The repository the version was first published to, where the store named one.
    , vorEntryConnection :: Maybe Text
    -- ^ The external connection the version was ingested through, where the store named one.
    }
    deriving stock (Eq, Show)

{- | One version the store reported holding, with the evidence a later decision reads. Two
observations of the same version in different repositories stay two observations.
-}
data VersionObservation = VersionObservation
    { obsIdentity :: RepositoryIdentity
    , obsPackage :: PackageName
    , obsVersion :: Version
    , obsStatus :: CA.PackageVersionStatus
    -- ^ CodeArtifact's own status, kept beside the projection so an unexpected one survives.
    , obsPresence :: VersionPresence
    , obsRevision :: Maybe Text
    -- ^ The store's revision of this version, 'Nothing' where it reported none.
    , obsOrigin :: Maybe VersionOrigin
    -- ^ 'Nothing' where the store reported no origin at all.
    }
    deriving stock (Eq, Show)

-- | The observations in one listing page, one per entry and never merged by version.
observationsOfPage :: RepositoryIdentity -> PackageName -> [CA.PackageVersionSummary] -> [VersionObservation]
observationsOfPage repository name = map observed
  where
    observed summary =
        VersionObservation
            { obsIdentity = repository
            , obsPackage = name
            , obsVersion = mkVersion (ridEcosystem repository) (summary ^. CAL.packageVersionSummary_version)
            , obsStatus = summary ^. CAL.packageVersionSummary_status
            , obsPresence = presenceOf (summary ^. CAL.packageVersionSummary_status)
            , obsRevision = nonBlank =<< summary ^. CAL.packageVersionSummary_revision
            , obsOrigin = originOf <$> summary ^. CAL.packageVersionSummary_origin
            }

-- | The versions in one listing page, projected to what the sweep reads.
versionsOfPage :: RepositoryIdentity -> PackageName -> [CA.PackageVersionSummary] -> [StoredVersion]
versionsOfPage repository name = map storedOfObservation . observationsOfPage repository name

-- | The sweep's view of one observation: which version, and whether the store still serves it.
storedOfObservation :: VersionObservation -> StoredVersion
storedOfObservation observation =
    StoredVersion
        { storedVersion = obsVersion observation
        , storedPresence = obsPresence observation
        }

-- A blank entry point names nothing, so it reads as absent rather than as an empty name.
originOf :: CA.PackageVersionOrigin -> VersionOrigin
originOf origin =
    VersionOrigin
        { vorType = origin ^. CAL.packageVersionOrigin_originType
        , vorEntryRepository = nonBlank =<< (entry >>= (^. CAL.domainEntryPoint_repositoryName))
        , vorEntryConnection = nonBlank =<< (entry >>= (^. CAL.domainEntryPoint_externalConnectionName))
        }
  where
    entry = origin ^. CAL.packageVersionOrigin_domainEntryPoint
