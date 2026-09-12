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
    VersionReadFault (..),
    classifyVersionRead,

    -- * Where an observation was made
    RepositoryIdentity (..),
    identityOfStore,

    -- * What one observation preserves
    VersionObservation (..),
    VersionOrigin (..),
    LocalVersionRead (..),

    -- * One listing page
    observationsOfPage,
    versionsOfPage,
    storedOfObservation,

    -- * One version, read directly
    describeVersionRequest,
    readOfAnswer,
) where

import Amazonka qualified as AWS
import Amazonka.CodeArtifact qualified as CA
import Amazonka.CodeArtifact.Lens qualified as CAL
import Data.Text qualified as T
import Lens.Micro ((.~), (?~), (^.))

import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Maintenance (
    StoreFault,
    StoredVersion (..),
    VersionPresence,
    protocolFault,
 )
import Ecluse.Core.Text (nonBlank)
import Ecluse.Core.Version (Version, mkVersion, renderVersion)
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (
    CodeArtifactStore (..),
    classifyStoreFault,
    formatEcosystem,
    formatToken,
    packageCoordinates,
    presenceOf,
    storePackageFormat,
 )

{- | The CodeArtifact calls that observe an inventory. The direct version read keeps absence out
of the fault channel, because CodeArtifact refuses a version it does not hold.
-}
data ReadPlane = ReadPlane
    { rpListPackages :: CA.ListPackages -> IO (Either StoreFault CA.ListPackagesResponse)
    , rpListVersions :: CA.ListPackageVersions -> IO (Either StoreFault CA.ListPackageVersionsResponse)
    , rpDescribeRepository :: CA.DescribeRepository -> IO (Either StoreFault CA.DescribeRepositoryResponse)
    , rpListTags :: CA.ListTagsForResource -> IO (Either StoreFault CA.ListTagsForResourceResponse)
    , rpDescribeVersion :: CA.DescribePackageVersion -> IO (Either VersionReadFault CA.DescribePackageVersionResponse)
    }

-- | Why a direct version read carried no description.
data VersionReadFault
    = -- | The repository holds no such version, which is evidence rather than a failure.
      VersionNotHeld
    | -- | The read did not land, or the store refused it for another reason.
      VersionUnread StoreFault
    deriving stock (Eq, Show)

{- | Classify an @amazonka@ error for a direct version read, keeping CodeArtifact's own
"no such resource" apart from every refusal a sweep must not read as absence.
-}
classifyVersionRead :: AWS.Error -> VersionReadFault
classifyVersionRead err
    | resourceNotFound err = VersionNotHeld
    | otherwise = VersionUnread (classifyStoreFault err)

{- @amazonka@ strips the @Exception@ suffix from a service's error code, so
@ResourceNotFoundException@ arrives as @ResourceNotFound@. -}
resourceNotFound :: AWS.Error -> Bool
resourceNotFound = \case
    AWS.ServiceError service -> case service ^. AWS.serviceError_code of
        AWS.ErrorCode code -> T.toLower code == "resourcenotfound"
    _ -> False

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

-- | What one direct read of a version established about this repository.
data LocalVersionRead
    = -- | The store described the version, and the description named the one asked for.
      VersionObserved VersionObservation
    | -- | The store answered that this repository holds no such version.
      VersionAbsentLocally
    | -- | No usable evidence: the read did not land, or the description did not hold up.
      VersionEvidenceIncomplete StoreFault
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

{- | Describe one version. Every coordinate comes from the bound store, so the request cannot
address a repository this handle was not built for.
-}
describeVersionRequest :: CodeArtifactStore -> PackageName -> Version -> CA.DescribePackageVersion
describeVersionRequest store name version =
    CA.newDescribePackageVersion
        (casDomain store)
        (casRepository store)
        (storePackageFormat store)
        package
        (renderVersion version)
        & (CAL.describePackageVersion_domainOwner ?~ casDomainOwner store)
        & (CAL.describePackageVersion_namespace .~ namespace)
  where
    (namespace, package) = packageCoordinates name

{- | Read one @DescribePackageVersion@ answer as evidence, refusing a description that names
anything other than the version asked for.
-}
readOfAnswer ::
    CodeArtifactStore ->
    PackageName ->
    Version ->
    Either VersionReadFault CA.DescribePackageVersionResponse ->
    LocalVersionRead
readOfAnswer store name version = \case
    Left VersionNotHeld -> VersionAbsentLocally
    Left (VersionUnread fault) -> VersionEvidenceIncomplete fault
    Right response ->
        evidenceOf store name version (response ^. CAL.describePackageVersionResponse_packageVersion)

-- A description with no status says nothing about what the store still does with the version.
evidenceOf :: CodeArtifactStore -> PackageName -> Version -> CA.PackageVersionDescription -> LocalVersionRead
evidenceOf store name version description =
    case identityMismatch store name version description of
        Just mismatch -> refused ("the store described " <> mismatch)
        Nothing ->
            maybe
                (refused "the store described the version without a status")
                observed
                (description ^. CAL.packageVersionDescription_status)
  where
    refused = VersionEvidenceIncomplete . protocolFault

    observed status =
        VersionObserved
            VersionObservation
                { obsIdentity = identityOfStore store
                , obsPackage = name
                , obsVersion = version
                , obsStatus = status
                , obsPresence = presenceOf status
                , obsRevision = nonBlank =<< description ^. CAL.packageVersionDescription_revision
                , obsOrigin = originOf <$> description ^. CAL.packageVersionDescription_origin
                }

{- The first identity field the description disagreed with. A field it did not supply is left
unchecked, because there is nothing to disagree with. -}
identityMismatch :: CodeArtifactStore -> PackageName -> Version -> CA.PackageVersionDescription -> Maybe Text
identityMismatch store name version description =
    listToMaybe
        ( catMaybes
            [ disagreement "format" (Just (formatToken (casFormat store))) (CA.fromPackageFormat <$> description ^. CAL.packageVersionDescription_format)
            , disagreement "namespace" namespace (nonBlank =<< description ^. CAL.packageVersionDescription_namespace)
            , disagreement "package" (Just package) (nonBlank =<< description ^. CAL.packageVersionDescription_packageName)
            , disagreement "version" (Just (renderVersion version)) (nonBlank =<< description ^. CAL.packageVersionDescription_version)
            ]
        )
  where
    (namespace, package) = packageCoordinates name

disagreement :: Text -> Maybe Text -> Maybe Text -> Maybe Text
disagreement field expected reported = do
    stated <- reported
    guard (Just stated /= expected)
    pure (field <> " " <> stated <> ", not the one asked for")
