-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The AWS CodeArtifact leaf of the store maintenance handle: enumerate a repository's
packages and versions, delete versions from it, and read the two verdicts a sweep needs. This is
__control plane__ only, on @amazonka@, while the data plane stays on @http-client@. Its five
calls are a 'ControlPlane' record, built once from a discovered identity and captured in the
handle's closures, so the backend's state never reaches the proxy's @Env@ and a spec can drive
the sequencing without one. The decisions live in
"Ecluse.Runtime.Maintenance.CodeArtifact.Decide", the drives in "Ecluse.Core.Registry.Maintenance".
-}
module Ecluse.Runtime.Maintenance.CodeArtifact (
    newCodeArtifactMaintenance,
    maintenanceForEnv,

    -- * The calls the handle makes
    ControlPlane (..),
    controlPlaneFor,
    maintenanceFor,
) where

import Amazonka qualified as AWS
import Amazonka.CodeArtifact qualified as CA
import Amazonka.CodeArtifact.Lens qualified as CAL
import Lens.Micro ((^.))

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Maintenance (
    ConsentVerdict,
    StoreFault,
    StoreMaintenance (..),
    StoredVersion,
    VersionOutcome,
    chunksOfCeiling,
    deleteAll,
    pageAll,
 )
import Ecluse.Core.Version (Version)
import Ecluse.Runtime.Aws.Env (newAwsEnv)
import Ecluse.Runtime.Aws.Fault (sendClassified)
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (
    CodeArtifactStore (..),
    arnOfDescription,
    classifyRepository,
    classifyStoreFault,
    codeArtifactFacts,
    consentOfTags,
    deleteCeiling,
    deleteRequest,
    describeRepositoryRequest,
    foldDeleteResponse,
    formatEcosystem,
    listPackagesRequest,
    listTagsRequest,
    listVersionsRequest,
    packagesOfPage,
    repositoryOfResponse,
    versionsOfPage,
 )

{- | The five control-plane calls this leaf makes, one field each, so the sequencing around them
is drivable from response values of @amazonka@'s own types.
-}
data ControlPlane = ControlPlane
    { cpListPackages :: CA.ListPackages -> IO (Either StoreFault CA.ListPackagesResponse)
    , cpListVersions :: CA.ListPackageVersions -> IO (Either StoreFault CA.ListPackageVersionsResponse)
    , cpDeleteVersions :: CA.DeletePackageVersions -> IO (Either StoreFault CA.DeletePackageVersionsResponse)
    , cpListTags :: CA.ListTagsForResource -> IO (Either StoreFault CA.ListTagsForResourceResponse)
    , cpDescribeRepository :: CA.DescribeRepository -> IO (Either StoreFault CA.DescribeRepositoryResponse)
    }

{- | Build the maintenance handle for one CodeArtifact repository, with AWS credentials
discovered the standard way (environment, instance role, container role, SSO, STS).
-}
newCodeArtifactMaintenance :: CodeArtifactStore -> IO StoreMaintenance
newCodeArtifactMaintenance store =
    maintenanceForEnv store <$> newAwsEnv (Just (casRegion store)) Nothing CA.defaultService

{- | Build the handle over a caller-supplied @amazonka@ 'AWS.Env'. Exposed so a test can hold the
handle, and the facts it supplies, without discovering an ambient AWS identity.
-}
maintenanceForEnv :: CodeArtifactStore -> AWS.Env -> StoreMaintenance
maintenanceForEnv store env = maintenanceFor store (controlPlaneFor env)

-- | Every call sent over one env, with the AWS error folded into a 'StoreFault'.
controlPlaneFor :: AWS.Env -> ControlPlane
controlPlaneFor env =
    ControlPlane
        { cpListPackages = sendStore env
        , cpListVersions = sendStore env
        , cpDeleteVersions = sendStore env
        , cpListTags = sendStore env
        , cpDescribeRepository = sendStore env
        }

-- | Build the handle over a caller-supplied 'ControlPlane', which is all the effects it has.
maintenanceFor :: CodeArtifactStore -> ControlPlane -> StoreMaintenance
maintenanceFor store plane =
    StoreMaintenance
        { storeFacts = codeArtifactFacts
        , enumeratePackages = pageAll (packagePage plane store)
        , enumerateVersions = pageAll . versionPage plane store
        , deleteVersions = deleteChunks plane store
        , -- CodeArtifact has no call that reports what a delete would do without doing it.
          rehearseDelete = Nothing
        , verifyConsent = readConsent plane store
        , classifyStore = fmap (fmap classifyRepository) (describeStore plane store)
        }

sendStore :: (AWS.AWSRequest a) => AWS.Env -> a -> IO (Either StoreFault (AWS.AWSResponse a))
sendStore = sendClassified classifyStoreFault

packagePage :: ControlPlane -> CodeArtifactStore -> Maybe Text -> IO (Either StoreFault (Maybe Text, [PackageName]))
packagePage plane store token =
    fmap page <$> cpListPackages plane (listPackagesRequest store token)
  where
    page response =
        ( response ^. CAL.listPackagesResponse_nextToken
        , packagesOfPage
            (formatEcosystem (casFormat store))
            (fromMaybe [] (response ^. CAL.listPackagesResponse_packages))
        )

versionPage ::
    ControlPlane ->
    CodeArtifactStore ->
    PackageName ->
    Maybe Text ->
    IO (Either StoreFault (Maybe Text, [StoredVersion]))
versionPage plane store name token =
    fmap page <$> cpListVersions plane (listVersionsRequest store name token)
  where
    page response =
        ( response ^. CAL.listPackageVersionsResponse_nextToken
        , versionsOfPage
            (formatEcosystem (casFormat store))
            (fromMaybe [] (response ^. CAL.listPackageVersionsResponse_versions))
        )

deleteChunks :: ControlPlane -> CodeArtifactStore -> PackageName -> [Version] -> IO [(Version, VersionOutcome)]
deleteChunks plane store name versions =
    deleteAll send (chunksOfCeiling deleteCeiling versions)
  where
    send batch =
        fmap (foldDeleteResponse batch) <$> cpDeleteVersions plane (deleteRequest store name batch)

-- The consent marker is a tag on the repository, and a tag read is addressed by ARN, so
-- the description comes first.
readConsent :: ControlPlane -> CodeArtifactStore -> IO (Either StoreFault ConsentVerdict)
readConsent plane store =
    describeStore plane store >>= \case
        Left fault -> pure (Left fault)
        Right description -> case arnOfDescription description of
            Left fault -> pure (Left fault)
            Right arn -> fmap tags <$> cpListTags plane (listTagsRequest arn)
  where
    tags response = consentOfTags (fromMaybe [] (response ^. CAL.listTagsForResourceResponse_tags))

describeStore :: ControlPlane -> CodeArtifactStore -> IO (Either StoreFault CA.RepositoryDescription)
describeStore plane store =
    (>>= repositoryOfResponse) <$> cpDescribeRepository plane (describeRepositoryRequest store)
