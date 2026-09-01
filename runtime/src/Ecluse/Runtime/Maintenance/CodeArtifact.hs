-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The AWS CodeArtifact leaf of the store maintenance handle: enumerate a repository's
packages and versions, delete versions from it, and read the two verdicts a sweep needs
before it deletes anything. This is __control plane__ only, on @amazonka@, while the data
plane that serves and mirrors packages stays on @http-client@. The env is built once here
and captured in the handle's closures, so the backend's state never reaches the proxy's
@Env@. Every decision lives next door in
"Ecluse.Runtime.Maintenance.CodeArtifact.Decide", and the backend-neutral paging, chunking,
and delete drives live in "Ecluse.Core.Registry.Maintenance".
-}
module Ecluse.Runtime.Maintenance.CodeArtifact (
    newCodeArtifactMaintenance,
    maintenanceForEnv,
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

{- | Build the maintenance handle for one CodeArtifact repository, with AWS credentials
discovered the standard way (environment, instance role, container role, SSO, STS).
-}
newCodeArtifactMaintenance :: CodeArtifactStore -> IO StoreMaintenance
newCodeArtifactMaintenance store =
    maintenanceForEnv store <$> newAwsEnv (Just (casRegion store)) Nothing CA.defaultService

{- | Build the handle over a caller-supplied @amazonka@ 'AWS.Env'. Exposed so a test can
hold the handle, and the facts it supplies, without discovering an ambient AWS identity.
-}
maintenanceForEnv :: CodeArtifactStore -> AWS.Env -> StoreMaintenance
maintenanceForEnv store env =
    StoreMaintenance
        { storeFacts = codeArtifactFacts
        , enumeratePackages = pageAll (packagePage env store)
        , enumerateVersions = pageAll . versionPage env store
        , deleteVersions = deleteChunks env store
        , -- CodeArtifact has no call that reports what a delete would do without doing it.
          rehearseDelete = Nothing
        , verifyConsent = readConsent env store
        , classifyStore = fmap (fmap classifyRepository) (describeStore env store)
        }

sendStore :: (AWS.AWSRequest a) => AWS.Env -> a -> IO (Either StoreFault (AWS.AWSResponse a))
sendStore = sendClassified classifyStoreFault

packagePage :: AWS.Env -> CodeArtifactStore -> Maybe Text -> IO (Either StoreFault (Maybe Text, [PackageName]))
packagePage env store token =
    fmap page <$> sendStore env (listPackagesRequest store token)
  where
    page response =
        ( response ^. CAL.listPackagesResponse_nextToken
        , packagesOfPage
            (formatEcosystem (casFormat store))
            (fromMaybe [] (response ^. CAL.listPackagesResponse_packages))
        )

versionPage ::
    AWS.Env ->
    CodeArtifactStore ->
    PackageName ->
    Maybe Text ->
    IO (Either StoreFault (Maybe Text, [StoredVersion]))
versionPage env store name token =
    fmap page <$> sendStore env (listVersionsRequest store name token)
  where
    page response =
        ( response ^. CAL.listPackageVersionsResponse_nextToken
        , versionsOfPage
            (formatEcosystem (casFormat store))
            (fromMaybe [] (response ^. CAL.listPackageVersionsResponse_versions))
        )

deleteChunks :: AWS.Env -> CodeArtifactStore -> PackageName -> [Version] -> IO [(Version, VersionOutcome)]
deleteChunks env store name versions =
    deleteAll send (chunksOfCeiling deleteCeiling versions)
  where
    send batch = fmap (foldDeleteResponse batch) <$> sendStore env (deleteRequest store name batch)

-- The consent marker is a tag on the repository, and a tag read is addressed by ARN, so
-- the description comes first.
readConsent :: AWS.Env -> CodeArtifactStore -> IO (Either StoreFault ConsentVerdict)
readConsent env store =
    describeStore env store >>= \case
        Left fault -> pure (Left fault)
        Right description -> case arnOfDescription description of
            Left fault -> pure (Left fault)
            Right arn -> fmap tags <$> sendStore env (listTagsRequest arn)
  where
    tags response = consentOfTags (fromMaybe [] (response ^. CAL.listTagsForResourceResponse_tags))

describeStore :: AWS.Env -> CodeArtifactStore -> IO (Either StoreFault CA.RepositoryDescription)
describeStore env store =
    (>>= repositoryOfResponse) <$> sendStore env (describeRepositoryRequest store)
