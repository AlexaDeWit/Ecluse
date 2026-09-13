-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The AWS CodeArtifact leaf of the store maintenance handle. This is __control plane__ only,
on @amazonka@, while the data plane stays on @http-client@. The calls are a 'ControlPlane' record
built once from a discovered identity and captured in the handle's closures, so the backend's
state never reaches the proxy's @Env@ and a spec can drive the sequencing without one. The
read-only calls are their own record ("Ecluse.Runtime.Maintenance.CodeArtifact.Read"), and the
decisions live in "Ecluse.Runtime.Maintenance.CodeArtifact.Decide".
-}
module Ecluse.Runtime.Maintenance.CodeArtifact (
    newCodeArtifactMaintenance,
    newCodeArtifactObservation,
    maintenanceForEnv,

    -- * The calls the handle makes
    ControlPlane (..),
    controlPlaneFor,
    readPlaneFor,
    maintenanceFor,
    observationFor,
    boundedObservationFor,
) where

import Amazonka qualified as AWS
import Amazonka.CodeArtifact qualified as CA
import Amazonka.CodeArtifact.Lens qualified as CAL
import Lens.Micro ((^.))

import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Maintenance (
    ConsentVerdict,
    NameAlphabet,
    NamePrefix,
    StoreCursor (..),
    StoreFault,
    StoreMaintenance (..),
    StoreManifestRead,
    StoreObservation (..),
    StoredVersion,
    VersionOutcome,
    chunksOfCeiling,
    collectPagesBounded,
    deleteAll,
    pageAll,
    pageSource,
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
    cursorOfTags,
    cursorTagRequest,
    cursorUntagRequest,
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
 )
import Ecluse.Runtime.Maintenance.CodeArtifact.Read (
    ReadPlane (..),
    identityOfStore,
    versionsOfPage,
 )

{- | The control-plane calls this leaf makes, so the sequencing around them is drivable from
response values of @amazonka@'s own types. The reads are held apart from the writes.
-}
data ControlPlane = ControlPlane
    { cpRead :: ReadPlane
    , cpDeleteVersions :: CA.DeletePackageVersions -> IO (Either StoreFault CA.DeletePackageVersionsResponse)
    , cpTagResource :: CA.TagResource -> IO (Either StoreFault CA.TagResourceResponse)
    , cpUntagResource :: CA.UntagResource -> IO (Either StoreFault CA.UntagResourceResponse)
    }

{- | Build the maintenance handle for one CodeArtifact repository, over an environment whose AWS
credentials are discovered the standard way.
-}
newCodeArtifactMaintenance :: NameAlphabet -> StoreManifestRead -> CodeArtifactStore -> IO StoreMaintenance
newCodeArtifactMaintenance alphabet readManifest store =
    maintenanceForEnv alphabet readManifest store
        <$> newAwsEnv (Just (casRegion store)) Nothing CA.defaultService

{- | Build the observing calls alone for one repository, over an environment discovered the same
way. No deletion, no tag write, and no publication is built, so the caller holds none.
-}
newCodeArtifactObservation :: Int -> NameAlphabet -> StoreManifestRead -> CodeArtifactStore -> IO StoreObservation
newCodeArtifactObservation limit alphabet readManifest store =
    boundedObservationFor limit alphabet readManifest store . readPlaneFor
        <$> newAwsEnv (Just (casRegion store)) Nothing CA.defaultService

{- | Build the handle over a caller-supplied @amazonka@ 'AWS.Env'. Exposed so a test can hold the
handle, and the facts it supplies, without discovering an ambient AWS identity.
-}
maintenanceForEnv :: NameAlphabet -> StoreManifestRead -> CodeArtifactStore -> AWS.Env -> StoreMaintenance
maintenanceForEnv alphabet readManifest store env =
    maintenanceFor alphabet readManifest store (controlPlaneFor env)

-- | Every call sent over one env, with the AWS error folded into a 'StoreFault'.
controlPlaneFor :: AWS.Env -> ControlPlane
controlPlaneFor env =
    ControlPlane
        { cpRead = readPlaneFor env
        , cpDeleteVersions = sendStore env
        , cpTagResource = sendStore env
        , cpUntagResource = sendStore env
        }

-- | The observing calls alone, over one env, so a caller handed these can change nothing.
readPlaneFor :: AWS.Env -> ReadPlane
readPlaneFor env =
    ReadPlane
        { rpListPackages = sendStore env
        , rpListVersions = sendStore env
        , rpDescribeRepository = sendStore env
        , rpListTags = sendStore env
        }

{- | Build the handle over a caller-supplied 'ControlPlane' and the manifest read the root
assembled, which together are every effect it has.
-}
maintenanceFor :: NameAlphabet -> StoreManifestRead -> CodeArtifactStore -> ControlPlane -> StoreMaintenance
maintenanceFor alphabet readManifest store plane =
    StoreMaintenance
        { storeFacts = obFacts observed
        , listPackagesIn = obListPackagesIn observed
        , enumerateVersions = obEnumerateVersions observed
        , readStoreManifest = obReadManifest observed
        , deleteVersions = deleteChunks plane store
        , verifyConsent = obVerifyConsent observed
        , classifyStore = obClassifyStore observed
        , storeCursor = Just (walkCursor alphabet plane store)
        }
  where
    observed = observationFor alphabet readManifest store (cpRead plane)

-- | The observing calls over one 'ReadPlane', which is every effect they have.
observationFor :: NameAlphabet -> StoreManifestRead -> CodeArtifactStore -> ReadPlane -> StoreObservation
observationFor alphabet readManifest store observer =
    StoreObservation
        { obFacts = codeArtifactFacts alphabet
        , obListPackagesIn = pageSource . packagePage observer store
        , obEnumerateVersions = pageAll . versionPage observer store
        , obReadManifest = readManifest
        , obVerifyConsent = readConsent observer store
        , obClassifyStore = fmap (fmap classifyRepository) (describeStore observer store)
        }

-- | Build observation with version pagination bounded before another page is requested.
boundedObservationFor :: Int -> NameAlphabet -> StoreManifestRead -> CodeArtifactStore -> ReadPlane -> StoreObservation
boundedObservationFor limit alphabet readManifest store observer =
    (observationFor alphabet readManifest store observer)
        { obEnumerateVersions = collectPagesBounded limit . pageSource . versionPage observer store
        }

sendStore :: (AWS.AWSRequest a) => AWS.Env -> a -> IO (Either StoreFault (AWS.AWSResponse a))
sendStore = sendClassified classifyStoreFault

packagePage :: ReadPlane -> CodeArtifactStore -> NamePrefix -> Maybe Text -> IO (Either StoreFault (Maybe Text, [PackageName]))
packagePage observer store prefix token =
    fmap page <$> rpListPackages observer (listPackagesRequest store prefix token)
  where
    page response =
        ( response ^. CAL.listPackagesResponse_nextToken
        , packagesOfPage
            (formatEcosystem (casFormat store))
            (fromMaybe [] (response ^. CAL.listPackagesResponse_packages))
        )

versionPage ::
    ReadPlane ->
    CodeArtifactStore ->
    PackageName ->
    Maybe Text ->
    IO (Either StoreFault (Maybe Text, [StoredVersion]))
versionPage observer store name token =
    fmap page <$> rpListVersions observer (listVersionsRequest store name token)
  where
    page response =
        ( response ^. CAL.listPackageVersionsResponse_nextToken
        , versionsOfPage
            (identityOfStore store)
            name
            (fromMaybe [] (response ^. CAL.listPackageVersionsResponse_versions))
        )

deleteChunks :: ControlPlane -> CodeArtifactStore -> PackageName -> [Version] -> IO [(Version, VersionOutcome)]
deleteChunks plane store name versions =
    deleteAll send (chunksOfCeiling deleteCeiling versions)
  where
    send batch =
        fmap (foldDeleteResponse batch) <$> cpDeleteVersions plane (deleteRequest store name batch)

-- The consent marker is a tag on the repository, so the ARN comes first.
readConsent :: ReadPlane -> CodeArtifactStore -> IO (Either StoreFault ConsentVerdict)
readConsent observer store =
    withRepositoryArn observer store $ \arn ->
        fmap (consentOfTags . tagsOfResponse) <$> rpListTags observer (listTagsRequest arn)

{- The walk cursor is a second tag on the same repository, the only one this leaf writes. Its
three calls address the repository by ARN, exactly as the consent read does. -}
walkCursor :: NameAlphabet -> ControlPlane -> CodeArtifactStore -> StoreCursor
walkCursor alphabet plane store =
    StoreCursor
        { readCursor = withRepositoryArn observer store $ \arn ->
            fmap (cursorOfTags alphabet eco . tagsOfResponse) <$> rpListTags observer (listTagsRequest arn)
        , writeCursor = \prefix -> withRepositoryArn observer store $ \arn ->
            void <$> cpTagResource plane (cursorTagRequest eco arn prefix)
        , clearCursor = withRepositoryArn observer store $ \arn ->
            void <$> cpUntagResource plane (cursorUntagRequest eco arn)
        }
  where
    observer = cpRead plane

    eco :: Ecosystem
    eco = formatEcosystem (casFormat store)

-- A tag call is addressed by ARN, which only the repository description carries.
withRepositoryArn :: ReadPlane -> CodeArtifactStore -> (Text -> IO (Either StoreFault a)) -> IO (Either StoreFault a)
withRepositoryArn observer store act =
    describeStore observer store >>= \case
        Left fault -> pure (Left fault)
        Right description -> either (pure . Left) act (arnOfDescription description)

tagsOfResponse :: CA.ListTagsForResourceResponse -> [CA.Tag]
tagsOfResponse response = fromMaybe [] (response ^. CAL.listTagsForResourceResponse_tags)

describeStore :: ReadPlane -> CodeArtifactStore -> IO (Either StoreFault CA.RepositoryDescription)
describeStore observer store =
    (>>= repositoryOfResponse) <$> rpDescribeRepository observer (describeRepositoryRequest store)
