-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | CodeArtifact control-plane handles with authenticated data-plane readers bound by composition.
module Ecluse.Runtime.Maintenance.CodeArtifact.Internal (
    newCodeArtifactMaintenance,
    newCodeArtifactObservation,
    newCodeArtifactUpstreamProbe,
    newCodeArtifactCacheMaintenance,
    newCodeArtifactCacheObservation,
    cacheMaintenanceFor,

    -- * The calls the handle makes
    ControlPlane (..),
    controlPlaneFor,
    maintenanceFor,
    observationFor,
    boundedObservationFor,
    probeUpstreamSafety,
) where

import Amazonka qualified as AWS
import Amazonka.CodeArtifact qualified as CA
import Amazonka.CodeArtifact.Lens qualified as CAL
import Lens.Micro ((^.))
import UnliftIO (tryAny)

import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Exchange (newSingleAttemptManager)
import Ecluse.Core.Registry.Maintenance (
    ConsentVerdict,
    DeleteGuard,
    StoreClass (StoreDestroyable),
    StoreCursor (..),
    StoreDeletion (..),
    StoreFault,
    StoreMaintenance (..),
    StoreManifestRead,
    StoreObservation (..),
    StoredVersion,
    VersionOutcome,
    chunksOfCeiling,
    collectPagesBounded,
    deleteAll,
    maintenanceOf,
    pageAll,
    pageSource,
 )
import Ecluse.Core.Registry.Maintenance.NameSpace (
    NameAlphabet,
    NamePrefix,
 )
import Ecluse.Core.Registry.Maintenance.Upstream (
    RepositoryLinks,
    UndecidabilityReason (NetworkFailure),
    UnsafeReason (InsufficientPermissions),
    UpstreamSafety (Undecidable, Unsafe),
    walkUpstreamChain,
 )
import Ecluse.Core.Rules.Install (InstallArtifactRead)
import Ecluse.Core.Version (Version)
import Ecluse.Runtime.Aws.Env (newAwsEnv, singleAttemptEnv)
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
    describeRepositoryGrant,
    describeRepositoryRequest,
    describeUpstreamRefusal,
    describeUpstreamRequest,
    foldDeleteResponse,
    formatEcosystem,
    listPackagesRequest,
    listTagsRequest,
    listVersionsRequest,
    listVersionsResult,
    packagesOfPage,
    repositoryOfResponse,
    repositoryOfStore,
    upstreamLinksOf,
 )
import Ecluse.Runtime.Maintenance.CodeArtifact.Read (
    ReadPlane (..),
    identityOfStore,
    versionsOfPage,
 )

-- | Separate read and write calls allow observation without granting deletion authority.
data ControlPlane = ControlPlane
    { cpRead :: ReadPlane
    , cpDeleteVersions :: CA.DeletePackageVersions -> IO (Either StoreFault CA.DeletePackageVersionsResponse)
    , cpTagResource :: CA.TagResource -> IO (Either StoreFault CA.TagResourceResponse)
    , cpUntagResource :: CA.UntagResource -> IO (Either StoreFault CA.UntagResourceResponse)
    }

-- | Build repository maintenance with ambient AWS credentials and single-attempt destructive transport.
newCodeArtifactMaintenance :: Int -> NameAlphabet -> StoreManifestRead -> InstallArtifactRead -> CodeArtifactStore -> IO StoreMaintenance
newCodeArtifactMaintenance limit alphabet readManifest readArtifact store = do
    env <- newMaintenanceEnv store
    boundedMaintenance limit alphabet readManifest readArtifact store <$> controlPlaneFor env

-- | Build repository observation with ambient credentials and no write calls.
newCodeArtifactObservation :: Int -> NameAlphabet -> StoreManifestRead -> InstallArtifactRead -> CodeArtifactStore -> IO StoreObservation
newCodeArtifactObservation limit alphabet readManifest readArtifact store =
    boundedObservationFor limit alphabet readManifest readArtifact store . readPlaneFor <$> newMaintenanceEnv store

-- | Probe upstream refill without constructing maintenance writes.
newCodeArtifactUpstreamProbe :: CodeArtifactStore -> IO UpstreamSafety
newCodeArtifactUpstreamProbe store = withReadableIdentity $ do
    env <- newMaintenanceEnv store
    probeUpstreamSafety (readPlaneFor env) store

-- | Build cache deletion with target-local reads and consent, allowing its declared refill role.
newCodeArtifactCacheMaintenance :: Int -> NameAlphabet -> StoreManifestRead -> InstallArtifactRead -> CodeArtifactStore -> IO StoreMaintenance
newCodeArtifactCacheMaintenance limit alphabet readManifest readArtifact store = do
    env <- newMaintenanceEnv store
    plane <- controlPlaneFor env
    pure (cacheMaintenanceFor limit alphabet readManifest readArtifact store plane)

-- | Observe the configured cache under its distinct refill classification without constructing writes.
newCodeArtifactCacheObservation :: Int -> NameAlphabet -> StoreManifestRead -> InstallArtifactRead -> CodeArtifactStore -> IO StoreObservation
newCodeArtifactCacheObservation limit alphabet readManifest readArtifact store = do
    env <- newMaintenanceEnv store
    let readCalls = readPlaneFor env
    pure (boundedObservationFor limit alphabet readManifest readArtifact store readCalls){obClassifyStore = cacheClassification readCalls store}

-- | Build the configured cache capability over injected calls without relaxing the mirror constructor.
cacheMaintenanceFor :: Int -> NameAlphabet -> StoreManifestRead -> InstallArtifactRead -> CodeArtifactStore -> ControlPlane -> StoreMaintenance
cacheMaintenanceFor limit alphabet readManifest readArtifact store plane =
    (boundedMaintenance limit alphabet readManifest readArtifact store plane){classifyStore = cacheClassification (cpRead plane) store, storeCursor = Nothing}

boundedMaintenance :: Int -> NameAlphabet -> StoreManifestRead -> InstallArtifactRead -> CodeArtifactStore -> ControlPlane -> StoreMaintenance
boundedMaintenance limit alphabet readManifest readArtifact store plane =
    (maintenanceFor alphabet readManifest readArtifact store plane)
        { enumerateVersions = obEnumerateVersions (boundedObservationFor limit alphabet readManifest readArtifact store (cpRead plane))
        }

cacheClassification :: ReadPlane -> CodeArtifactStore -> IO (Either StoreFault StoreClass)
cacheClassification readCalls store = fmap (const StoreDestroyable) <$> describeStore readCalls store

{- The env every maintenance call is sent over. Its manager drops http-client's hidden replay on a
reused connection, so an attempt the SDK did not make is not made below it either. -}
newMaintenanceEnv :: CodeArtifactStore -> IO AWS.Env
newMaintenanceEnv store = do
    env <- newAwsEnv (Just (casRegion store)) Nothing CA.defaultService
    manager <- newSingleAttemptManager
    pure env{AWS.manager = manager}

-- | Every call sent over one env, with the AWS error folded into a 'StoreFault'.
controlPlaneFor :: AWS.Env -> IO ControlPlane
controlPlaneFor env = do
    deletionManager <- newSingleAttemptManager
    pure
        ControlPlane
            { cpRead = readPlaneFor env
            , cpDeleteVersions = sendStore (singleAttemptEnv env{AWS.manager = deletionManager})
            , cpTagResource = sendStore env
            , cpUntagResource = sendStore env
            }

-- The observing calls alone, over one env, so a caller handed these can change nothing.
readPlaneFor :: AWS.Env -> ReadPlane
readPlaneFor env =
    ReadPlane
        { rpListPackages = sendStore env
        , rpListVersions = fmap listVersionsResult . sendClassified id env
        , rpDescribeRepository = sendStore env
        , rpDescribeUpstream = sendClassified describeUpstreamRefusal env
        , rpListTags = sendStore env
        }

-- | Supplied control-plane calls and the role-specific manifest reader provide every effect.
maintenanceFor :: NameAlphabet -> StoreManifestRead -> InstallArtifactRead -> CodeArtifactStore -> ControlPlane -> StoreMaintenance
maintenanceFor alphabet readManifest readArtifact store plane =
    maintenanceOf
        (observationFor alphabet readManifest readArtifact store (cpRead plane))
        StoreDeletion
            { dlDeleteVersions = deleteChunks plane store
            , dlCursor = Just (walkCursor alphabet plane store)
            }

-- | Bind observations to the supplied metadata and artifact readers.
observationFor :: NameAlphabet -> StoreManifestRead -> InstallArtifactRead -> CodeArtifactStore -> ReadPlane -> StoreObservation
observationFor alphabet readManifest readArtifact store observer =
    StoreObservation
        { obFacts = codeArtifactFacts alphabet store
        , obListPackagesIn = pageSource . packagePage observer store
        , obEnumerateVersions = pageAll . versionPage observer store
        , obReadArtifact = readArtifact
        , obReadManifest = readManifest
        , obVerifyConsent = readConsent observer store
        , obClassifyStore = fmap (fmap classifyRepository) (describeStore observer store)
        , obProbeUpstream = probeUpstreamSafety observer store
        }

-- | Walk upstreams under the existing bounds, using only this role's credentials.
probeUpstreamSafety :: ReadPlane -> CodeArtifactStore -> IO UpstreamSafety
probeUpstreamSafety observer store =
    withReadableIdentity (walkUpstreamChain linksOf (repositoryOfStore store))
  where
    linksOf repository =
        describedLinks <$> rpDescribeUpstream observer (describeUpstreamRequest store repository)

-- An answer that described no repository settled nothing, so the question stays open.
describedLinks :: Either UpstreamSafety CA.DescribeRepositoryResponse -> Either UpstreamSafety RepositoryLinks
describedLinks response = do
    described <- response
    maybe (Left (Undecidable NetworkFailure)) (Right . upstreamLinksOf) (described ^. CAL.describeRepositoryResponse_repository)

{- A service refusal comes back as a value, so a throw here is the identity itself: none was
discovered, or the one discovered could not be renewed. An identity that cannot ask fails closed. -}
withReadableIdentity :: IO UpstreamSafety -> IO UpstreamSafety
withReadableIdentity =
    fmap (fromRight (Unsafe (InsufficientPermissions describeRepositoryGrant))) . tryAny

-- | Build observation with version pagination bounded before another page is requested.
boundedObservationFor :: Int -> NameAlphabet -> StoreManifestRead -> InstallArtifactRead -> CodeArtifactStore -> ReadPlane -> StoreObservation
boundedObservationFor limit alphabet readManifest readArtifact store observer =
    (observationFor alphabet readManifest readArtifact store observer)
        { obEnumerateVersions = collectPagesBounded limit . pageSource . versionPage observer store
        }

sendStore :: (AWS.AWSRequest a) => AWS.Env -> a -> IO (Either StoreFault (AWS.AWSResponse a))
sendStore = sendClassified classifyStoreFault

packagePage :: ReadPlane -> CodeArtifactStore -> NamePrefix -> Maybe Text -> IO (Either StoreFault (Maybe Text, [PackageName]))
packagePage observer store prefix token =
    (>>= page) <$> rpListPackages observer (listPackagesRequest store prefix token)
  where
    page response = do
        names <-
            packagesOfPage
                (formatEcosystem (casFormat store))
                (fromMaybe [] (response ^. CAL.listPackagesResponse_packages))
        pure (response ^. CAL.listPackagesResponse_nextToken, names)

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

deleteChunks :: ControlPlane -> CodeArtifactStore -> DeleteGuard -> PackageName -> [Version] -> IO [(Version, VersionOutcome)]
deleteChunks plane store checks name versions =
    deleteAll checks send (chunksOfCeiling deleteCeiling versions)
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
