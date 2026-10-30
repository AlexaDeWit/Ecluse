-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | A CodeArtifact control plane over one fixed inventory, for driving the production maintenance
handle without AWS. Every native call waits a fixed delay, then answers one page of the size the
request asked for. The plane counts each call by kind, keeps the walk cursor tag, and refuses every
delete.
-}
module Ecluse.Runtime.Test.PackageStore.FixedPages (
    -- * The fixture
    FixedRepository (..),
    NativeCall (..),
    FixedPlane (..),
    newFixedPlane,
    fixedVersion,

    -- * The handles the composition root builds over it
    fixedMirror,
    fixedCache,

    -- * Response builders
    packagesPage,
    versionsPage,
    versionSummariesPage,
    taggedWith,
    markerTag,
    describing,
) where

import Amazonka.CodeArtifact qualified as CA
import Amazonka.CodeArtifact.Lens qualified as CAL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (NominalDiffTime)
import Lens.Micro ((.~), (?~), (^.))

import Ecluse.Core.Clock (monoSecondsBetween, monotonicNow, waitSeconds)
import Ecluse.Core.Registry.Maintenance (
    StoreFault,
    StoreMaintenance (enumerateVersions),
    StoreManifestRead,
    StoreObservation (obEnumerateVersions),
    protocolFault,
 )
import Ecluse.Core.Registry.Maintenance.NameSpace (NameAlphabet)
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (CodeArtifactStore (..), formatEcosystem)
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide.Internal (consentTagKey, consentTagValue, cursorTagKey)
import Ecluse.Runtime.Maintenance.CodeArtifact.Internal (ControlPlane (..), boundedObservationFor, cacheMaintenanceFor, maintenanceFor)
import Ecluse.Runtime.Maintenance.CodeArtifact.Read (ReadPlane (..))

-- | One repository's packages, the page size for a request without @maxResults@, and the wait before each answer.
data FixedRepository = FixedRepository
    { frStore :: CodeArtifactStore
    , frPackages :: Map Text Int
    -- ^ Each package's version count. 'fixedVersion' spells the versions.
    , frDefaultPageSize :: Int
    -- ^ The page size for a request without @maxResults@, for which AWS documents no default.
    , frDelay :: NominalDiffTime
    }

-- | One native call, named after the AWS operation. A package listing that names nothing counts apart.
data NativeCall
    = ListPackages
    | ListPackagesEmpty
    | ListPackageVersions
    | ReadManifest
    | DescribeRepository
    | ListTagsForResource
    | TagResource
    | UntagResource
    | DeletePackageVersions
    deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | The planes a handle is built over, and what they observed.
data FixedPlane = FixedPlane
    { fpRepository :: FixedRepository
    , fpControl :: ControlPlane
    , fpReadManifest :: StoreManifestRead -> StoreManifestRead
    -- ^ Delay and count one data-plane read around the caller's own decode.
    , fpCalls :: IO (Map NativeCall Int)
    , fpDelayed :: IO Double
    -- ^ Seconds of fixture delay served so far, apart from any quota or chunk wait.
    , fpCursorWrites :: IO [Text]
    -- ^ Every cursor bucket written, oldest first.
    , fpCursor :: IO (Maybe Text)
    }

data FixedState = FixedState
    { fsCalls :: IORef (Map NativeCall Int)
    , fsDelayed :: IORef Double
    , fsCursor :: IORef (Maybe Text)
    , fsWrites :: IORef [Text]
    }

-- | The version at one index, as the fixture lists it and as the synthetic documents spell it.
fixedVersion :: Int -> Text
fixedVersion index = "1.0." <> show index

-- | Build the planes over a fresh count, no cursor, and the operator's consent tag.
newFixedPlane :: FixedRepository -> IO FixedPlane
newFixedPlane repository = do
    cells <- FixedState <$> newIORef Map.empty <*> newIORef 0 <*> newIORef Nothing <*> newIORef []
    let answer :: NativeCall -> IO a -> IO a
        answer = serve repository cells
    pure
        FixedPlane
            { fpRepository = repository
            , fpControl =
                ControlPlane
                    { cpRead =
                        ReadPlane
                            { rpListPackages = listPackages repository cells
                            , rpListVersions = answer ListPackageVersions . pure . listVersions repository
                            , rpDescribeRepository = \_ -> answer DescribeRepository (pure (Right (described repository)))
                            , rpDescribeUpstream = \_ -> answer DescribeRepository (pure (Right (described repository)))
                            , rpListTags = \_ -> answer ListTagsForResource (Right . taggedWith . tagsHeld repository <$> readIORef (fsCursor cells))
                            }
                    , cpDeleteVersions = \_ -> answer DeletePackageVersions (pure (Left (protocolFault "the fixture refuses every delete")))
                    , cpTagResource = answer TagResource . writeCursor repository cells
                    , cpUntagResource = \_ -> answer UntagResource (Right (CA.newUntagResourceResponse 200) <$ writeIORef (fsCursor cells) Nothing)
                    }
            , fpReadManifest = \readManifest name -> answer ReadManifest (readManifest name)
            , fpCalls = readIORef (fsCalls cells)
            , fpDelayed = readIORef (fsDelayed cells)
            , fpCursorWrites = reverse <$> readIORef (fsWrites cells)
            , fpCursor = readIORef (fsCursor cells)
            }

-- | The mirror handle 'Ecluse.Runtime.Maintenance.CodeArtifact.newCodeArtifactMaintenance' builds, over the fixture.
fixedMirror :: Int -> NameAlphabet -> StoreManifestRead -> FixedPlane -> StoreMaintenance
fixedMirror limit alphabet readManifest plane =
    (maintenanceFor alphabet reading store control)
        { enumerateVersions = obEnumerateVersions (boundedObservationFor limit alphabet reading store (cpRead control))
        }
  where
    store = frStore (fpRepository plane)
    control = fpControl plane
    reading = fpReadManifest plane readManifest

-- | The cache handle 'Ecluse.Runtime.Maintenance.CodeArtifact.newCodeArtifactCacheMaintenance' builds, over the fixture.
fixedCache :: Int -> NameAlphabet -> StoreManifestRead -> FixedPlane -> StoreMaintenance
fixedCache limit alphabet readManifest plane =
    cacheMaintenanceFor limit alphabet (fpReadManifest plane readManifest) (frStore (fpRepository plane)) (fpControl plane)

-- Wait the fixture delay, count the call, then answer.
serve :: FixedRepository -> FixedState -> NativeCall -> IO a -> IO a
serve repository cells kind respond = do
    started <- monotonicNow
    waitSeconds (frDelay repository)
    ended <- monotonicNow
    modifyIORef' (fsDelayed cells) (+ monoSecondsBetween started ended)
    modifyIORef' (fsCalls cells) (Map.insertWith (+) kind 1)
    respond

listPackages :: FixedRepository -> FixedState -> CA.ListPackages -> IO (Either StoreFault CA.ListPackagesResponse)
listPackages repository cells request =
    serve repository cells (if null names then ListPackagesEmpty else ListPackages) $
        pure (packagesPage token names <$ addressed repository (request ^. CAL.listPackages_domain) (request ^. CAL.listPackages_repository))
  where
    prefix = fromMaybe "" (request ^. CAL.listPackages_packagePrefix)
    inBucket = Map.takeWhileAntitone (prefix `T.isPrefixOf`) (Map.dropWhileAntitone (< prefix) (frPackages repository))
    (token, names) = pageOf (pageSizeOf repository (request ^. CAL.listPackages_maxResults)) (request ^. CAL.listPackages_nextToken) (Map.keys inBucket)

-- A package the repository does not hold lists empty, as the leaf reads a missing package.
listVersions :: FixedRepository -> CA.ListPackageVersions -> Either StoreFault CA.ListPackageVersionsResponse
listVersions repository request = do
    addressed repository (request ^. CAL.listPackageVersions_domain) (request ^. CAL.listPackageVersions_repository)
    let count = Map.findWithDefault 0 (request ^. CAL.listPackageVersions_package) (frPackages repository)
    pure (uncurry versionsPage (pageOf (pageSizeOf repository (request ^. CAL.listPackageVersions_maxResults)) (request ^. CAL.listPackageVersions_nextToken) (map fixedVersion [0 .. count - 1])))

pageSizeOf :: FixedRepository -> Maybe Natural -> Int
pageSizeOf repository = maybe (frDefaultPageSize repository) fromIntegral

writeCursor :: FixedRepository -> FixedState -> CA.TagResource -> IO (Either StoreFault CA.TagResourceResponse)
writeCursor repository cells request = case find ((== cursorKey repository) . (^. CAL.tag_key)) (request ^. CAL.tagResource_tags) of
    Nothing -> pure (Left (protocolFault "the tag write carried no cursor"))
    Just tag -> do
        writeIORef (fsCursor cells) (Just (tag ^. CAL.tag_value))
        modifyIORef' (fsWrites cells) (tag ^. CAL.tag_value :)
        pure (Right (CA.newTagResourceResponse 200))

-- The page a continuation token starts, and the token of the page after it.
pageOf :: Int -> Maybe Text -> [a] -> (Maybe Text, [a])
pageOf size token items = (next, page)
  where
    offset = fromMaybe 0 (readMaybe . toString =<< token)
    page = take size (drop offset items)
    next = if offset + size < length items then Just (show (offset + size)) else Nothing

addressed :: FixedRepository -> Text -> Text -> Either StoreFault ()
addressed repository domain name =
    unless
        (domain == casDomain store && name == casRepository store)
        (Left (protocolFault "the call addressed another repository"))
  where
    store = frStore repository

described :: FixedRepository -> CA.DescribeRepositoryResponse
described repository =
    describing (CA.newRepositoryDescription & CAL.repositoryDescription_arn ?~ arn)
  where
    store = frStore repository
    arn = "arn:aws:codeartifact:" <> casRegion store <> ":" <> casDomainOwner store <> ":repository/" <> casDomain store <> "/" <> casRepository store

tagsHeld :: FixedRepository -> Maybe Text -> [CA.Tag]
tagsHeld repository cursor = markerTag : [CA.newTag (cursorKey repository) bucket | bucket <- maybeToList cursor]

cursorKey :: FixedRepository -> Text
cursorKey = cursorTagKey . formatEcosystem . casFormat . frStore

-- | A @ListPackages@ page naming these packages, continued by the token when there is one.
packagesPage :: Maybe Text -> [Text] -> CA.ListPackagesResponse
packagesPage token names =
    CA.newListPackagesResponse 200
        & (CAL.listPackagesResponse_nextToken .~ token)
        & (CAL.listPackagesResponse_packages ?~ [CA.newPackageSummary & CAL.packageSummary_package ?~ name | name <- names])

-- | A @ListPackageVersions@ page of published versions, continued by the token when there is one.
versionsPage :: Maybe Text -> [Text] -> CA.ListPackageVersionsResponse
versionsPage token raws = versionSummariesPage token [CA.newPackageVersionSummary raw CA.PackageVersionStatus_Published | raw <- raws]

-- | A @ListPackageVersions@ page of these entries, for a status or revision other than the default.
versionSummariesPage :: Maybe Text -> [CA.PackageVersionSummary] -> CA.ListPackageVersionsResponse
versionSummariesPage token summaries =
    CA.newListPackageVersionsResponse 200
        & (CAL.listPackageVersionsResponse_nextToken .~ token)
        & (CAL.listPackageVersionsResponse_versions ?~ summaries)

-- | A @ListTagsForResource@ answer carrying these tags.
taggedWith :: [CA.Tag] -> CA.ListTagsForResourceResponse
taggedWith tags = CA.newListTagsForResourceResponse 200 & (CAL.listTagsForResourceResponse_tags ?~ tags)

-- | The operator's consent tag.
markerTag :: CA.Tag
markerTag = CA.newTag consentTagKey consentTagValue

-- | A @DescribeRepository@ answer carrying the given description.
describing :: CA.RepositoryDescription -> CA.DescribeRepositoryResponse
describing description =
    CA.newDescribeRepositoryResponse 200 & (CAL.describeRepositoryResponse_repository ?~ description)
