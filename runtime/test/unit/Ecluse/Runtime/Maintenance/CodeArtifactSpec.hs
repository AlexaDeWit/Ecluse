-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | CodeArtifact maintenance requests and their sweep callers over a recording control plane.
module Ecluse.Runtime.Maintenance.CodeArtifactSpec (spec) where

import Data.List (lookup)
import Data.Text qualified as T
import Lens.Micro ((.~), (?~), (^.))
import Network.HTTP.Types (Status, status403, status503)
import Test.Hspec
import UnliftIO.Exception (throwIO)

import Amazonka qualified as AWS
import Amazonka.Auth (fromKeys)
import Amazonka.CodeArtifact qualified as CA
import Amazonka.CodeArtifact.Lens qualified as CAL

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Fault (TransportCause (TransportProtocol), tfDetail, transportFault)
import Ecluse.Core.Package (PackageName, mkPackageName, renderPackageName)
import Ecluse.Core.Registry.Maintenance (
    CompletionNotion (CompletesOnCall),
    ConsentVerdict (ConsentGranted, ConsentWithheld),
    DeleteCeiling (AtMost),
    RefillPosture (RefillPermitted),
    RetryAdvice (RetryFutile),
    StoreClass (StoreDestroyable, StorePreserved),
    StoreCursor (..),
    StoreFacts (..),
    StoreFault (..),
    StoreMaintenance (..),
    StoreManifestRead,
    StoreObservation (obEnumerateVersions, obListPackagesIn, obProbeUpstream),
    StoredVersion (..),
    VersionOutcome (VersionRefused, VersionRemoved, VersionUncertain, VersionUnreached),
    VersionPresence (VersionServed),
    chunksOfCeiling,
    collectPages,
    refusalCode,
 )
import Ecluse.Core.Registry.Maintenance.NameSpace (
    NameAlphabet,
    mkNameAlphabet,
    renderNamePrefix,
 )
import Ecluse.Core.Registry.Maintenance.Upstream (
    ExternalConnection (ExternalConnection),
    RepositoryName (RepositoryName),
    UndecidabilityReason (ChainBoundExceeded, NetworkFailure),
    UnsafeReason (ConfigurationEvidence, InsufficientPermissions),
    UpstreamSafety (Safe, Undecidable, Unsafe),
    upstreamCallCeiling,
    upstreamHopCeiling,
 )
import Ecluse.Core.Registry.Sweep.Outcome (CycleHalt (HaltDeletionCap, HaltStoreFault))
import Ecluse.Core.Registry.Sweep.Package (sweepPackageGroup)
import Ecluse.Core.Registry.Sweep.Types (SweepMount (smStore), SweepPacing (swpDeletionCap), SweepState (stIssued), newSweepState)
import Ecluse.Core.Telemetry.Metrics (SweepResult (SweepDeleted, SweepExamined, SweepKept))
import Ecluse.Core.Version (Version, mkVersion, renderVersion)
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide.Internal (
    CodeArtifactStore (..),
    codeArtifactFormat,
    consentTagKey,
    consentTagValue,
    cursorTagKey,
    describeRepositoryGrant,
    describeUpstreamRefusal,
 )
import Ecluse.Runtime.Maintenance.CodeArtifact.Internal (
    ControlPlane (..),
    boundedObservationFor,
    cacheMaintenanceFor,
    controlPlaneFor,
    maintenanceFor,
    observationFor,
    probeUpstreamSafety,
 )
import Ecluse.Runtime.Maintenance.CodeArtifact.Read (ReadPlane (..))
import Ecluse.Test.Maintenance (testDeleteGuard, withBucket)
import Ecluse.Test.Package (sampleManifest)
import Ecluse.Test.Rules (denyRule)
import Ecluse.Test.Sweep (RecordedSweep (recPorts, recResults), recordingPorts, testMount, testPacing)

{- | The CodeArtifact handle's facts and the sequencing around its calls, driven over 'ControlPlane'
answers built from @amazonka@'s own types. Each decision is covered in "Ecluse.Runtime.Maintenance.CodeArtifact.DecideSpec".
-}
spec :: Spec
spec = maybe noNpmFormat handleCases npmStore

noNpmFormat :: Spec
noNpmFormat = it "has a CodeArtifact format for npm" $ expectationFailure "npm resolved to no CodeArtifact format"

handleCases :: CodeArtifactStore -> Spec
handleCases store = do
    factCases store
    enumerationCases store
    deleteCases store
    consentCases store
    classificationCases store
    upstreamCases store
    cursorCases store

factCases :: CodeArtifactStore -> Spec
factCases store = describe "the CodeArtifact handle's standing facts" $ do
    it "names the backend the Dredger's boot line records" $ do
        facts <- factsFor store
        factBackend facts `shouldBe` "codeArtifact"

    it "accepts 100 versions per destructive call" $ do
        facts <- factsFor store
        factDeleteCeiling facts `shouldBe` AtMost 100

    it "records that CodeArtifact re-admits a version published again after a delete" $ do
        facts <- factsFor store
        factRefill facts `shouldBe` RefillPermitted

    it "records that the delete is done by the time the call answers" $ do
        facts <- factsFor store
        factCompletion facts `shouldBe` CompletesOnCall

    it "carries the alphabet it was built with, which is the mount ecosystem's own" $ do
        facts <- factsFor store
        factNameAlphabet facts `shouldBe` testAlphabet

    it "reads a manifest through the read it was handed, never through the control plane" $ do
        outcome <- readStoreManifest (handleOver store inertPlane) aPackage
        fmap detailOf (leftToMaybe outcome) `shouldBe` Just "the spec wired no manifest read"

    it "enumerates over a read plane alone, which carries no call that changes the repository" $ do
        answer <- answersFrom [packagesPage Nothing ["lodash"]]
        let observed = observationFor testAlphabet unwiredRead store inertReader{rpListPackages = const answer}
        outcome <- withBucket "" (collectPages . obListPackagesIn observed)
        fmap (map renderPackageName) outcome `shouldBe` Right ["lodash"]

enumerationCases :: CodeArtifactStore -> Spec
enumerationCases store = describe "the handle's paged enumerations" $ do
    it "pages the package listing to exhaustion, sending back the token the last page returned" $ do
        tokens <- newIORef []
        answer <- answersFrom [packagesPage (Just "p2") ["lodash"], packagesPage Nothing ["axios"]]
        let plane = reading inertReader{rpListPackages = \request -> record tokens (request ^. CAL.listPackages_nextToken) >> answer}
        outcome <- listBucket store plane ""
        fmap (map renderPackageName) outcome `shouldBe` Right ["lodash", "axios"]
        readIORef tokens `shouldReturn` [Nothing, Just "p2"]

    it "reads a package page carrying no packages field as an empty page" $ do
        let plane = reading inertReader{rpListPackages = \_ -> pure (Right (CA.newListPackagesResponse 200))}
        listBucket store plane "" `shouldReturn` Right []

    it "sends the bucket as the listing's own package prefix, so the store does the filtering" $ do
        prefixes <- newIORef []
        answer <- answersFrom [packagesPage Nothing ["lodash"]]
        let plane = reading inertReader{rpListPackages = \request -> record prefixes (request ^. CAL.listPackages_packagePrefix) >> answer}
        _ <- listBucket store plane "l"
        readIORef prefixes `shouldReturn` [Just "l"]

    it "sends no prefix at all for the bucket that covers the whole store" $ do
        prefixes <- newIORef []
        answer <- answersFrom [packagesPage Nothing ["lodash"]]
        let plane = reading inertReader{rpListPackages = \request -> record prefixes (request ^. CAL.listPackages_packagePrefix) >> answer}
        _ <- listBucket store plane ""
        readIORef prefixes `shouldReturn` [Nothing]

    it "pages a package's versions to exhaustion, sending back the token the last page returned" $ do
        tokens <- newIORef []
        answer <- answersFrom [versionsPage (Just "v2") ["1.0.0"], versionsPage Nothing ["1.1.0"]]
        let plane = reading inertReader{rpListVersions = \request -> record tokens (request ^. CAL.listPackageVersions_nextToken) >> answer}
        outcome <- enumerateVersions (handleOver store plane) aPackage
        outcome `shouldBe` Right [served "1.0.0", served "1.1.0"]
        readIORef tokens `shouldReturn` [Nothing, Just "v2"]

    it "reads a version page carrying no versions field as an empty page" $ do
        let plane = reading inertReader{rpListVersions = \_ -> pure (Right (CA.newListPackageVersionsResponse 200))}
        enumerateVersions (handleOver store plane) aPackage `shouldReturn` Right []

    it "stops preview pagination at the version bound without requesting a later page" $ do
        tokens <- newIORef []
        answer <- answersFrom [versionsPage (Just "v2") ["1.0.0"], versionsPage (Just "v3") ["1.1.0"], versionsPage Nothing ["1.2.0"]]
        let versionReader = inertReader{rpListVersions = \request -> record tokens (request ^. CAL.listPackageVersions_nextToken) >> answer}
            observation = boundedObservationFor 1 (mkNameAlphabet "abc") (\_ -> fail "inventory does not read metadata") store versionReader
        result <- obEnumerateVersions observation aPackage
        result `shouldSatisfy` isLeft
        readIORef tokens `shouldReturn` [Nothing, Just "v2"]

deleteCases :: CodeArtifactStore -> Spec
deleteCases store = describe "the handle's chunked delete" $ do
    for_ [1, 2 :: Int] $ \faultAt ->
        for_ [False, True] $ \throughSweep ->
            it ("stops after CodeArtifact chunk fault " <> show faultAt <> ", through sweep: " <> show throughSweep) $ do
                let count = faultAt * 100 + 1
                    versions = versionRun count
                    successful = (faultAt - 1) * 100
                requests <- newIORef []
                let plane =
                        (reading (stillHolding versions))
                            { cpDeleteVersions = \request -> do
                                let submitted = request ^. CAL.deletePackageVersions_versions
                                record requests submitted
                                issued <- length <$> readIORef requests
                                pure (if issued == faultAt then Left storeUnreachable else Right (allRemoved submitted))
                            }
                    handle = (handleOver store plane){readStoreManifest = \_ -> pure (Right (sampleManifest aPackage versions))}
                outcomes <-
                    if throughSweep
                        then do
                            recorded <- newIORef []
                            rec' <- recordingPorts Nothing
                            counters <- newSweepState
                            let tracked =
                                    handle
                                        { deleteVersions = \checks name selected -> do
                                            result <- deleteVersions handle checks name selected
                                            writeIORef recorded result
                                            pure result
                                        }
                            -- The configured cap must exceed the default 100 to reach a second backend chunk.
                            let swept = testMount tracked [denyRule] []
                            halt <-
                                sweepPackageGroup
                                    testPacing{swpDeletionCap = count}
                                    (recPorts rec')
                                    counters
                                    swept
                                    aPackage
                                    [(smStore swept, [StoredVersion v VersionServed Nothing | v <- versions])]
                            {- The fault abandons the last chunk before its recheck, so the cap charges
                            every version the backend was handed and none it never reached. -}
                            readIORef (stIssued counters) `shouldReturn` (count - 1)
                            halt `shouldSatisfy` \case
                                Just (HaltStoreFault Npm _ detail) -> "cleanup remains incomplete" `T.isInfixOf` detail
                                _ -> False
                            recResults rec'
                                `shouldReturn` (replicate count SweepExamined <> replicate successful SweepDeleted <> replicate (count - successful) SweepKept)
                            readIORef recorded
                        else deleteVersions handle testDeleteGuard aPackage versions
                readIORef requests `shouldReturn` map (map renderVersion) (take faultAt (chunksOfCeiling (AtMost 100) versions))
                outcomes `shouldBe` zip versions (replicate successful VersionRemoved <> replicate (min 100 (count - successful)) (VersionUncertain storeUnreachable) <> replicate (max 0 (count - successful - 100)) (VersionUnreached storeUnreachable))

    it "continues the sweep after per-version refusals in an earlier chunk" $ do
        requests <- newIORef []
        let versions = versionRun 101
            plane =
                (reading (stillHolding versions))
                    { cpDeleteVersions = \request -> do
                        let submitted = request ^. CAL.deletePackageVersions_versions
                        record requests submitted
                        pure (Right (if length submitted == 100 then CA.newDeletePackageVersionsResponse 200 else allRemoved submitted))
                    }
            handle = (handleOver store plane){readStoreManifest = \_ -> pure (Right (sampleManifest aPackage versions))}
        rec' <- recordingPorts Nothing
        counters <- newSweepState
        let swept = testMount handle [denyRule] []
        sweepPackageGroup
            testPacing{swpDeletionCap = 101}
            (recPorts rec')
            counters
            swept
            aPackage
            [(smStore swept, [StoredVersion v VersionServed Nothing | v <- versions])]
            `shouldReturn` Just (HaltDeletionCap 101 101 Nothing)
        map length <$> readIORef requests `shouldReturn` [100, 1]
        recResults rec' `shouldReturn` (replicate 101 SweepExamined <> replicate 100 SweepKept <> [SweepDeleted])

    it "splits 101 versions into a call of 100 and a call of 1, and reports one outcome each" $ do
        sizes <- newIORef []
        let plane =
                inertPlane
                    { cpDeleteVersions = \request -> do
                        let submitted = request ^. CAL.deletePackageVersions_versions
                        record sizes (length submitted)
                        pure (Right (allRemoved submitted))
                    }
        outcomes <- deleteVersions (handleOver store plane) testDeleteGuard aPackage (versionRun 101)
        readIORef sizes `shouldReturn` [100, 1]
        map snd outcomes `shouldBe` replicate 101 VersionRemoved

    it "refuses a version the store answered for neither way, never reports it removed" $ do
        let plane = inertPlane{cpDeleteVersions = \_ -> pure (Right (CA.newDeletePackageVersionsResponse 200))}
        outcomes <- deleteVersions (handleOver store plane) testDeleteGuard aPackage (versionRun 2)
        map (refusalCodeOf . snd) outcomes `shouldBe` replicate 2 (Just "UNREPORTED")

    it "stops at the first faulted chunk and marks every submitted version unreached" $ do
        calls <- newIORef (0 :: Int)
        let plane = inertPlane{cpDeleteVersions = \_ -> modifyIORef' calls (+ 1) >> pure (Left storeUnreachable)}
        outcomes <- deleteVersions (handleOver store plane) testDeleteGuard aPackage (versionRun 101)
        readIORef calls `shouldReturn` 1
        map snd outcomes `shouldBe` replicate 100 (VersionUncertain storeUnreachable) <> [VersionUnreached storeUnreachable]

consentCases :: CodeArtifactStore -> Spec
consentCases store = describe "the handle's consent read" $ do
    it "describes the repository before it reads the tags, because a tag read is addressed by ARN" $ do
        calls <- newIORef []
        verdict <- consentUnder store calls (Right describedWithArn) (Right (taggedWith [markerTag]))
        readIORef calls `shouldReturn` ["describe", "tags"]
        verdict `shouldBe` Right ConsentGranted

    it "withholds consent from a repository carrying no marker tag" $ do
        calls <- newIORef []
        verdict <- consentUnder store calls (Right describedWithArn) (Right (taggedWith []))
        verdict `shouldSatisfy` either (const False) withheld

    it "reports a describe that did not land, and reads no tags after it" $ do
        calls <- newIORef []
        verdict <- consentUnder store calls (Left storeUnreachable) (Right (taggedWith [markerTag]))
        verdict `shouldBe` Left storeUnreachable
        readIORef calls `shouldReturn` ["describe"]

    it "refuses a description carrying no ARN rather than read tags off an invented one" $ do
        calls <- newIORef []
        verdict <- consentUnder store calls (Right describedWithoutArn) (Right (taggedWith [markerTag]))
        first detailOf verdict `shouldBe` Left "the store described the repository without an ARN"
        readIORef calls `shouldReturn` ["describe"]

classificationCases :: CodeArtifactStore -> Spec
classificationCases store = describe "the handle's store classification" $ do
    it "permits refill only through the separate cache capability" $ do
        let plane = reading inertReader{rpDescribeRepository = \_ -> pure (Right (describing routedDescription))}
            cache = cacheMaintenanceFor 100 testAlphabet unwiredRead store plane
        classifyStore cache `shouldReturn` Right StoreDestroyable
        classifyStore (handleOver store plane) >>= (`shouldSatisfy` either (const False) (preservedNaming "shared"))

    it "classifies a repository holding only what was published to it as destroyable" $
        classifyUnder store (Right describedWithArn) `shouldReturn` Right StoreDestroyable

    it "names the upstream that would serve a deleted version again" $ do
        verdict <- classifyUnder store (Right (describing routedDescription))
        verdict `shouldSatisfy` either (const False) (preservedNaming "shared")

    it "reports a describe that did not land" $
        classifyUnder store (Left storeUnreachable) `shouldReturn` Left storeUnreachable

    it "refuses a response describing no repository rather than invent a verdict for one" $ do
        verdict <- classifyUnder store (Right (CA.newDescribeRepositoryResponse 200))
        first detailOf verdict `shouldBe` Left "the store described no repository"

{- The probe of a private upstream, over describe answers seeded per repository, so the walk's
own bounds and the refusals it reads are drivable without a repository. -}
upstreamCases :: CodeArtifactStore -> Spec
upstreamCases store = describe "the handle's private upstream probe" $ do
    it "reads a repository that aggregates nothing as safe" $
        probeOver store [("mirror", CA.newRepositoryDescription)] `shouldReturn` Safe

    it "reports the repository's own connection to a public registry" $
        probeOver store [("mirror", connectedTo "public:npmjs")]
            `shouldReturn` Unsafe (ConfigurationEvidence (RepositoryName "mirror") (ExternalConnection "public:npmjs"))

    it "reports a connection carried by a repository further along the chain" $
        probeOver store [("mirror", routedTo ["shared"]), ("shared", connectedTo "public:npmjs")]
            `shouldReturn` Unsafe (ConfigurationEvidence (RepositoryName "shared") (ExternalConnection "public:npmjs"))

    it "terminates on a cycle rather than describing a repository twice" $
        probeOver store [("mirror", routedTo ["shared"]), ("shared", routedTo ["mirror"])] `shouldReturn` Safe

    it "leaves a chain deeper than the hop ceiling undecided, never safe" $
        probeOver store (chainOf (upstreamHopCeiling + 2)) `shouldReturn` Undecidable ChainBoundExceeded

    it "leaves a chain wider than the call ceiling undecided, never safe" $
        probeOver store (fanOf (upstreamCallCeiling + 5)) `shouldReturn` Undecidable ChainBoundExceeded

    it "reads a refused identity as unsafe, because it cannot clear the repository" $
        probeRefusing store (serviceError status403 "AccessDeniedException")
            `shouldReturn` Unsafe (InsufficientPermissions describeRepositoryGrant)

    it "leaves a faulted call undecided" $
        probeRefusing store (serviceError status503 "ServiceUnavailable") `shouldReturn` Undecidable NetworkFailure

    it "reads an identity that could not ask at all as unsafe, which fails closed" $
        -- A service refusal comes back as a value, so a throw is the identity: none was discovered,
        -- or the one discovered could not be renewed.
        probeUpstreamSafety inertReader{rpDescribeUpstream = \_ -> throwIO NoIdentity} store
            `shouldReturn` Unsafe (InsufficientPermissions describeRepositoryGrant)

    it "leaves an answer that described no repository undecided" $
        probeOver store [] `shouldReturn` Undecidable NetworkFailure

    it "offers the probe on the whole handle and on the observing calls alike" $ do
        let plane = reading inertReader{rpDescribeUpstream = \_ -> pure (Right (describing CA.newRepositoryDescription))}
        probeUpstream (handleOver store plane) `shouldReturn` Safe
        obProbeUpstream (observationFor testAlphabet unwiredRead store (cpRead plane)) `shouldReturn` Safe

-- Probe over describes seeded per repository. A repository absent here is one the store answered nothing for.
probeOver :: CodeArtifactStore -> [(Text, CA.RepositoryDescription)] -> IO UpstreamSafety
probeOver store chain = probeUpstreamSafety inertReader{rpDescribeUpstream = answer} store
  where
    answer request =
        pure . Right . maybe (CA.newDescribeRepositoryResponse 200) describing $
            lookup (request ^. CAL.describeRepository_repository) chain

-- Probe over a call the service refused, read through the leaf's own classification of that refusal.
probeRefusing :: CodeArtifactStore -> AWS.Error -> IO UpstreamSafety
probeRefusing store err =
    probeUpstreamSafety inertReader{rpDescribeUpstream = \_ -> pure (Left (describeUpstreamRefusal err))} store

serviceError :: Status -> Text -> AWS.Error
serviceError status code =
    AWS.ServiceError (AWS.ServiceError' "CodeArtifact" status [] (AWS.newErrorCode code) Nothing Nothing)

-- | The typed stand-in for a client whose identity could not be discovered or renewed.
data NoIdentity = NoIdentity
    deriving stock (Show)

instance Exception NoIdentity

connectedTo :: Text -> CA.RepositoryDescription
connectedTo connection =
    CA.newRepositoryDescription
        & (CAL.repositoryDescription_externalConnections ?~ [CA.newRepositoryExternalConnectionInfo & (CAL.repositoryExternalConnectionInfo_externalConnectionName ?~ connection)])

routedTo :: [Text] -> CA.RepositoryDescription
routedTo upstreams =
    CA.newRepositoryDescription
        & (CAL.repositoryDescription_upstreams ?~ [CA.newUpstreamRepositoryInfo & (CAL.upstreamRepositoryInfo_repositoryName ?~ name) | name <- upstreams])

-- A chain of the given length from the store's own repository, each forwarding to the next.
chainOf :: Int -> [(Text, CA.RepositoryDescription)]
chainOf depth = [("mirror", routedTo ["hop1"])] <> [(hop n, routedTo [hop (n + 1)]) | n <- [1 .. depth]]
  where
    hop n = "hop" <> show n

-- The store's own repository forwarding to the given number of others, each aggregating nothing.
fanOf :: Int -> [(Text, CA.RepositoryDescription)]
fanOf width = [("mirror", routedTo (map leaf [1 .. width]))] <> [(leaf n, CA.newRepositoryDescription) | n <- [1 .. width]]
  where
    leaf n = "leaf" <> show n

cursorCases :: CodeArtifactStore -> Spec
cursorCases store = describe "the handle's walk cursor" $ do
    it "offers one, because a repository tag is somewhere to keep it" $
        isJust (storeCursor (handleOver store inertPlane)) `shouldBe` True

    it "reads back the bucket the cursor tag records, describing the repository first" $ do
        calls <- newIORef []
        let plane =
                reading
                    inertReader
                        { rpDescribeRepository = \_ -> record calls "describe" >> pure (Right describedWithArn)
                        , rpListTags = \_ -> record calls "tags" >> pure (Right (taggedWith [markerTag, cursorTag "l"]))
                        }
        withCursor store plane $ \cursor -> do
            outcome <- readCursor cursor
            fmap (fmap renderNamePrefix) outcome `shouldBe` Right (Just "l")
            readIORef calls `shouldReturn` (["describe", "tags"] :: [Text])

    it "reads no cursor from a repository carrying the consent tag alone" $ do
        let plane =
                reading
                    inertReader
                        { rpDescribeRepository = \_ -> pure (Right describedWithArn)
                        , rpListTags = \_ -> pure (Right (taggedWith [markerTag]))
                        }
        withCursor store plane $ \cursor -> readCursor cursor `shouldReturn` Right Nothing

    it "writes exactly the one cursor key, so the consent tag stays out of its reach" $ do
        written <- newIORef []
        let plane =
                (reading inertReader{rpDescribeRepository = \_ -> pure (Right describedWithArn)})
                    { cpTagResource = \request -> do
                        record written (map (^. CAL.tag_key) (request ^. CAL.tagResource_tags))
                        pure (Right (CA.newTagResourceResponse 200))
                    }
        withBucket "l" $ \completed -> withCursor store plane $ \cursor -> do
            writeCursor cursor completed `shouldReturn` Right ()
            readIORef written `shouldReturn` [[cursorTagKey Npm]]
            cursorTagKey Npm `shouldNotBe` consentTagKey

    it "clears the walk by removing that one key and no other" $ do
        removed <- newIORef []
        let plane =
                (reading inertReader{rpDescribeRepository = \_ -> pure (Right describedWithArn)})
                    { cpUntagResource = \request -> do
                        record removed (request ^. CAL.untagResource_tagKeys)
                        pure (Right (CA.newUntagResourceResponse 200))
                    }
        withCursor store plane $ \cursor -> do
            clearCursor cursor `shouldReturn` Right ()
            readIORef removed `shouldReturn` [[cursorTagKey Npm]]

    it "reports a describe that did not land, and writes nothing after it" $ do
        let plane = reading inertReader{rpDescribeRepository = \_ -> pure (Left storeUnreachable)}
        withBucket "l" $ \completed -> withCursor store plane $ \cursor ->
            writeCursor cursor completed `shouldReturn` Left storeUnreachable

    it "refuses a description carrying no ARN rather than address a tag call to an invented one" $ do
        let plane = reading inertReader{rpDescribeRepository = \_ -> pure (Right describedWithoutArn)}
        withCursor store plane $ \cursor ->
            first detailOf <$> readCursor cursor
                `shouldReturn` Left "the store described the repository without an ARN"

    it "reports a tag read that did not land" $ do
        let plane =
                reading
                    inertReader
                        { rpDescribeRepository = \_ -> pure (Right describedWithArn)
                        , rpListTags = \_ -> pure (Left storeUnreachable)
                        }
        withCursor store plane $ \cursor -> readCursor cursor `shouldReturn` Left storeUnreachable

    it "reports a cursor write that did not land" $ do
        let plane =
                (reading inertReader{rpDescribeRepository = \_ -> pure (Right describedWithArn)})
                    { cpTagResource = \_ -> pure (Left storeUnreachable)
                    }
        withBucket "l" $ \completed -> withCursor store plane $ \cursor ->
            writeCursor cursor completed `shouldReturn` Left storeUnreachable

    it "reports a clear that did not land, so a halted walk keeps the cursor it had" $ do
        let plane =
                (reading inertReader{rpDescribeRepository = \_ -> pure (Right describedWithArn)})
                    { cpUntagResource = \_ -> pure (Left storeUnreachable)
                    }
        withCursor store plane $ \cursor -> clearCursor cursor `shouldReturn` Left storeUnreachable

{- Run one cursor call over a wired plane. The handle offers a cursor on every CodeArtifact
repository, so a case that finds none has found a regression rather than a backend arm. -}
withCursor :: CodeArtifactStore -> ControlPlane -> (StoreCursor -> Expectation) -> Expectation
withCursor store plane act = case storeCursor (handleOver store plane) of
    Nothing -> expectationFailure "the CodeArtifact handle offers a walk cursor"
    Just cursor -> act cursor

cursorTag :: Text -> CA.Tag
cursorTag = CA.newTag (cursorTagKey Npm)

-- | Read the consent verdict over a plane that records the order of the two calls.
consentUnder ::
    CodeArtifactStore ->
    IORef [Text] ->
    Either StoreFault CA.DescribeRepositoryResponse ->
    Either StoreFault CA.ListTagsForResourceResponse ->
    IO (Either StoreFault ConsentVerdict)
consentUnder store calls described tagged =
    verifyConsent . handleOver store . reading $
        inertReader
            { rpDescribeRepository = \_ -> record calls "describe" >> pure described
            , rpListTags = \_ -> record calls "tags" >> pure tagged
            }

-- | Classify the store over a plane whose describe call answers with the given outcome.
classifyUnder :: CodeArtifactStore -> Either StoreFault CA.DescribeRepositoryResponse -> IO (Either StoreFault StoreClass)
classifyUnder store described =
    classifyStore (handleOver store (reading inertReader{rpDescribeRepository = \_ -> pure described}))

{- Every call answers with a fault naming itself, so a case wires only the fields it drives and a
call it did not expect reads as a failure rather than a silent success. -}
inertPlane :: ControlPlane
inertPlane =
    ControlPlane
        { cpRead = inertReader
        , cpDeleteVersions = unexpected "DeletePackageVersions"
        , cpTagResource = unexpected "TagResource"
        , cpUntagResource = unexpected "UntagResource"
        }

inertReader :: ReadPlane
inertReader =
    ReadPlane
        { rpListPackages = unexpected "ListPackages"
        , rpListVersions = unexpected "ListPackageVersions"
        , rpDescribeRepository = unexpected "DescribeRepository"
        , rpDescribeUpstream = \_ -> fail "the spec wired no DescribeRepository answer for the probe"
        , rpListTags = unexpected "ListTagsForResource"
        }

unexpected :: Text -> a -> IO (Either StoreFault b)
unexpected name _ = pure (Left (faultSaying ("the spec wired no " <> name <> " answer")))

-- The inert plane with its reads replaced, which is how a case wires one read call.
reading :: ReadPlane -> ControlPlane
reading observer = inertPlane{cpRead = observer}

{- The reads a grouped sweep makes before each batch: the inventory it reassesses against and the
two standing permissions. It keeps every version, so the confirmation reports an incomplete cleanup. -}
stillHolding :: [Version] -> ReadPlane
stillHolding versions =
    inertReader
        { rpListVersions = \_ -> pure (Right (versionsPage Nothing (map renderVersion versions)))
        , rpDescribeRepository = \_ -> pure (Right describedWithArn)
        , rpListTags = \_ -> pure (Right (taggedWith [markerTag]))
        }

-- Answer from a fixed sequence, one response per call, so a paging walk is drivable.
answersFrom :: [a] -> IO (IO (Either StoreFault a))
answersFrom responses = do
    remaining <- newIORef responses
    pure . atomicModifyIORef' remaining $ \case
        [] -> ([], Left (faultSaying "the spec ran out of responses"))
        (response : rest) -> (rest, Right response)

record :: IORef [a] -> a -> IO ()
record ref value = modifyIORef' ref (<> [value])

packagesPage :: Maybe Text -> [Text] -> CA.ListPackagesResponse
packagesPage token names =
    CA.newListPackagesResponse 200
        & (CAL.listPackagesResponse_nextToken .~ token)
        & (CAL.listPackagesResponse_packages ?~ [CA.newPackageSummary & CAL.packageSummary_package ?~ name | name <- names])

versionsPage :: Maybe Text -> [Text] -> CA.ListPackageVersionsResponse
versionsPage token raws =
    CA.newListPackageVersionsResponse 200
        & (CAL.listPackageVersionsResponse_nextToken .~ token)
        & (CAL.listPackageVersionsResponse_versions ?~ [CA.newPackageVersionSummary raw CA.PackageVersionStatus_Published | raw <- raws])

allRemoved :: [Text] -> CA.DeletePackageVersionsResponse
allRemoved raws =
    CA.newDeletePackageVersionsResponse 200
        & (CAL.deletePackageVersionsResponse_successfulVersions ?~ fromList [(raw, CA.newSuccessfulPackageVersionInfo) | raw <- raws])

taggedWith :: [CA.Tag] -> CA.ListTagsForResourceResponse
taggedWith tags = CA.newListTagsForResourceResponse 200 & (CAL.listTagsForResourceResponse_tags ?~ tags)

markerTag :: CA.Tag
markerTag = CA.newTag consentTagKey consentTagValue

describing :: CA.RepositoryDescription -> CA.DescribeRepositoryResponse
describing description =
    CA.newDescribeRepositoryResponse 200 & (CAL.describeRepositoryResponse_repository ?~ description)

describedWithArn :: CA.DescribeRepositoryResponse
describedWithArn =
    describing (CA.newRepositoryDescription & CAL.repositoryDescription_arn ?~ "arn:aws:codeartifact:::repository/acme/mirror")

describedWithoutArn :: CA.DescribeRepositoryResponse
describedWithoutArn = describing CA.newRepositoryDescription

routedDescription :: CA.RepositoryDescription
routedDescription =
    CA.newRepositoryDescription
        & (CAL.repositoryDescription_upstreams ?~ [CA.newUpstreamRepositoryInfo & CAL.upstreamRepositoryInfo_repositoryName ?~ "shared"])

-- What a fault says, so an assertion reads the refusal rather than only that one happened.
detailOf :: StoreFault -> Text
detailOf = tfDetail . faultTransport

refusalCodeOf :: VersionOutcome -> Maybe Text
refusalCodeOf = \case
    VersionRefused refusal -> Just (refusalCode refusal)
    _ -> Nothing

served :: Text -> StoredVersion
served raw = StoredVersion{storedVersion = mkVersion Npm raw, storedPresence = VersionServed, storedRevision = Nothing}

withheld :: ConsentVerdict -> Bool
withheld = \case
    ConsentWithheld _ -> True
    ConsentGranted -> False

preservedNaming :: Text -> StoreClass -> Bool
preservedNaming named = \case
    StorePreserved reason -> named `T.isInfixOf` reason
    StoreDestroyable -> False

storeUnreachable :: StoreFault
storeUnreachable = faultSaying "the store did not answer"

faultSaying :: Text -> StoreFault
faultSaying detail =
    StoreFault{faultTransport = transportFault TransportProtocol detail, faultRetry = RetryFutile}

aPackage :: PackageName
aPackage = mkPackageName Npm Nothing "lodash"

versionRun :: Int -> [Version]
versionRun n = [mkVersion Npm ("1.0." <> show i) | i <- [1 .. n]]

factsFor :: CodeArtifactStore -> IO StoreFacts
factsFor store = storeFacts <$> handleFor store

{- A two-character alphabet over the names these cases seed, so the bucket the handle sends is
readable without this spec knowing an ecosystem's grammar. -}
testAlphabet :: NameAlphabet
testAlphabet = mkNameAlphabet "al"

handleOver :: CodeArtifactStore -> ControlPlane -> StoreMaintenance
handleOver = maintenanceFor testAlphabet unwiredRead

{- The manifest read is the composition root's, not this leaf's, so these cases hand it one that
reports being unwired rather than one that reaches a store. -}
unwiredRead :: StoreManifestRead
unwiredRead _ = pure (Left (faultSaying "the spec wired no manifest read"))

listBucket :: CodeArtifactStore -> ControlPlane -> Text -> IO (Either StoreFault [PackageName])
listBucket store plane raw =
    withBucket raw (collectPages . listPackagesIn (handleOver store plane))

-- Dummy static credentials: the handle is held and read, never sent anywhere.
handleFor :: CodeArtifactStore -> IO StoreMaintenance
handleFor store =
    AWS.newEnv (pure . fromKeys (AWS.AccessKey "AKIDtestkey") (AWS.SecretKey "testsecretkey"))
        >>= fmap (handleOver store) . controlPlaneFor

npmStore :: Maybe CodeArtifactStore
npmStore = coordinates <$> codeArtifactFormat Npm
  where
    coordinates format =
        CodeArtifactStore
            { casDomain = "acme"
            , casDomainOwner = "111122223333"
            , casRegion = "eu-west-1"
            , casRepository = "mirror"
            , casFormat = format
            }
