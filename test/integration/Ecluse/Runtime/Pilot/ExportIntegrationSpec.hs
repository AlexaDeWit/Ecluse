-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Guarded advisory publication and consumer polling against the ministack S3 service.
Requires a Docker daemon.
-}
module Ecluse.Runtime.Pilot.ExportIntegrationSpec (
    spec,
) where

import Control.Monad.Trans.Resource (runResourceT)
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Network.HTTP.Client qualified as HTTP
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, SpecWith, aroundAll, aroundWith, describe, expectationFailure, it, shouldBe, shouldReturn, shouldSatisfy)
import TestContainers (Container, containerAddress)
import UnliftIO.Async (concurrently)
import UnliftIO.STM (checkSTM, stateTVar)
import UnliftIO.Timeout (timeout)

import Amazonka qualified as AWS
import Amazonka.Env.Hooks qualified as Hooks
import Amazonka.S3 qualified as S3
import Amazonka.S3.Lens qualified as S3L
import Data.Time (UTCTime)
import Ecluse.Config.AdvisoryStore (AdvisoryStoreUrl, advisoryStoreBucket, mkAdvisoryStoreUrl)
import Ecluse.Config.Ambient (parseEndpointUrl)
import Ecluse.Core.Cve (CveDbRejected (CveDbWrongEpoch))
import Ecluse.Core.Cve.Slot (AdvisorySource (..), currentAdvisorySource, newCveSlot)
import Ecluse.Core.Cve.Types (DbEtag (DbEtag))
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI), ecosystemName)
import Ecluse.Core.Osv.Compile (OsvCandidate, candidatePath)
import Ecluse.Core.Osv.Provenance (ProvenanceSource (OsvExport))
import Ecluse.Core.Osv.Publication
import Ecluse.Core.Osv.Schema (EpssRequirement (..), osvDbFileName, osvSchemaEpoch)
import Ecluse.Integration.Ministack (MinistackPool, objectMetadata, withMinistack)
import Ecluse.Runtime.Aws.Env (AwsEndpoint)
import Ecluse.Runtime.Aws.S3 (buildS3Env)
import Ecluse.Runtime.Cve.Sync.Internal (SyncEnv (..), SyncOutcome (..), newS3CveSource, s3CveFetchFor, syncStep)
import Ecluse.Runtime.Pilot.Export (PublicationStop (..), Published (..), publishCandidate)
import Ecluse.Runtime.Test.Advisory (advisoryDriver)
import Ecluse.Test.Contract.Advisory (AdvisoryDriver, AdvisoryFixture (..), advisoryContract)
import Ecluse.Test.Corpus.Advisories (AdvisoryInputs (aiOsvZip), fillerTargets, syntheticAdvisories)
import Ecluse.Test.Log (runQuietKatip)
import Ecluse.Test.Osv (CorpusVersion (CorpusV1, CorpusV2), mkDbWithWrongEpoch, mkMinimalValidDb, osvCorpusZip)
import Ecluse.Test.OsvDb (SourceServer, withServedCandidate, withSourceServer)
import Ecluse.Test.Poll (pollUntil, retryingIO)
import Lens.Micro ((.~), (?~), (^.))

-- | The last-good contract over operator writes, and guarded publication against ministack.
spec :: MinistackPool -> Spec
spec pool =
    describe "S3 Export Integration" $
        aroundAll (withMinistack pool) $ do
            aroundWith withS3AdvisoryDriver $
                advisoryContract "advisory/s3" (CveDbWrongEpoch (osvSchemaEpoch + 1))
            for_ [Npm, PyPI] guardedPublicationSpec
            for_ [Npm, PyPI] $ \eco -> for_ [False, True] (shrinkSpec eco)

guardedPublicationSpec :: Ecosystem -> SpecWith Container
guardedPublicationSpec eco = do
    it (label "native-001" "creates the first artifact only while the key is absent, with its publication metadata") $ \container ->
        withGuarded container eco "first" $ \store server dir -> do
            published <- served server "Wed, 01 Oct 2026 10:00:00 GMT" CorpusV1 dir (publish store) >>= expectPublished
            plPrecondition (pubPlan published) `shouldBe` CreateOnly
            metadata <- storedMetadata store
            HashMap.lookup "baseline-adjustment" metadata `shouldBe` Just "initial"
            HashMap.lookup "osv-last-modified" metadata `shouldBe` Just "2026-10-01T10:00:00Z"
            HashMap.lookup "ecosystem" metadata `shouldBe` Just (ecosystemName eco)
            consumer <- consumerFor store eco dir
            syncStep consumer Nothing >>= (`shouldSatisfy` swapped)
            -- An operator writes the next key after Pilot reads it absent, so the store must refuse the create.
            let contested = store{stKey = "contested-" <> stKey store}
            mkMinimalValidDb (dir </> "operator.db") "pkg-a"
            (hooked, planted) <- writeBeforeFirstPut contested (dir </> "operator.db")
            served server "Wed, 01 Oct 2026 10:00:00 GMT" CorpusV1 (dir </> "contested") (publish contested{stAws = hooked}) >>= \case
                Left (PublicationRefused (PredecessorUnreadable _)) -> pass
                other -> expectationFailure ("expected the create refused once the key was taken, got " <> show other)
            operatorEtag <- planted
            operatorEtag `shouldSatisfy` isJust
            storedEtag contested `shouldReturn` operatorEtag

    it (label "native-002" "refreshes unchanged input under a new revision, which the consumer swaps in with a later push time") $ \container ->
        withGuarded container eco "refresh" $ \store server dir -> do
            consumer <- consumerFor store eco dir
            served server "Wed, 01 Oct 2026 10:00:00 GMT" CorpusV1 dir $ \candidate -> do
                void (publish store candidate >>= expectPublished)
                firstSeen <- syncStep consumer Nothing >>= swappedEtag
                firstPush <- pushedAt consumer
                firstHead <- headObject store
                -- HTTP Last-Modified carries whole seconds, so republish until the consumer can tell the pushes apart.
                let republished = do
                        void (publish store candidate >>= expectPublished)
                        later <- headObject store
                        pure (later ^. S3L.headObjectResponse_lastModified > firstHead ^. S3L.headObjectResponse_lastModified)
                pollUntil 21 500_000 id republished >>= (`shouldBe` True)
                refreshed <- syncStep consumer (Just firstSeen) >>= swappedEtag
                refreshed `shouldSatisfy` (/= firstSeen)
                pushedAt consumer >>= (`shouldSatisfy` (> firstPush))

    it (label "native-003" "refuses an earlier-data run that finishes after a newer one, and leaves the consumer's push time alone") $ \container ->
        withGuarded container eco "older" $ \store server dir -> do
            consumer <- consumerFor store eco dir
            served server "Wed, 01 Oct 2026 10:00:00 GMT" CorpusV1 (dir </> "older") $ \older -> do
                served server "Wed, 01 Oct 2026 11:00:00 GMT" CorpusV2 (dir </> "newer") $ \newer ->
                    void (publish store newer >>= expectPublished)
                seen <- syncStep consumer Nothing >>= swappedEtag
                push <- pushedAt consumer
                before <- storedEtag store
                publish store older >>= \case
                    Left (PublicationRefused (SourceOutOfOrder _ (OlderSnapshot _ _))) -> pass
                    other -> expectationFailure ("expected the older run refused, got " <> show other)
                storedEtag store `shouldReturn` before
                syncStep consumer (Just seen) >>= (`shouldSatisfy` unchanged)
                pushedAt consumer `shouldReturn` push

    it (label "native-004" "lets exactly one of two first writers create the object, and keeps the newer data") $ \container ->
        withGuarded container eco "race" $ \store server dir ->
            served server "Wed, 01 Oct 2026 10:00:00 GMT" CorpusV1 (dir </> "older") $ \older ->
                served server "Wed, 01 Oct 2026 11:00:00 GMT" CorpusV2 (dir </> "newer") $ \newer -> do
                    -- Each writer's first PUT waits for the other's, so both read the key absent first.
                    (raced, firstPuts) <- putsMeetFirst store
                    (olderOutcome, newerOutcome) <- concurrently (publish raced older) (publish raced newer)
                    firstPuts `shouldReturn` 2
                    newerOutcome `shouldSatisfy` isRight
                    olderOutcome `shouldSatisfy` either refusedAsOlder (const True)
                    [plPrecondition (pubPlan published) | Right published <- [olderOutcome, newerOutcome]]
                        `shouldSatisfy` ((== 1) . length . filter (== CreateOnly))
                    HashMap.lookup "osv-last-modified" <$> storedMetadata store `shouldReturn` Just "2026-10-01T11:00:00Z"

    it (label "native-005" "refuses to replace an object written without publication metadata") $ \container ->
        withGuarded container eco "legacy" $ \store server dir -> do
            mkMinimalValidDb (dir </> "legacy.db") "pkg-a"
            void (putDirect store (dir </> "legacy.db") HashMap.empty)
            before <- storedEtag store
            served server "Wed, 01 Oct 2026 10:00:00 GMT" CorpusV1 dir (publish store) >>= \case
                Left (PublicationRefused (PredecessorUnreadable (EnvelopeMissing keys))) -> toList keys `shouldSatisfy` elem "baseline-bytes"
                other -> expectationFailure ("expected a missing-metadata refusal, got " <> show other)
            storedEtag store `shouldReturn` before

    it (label "native-007" "lets an operator restore an older artifact directly, which consumers accept and Pilot orders from") $ \container ->
        withGuarded container eco "restore" $ \store server dir -> do
            consumer <- consumerFor store eco dir
            served server "Wed, 01 Oct 2026 10:00:00 GMT" CorpusV1 (dir </> "g1") $ \g1 -> do
                void (publish store g1 >>= expectPublished)
                g1Metadata <- storedMetadata store
                served server "Wed, 01 Oct 2026 11:00:00 GMT" CorpusV2 (dir </> "g2") $ \g2 ->
                    void (publish store g2 >>= expectPublished)
                seen <- syncStep consumer Nothing >>= swappedEtag
                -- A version restore copies the older object's bytes and metadata.
                restored <- putDirect store (candidatePath g1) g1Metadata
                syncStep consumer (Just seen) >>= swappedEtag >>= (`shouldBe` restored)
            served server "Wed, 01 Oct 2026 12:00:00 GMT" CorpusV2 (dir </> "g3") $ \g3 ->
                plOsvOrder . pubPlan <$> (publish store g3 >>= expectPublished) `shouldReturn` OrderedForward
  where
    label caseId = caseLabel ("integration/advisory-export/" <> caseId <> "/" <> ecosystemName eco)
    served server stamp version dir use = do
        archive <- osvCorpusZip version
        withServedCandidate server eco archive (Just stamp) dir use
    swapped = \case
        SyncSwapped _ _ -> True
        _ -> False
    unchanged = \case
        SyncUnchanged -> True
        _ -> False
    refusedAsOlder = \case
        PublicationRefused (SourceOutOfOrder OsvExport (OlderSnapshot _ _)) -> True
        _ -> False

shrinkSpec :: Ecosystem -> Bool -> SpecWith Container
shrinkSpec eco versioned =
    it (caseLabel ("integration/advisory-export/native-006/" <> ecosystemName eco <> "/" <> bucketKind) "refuses a sharp shrink against the baseline the metadata carries, with no history read") $ \container ->
        withGuarded container eco ("shrink-" <> bucketKind) $ \store server dir -> do
            when versioned $
                void (runResourceT (AWS.send (stAws store) (S3.newPutBucketVersioning (S3.BucketName (bucketOf store)) (S3.newVersioningConfiguration & S3L.versioningConfiguration_status ?~ S3.BucketVersioningStatus_Enabled))))
            large <- aiOsvZip <$> syntheticAdvisories eco (fillerTargets 400)
            small <- aiOsvZip <$> syntheticAdvisories eco (fillerTargets 20)
            void (withServedCandidate server eco large (Just "Wed, 01 Oct 2026 10:00:00 GMT") (dir </> "large") (publish store) >>= expectPublished)
            metadata <- storedMetadata store
            withServedCandidate server eco small (Just "Wed, 01 Oct 2026 11:00:00 GMT") (dir </> "small") $ \candidate -> do
                -- An operator write leaves an object of the candidate's own size under the large publication's
                -- metadata, so only the metadata's baseline can refuse the candidate.
                void (putDirect store (candidatePath candidate) metadata)
                storedBytes <- maybe (fail "the stored object has no length") pure . (^. S3L.headObjectResponse_contentLength) =<< headObject store
                before <- storedEtag store
                publish store candidate >>= \case
                    Left (PublicationRefused (ShrinkPastBaseline reading)) -> do
                        Just (show (srBaselineBytes reading)) `shouldBe` HashMap.lookup "baseline-bytes" metadata
                        toRational (srCandidateBytes reading) `shouldSatisfy` (>= toRational storedBytes * (100 - unPercent (spMaxShrink (stPolicy store))) / 100)
                    other -> expectationFailure ("expected a shrink refusal, got " <> show other)
                storedEtag store `shouldReturn` before
  where
    bucketKind = if versioned then "versioned" else "unversioned"

-- One case's bucket, the key it publishes under, and the shipped guard.
data Store = Store
    { stEndpoint :: AwsEndpoint
    , stAws :: AWS.Env
    , stStore :: AdvisoryStoreUrl
    , stKey :: Text
    , stPolicy :: ShrinkPolicy
    }

withGuarded :: Container -> Ecosystem -> Text -> (Store -> SourceServer -> FilePath -> IO ()) -> IO ()
withGuarded container eco name use =
    withSystemTempDirectory "ecluse-guarded-export" $ \dir -> do
        store <- createdBucket container ("s3://guarded-" <> name <> "-" <> ecosystemName eco)
        withSourceServer $ \server -> use store{stKey = toText (osvDbFileName (ecosystemName eco))} server dir

publish :: Store -> OsvCandidate -> IO (Either PublicationStop Published)
publish store candidate = runQuietKatip (publishCandidate Nothing (stAws store) (stPolicy store) DurableBaseline Nothing (bucketOf store) (stKey store) candidate)

expectPublished :: Either PublicationStop Published -> IO Published
expectPublished = either (fail . show) pure

bucketOf :: Store -> Text
bucketOf = advisoryStoreBucket . stStore

headObject :: Store -> IO S3.HeadObjectResponse
headObject store = runResourceT (AWS.send (stAws store) (S3.newHeadObject (S3.BucketName (bucketOf store)) (S3.ObjectKey (stKey store))))

storedMetadata :: Store -> IO (HashMap.HashMap Text Text)
storedMetadata store = objectMetadata (stAws store) (bucketOf store) (S3.ObjectKey (stKey store))

storedEtag :: Store -> IO (Maybe S3.ETag)
storedEtag store = (^. S3L.headObjectResponse_eTag) <$> headObject store

-- An operator's direct write: no guard, no precondition.
putDirect :: Store -> FilePath -> HashMap.HashMap Text Text -> IO DbEtag
putDirect store path metadata = do
    body <- AWS.chunkedFile 1048576 path
    response <- runResourceT (AWS.send (stAws store) (S3.newPutObject (S3.BucketName (bucketOf store)) (S3.ObjectKey (stKey store)) body & S3L.putObject_metadata .~ metadata))
    maybe (fail "the direct write has no ETag") (\(S3.ETag tag) -> pure (DbEtag (decodeUtf8 tag))) (response ^. S3L.putObjectResponse_eTag)

-- The store's env with a write of @path@ to its key just before its first PUT, and a read of the
-- revision that write left, 'Nothing' until it has run.
writeBeforeFirstPut :: Store -> FilePath -> IO (AWS.Env, IO (Maybe S3.ETag))
writeBeforeFirstPut store path = do
    fired <- newIORef False
    planted <- newIORef Nothing
    let plant request = do
            isFirst <- if HTTP.method request == "PUT" then not <$> atomicModifyIORef' fired (True,) else pure False
            when isFirst (putDirect store path HashMap.empty >> storedEtag store >>= writeIORef planted)
    pure (beforeEachRequest plant (stAws store), readIORef planted)

{- The store with each of its first two PUTs held until both have started, for at most five
seconds, and a count of the PUTs that met the other. Any later PUT passes at once.
-}
putsMeetFirst :: Store -> IO (Store, IO Int)
putsMeetFirst store = do
    started <- newTVarIO (0 :: Int)
    met <- newTVarIO (0 :: Int)
    let hold request =
            when (HTTP.method request == "PUT") $ do
                arrived <- atomically (stateTVar started (\n -> (n + 1, n + 1)))
                when (arrived <= 2) $
                    timeout 5_000_000 (atomically (readTVar started >>= checkSTM . (>= 2))) >>= \case
                        Just () -> atomically (modifyTVar' met (+ 1))
                        Nothing -> pass
    pure (store{stAws = beforeEachRequest hold (stAws store)}, readTVarIO met)

-- Run @observe@ on each HTTP request the env sends, just before it goes out.
beforeEachRequest :: (HTTP.Request -> IO ()) -> AWS.Env -> AWS.Env
beforeEachRequest observe env =
    env{AWS.hooks = Hooks.clientRequestHook (\previous current request -> observe request >> previous current request) (AWS.hooks env)}

consumerFor :: Store -> Ecosystem -> FilePath -> IO SyncEnv
consumerFor store eco dir = do
    source <- newS3CveSource (Just (stEndpoint store))
    slot <- newCveSlot
    pure (SyncEnv (s3CveFetchFor source (bucketOf store) (stKey store) (512 * 1024 * 1024)) eco EpssRequired (dir </> "consumer.db") slot ("s3://" <> bucketOf store))

swappedEtag :: SyncOutcome -> IO DbEtag
swappedEtag = \case
    SyncSwapped etag _ -> pure etag
    other -> fail ("expected the artifact swapped in, got " <> show other)

pushedAt :: SyncEnv -> IO (Maybe UTCTime)
pushedAt consumer = (asPushedAt =<<) <$> currentAdvisorySource (syncSlot consumer)

-- Operator writes stand for the publisher here: the contract is about what consumers keep.
withS3AdvisoryDriver :: (AdvisoryDriver -> IO ()) -> Container -> IO ()
withS3AdvisoryDriver use container =
    withSystemTempDirectory "ecluse-advisory-contract" $ \dir -> do
        store <- createdBucket container "s3://advisory-contract"
        let contract = store{stKey = "contract.db"}
            goodPath = dir <> "/good.db"
            badPath = dir <> "/bad.db"
        mkMinimalValidDb goodPath "pkg-a"
        mkDbWithWrongEpoch badPath
        bytes <- readFileBS goodPath
        slot <- newCveSlot
        source <- newS3CveSource (Just (stEndpoint store))
        let env =
                SyncEnv
                    { syncFetch = s3CveFetchFor source (bucketOf contract) (stKey contract) (512 * 1024 * 1024)
                    , syncEcosystem = Npm
                    , syncEpssRequirement = EpssOptional
                    , syncDbPath = dir <> "/live.db"
                    , syncSlot = slot
                    , syncStoreRef = "s3://" <> bucketOf contract
                    }
            publishFixture = \case
                AcceptedArtifact -> putDirect contract goodPath HashMap.empty
                RefusedArtifact -> putDirect contract badPath HashMap.empty
        advisoryDriver bytes publishFixture (pure env) >>= use

createdBucket :: Container -> Text -> IO Store
createdBucket container url = do
    let (host, port) = containerAddress container 4566
        endpointUrl = "http://" <> host <> ":" <> T.pack (show port)
    store <- either (fail . toString) pure (mkAdvisoryStoreUrl "advisories.url" url)
    -- The override is the ambient AWS_ENDPOINT_URL a released image carries, resolved
    -- through the same parser the boot uses.
    endpoint <- either (const (fail ("ExportIntegrationSpec: unparseable endpoint for " <> toString host))) pure (parseEndpointUrl endpointUrl)
    base <- buildS3Env (Just endpoint)
    let regioned = base{AWS.region = AWS.Region' "us-east-1"}
    -- The readiness wait only proves the port accepts connections, so the S3 gateway may
    -- still be warming when the first CreateBucket lands.
    retryingIO 21 500_000 (void (runResourceT (AWS.send regioned (S3.newCreateBucket (S3.BucketName (advisoryStoreBucket store))))))
    policy <- maybe (fail "the shipped guard") pure (ShrinkPolicy <$> mkPercent 2 <*> mkPercent 5)
    pure Store{stEndpoint = endpoint, stAws = base, stStore = store, stKey = "artifact.db", stPolicy = policy}
