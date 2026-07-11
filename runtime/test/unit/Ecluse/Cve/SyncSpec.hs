module Ecluse.Cve.SyncSpec (spec) where

import Conduit (runConduit, yieldMany, (.|))
import Data.Conduit.Combinators qualified as C
import Katip (Environment (Environment), KatipContextT, Namespace (Namespace), SimpleLogPayload, initLogEnv, runKatipContextT)
import Network.HTTP.Types (Status, hContentType, status200, status404)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, anyException, describe, expectationFailure, it, shouldBe, shouldReturn, shouldSatisfy, shouldThrow)
import UnliftIO.Async (async, cancel, waitCatch, withAsync)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwString)

import Amazonka qualified as AWS
import Amazonka.Auth (fromKeys)
import Amazonka.S3 qualified as S3
import Data.ByteString.Lazy qualified as LBS

import Ecluse.Core.Cve (CveDbRejected (..), CveLookup (cveRemediationProbe))
import Ecluse.Core.Cve.Slot (CveSlot, newCveSlot, withSlotLookup)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Osv.Schema (osvSchemaEpoch)
import Ecluse.Runtime.Cve.Sync (
    CveFetch (..),
    DbEtag (..),
    OsvDbFetchFault (..),
    SyncEnv (..),
    SyncOutcome (..),
    SyncSchedule (..),
    cappedAt,
    discardTemp,
    runCveSync,
    s3CveFetch,
    syncStep,
 )
import Ecluse.Test.Osv (mkDbWithMalformedProvenance, mkDbWithWrongEpoch, mkMinimalValidDb)

-- | An env over a fresh slot and a temp data dir, handed to each case.
withSyncEnv :: (FilePath -> CveSlot -> (CveFetch -> SyncEnv) -> IO a) -> IO a
withSyncEnv use =
    withSystemTempDirectory "ecluse-cve-sync" $ \dir -> do
        slot <- newCveSlot
        let envWith fetch =
                SyncEnv
                    { syncFetch = fetch
                    , syncEcosystem = Npm
                    , syncDbPath = dir </> "npm-osv-schema2.db"
                    , syncSlot = slot
                    }
        use dir slot envWith

{- | A fetch whose HEAD answers the given ETag and whose download builds an
artifact via the given writer, returning the same ETag.
-}
fetchServing :: Maybe Text -> (FilePath -> IO ()) -> CveFetch
fetchServing mEtag write =
    CveFetch
        { fetchHeadEtag = pure (DbEtag <$> mEtag)
        , fetchDownload = \dest -> case mEtag of
            Nothing -> throwString "download called with no object present"
            Just etag -> write dest $> DbEtag etag
        }

-- | Does the slot's current generation answer the probe for this package?
probesFor :: CveSlot -> Text -> IO (Maybe Bool)
probesFor slot pkg = withSlotLookup slot (traverse (\l -> cveRemediationProbe l pkg "1.0.0"))

-- | Wait (bounded) until the slot serves a generation probing True for the package.
awaitServing :: CveSlot -> Text -> IO ()
awaitServing slot pkg = go (200 :: Int)
  where
    go 0 = expectationFailure ("slot never served an artifact for " <> toString pkg)
    go n =
        probesFor slot pkg >>= \case
            Just True -> pass
            _ -> threadDelay 25_000 >> go (n - 1)

-- | Run a Katip-constrained action against a scribe-less environment.
runQuiet :: KatipContextT IO a -> IO a
runQuiet action = do
    logEnv <- initLogEnv (Namespace ["ecluse"]) (Environment "test")
    runKatipContextT logEnv (mempty :: SimpleLogPayload) mempty action

stubS3Env :: Int -> IO AWS.Env
stubS3Env port = do
    let auth = fromKeys (AWS.AccessKey "AKIDtestkey") (AWS.SecretKey "testsecretkey")
    env <- AWS.newEnv (pure . auth)
    pure $ AWS.configureService (customS3Endpoint (False, "127.0.0.1", port)) env

customS3Endpoint :: (Bool, Text, Int) -> AWS.Service
customS3Endpoint (secure, host, port) =
    (AWS.setEndpoint secure (encodeUtf8 host) port S3.defaultService)
        { AWS.s3AddressingStyle = AWS.S3AddressingStylePath
        }

stubS3 :: Maybe Text -> Status -> LByteString -> Application
stubS3 mEtag status body _req respond =
    respond
        ( responseLBS
            status
            ( (hContentType, "application/octet-stream")
                : maybeToList ((\e -> ("ETag", encodeUtf8 e)) <$> mEtag)
                    <> [("Content-Length", show (LBS.length body))]
            )
            body
        )

spec :: Spec
spec = do
    describe "syncStep" $ do
        it "reports the object absent without attempting a download" $
            withSyncEnv $ \_ _ envWith -> do
                let fetch =
                        CveFetch
                            { fetchHeadEtag = pure Nothing
                            , fetchDownload = \_ -> throwString "must not download"
                            }
                syncStep (envWith fetch) Nothing >>= \case
                    SyncAbsent -> pass
                    other -> expectationFailure ("expected SyncAbsent, got " <> show other)

        it "does nothing when the remote ETag matches the last seen one" $
            withSyncEnv $ \_ _ envWith -> do
                let fetch =
                        CveFetch
                            { fetchHeadEtag = pure (Just (DbEtag "e1"))
                            , fetchDownload = \_ -> throwString "must not download"
                            }
                syncStep (envWith fetch) (Just (DbEtag "e1")) >>= \case
                    SyncUnchanged -> pass
                    other -> expectationFailure ("expected SyncUnchanged, got " <> show other)

        it "downloads, verifies, renames onto the canonical name, and swaps in" $
            withSyncEnv $ \_ slot envWith -> do
                let env = envWith (fetchServing (Just "e1") (`mkMinimalValidDb` "pkg-a"))
                syncStep env Nothing >>= \case
                    SyncSwapped etag meta -> do
                        etag `shouldBe` DbEtag "e1"
                        meta `shouldSatisfy` elem ("ecosystem", "npm")
                    other -> expectationFailure ("expected SyncSwapped, got " <> show other)
                probesFor slot "pkg-a" `shouldReturn` Just True
                doesFileExist (syncDbPath env) `shouldReturn` True
                doesFileExist (syncDbPath env <> ".tmp") `shouldReturn` False

        it "a second artifact displaces the first" $
            withSyncEnv $ \_ slot envWith -> do
                void (syncStep (envWith (fetchServing (Just "e1") (`mkMinimalValidDb` "pkg-a"))) Nothing)
                void (syncStep (envWith (fetchServing (Just "e2") (`mkMinimalValidDb` "pkg-b"))) (Just (DbEtag "e1")))
                probesFor slot "pkg-b" `shouldReturn` Just True
                probesFor slot "pkg-a" `shouldReturn` Just False

        it "a refused artifact is discarded and the last-good generation keeps serving" $
            withSyncEnv $ \_ slot envWith -> do
                let goodEnv = envWith (fetchServing (Just "e1") (`mkMinimalValidDb` "pkg-a"))
                void (syncStep goodEnv Nothing)
                let badEnv = envWith (fetchServing (Just "e2") mkDbWithWrongEpoch)
                syncStep badEnv (Just (DbEtag "e1")) >>= \case
                    SyncRejected etag rejection -> do
                        etag `shouldBe` DbEtag "e2"
                        rejection `shouldBe` CveDbWrongEpoch (osvSchemaEpoch + 1)
                    other -> expectationFailure ("expected SyncRejected, got " <> show other)
                probesFor slot "pkg-a" `shouldReturn` Just True
                doesFileExist (syncDbPath badEnv <> ".tmp") `shouldReturn` False

        it "a download that fails mid-stream discards the partial temp file" $
            withSyncEnv $ \_ _ envWith -> do
                let fetch =
                        CveFetch
                            { fetchHeadEtag = pure (Just (DbEtag "e1"))
                            , fetchDownload = \dest -> do
                                writeFileBS dest "partial bytes"
                                throwString "connection reset mid-stream"
                            }
                    env = envWith fetch
                syncStep env Nothing `shouldThrow` anyException
                doesFileExist (syncDbPath env <> ".tmp") `shouldReturn` False

        it "a malformed provenance row is rejected, discards the temp, and keeps the last-good generation" $
            withSyncEnv $ \_ slot envWith -> do
                void (syncStep (envWith (fetchServing (Just "e1") (`mkMinimalValidDb` "pkg-a"))) Nothing)
                let env = envWith (fetchServing (Just "e2") mkDbWithMalformedProvenance)
                syncStep env (Just (DbEtag "e1")) >>= \case
                    SyncRejected etag rej@(CveDbMetaUnreadable _) -> do
                        etag `shouldBe` DbEtag "e2"
                        show rej `shouldSatisfy` not . (null :: [Char] -> Bool)
                    other -> expectationFailure ("expected SyncRejected CveDbMetaUnreadable, got " <> show other)
                doesFileExist (syncDbPath env <> ".tmp") `shouldReturn` False
                probesFor slot "pkg-a" `shouldReturn` Just True

        it "a swapper cancelled while draining never closes the newly published generation" $
            withSyncEnv $ \_ slot envWith -> do
                void (syncStep (envWith (fetchServing (Just "e1") (`mkMinimalValidDb` "pkg-a"))) Nothing)
                insideReader <- newEmptyMVar
                releaseReader <- newEmptyMVar
                pinned <- async $ withSlotLookup slot $ \_ -> do
                    putMVar insideReader ()
                    takeMVar releaseReader
                takeMVar insideReader
                -- The swap publishes the new generation, then parks draining
                -- the displaced one, whose reader is still pinned inside.
                swapper <- async (void (syncStep (envWith (fetchServing (Just "e2") (`mkMinimalValidDb` "pkg-b"))) (Just (DbEtag "e1"))))
                awaitServing slot "pkg-b"
                cancel swapper
                putMVar releaseReader ()
                void (waitCatch pinned)
                -- The cancellation interrupted the drain wait, never the
                -- published generation: the slot must still answer.
                probesFor slot "pkg-b" `shouldReturn` Just True

    describe "runCveSync" $ do
        it "the boot burst retries through transport faults until the artifact lands" $
            withSyncEnv $ \_ slot envWith -> do
                calls <- newIORef (0 :: Int)
                notified <- newIORef (0 :: Int)
                let flaky =
                        CveFetch
                            { fetchHeadEtag = do
                                n <- atomicModifyIORef' calls (\n -> (n + 1, n + 1))
                                if n <= 2
                                    then throwString "transport down"
                                    else pure (Just (DbEtag "e1"))
                            , fetchDownload = \dest -> mkMinimalValidDb dest "pkg-a" $> DbEtag "e1"
                            }
                    schedule = SyncSchedule{schedBootBackoff = replicate 5 10_000, schedPollDelay = 5_000_000}
                withAsync (runQuiet (runCveSync (envWith flaky) schedule (modifyIORef' notified (+ 1)))) $ \_ -> do
                    awaitServing slot "pkg-a"
                    readIORef notified `shouldReturn` 1

        it "the boot burst is allowed to fail; the poll recovers when the artifact appears" $
            withSyncEnv $ \_ slot envWith -> do
                published <- newTVarIO False
                let lateFetch =
                        CveFetch
                            { fetchHeadEtag =
                                readTVarIO published <&> \case
                                    False -> Nothing
                                    True -> Just (DbEtag "e1")
                            , fetchDownload = \dest -> mkMinimalValidDb dest "pkg-a" $> DbEtag "e1"
                            }
                    -- A short burst that will exhaust before publication, then a
                    -- fast poll that finds the artifact once it exists.
                    schedule = SyncSchedule{schedBootBackoff = [5_000, 5_000], schedPollDelay = 25_000}
                withAsync (runQuiet (runCveSync (envWith lateFetch) schedule pass)) $ \_ -> do
                    -- Burst window passes with nothing published; still serving nothing.
                    threadDelay 100_000
                    probesFor slot "pkg-a" `shouldReturn` Nothing
                    atomically (writeTVar published True)
                    awaitServing slot "pkg-a"

        it "the boot burst concedes on a rejected artifact and its remembered ETag stops re-downloads" $
            withSyncEnv $ \_ slot envWith -> do
                downloads <- newIORef (0 :: Int)
                let fetch =
                        CveFetch
                            { fetchHeadEtag = pure (Just (DbEtag "bad"))
                            , fetchDownload = \dest -> do
                                modifyIORef' downloads (+ 1)
                                mkDbWithWrongEpoch dest
                                pure (DbEtag "bad")
                            }
                    schedule = SyncSchedule{schedBootBackoff = replicate 5 10_000, schedPollDelay = 20_000}
                withAsync (runQuiet (runCveSync (envWith fetch) schedule pass)) $ \_ -> do
                    threadDelay 200_000
                    -- One download despite the burst budget and several polls:
                    -- identical bytes cannot verify differently, so the
                    -- remembered ETag reads as unchanged until a re-publish.
                    readIORef downloads `shouldReturn` 1
                    probesFor slot "pkg" `shouldReturn` Nothing

        it "the boot burst concedes on a malformed provenance row" $
            withSyncEnv $ \_ slot envWith -> do
                downloads <- newIORef (0 :: Int)
                let fetch =
                        CveFetch
                            { fetchHeadEtag = pure (Just (DbEtag "bad-meta"))
                            , fetchDownload = \dest -> do
                                modifyIORef' downloads (+ 1)
                                mkDbWithMalformedProvenance dest
                                pure (DbEtag "bad-meta")
                            }
                    schedule = SyncSchedule{schedBootBackoff = replicate 5 10_000, schedPollDelay = 20_000}
                withAsync (runQuiet (runCveSync (envWith fetch) schedule pass)) $ \_ -> do
                    threadDelay 200_000
                    readIORef downloads `shouldReturn` 1
                    probesFor slot "pkg" `shouldReturn` Nothing

        it "the poll triggers SyncUnchanged when the ETag is stable" $
            withSyncEnv $ \_ _ envWith -> do
                heads <- newIORef (0 :: Int)
                downloads <- newIORef (0 :: Int)
                let fetch =
                        CveFetch
                            { fetchHeadEtag = do
                                modifyIORef' heads (+ 1)
                                pure (Just (DbEtag "e1"))
                            , fetchDownload = \dest -> do
                                modifyIORef' downloads (+ 1)
                                mkMinimalValidDb dest "pkg"
                                pure (DbEtag "e1")
                            }
                    schedule = SyncSchedule{schedBootBackoff = [], schedPollDelay = 20_000}
                withAsync (runQuiet (runCveSync (envWith fetch) schedule pass)) $ \_ -> do
                    threadDelay 200_000
                    -- Should have downloaded once in the boot burst (even with empty backoff)
                    -- and then polled several times, finding it unchanged.
                    readIORef downloads `shouldReturn` 1
                    n <- readIORef heads
                    n `shouldSatisfy` (> 2)

    describe "discardTemp robustness" $ do
        it "is a no-op when the file does not exist" $
            discardTemp "/no/such/file/deliberate"

    describe "s3HeadEtag / s3Download" $ do
        it "s3HeadEtag returns Nothing on 404" $
            testWithApplication (pure (stubS3 (Just "e1") status404 "not found")) $ \port -> do
                env <- stubS3Env port
                let fetch = s3CveFetch env "bucket" "key" 1024
                fetchHeadEtag fetch `shouldReturn` Nothing

        it "s3Download fast-fails on Content-Length over cap" $
            testWithApplication (pure (stubS3 (Just "e1") status200 "too large")) $ \port -> do
                env <- stubS3Env port
                let fetch = s3CveFetch env "bucket" "key" 5
                withSystemTempDirectory "ecluse-s3-sync" $ \dir -> do
                    fetchDownload fetch (dir </> "out") `shouldThrow` (== OsvDbTooLarge 5)

    describe "Show and Eq instances" $ do
        let (isNotNull :: [Char] -> Bool) = not . null
        it "Show and Eq DbEtag" $ do
            let e1 = DbEtag "e"
                e2 = DbEtag "f"
            show e1 `shouldBe` ("DbEtag \"e\"" :: [Char])
            e1 `shouldBe` e1
            e1 `shouldSatisfy` (/= e2)

        it "Show and Eq SyncOutcome" $ do
            let etag = DbEtag "e"
                meta = [("k", "v")]
                rej = CveDbMetaUnreadable ["err"]
                s1 = SyncSwapped etag meta
                s2 = SyncUnchanged
                s3 = SyncAbsent
                s4 = SyncRejected etag rej
            forM_ [s1, s2, s3, s4] $ \s -> do
                show s `shouldSatisfy` isNotNull
                s `shouldBe` s
            s1 `shouldSatisfy` (/= s2)

        it "Show and Eq OsvDbFetchFault" $ do
            let f1 = OsvDbTooLarge 1024
                f2 = OsvDbTooLarge 2048
                f3 = OsvDbNoEtag
            show f1 `shouldSatisfy` isNotNull
            show f3 `shouldSatisfy` isNotNull
            f1 `shouldBe` f1
            f1 `shouldSatisfy` (/= f2)
            f1 `shouldSatisfy` (/= f3)

    describe "cappedAt" $ do
        it "passes a stream that ends exactly at the cap through unchanged" $ do
            out <- runConduit (yieldMany (["ab", "cd"] :: [ByteString]) .| cappedAt 4 .| C.sinkList)
            mconcat out `shouldBe` ("abcd" :: ByteString)

        it "throws OsvDbTooLarge the moment the stream oversteps the cap" $
            runConduit (yieldMany (["ab", "cde"] :: [ByteString]) .| cappedAt 4 .| C.sinkList)
                `shouldThrow` (== OsvDbTooLarge 4)
