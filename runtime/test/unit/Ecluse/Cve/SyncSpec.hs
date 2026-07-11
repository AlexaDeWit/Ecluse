module Ecluse.Cve.SyncSpec (spec) where

import Conduit (runConduit, yieldMany, (.|))
import Data.Conduit.Combinators qualified as C
import Katip (Environment (Environment), KatipContextT, Namespace (Namespace), SimpleLogPayload, initLogEnv, runKatipContextT)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, anyException, describe, expectationFailure, it, shouldBe, shouldReturn, shouldSatisfy, shouldThrow)
import UnliftIO.Async (async, cancel, waitCatch, withAsync)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwIO)

import Ecluse.Core.Cve (CveDbRejected (CveDbIntegrityFailed, CveDbWrongEpoch), CveLookup (cveRemediationProbe))
import Ecluse.Core.Cve.Slot (CveSlot, newCveSlot, withSlotLookup)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Fault (TransportCause (TransportUnreachable), transportFault)
import Ecluse.Core.Osv.Schema (osvDbFileName, osvSchemaEpoch)
import Ecluse.Runtime.Cve.Sync (
    CveFetch (..),
    DbEtag (..),
    OsvDbCapExceeded (OsvDbCapExceeded),
    OsvDbFetchFault (OsvDbTransport),
    SyncEnv (..),
    SyncOutcome (..),
    SyncSchedule (..),
    cappedAt,
    runCveSync,
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
                    , syncDbPath = dir </> osvDbFileName "npm"
                    , syncSlot = slot
                    }
        use dir slot envWith

{- | A typed stand-in for an exception escaping the fetch's typed contract (a
broken harness premise or a simulated invariant break), so no double throws a
stringly exception.
-}
newtype SyncSpecEscape = SyncSpecEscape Text
    deriving stock (Eq, Show)

instance Exception SyncSpecEscape

{- | A fetch whose HEAD answers the given ETag and whose download builds an
artifact via the given writer, returning the same ETag.
-}
fetchServing :: Maybe Text -> (FilePath -> IO ()) -> CveFetch
fetchServing mEtag write =
    CveFetch
        { fetchHeadEtag = pure (Right (DbEtag <$> mEtag))
        , fetchDownload = \dest -> case mEtag of
            Nothing -> throwIO (SyncSpecEscape "download called with no object present")
            Just etag -> write dest $> Right (DbEtag etag)
        }

-- | The typed transport fault the failing-fetch doubles report.
transportDown :: OsvDbFetchFault
transportDown = OsvDbTransport (transportFault TransportUnreachable "transport down")

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

spec :: Spec
spec = do
    describe "syncStep" $ do
        it "reports the object absent without attempting a download" $
            withSyncEnv $ \_ _ envWith -> do
                let fetch =
                        CveFetch
                            { fetchHeadEtag = pure (Right Nothing)
                            , fetchDownload = \_ -> throwIO (SyncSpecEscape "must not download")
                            }
                syncStep (envWith fetch) Nothing >>= \case
                    SyncAbsent -> pass
                    other -> expectationFailure ("expected SyncAbsent, got " <> show other)

        it "does nothing when the remote ETag matches the last seen one" $
            withSyncEnv $ \_ _ envWith -> do
                let fetch =
                        CveFetch
                            { fetchHeadEtag = pure (Right (Just (DbEtag "e1")))
                            , fetchDownload = \_ -> throwIO (SyncSpecEscape "must not download")
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

        it "a download that faults mid-stream is a SyncFetchFaulted outcome and the partial temp file is discarded" $
            withSyncEnv $ \_ _ envWith -> do
                let fetch =
                        CveFetch
                            { fetchHeadEtag = pure (Right (Just (DbEtag "e1")))
                            , fetchDownload = \dest -> do
                                writeFileBS dest "partial bytes"
                                pure (Left transportDown)
                            }
                    env = envWith fetch
                syncStep env Nothing >>= \case
                    SyncFetchFaulted fault -> fault `shouldBe` transportDown
                    other -> expectationFailure ("expected SyncFetchFaulted, got " <> show other)
                doesFileExist (syncDbPath env <> ".tmp") `shouldReturn` False

        it "a head fault is a SyncFetchFaulted outcome; nothing is downloaded" $
            withSyncEnv $ \_ _ envWith -> do
                let fetch =
                        CveFetch
                            { fetchHeadEtag = pure (Left transportDown)
                            , fetchDownload = \_ -> throwIO (SyncSpecEscape "must not download")
                            }
                syncStep (envWith fetch) Nothing >>= \case
                    SyncFetchFaulted fault -> fault `shouldBe` transportDown
                    other -> expectationFailure ("expected SyncFetchFaulted, got " <> show other)

        it "residue: a download that throws past its typed contract still discards the partial temp file" $
            withSyncEnv $ \_ _ envWith -> do
                -- The fetch contract reports every failure as a value, so a throw
                -- here is an invariant break; the onException guard must still
                -- discard the partial download on its way up.
                let fetch =
                        CveFetch
                            { fetchHeadEtag = pure (Right (Just (DbEtag "e1")))
                            , fetchDownload = \dest -> do
                                writeFileBS dest "partial bytes"
                                throwIO (SyncSpecEscape "connection reset mid-stream")
                            }
                    env = envWith fetch
                syncStep env Nothing `shouldThrow` anyException
                doesFileExist (syncDbPath env <> ".tmp") `shouldReturn` False

        it "an artifact whose meta values violate the strict declaration is refused and its ETag remembered" $
            withSyncEnv $ \_ slot envWith -> do
                void (syncStep (envWith (fetchServing (Just "e1") (`mkMinimalValidDb` "pkg-a"))) Nothing)
                downloads <- newIORef (0 :: Int)
                let fetch =
                        CveFetch
                            { fetchHeadEtag = pure (Right (Just (DbEtag "e2")))
                            , fetchDownload = \dest -> do
                                modifyIORef' downloads (+ 1)
                                mkDbWithMalformedProvenance dest
                                pure (Right (DbEtag "e2"))
                            }
                    env = envWith fetch
                syncStep env (Just (DbEtag "e1")) >>= \case
                    SyncRejected etag (CveDbIntegrityFailed _) -> etag `shouldBe` DbEtag "e2"
                    other -> expectationFailure ("expected SyncRejected on the forged artifact, got " <> show other)
                doesFileExist (syncDbPath env <> ".tmp") `shouldReturn` False
                probesFor slot "pkg-a" `shouldReturn` Just True
                -- The remembered ETag turns the next poll into a no-op: the same
                -- bad object is never re-downloaded.
                syncStep env (Just (DbEtag "e2")) >>= \case
                    SyncUnchanged -> pass
                    other -> expectationFailure ("expected SyncUnchanged on the remembered ETag, got " <> show other)
                readIORef downloads `shouldReturn` 1

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
        it "the boot burst retries through typed fetch faults until the artifact lands" $
            withSyncEnv $ \_ slot envWith -> do
                calls <- newIORef (0 :: Int)
                notified <- newIORef (0 :: Int)
                let flaky =
                        CveFetch
                            { fetchHeadEtag = do
                                n <- atomicModifyIORef' calls (\n -> (n + 1, n + 1))
                                pure $
                                    if n <= 2
                                        then Left transportDown
                                        else Right (Just (DbEtag "e1"))
                            , fetchDownload = \dest -> mkMinimalValidDb dest "pkg-a" $> Right (DbEtag "e1")
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
                                    False -> Right Nothing
                                    True -> Right (Just (DbEtag "e1"))
                            , fetchDownload = \dest -> mkMinimalValidDb dest "pkg-a" $> Right (DbEtag "e1")
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
                            { fetchHeadEtag = pure (Right (Just (DbEtag "bad")))
                            , fetchDownload = \dest -> do
                                modifyIORef' downloads (+ 1)
                                mkDbWithWrongEpoch dest
                                pure (Right (DbEtag "bad"))
                            }
                    schedule = SyncSchedule{schedBootBackoff = replicate 5 10_000, schedPollDelay = 20_000}
                withAsync (runQuiet (runCveSync (envWith fetch) schedule pass)) $ \_ -> do
                    threadDelay 200_000
                    -- One download despite the burst budget and several polls:
                    -- identical bytes cannot verify differently, so the
                    -- remembered ETag reads as unchanged until a re-publish.
                    readIORef downloads `shouldReturn` 1
                    probesFor slot "pkg" `shouldReturn` Nothing

    describe "cappedAt" $ do
        it "passes a stream that ends exactly at the cap through unchanged" $ do
            out <- runConduit (yieldMany (["ab", "cd"] :: [ByteString]) .| cappedAt 4 .| C.sinkList)
            mconcat out `shouldBe` ("abcd" :: ByteString)

        it "throws the confined cap exception the moment the stream oversteps the cap" $
            -- The conduit's mid-stream escape; the adapter boundary ('s3Download')
            -- folds it into the 'OsvDbTooLarge' value on the 'CveFetch' channel.
            runConduit (yieldMany (["ab", "cde"] :: [ByteString]) .| cappedAt 4 .| C.sinkList)
                `shouldThrow` (== OsvDbCapExceeded 4)
