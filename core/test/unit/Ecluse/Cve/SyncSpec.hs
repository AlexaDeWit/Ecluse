module Ecluse.Cve.SyncSpec (spec) where

import Katip (Environment (Environment), KatipContextT, Namespace (Namespace), SimpleLogPayload, initLogEnv, runKatipContextT)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldReturn, shouldSatisfy)
import UnliftIO.Async (withAsync)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwString)

import Ecluse.Core.Cve (CveDbRejected (CveDbWrongEpoch), CveLookup (cveRemediationProbe))
import Ecluse.Core.Cve.Slot (CveSlot, newCveSlot, withSlotLookup)
import Ecluse.Core.Cve.Sync (
    CveFetch (..),
    DbEtag (..),
    SyncEnv (..),
    SyncOutcome (..),
    SyncSchedule (..),
    runCveSync,
    syncStep,
 )
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Osv.Schema (osvSchemaEpoch)
import Ecluse.Test.Osv (mkDbWithWrongEpoch, mkMinimalValidDb)

-- | An env over a fresh slot and a temp data dir, handed to each case.
withSyncEnv :: (FilePath -> CveSlot -> (CveFetch -> SyncEnv) -> IO a) -> IO a
withSyncEnv use =
    withSystemTempDirectory "ecluse-cve-sync" $ \dir -> do
        slot <- newCveSlot
        let envWith fetch =
                SyncEnv
                    { syncFetch = fetch
                    , syncEcosystem = Npm
                    , syncDbPath = dir </> "npm-osv-schema1.db"
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
