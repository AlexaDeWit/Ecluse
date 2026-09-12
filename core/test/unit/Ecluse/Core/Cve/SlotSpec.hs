-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Cve.SlotSpec (spec) where

import Control.Concurrent.STM (check)
import GHC.Clock (getMonotonicTime)
import GHC.Conc (BlockReason (BlockedOnException), ThreadStatus (ThreadBlocked), threadStatus)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldSatisfy)
import UnliftIO.Async (async, asyncThreadId, cancel, poll, wait, waitCatch, withAsync)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (mask_)
import UnliftIO.Timeout (timeout)

import Ecluse.Core.Cve (AdvisoryRange (..), CveDb (..), CveLookup (..), DbEtag (..))
import Ecluse.Core.Cve.Slot (currentAdvisoryEtag, generationInstalledAt, newCveSlot, swapIn, withSlotLookup)
import Ecluse.Core.Osv.Provenance (noProvenance)
import Ecluse.Core.Osv.Types (UpperBound (FixedBefore))
import Ecluse.Test.Cve (fakeCveLookup)

fakeDb :: Text -> IORef [Text] -> CveDb
fakeDb tag closeLog =
    CveDb
        { cveDbLookup = fakeCveLookup [(tag, AdvisoryRange "GHSA-slot-0001" Nothing (Just "0") (FixedBefore "1.0.0") Nothing)]
        , cveDbClose = atomicModifyIORef' closeLog (\tags -> (tags <> [tag], ()))
        , cveDbMeta = []
        , cveDbProvenance = noProvenance
        }

generationSeen :: Maybe CveLookup -> IO (Maybe Bool)
generationSeen = traverse (\l -> cveRemediationProbe l "gen-b" "1.0.0")

spec :: Spec
spec = describe "CveSlot" $ do
    it "hands Nothing before the first swap (the pre-first-sync abstain path)" $ do
        slot <- newCveSlot
        withSlotLookup slot (pure . isJust) `shouldReturn` False

    it "hands the installed generation's view after a swap" $ do
        closeLog <- newIORef []
        slot <- newCveSlot
        swapIn slot (DbEtag "gen-a") Nothing (fakeDb "gen-a" closeLog)
        withSlotLookup slot (traverse (\l -> cveRemediationProbe l "gen-a" "1.0.0"))
            `shouldReturn` Just True
        readIORef closeLog `shouldReturn` []

    it "a swap closes the displaced generation once its readers drain, and not before" $ do
        closeLog <- newIORef []
        slot <- newCveSlot
        swapIn slot (DbEtag "gen-a") Nothing (fakeDb "gen-a" closeLog)

        insideReader <- newEmptyMVar
        releaseReader <- newEmptyMVar
        pinned <- async $ withSlotLookup slot $ \mLookup -> do
            putMVar insideReader ()
            takeMVar releaseReader
            generationSeen mLookup

        takeMVar insideReader
        swapper <- async (swapIn slot (DbEtag "gen-b") Nothing (fakeDb "gen-b" closeLog))
        threadDelay 50_000
        readIORef closeLog `shouldReturn` []

        putMVar releaseReader ()
        wait pinned `shouldReturn` Just False
        wait swapper
        readIORef closeLog `shouldReturn` ["gen-a"]

        withSlotLookup slot generationSeen `shouldReturn` Just True

    for_ [False, True] $ \cancelReader ->
        it ("retires after swapper cancellation and reader " <> if cancelReader then "cancellation" else "release") $ do
            closeLog <- newIORef []
            slot <- newCveSlot
            swapIn slot (DbEtag "gen-a") Nothing (fakeDb "gen-a" closeLog)
            insideReader <- newEmptyMVar
            releaseReader <- newEmptyMVar
            insideSwapper <- newEmptyMVar
            withAsync
                ( withSlotLookup slot $ \lookupA -> do
                    putMVar insideReader ()
                    takeMVar releaseReader
                    generationSeen lookupA `shouldReturn` Just False
                )
                $ \pinned -> do
                    takeMVar insideReader
                    withAsync
                        ( mask_ $ do
                            putMVar insideSwapper ()
                            swapIn slot (DbEtag "gen-b") Nothing (fakeDb "gen-b" closeLog)
                        )
                        $ \swapper -> do
                            takeMVar insideSwapper
                            timeout 1_000_000 (cancel swapper) `shouldReturn` Just ()
                            currentAdvisoryEtag slot `shouldReturn` Just (DbEtag "gen-b")
                            withSlotLookup slot generationSeen `shouldReturn` Just True
                            readIORef closeLog `shouldReturn` []
                            if cancelReader
                                then timeout 1_000_000 (cancel pinned) `shouldReturn` Just ()
                                else do
                                    putMVar releaseReader ()
                                    timeout 1_000_000 (wait pinned) `shouldReturn` Just ()
                    readIORef closeLog `shouldReturn` ["gen-a"]
                    swapIn slot (DbEtag "gen-c") Nothing (fakeDb "gen-c" closeLog)
                    readIORef closeLog `shouldReturn` ["gen-a", "gen-b"]

    it "finishes retirement when the last reader is cancelled during close" $ do
        closeLog <- newIORef []
        slot <- newCveSlot
        insideReader <- newEmptyMVar
        releaseReader <- newEmptyMVar
        insideClose <- newEmptyMVar
        finishClose <- newEmptyMVar
        insideSwapper <- newEmptyMVar
        let db = fakeDb "gen-a" closeLog
            closingDb = db{cveDbClose = putMVar insideClose () >> takeMVar finishClose >> cveDbClose db}
        swapIn slot (DbEtag "gen-a") Nothing closingDb
        withAsync
            (withSlotLookup slot $ \_ -> putMVar insideReader () >> takeMVar releaseReader)
            $ \pinned -> do
                takeMVar insideReader
                withAsync
                    ( mask_ $ do
                        putMVar insideSwapper ()
                        swapIn slot (DbEtag "gen-b") Nothing (fakeDb "gen-b" closeLog)
                    )
                    $ \swapper -> do
                        takeMVar insideSwapper
                        timeout 1_000_000 (cancel swapper) `shouldReturn` Just ()
                        putMVar releaseReader ()
                        takeMVar insideClose
                        withAsync (cancel pinned) $ \canceller -> do
                            let awaitCancellation = do
                                    status <- threadStatus (asyncThreadId canceller)
                                    unless (status == ThreadBlocked BlockedOnException) $ do
                                        threadDelay 1_000
                                        awaitCancellation
                            cancellationPending <- timeout 1_000_000 awaitCancellation
                            stillClosing <- isNothing <$> poll pinned
                            putMVar finishClose ()
                            timeout 1_000_000 (wait canceller) `shouldReturn` Just ()
                            cancellationPending `shouldBe` Just ()
                            stillClosing `shouldBe` True
                        void (waitCatch pinned)
                readIORef closeLog `shouldReturn` ["gen-a"]
                swapIn slot (DbEtag "gen-c") Nothing (fakeDb "gen-c" closeLog)
                readIORef closeLog `shouldReturn` ["gen-a", "gen-b"]

    it "each swap retires exactly the generation it displaced" $ do
        closeLog <- newIORef []
        slot <- newCveSlot
        swapIn slot (DbEtag "gen-a") Nothing (fakeDb "gen-a" closeLog)
        swapIn slot (DbEtag "gen-b") Nothing (fakeDb "gen-b" closeLog)
        swapIn slot (DbEtag "gen-c") Nothing (fakeDb "gen-c" closeLog)
        readIORef closeLog `shouldReturn` ["gen-a", "gen-b"]

    it "concurrent readers all pin the generation; the swap waits for the last" $ do
        closeLog <- newIORef []
        slot <- newCveSlot
        swapIn slot (DbEtag "gen-a") Nothing (fakeDb "gen-a" closeLog)

        entered <- newTVarIO (0 :: Int)
        gate <- newEmptyMVar
        readers <- forM [1 :: Int .. 8] $ \_ -> async $
            withSlotLookup slot $ \mLookup -> do
                atomically (modifyTVar' entered (+ 1))
                readMVar gate
                generationSeen mLookup
        atomically (readTVar entered >>= check . (== 8))
        swapper <- async (swapIn slot (DbEtag "gen-b") Nothing (fakeDb "gen-b" closeLog))
        threadDelay 50_000
        readIORef closeLog `shouldReturn` []

        putMVar gate ()
        results <- traverse wait readers
        wait swapper
        readIORef closeLog `shouldReturn` ["gen-a"]
        results `shouldBe` replicate 8 (Just False)

    it "reports the active generation's ETag for the audit trail, Nothing before the first swap" $ do
        closeLog <- newIORef []
        slot <- newCveSlot
        currentAdvisoryEtag slot `shouldReturn` Nothing
        swapIn slot (DbEtag "gen-a") Nothing (fakeDb "gen-a" closeLog)
        currentAdvisoryEtag slot `shouldReturn` Just (DbEtag "gen-a")
        swapIn slot (DbEtag "gen-b") Nothing (fakeDb "gen-b" closeLog)
        currentAdvisoryEtag slot `shouldReturn` Just (DbEtag "gen-b")

    it "stamps its creation time, so the age gauge reads a real interval before the first swap" $ do
        before <- getMonotonicTime
        slot <- newCveSlot
        after <- getMonotonicTime
        stamp <- generationInstalledAt slot
        stamp `shouldSatisfy` within before after

    it "restamps on the swap that installs a generation, and the new stamp is the install time" $ do
        closeLog <- newIORef []
        slot <- newCveSlot
        created <- generationInstalledAt slot
        threadDelay 2_000
        before <- getMonotonicTime
        swapIn slot (DbEtag "gen-a") Nothing (fakeDb "gen-a" closeLog)
        after <- getMonotonicTime
        installed <- generationInstalledAt slot
        installed `shouldSatisfy` within before after
        installed `shouldSatisfy` (> created)

    it "leaves the stamp alone for a read, and for a poll that installs nothing" $ do
        closeLog <- newIORef []
        slot <- newCveSlot
        swapIn slot (DbEtag "gen-a") Nothing (fakeDb "gen-a" closeLog)
        installed <- generationInstalledAt slot
        threadDelay 2_000
        void (withSlotLookup slot (pure . isJust))
        void (currentAdvisoryEtag slot)
        generationInstalledAt slot `shouldReturn` installed

within :: Double -> Double -> Double -> Bool
within before after stamp = stamp >= before && stamp <= after
