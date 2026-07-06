module Ecluse.Cve.SlotSpec (spec) where

import Control.Concurrent.STM (check)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import UnliftIO.Async (async, wait)
import UnliftIO.Concurrent (threadDelay)

import Ecluse.Core.Cve (AdvisoryRange (..), CveDb (..), CveLookup (..), DbEtag (..))
import Ecluse.Core.Cve.Slot (currentAdvisoryEtag, newCveSlot, swapIn, withSlotLookup)
import Ecluse.Test.Cve (fakeCveLookup)

{- | A fake owning resource over the in-memory lookup, recording every close so
the tests can pin exactly when a displaced generation is retired.
-}
fakeDb :: Text -> IORef [Text] -> CveDb
fakeDb tag closeLog =
    CveDb
        { cveDbLookup = fakeCveLookup [(tag, AdvisoryRange "GHSA-slot-0001" Nothing (Just "0") (Just "1.0.0") Nothing)]
        , cveDbClose = modifyIORef' closeLog (<> [tag])
        , cveDbMeta = []
        }

-- | Which artifact generation answered: probe the tag each fake keys its row by.
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
        swapIn slot (DbEtag "gen-a") (fakeDb "gen-a" closeLog)
        withSlotLookup slot (traverse (\l -> cveRemediationProbe l "gen-a" "1.0.0"))
            `shouldReturn` Just True
        -- Nothing was displaced, so nothing was closed.
        readIORef closeLog `shouldReturn` []

    it "a swap closes the displaced generation once its readers drain, and not before" $ do
        closeLog <- newIORef []
        slot <- newCveSlot
        swapIn slot (DbEtag "gen-a") (fakeDb "gen-a" closeLog)

        insideReader <- newEmptyMVar
        releaseReader <- newEmptyMVar
        pinned <- async $ withSlotLookup slot $ \mLookup -> do
            putMVar insideReader ()
            takeMVar releaseReader
            -- The old generation must still answer, even though the swap below
            -- has already landed: the bracket pinned it.
            generationSeen mLookup

        takeMVar insideReader
        swapper <- async (swapIn slot (DbEtag "gen-b") (fakeDb "gen-b" closeLog))
        -- Give the swap every chance to (wrongly) close early: it must be
        -- parked draining while the reader is inside.
        threadDelay 50_000
        readIORef closeLog `shouldReturn` []

        putMVar releaseReader ()
        -- The pinned reader saw gen-a (no gen-b row), never a torn generation.
        wait pinned `shouldReturn` Just False
        wait swapper
        readIORef closeLog `shouldReturn` ["gen-a"]

        -- Readers arriving after the swap see the new generation.
        withSlotLookup slot generationSeen `shouldReturn` Just True

    it "each swap retires exactly the generation it displaced" $ do
        closeLog <- newIORef []
        slot <- newCveSlot
        swapIn slot (DbEtag "gen-a") (fakeDb "gen-a" closeLog)
        swapIn slot (DbEtag "gen-b") (fakeDb "gen-b" closeLog)
        swapIn slot (DbEtag "gen-c") (fakeDb "gen-c" closeLog)
        readIORef closeLog `shouldReturn` ["gen-a", "gen-b"]

    it "concurrent readers all pin the generation; the swap waits for the last" $ do
        closeLog <- newIORef []
        slot <- newCveSlot
        swapIn slot (DbEtag "gen-a") (fakeDb "gen-a" closeLog)

        entered <- newTVarIO (0 :: Int)
        gate <- newEmptyMVar
        readers <- forM [1 :: Int .. 8] $ \_ -> async $
            withSlotLookup slot $ \mLookup -> do
                atomically (modifyTVar' entered (+ 1))
                readMVar gate
                generationSeen mLookup
        -- Only swap once every reader has acquired (pinned) the generation.
        atomically (readTVar entered >>= check . (== 8))
        swapper <- async (swapIn slot (DbEtag "gen-b") (fakeDb "gen-b" closeLog))
        threadDelay 50_000
        readIORef closeLog `shouldReturn` []

        putMVar gate ()
        results <- traverse wait readers
        wait swapper
        readIORef closeLog `shouldReturn` ["gen-a"]
        -- Every pinned reader answered from gen-a; none observed gen-b.
        results `shouldBe` replicate 8 (Just False)

    it "reports the active generation's ETag for the audit trail, Nothing before the first swap" $ do
        closeLog <- newIORef []
        slot <- newCveSlot
        currentAdvisoryEtag slot `shouldReturn` Nothing
        swapIn slot (DbEtag "gen-a") (fakeDb "gen-a" closeLog)
        currentAdvisoryEtag slot `shouldReturn` Just (DbEtag "gen-a")
        swapIn slot (DbEtag "gen-b") (fakeDb "gen-b" closeLog)
        currentAdvisoryEtag slot `shouldReturn` Just (DbEtag "gen-b")
