-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The reporter over shared state: what reaches the log, and how rarely the serve path
writes to the cell the fold lives in. The fold itself is in
"Ecluse.Core.Rules.Outage.InternalSpec".
-}
module Ecluse.Core.Rules.OutageSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Time (addUTCTime)
import Test.Hspec

import Ecluse.Core.Rules.Outage
import Ecluse.Core.Rules.Outage.Internal (OutageStore (commitOutage), loggedAdmissionCap)
import Ecluse.Rules.Outage.Support (down, ident, isHealthy, period, up)
import Ecluse.Rules.Support (now)

spec :: Spec
spec = describe "sourceReporter" $ do
    it "emits each report through the shared state, and nothing per healthy evaluation" $ do
        (store, _, shared) <- countingStore
        clock <- newIORef now
        emitted <- newIORef []
        let reporter = sourceReporter period (readIORef clock) store (\r -> modifyIORef' emitted (r :))
        replicateM_ 3 (reportSource reporter (up "DenyIfCve"))
        reportSource reporter (down "DenyIfCve")
        replicateM_ 50 (reportSource reporter (down "DenyIfCve"))
        writeIORef clock (addUTCTime period now)
        reportSource reporter (down "DenyIfCve")
        reportSource reporter (up "DenyIfCve")
        reverse <$> readIORef emitted
            `shouldReturn` [ OutageBegan "DenyIfCve" "no advisory database loaded"
                           , OutageContinues now (Map.singleton "DenyIfCve" "no advisory database loaded")
                           , OutageRecovered now
                           ]
        readTVarIO shared >>= (`shouldSatisfy` isHealthy)

    it "commits only on a transition or a due reminder, never per evaluation inside the period" $ do
        -- The serve leg keeps evaluating through an outage, so the common path must stay a
        -- read and a pure check rather than a transaction on the shared state.
        (store, commits, _) <- countingStore
        clock <- newIORef now
        let reporter = sourceReporter period (readIORef clock) store (const pass)
        replicateM_ 10 (reportSource reporter (up "DenyIfCve"))
        readIORef commits `shouldReturn` 0
        reportSource reporter (down "DenyIfCve")
        readIORef commits `shouldReturn` 1
        forM_ [1 .. 200 :: Integer] $ \secs -> do
            writeIORef clock (addUTCTime (fromInteger secs) now)
            reportSource reporter (down "DenyIfCve")
        readIORef commits `shouldReturn` 1
        -- A second rule joining the outage changes the state, so it commits without a report.
        reportSource reporter (down "DenyIfEpss")
        readIORef commits `shouldReturn` 2
        writeIORef clock (addUTCTime period now)
        reportSource reporter (down "DenyIfCve")
        readIORef commits `shouldReturn` 3
        reportSource reporter (up "DenyIfCve")
        reportSource reporter (up "DenyIfEpss")
        readIORef commits `shouldReturn` 5

    it "commits an admission identity once, and decides its repeats on the read alone" $ do
        (store, commits, _) <- countingStore
        let reporter = sourceReporter period (pure now) store (const pass)
        noteAdmission reporter (ident "a" "1.0.0" ["DenyIfCve"]) `shouldReturn` True
        readIORef commits `shouldReturn` 0 -- healthy: logged, nothing to record
        reportSource reporter (down "DenyIfCve")
        noteAdmission reporter (ident "a" "1.0.0" ["DenyIfCve"]) `shouldReturn` True
        replicateM_ 100 (noteAdmission reporter (ident "a" "1.0.0" ["DenyIfCve"]) `shouldReturn` False)
        readIORef commits `shouldReturn` 2

    it "performs no commit for a repeat identity or an unchanged reading over a record at the cap" $ do
        -- The record holds thousands of identities here, so a decision that compared states
        -- would walk them all on the serve path. The count pins that nothing is written.
        (store, commits, _) <- countingStore
        let reporter = sourceReporter period (pure now) store (const pass)
        reportSource reporter (down "DenyIfCve")
        forM_ [1 .. loggedAdmissionCap] $ \n ->
            noteAdmission reporter (ident "a" (show n) ["DenyIfCve"]) `shouldReturn` True
        readIORef commits `shouldReturn` loggedAdmissionCap + 1
        replicateM_ 50 (noteAdmission reporter (ident "a" (show loggedAdmissionCap) ["DenyIfCve"]) `shouldReturn` False)
        replicateM_ 50 (reportSource reporter (down "DenyIfCve"))
        readIORef commits `shouldReturn` loggedAdmissionCap + 1

-- | The live store wrapped so the spec can count how many folds it committed.
countingStore :: IO (OutageStore, IORef Int, TVar OutageState)
countingStore = do
    shared <- newTVarIO Healthy
    commits <- newIORef (0 :: Int)
    let live = tvarOutageStore shared
    pure (live{commitOutage = \advance -> modifyIORef' commits (+ 1) >> commitOutage live advance}, commits, shared)
