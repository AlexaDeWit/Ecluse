-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Runtime.Telemetry.Batched.CellsSpec (spec) where

import Control.Concurrent (forkOn)
import Test.Hspec
import UnliftIO (concurrently_, forConcurrently_, try)
import UnliftIO.Exception (impureThrow)

import Ecluse.Runtime.Telemetry.Batched.Cells (casModify, drainCells, modifyOwnCell, newCells)
import Ecluse.Test.Support (withCapabilities)

spec :: Spec
spec = do
    describe "casModify" $ do
        it "keeps every update while several threads change one cell at once" $
            withCapabilities 4 $ do
                ref <- newIORef (0 :: Int)
                forConcurrently_ [1 .. writers] $ \_ -> replicateM_ updates (casModify ref (+ 1))
                readIORef ref `shouldReturn` writers * updates

        it "evaluates the new value before the swap, so a failing change leaves the cell as it was" $ do
            ref <- newIORef (7 :: Int)
            outcome <- try (casModify ref (const (impureThrow ChangeFailed)))
            outcome `shouldBe` Left ChangeFailed
            readIORef ref `shouldReturn` 7

    describe "modifyOwnCell and drainCells" $ do
        it "loses and repeats no update while writers race a drain" $
            withCapabilities 4 $ do
                cells <- newCells (0 :: Int)
                taken <- newIORef (0 :: Int)
                finished <- newIORef False
                let drainOnce = drainCells cells >>= \values -> modifyIORef' taken (+ sum values)
                    drainUntilFinished = drainOnce >> readIORef finished >>= bool drainUntilFinished pass
                concurrently_
                    (forConcurrently_ [1 .. writers] (\_ -> replicateM_ updates (modifyOwnCell cells (+ 1))) >> writeIORef finished True)
                    drainUntilFinished
                drainOnce
                readIORef taken `shouldReturn` writers * updates

        it "gives a capability added after the cells were made the first cell" $ do
            cells <- withCapabilities 1 (newCells (0 :: Int))
            withCapabilities 2 $ do
                written <- newEmptyMVar
                _ <- forkOn 1 (modifyOwnCell cells (+ 5) >> putMVar written ())
                takeMVar written
            drainCells cells `shouldReturn` [5]

-- A change whose result fails when evaluated.
data ChangeFailed = ChangeFailed
    deriving stock (Eq, Show)

instance Exception ChangeFailed

writers :: Int
writers = 8

updates :: Int
updates = 20_000
