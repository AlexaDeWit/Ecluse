-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Runtime.Telemetry.Batched.CellsSpec (spec) where

import Control.Concurrent (forkOn)
import Test.Hspec
import UnliftIO (concurrently_, forConcurrently_, try)
import UnliftIO.Exception (impureThrow)

import Ecluse.Runtime.Telemetry.Batched.Cells (Cells, drainCells, modifyOwnCell, newCells)
import Ecluse.Test.Support (withCapabilities)

spec :: Spec
spec = describe "modifyOwnCell and drainCells" $ do
    it "evaluates the new value before the swap, so a failing change leaves the cell as it was" $ do
        cells <- newCells (0 :: Int)
        modifyOwnCell cells (+ 7)
        outcome <- try (modifyOwnCell cells (const (impureThrow ChangeFailed)))
        outcome `shouldBe` Left ChangeFailed
        sum <$> drainCells cells `shouldReturn` 7

    it "loses and repeats no update while writers on several capabilities race a drain" $
        withCapabilities 4 $ do
            cells <- newCells (0 :: Int)
            raceDrain cells 20_000 `shouldReturn` writers * 20_000

    it "loses no update while every writer shares one cell long enough to force failed swaps" $ do
        -- Cells made on one capability give every later capability the same cell.
        cells <- withCapabilities 1 (newCells (0 :: Int))
        withCapabilities 4 (raceDrain cells 100_000) `shouldReturn` writers * 100_000

    it "gives a capability added after the cells were made the first cell" $ do
        cells <- withCapabilities 1 (newCells (0 :: Int))
        withCapabilities 2 $ do
            written <- newEmptyMVar
            _ <- forkOn 1 (modifyOwnCell cells (+ 5) >> putMVar written ())
            takeMVar written
        drainCells cells `shouldReturn` [5]

-- Every writer adds one per update while a drainer keeps taking the cells. Returns the total taken.
raceDrain :: Cells Int -> Int -> IO Int
raceDrain cells updates = do
    taken <- newIORef 0
    finished <- newIORef False
    let drainOnce = drainCells cells >>= \values -> modifyIORef' taken (+ sum values)
        drainUntilFinished = drainOnce >> readIORef finished >>= bool drainUntilFinished pass
    concurrently_
        (forConcurrently_ [1 .. writers] (\_ -> replicateM_ updates (modifyOwnCell cells (+ 1))) >> writeIORef finished True)
        drainUntilFinished
    drainOnce
    readIORef taken

-- A change whose result fails when evaluated.
data ChangeFailed = ChangeFailed
    deriving stock (Eq, Show)

instance Exception ChangeFailed

writers :: Int
writers = 8
