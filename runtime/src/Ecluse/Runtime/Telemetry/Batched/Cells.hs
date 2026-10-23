-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | One mutable cell per capability, written by compare-and-swap with evaluated values. A writer
updates the cell of the capability it runs on, so it races another writer only when one of them is
preempted between its read and its swap, and it never waits on another thread's unfinished update.
-}
module Ecluse.Runtime.Telemetry.Batched.Cells (
    Cells,
    newCells,
    modifyOwnCell,
    drainCells,
    casModify,
) where

import Data.Atomics (casIORef, peekTicket, readForCAS)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Conc (getNumCapabilities, myThreadId, threadCapability)
import GHC.IORef (atomicSwapIORef)

-- | One cell per capability the runtime had when the cells were made, each starting from the blank value.
data Cells a = Cells
    { cellsBlank :: a
    , cellsFirst :: IORef a
    -- ^ Capability 0's cell, which a capability added after construction shares.
    , cellsAll :: Vector (IORef a)
    }

-- | Make one cell per capability.
newCells :: a -> IO (Cells a)
newCells blank = do
    count <- getNumCapabilities
    home <- newIORef blank
    rest <- V.replicateM (count - 1) (newIORef blank)
    pure Cells{cellsBlank = blank, cellsFirst = home, cellsAll = V.cons home rest}

-- | Apply a change to the calling thread's cell.
modifyOwnCell :: Cells a -> (a -> a) -> IO ()
modifyOwnCell cells change = do
    (capability, _) <- threadCapability =<< myThreadId
    casModify (fromMaybe (cellsFirst cells) (cellsAll cells V.!? capability)) change

-- | Take every cell's value, leaving the blank value in its place.
drainCells :: Cells a -> IO [a]
drainCells cells = traverse (`atomicSwapIORef` cellsBlank cells) (V.toList (cellsAll cells))

{- | Replace a value with the change applied to it, retrying if another writer swapped first. The new
value is evaluated to weak head normal form before the swap, so a cell's type should be strict.
-}
casModify :: IORef a -> (a -> a) -> IO ()
casModify ref change = readForCAS ref >>= attempt
  where
    attempt ticket = do
        let !new = change (peekTicket ticket)
        (swapped, current) <- casIORef ref ticket new
        unless swapped (attempt current)
