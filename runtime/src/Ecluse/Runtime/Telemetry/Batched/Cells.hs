-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | One mutable cell per capability. A writer updates its own capability's cell by compare-and-swap,
evaluating the new value first, so it races another writer only when one is preempted between its
read and its swap. The loop is atomic-primops' own, compiled in the library, because coverage
instrumentation in the caller breaks the swap's pointer test. After 30 failed swaps it falls back
to a lazy update, which the next writer or a drain evaluates.
-}
module Ecluse.Runtime.Telemetry.Batched.Cells (
    Cells,
    newCells,
    modifyOwnCell,
    drainCells,
) where

import Data.Atomics (atomicModifyIORefCAS_)
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

-- | Apply a change to the calling thread's cell. A change that builds a strict value is evaluated whole.
modifyOwnCell :: Cells a -> (a -> a) -> IO ()
modifyOwnCell cells change = do
    (capability, _) <- threadCapability =<< myThreadId
    atomicModifyIORefCAS_ (fromMaybe (cellsFirst cells) (cellsAll cells V.!? capability)) change

-- | Take every cell's value, leaving the blank value in its place.
drainCells :: Cells a -> IO [a]
drainCells cells = traverse (`atomicSwapIORef` cellsBlank cells) (V.toList (cellsAll cells))
