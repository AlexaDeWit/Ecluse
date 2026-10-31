-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The least a full JSON listing read pays for each entry it retains. Per-byte charges are
calibrated on realistic listings, so a document of many tiny entries would retain more than its
bytes pay for. Such a read raises its workspace charge to the floor after each piece it walks, and
keeps the floor once it completes.
-}
module Ecluse.Core.Registry.Metadata.EntryFloor (
    entryFloorBytes,
    releaseFloorBytes,
    RetainedEntries (..),
    noRetainedEntries,
    entryFloorCharge,
    countingEntries,
    readFlooredWalk,
) where

import Control.Monad.ST (RealWorld, ST, stToIO)
import Data.ByteString qualified as BS
import Data.JsonStream.Lexer (Cursor)
import Data.Primitive.PrimVar (PrimVar, newPrimVar, readPrimVar, writePrimVar)

import Ecluse.Core.Registry.Json.Walk (readJsonWalkST)
import Ecluse.Core.Registry.JsonStream (Steps, StreamResult, maxChunkBytes)
import Ecluse.Core.Security (BodyLimit, LimitError)
import Ecluse.Core.Server.Admission.Budget (scaleCharge)

-- | The bytes each retained entry other than a version or file costs at least. A residency case measures dense listings to set it: see @docs/testing.md@.
entryFloorBytes :: Int
entryFloorBytes = 704

-- | The bytes each retained npm version or PyPI file costs at least, which the residency tier measures on minimal valid bodies.
releaseFloorBytes :: Int
releaseFloorBytes = 1152

-- | The entries a full read retains, by the floor each pays.
data RetainedEntries = RetainedEntries
    { retainedReleases :: Int
    -- ^ npm versions and PyPI files, dropped ones included, which pay 'releaseFloorBytes'.
    , retainedOthers :: Int
    -- ^ Every other retained entry, which pays 'entryFloorBytes'.
    }
    deriving stock (Eq, Show)

-- | No retained entries: a read with no floor, or a document no read produced.
noRetainedEntries :: RetainedEntries
noRetainedEntries = RetainedEntries 0 0

-- | The floors the retained entries owe, saturating at the largest charge. Each step of a walk computes it, so it stays in 'Int'.
entryFloorCharge :: RetainedEntries -> Int
entryFloorCharge entries
    | releases > bound || others > bound = maxBound
    | otherwise = releaseFloorBytes * releases + entryFloorBytes * others
  where
    releases = retainedReleases entries
    others = retainedOthers entries
    -- Up to this count each product stays within half the largest 'Int'.
    bound = maxBound `div` (2 * releaseFloorBytes)

-- | Continue a step after publishing the floors its accepted state's retained entries owe.
countingEntries :: PrimVar st Int -> (s -> RetainedEntries) -> (Either LimitError s -> ST st r) -> Either LimitError s -> ST st r
countingEntries owed entries next taken = do
    traverse_ (writePrimVar owed . entryFloorCharge . entries) taken
    next taken
{-# INLINE countingEntries #-}

{- | Walk a body, raising @pay@'s absolute workspace before each piece to the floors the walk has
published, less the per-byte charge at @permille@ the pieces read so far already paid.
-}
readFlooredWalk ::
    (Int -> IO ()) ->
    Int ->
    BodyLimit ->
    (PrimVar RealWorld Int -> Cursor -> ST RealWorld (Steps (ST RealWorld) s)) ->
    IO ByteString ->
    IO (Either LimitError (StreamResult s))
readFlooredWalk pay permille bound walk readChunk = do
    owed <- newPrimVar 0
    seen <- newPrimVar 0
    held <- newIORef BS.empty
    let piece = do
            floors <- readPrimVar owed
            bytes <- readPrimVar seen
            let shortfall = floors - scaleCharge permille bytes
            when (shortfall > 0) (pay shortfall)
            pending <- readIORef held
            chunk <- if BS.null pending then readChunk else pure pending
            let (next, rest) = BS.splitAt maxChunkBytes chunk
            writeIORef held rest
            writePrimVar seen (bytes + BS.length next)
            pure next
    readJsonWalkST stToIO bound (walk owed) piece
-- Specialise in the reading module, which turns full laziness off. Compiled here, npm and PyPI full reads allocated 30% to 60% more.
{-# INLINE readFlooredWalk #-}
