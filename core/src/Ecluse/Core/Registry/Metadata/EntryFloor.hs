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
import Ecluse.Core.Server.MemoryModel (saturatingCharge)

-- | The bytes each retained entry costs at least. A residency case measures dense listings to set it: see @docs/testing.md@.
entryFloorBytes :: Int
entryFloorBytes = 704

-- | The floor for a number of retained entries, saturating at the largest charge.
entryFloorCharge :: Int -> Int
entryFloorCharge retained = saturatingCharge (toInteger entryFloorBytes * toInteger retained)

-- | Continue a step after publishing the entries its accepted state retains.
countingEntries :: PrimVar st Int -> (s -> Int) -> (Either LimitError s -> ST st r) -> Either LimitError s -> ST st r
countingEntries counter entries next taken = do
    traverse_ (writePrimVar counter . entries) taken
    next taken
{-# INLINE countingEntries #-}

{- | Walk a body, raising @pay@'s absolute workspace before each piece to the floor for the entries
the walk has published, less the per-byte charge at @permille@ the pieces read so far already paid.
-}
readFlooredWalk ::
    (Int -> IO ()) ->
    Int ->
    BodyLimit ->
    (PrimVar RealWorld Int -> Cursor -> ST RealWorld (Steps (ST RealWorld) s)) ->
    IO ByteString ->
    IO (Either LimitError (StreamResult s))
readFlooredWalk pay permille bound walk readChunk = do
    entries <- newPrimVar 0
    seen <- newPrimVar 0
    held <- newIORef BS.empty
    let piece = do
            retained <- readPrimVar entries
            bytes <- readPrimVar seen
            let shortfall = entryFloorCharge retained - scaleCharge permille bytes
            when (shortfall > 0) (pay shortfall)
            pending <- readIORef held
            chunk <- if BS.null pending then readChunk else pure pending
            let (next, rest) = BS.splitAt maxChunkBytes chunk
            writeIORef held rest
            writePrimVar seen (bytes + BS.length next)
            pure next
    readJsonWalkST stToIO bound (walk entries) piece
-- Specialise in the reading module, which turns full laziness off. Compiled here, npm and PyPI full reads allocated 30% to 60% more.
{-# INLINE readFlooredWalk #-}
