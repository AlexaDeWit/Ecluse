-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Byte limits and whole-stream digests for advisory streams. Callers own the breach effect, so
ingestion and runtime downloads can share the traversal without sharing error types.
-}
module Ecluse.Core.Stream (boundBytes, boundLines, digestWhole) where

import Conduit (ConduitT, await, yield)
import Crypto.Hash (hashInit, hashUpdate)
import Data.ByteString qualified as BS

import Ecluse.Core.Snapshot (ContentDigest, digestFromContext)

-- | Pass chunks up to the byte cap. A breach hands the action the bytes seen and stops before the excess chunk, even if the action returns.
boundBytes :: (Monad m) => Int -> (Int -> m ()) -> ConduitT ByteString ByteString m ()
boundBytes cap onBreach = go 0
  where
    go !seen =
        await >>= \case
            Nothing -> pass
            Just chunk ->
                let seen' = seen + BS.length chunk
                 in if seen' > cap
                        then lift (onBreach seen')
                        else yield chunk >> go seen'

-- | Split a stream into lines, holding at most @cap@ bytes of one. A longer line hands the action its length and stops, even if the action returns.
boundLines :: (Monad m) => Int -> (Int -> m ()) -> ConduitT ByteString ByteString m ()
boundLines cap onBreach = go BS.empty
  where
    go pending =
        await >>= \case
            Nothing -> unless (BS.null pending) (yield pending)
            Just chunk -> split (pending <> chunk)
    split buffer = case BS.elemIndex newline buffer of
        Just end
            | end > cap -> lift (onBreach end)
            | otherwise -> yield (BS.take end buffer) >> split (BS.drop (end + 1) buffer)
        Nothing
            | BS.length buffer > cap -> lift (onBreach (BS.length buffer))
            | otherwise -> go buffer
    newline = 10

-- | Pass every chunk on and return their SHA-256 at the end. Fuse it with 'Data.Conduit.fuseBoth', which drains the source into the digest.
digestWhole :: (Monad m) => ConduitT ByteString ByteString m ContentDigest
digestWhole = go hashInit
  where
    go context =
        await >>= \case
            Nothing -> pure (digestFromContext context)
            Just chunk -> do
                let !next = hashUpdate context chunk
                yield chunk
                go next
