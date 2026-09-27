-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Byte limits for advisory streams. Callers own the breach effect, so ingestion
and runtime downloads can share the traversal without sharing error types.
-}
module Ecluse.Core.Stream (boundBytes, boundLines) where

import Conduit (ConduitT, await, yield)
import Data.ByteString qualified as BS

{- | Preserve chunks up to the byte cap. On breach, pass the observed byte count
to the action and stop without yielding the excess chunk, even if the action returns.
-}
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

{- | Split a stream into lines without their newline, holding at most @cap@ bytes of one line. A
longer line passes its length to the action and stops the stream, even if the action returns.
-}
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
