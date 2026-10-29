-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Charged LF framing keeps native consumers responsible for record syntax and state.
module Ecluse.Core.Registry.Metadata.RecordStream (recordSteps, finishRecordSteps) where

import Data.ByteString qualified as BS

import Ecluse.Core.Registry.JsonStream (Steps (..))
import Ecluse.Core.Security (LimitError)

-- | Charge fragments before retention and consume every physical record through EOF.
recordSteps :: (Monad m) => (s -> Int -> ByteString -> m Int) -> (s -> Int -> ByteString -> Either LimitError s) -> (s -> Steps m a) -> s -> Steps m a
recordSteps reserve consume complete = await [] 0
  where
    await pieces charge current = NeedData (receive current pieces charge)
    receive current pieces charge chunk
        | BS.null chunk = pure $ case pieces of
            [] -> complete current
            _ -> either Refused complete (consume current charge (BS.concat (reverse pieces)))
        | otherwise = splitLines current pieces charge chunk
    splitLines !current pieces charge chunk = case BS.elemIndex 10 chunk of
        Nothing -> do
            nextCharge <- reserve current charge chunk
            pure (await (chunk : pieces) nextCharge current)
        Just at -> do
            let (line, rest) = BS.splitAt at chunk
            nextCharge <- reserve current charge line
            let record = case pieces of [] -> line; _ -> BS.concat (reverse (line : pieces))
            case consume current nextCharge record of
                Left fault -> pure (Refused fault)
                Right next
                    | BS.length rest == 1 -> pure (await [] 0 next)
                    | otherwise -> splitLines next [] 0 (BS.drop 1 rest)

-- | Close an unterminated final record without inventing a source byte.
finishRecordSteps :: (Monad m) => Steps m a -> m (Steps m a)
finishRecordSteps = \case
    NeedData next -> next BS.empty
    step -> pure step
