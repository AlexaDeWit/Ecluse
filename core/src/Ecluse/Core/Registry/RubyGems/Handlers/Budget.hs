-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native requests bound actual upstream chunks and examined source documents independently.
module Ecluse.Core.Registry.RubyGems.Handlers.Budget (
    newNativeReadBudget,
    newNativeDocumentBudget,
) where

import UnliftIO.Exception (throwIO)

import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge))
import Ecluse.Core.Server.Pipeline.Origin (ObservedReadBoundExceeded (ObservedReadBoundExceeded))

-- | Refuse before another source chunk reaches its parser when cumulative input exceeds the cap.
newNativeReadBudget :: Int -> IO (Int -> IO ())
newNativeReadBudget cap = do
    count <- newNativeByteCounter cap
    pure $ \bytes -> do
        exceeded <- count bytes
        when exceeded (throwIO (ObservedReadBoundExceeded (BodyTooLarge (MetadataBodyLimit cap))))

-- | Refuse before admission when examined source documents, including cache hits, exceed the cap.
newNativeDocumentBudget :: Int -> response -> IO (Int -> IO (Either response ()))
newNativeDocumentBudget cap refusal = do
    count <- newNativeByteCounter cap
    pure $ \bytes -> do
        exceeded <- count bytes
        pure (if exceeded then Left refusal else Right ())

newNativeByteCounter :: Int -> IO (Int -> IO Bool)
newNativeByteCounter cap = do
    total <- newIORef (0 :: Integer)
    pure $ \bytes ->
        atomicModifyIORef' total (\seen -> let next = seen + toInteger (max 0 bytes) in (next, next > toInteger cap))
