-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | Optional retention with bounded external operations and backend-owned codecs.
module Ecluse.Core.Server.Cache.Backend (
    RetentionBackend,
    Recency (..),
    CacheOccupancy (..),
    externalBackend,
    supportsFullRetention,
) where

import UnliftIO.Exception (tryAny)
import UnliftIO.Timeout (timeout)

import Ecluse.Core.Server.Cache.Backend.Internal

{- | Wrap synchronous external reads and writes. Each operation gets at most one second.
The adapter owns TTL, bounded decoding, identity validation, and its storage representation.
-}
externalBackend :: Int -> (Recency -> k -> IO (Maybe v)) -> (k -> v -> IO ()) -> RetentionBackend k v
externalBackend micros readValue writeValue =
    RetentionBackend
        { rbStorage = ExternalStorage
        , rbLookup = \_ failed recency key -> join <$> bounded failed (readValue recency key)
        , rbInsert = \_ _ failed key value -> void (bounded failed (writeValue key value))
        }
  where
    bounded failed action = do
        result <- tryAny (timeout (max 1 (min 1_000_000 micros)) action)
        case result of
            Right (Just value) -> pure (Just value)
            _ -> failed $> Nothing

-- | Only external storage is eligible to retain full metadata.
supportsFullRetention :: RetentionBackend k v -> Bool
supportsFullRetention backend = rbStorage backend == ExternalStorage
