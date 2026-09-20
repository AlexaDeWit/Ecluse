-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | Optional retention with bounded external operations and backend-owned codecs.
module Ecluse.Core.Server.Cache.Backend (
    RetentionBackend,
    BackendStorage (..),
    Recency (..),
    CacheOccupancy (..),
    retentionBackend,
    supportsFullRetention,
) where

import UnliftIO.Exception (tryAny)
import UnliftIO.Timeout (timeout)

import Ecluse.Core.Server.Cache.Backend.Internal

{- | Build storage independently of request coalescing. External deadlines cap at one second.
Adapters own TTL, bounded decoding, identity validation, and storage representation.
-}
retentionBackend ::
    BackendStorage ->
    ((CacheOccupancy -> IO ()) -> Recency -> k -> IO (Maybe v)) ->
    ((CacheOccupancy -> IO ()) -> IO () -> k -> v -> IO ()) ->
    RetentionBackend k v
retentionBackend storage readValue writeValue =
    RetentionBackend
        { rbStorage = storage
        , rbLookup = \record failed recency key -> runBackend storage failed Nothing (readValue record recency key)
        , rbInsert = \record refused failed key value -> runBackend storage failed () (writeValue record refused key value)
        }

runBackend :: BackendStorage -> IO () -> a -> IO a -> IO a
runBackend storage failed fallback action = case storage of
    LocalStorage -> action
    ExternalStorage micros -> do
        result <- tryAny (timeout (max 1 (min 1_000_000 micros)) action)
        case result of
            Right (Just value) -> pure value
            _ -> failed $> fallback

-- | Only external storage is eligible to retain full metadata.
supportsFullRetention :: RetentionBackend k v -> Bool
supportsFullRetention backend = case rbStorage backend of
    LocalStorage -> False
    ExternalStorage _ -> True
