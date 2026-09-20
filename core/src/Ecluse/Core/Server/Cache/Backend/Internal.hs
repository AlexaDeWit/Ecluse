-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | Retention handles and their storage capability classification.
module Ecluse.Core.Server.Cache.Backend.Internal (
    RetentionBackend (..),
    BackendStorage (..),
    Recency (..),
    CacheOccupancy (..),
) where

-- | Backend-owned storage, independent of request coalescing and admission.
data RetentionBackend k v = RetentionBackend
    { rbStorage :: BackendStorage
    , rbLookup :: (CacheOccupancy -> IO ()) -> IO () -> Recency -> k -> IO (Maybe v)
    -- ^ Report occupancy changes and backend failure, then return a retained value.
    , rbInsert :: (CacheOccupancy -> IO ()) -> IO () -> IO () -> k -> v -> IO ()
    -- ^ Report occupancy, capacity refusal, and backend failure respectively.
    }

-- | Local storage cannot retain full metadata, regardless of its capacity.
data BackendStorage
    = LocalStorage
    | -- | External operation deadline, in microseconds, capped by the handle.
      ExternalStorage Int
    deriving stock (Eq, Show)

-- | Whether a probe contributes to eviction recency.
data Recency = PreserveRecency | RefreshRecency
    deriving stock (Eq, Show)

-- | Entry count and summed accounted bytes after a store mutation.
data CacheOccupancy = CacheOccupancy
    { occEntries :: Int
    , occBytes :: Int
    }
    deriving stock (Eq, Show)
