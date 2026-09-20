-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Cache configuration shared by tests and benchmarks.
module Ecluse.Test.Server.Cache (
    -- * Cache configuration fixtures
    defaultCacheConfig,
) where

import Ecluse.Core.Server.Cache (CacheConfig (..), StoreBudget (..))

-- | A 60-second TTL and 256 MiB split between locally eligible stores.
defaultCacheConfig :: CacheConfig
defaultCacheConfig =
    CacheConfig
        { cacheTtl = 60
        , cacheFullBudget = StoreBudget{sbMaxEntries = 0, sbMaxBytes = 0}
        , cacheVersionBudget = StoreBudget{sbMaxEntries = 4096, sbMaxBytes = (256 * 1024 * 1024) * 3 `div` 8}
        , cacheAssembledBudget = StoreBudget{sbMaxEntries = 1024, sbMaxBytes = 256 * 1024 * 1024 - (256 * 1024 * 1024) * 3 `div` 8}
        }
