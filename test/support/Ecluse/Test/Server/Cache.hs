-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Test and bench fixtures for "Ecluse.Core.Server.Cache".

This mirrors the module under test, under the @Ecluse.X -> Ecluse.Test.X@
convention this support library follows: the cache tunables the suites and the
performance harnesses build a metadata cache with when the tunables themselves
are not the axis under test. The live proxy derives its 'CacheConfig' from
configuration; this fixture stands in for that config-derived value.
-}
module Ecluse.Test.Server.Cache (
    -- * Cache configuration fixtures
    defaultCacheConfig,
) where

import Ecluse.Core.Server.Cache (CacheConfig (..), StoreBudget (..))

{- | The standard cache-tunables fixture: a 60-second TTL over a 256 MiB cache
aggregate split into the three stores' sub-budgets the way the live composition
root splits it (full 60%, version 15% at four times the entries, assembled the
remainder). Suites and harnesses pass it to
'Ecluse.Core.Server.Cache.newMetadataCache' wherever a cache is needed and the
tunables are not the axis under test.
-}
defaultCacheConfig :: CacheConfig
defaultCacheConfig =
    CacheConfig
        { cacheTtl = 60
        , cacheFullBudget = StoreBudget{sbMaxEntries = 1024, sbMaxBytes = 154 * 1024 * 1024}
        , cacheVersionBudget = StoreBudget{sbMaxEntries = 4096, sbMaxBytes = 38 * 1024 * 1024}
        , cacheAssembledBudget = StoreBudget{sbMaxEntries = 1024, sbMaxBytes = 64 * 1024 * 1024}
        }
