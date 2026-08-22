-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Test and bench fixtures for "Ecluse.Core.Server.Cache".

The module name follows this support library's @Ecluse.X -> Ecluse.Test.X@
convention. It holds the cache tunables the suites and the performance harnesses
build a metadata cache with when the tunables are not the axis under test. The
live proxy derives its 'CacheConfig' from configuration. This fixture stands in
for that config-derived value.
-}
module Ecluse.Test.Server.Cache (
    -- * Cache configuration fixtures
    defaultCacheConfig,
) where

import Ecluse.Core.Server.Cache (CacheConfig (..), StoreBudget (..))

{- | The standard cache-tunables fixture: a 60-second TTL over a 256 MiB aggregate, split into
three sub-budgets the way the live composition root does. Pass it wherever a spec needs a cache and
the tunables are not the axis under test.
-}
defaultCacheConfig :: CacheConfig
defaultCacheConfig =
    CacheConfig
        { cacheTtl = 60
        , cacheFullBudget = StoreBudget{sbMaxEntries = 1024, sbMaxBytes = 154 * 1024 * 1024}
        , cacheVersionBudget = StoreBudget{sbMaxEntries = 4096, sbMaxBytes = 38 * 1024 * 1024}
        , cacheAssembledBudget = StoreBudget{sbMaxEntries = 1024, sbMaxBytes = 64 * 1024 * 1024}
        }
