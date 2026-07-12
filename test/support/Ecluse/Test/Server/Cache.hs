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

import Ecluse.Core.Server.Cache (CacheConfig (..))

{- | The standard cache-tunables fixture: a 60-second TTL, 1024 entries, and a
256 MiB resident budget, the same balanced posture the shipped configuration
defaults to. Suites and harnesses pass it to
'Ecluse.Core.Server.Cache.newMetadataCache' wherever a cache is needed and the
tunables are not the axis under test.
-}
defaultCacheConfig :: CacheConfig
defaultCacheConfig =
    CacheConfig
        { cacheTtl = 60
        , cacheMaxEntries = 1024
        , cacheMaxBytes = 256 * 1024 * 1024
        }
