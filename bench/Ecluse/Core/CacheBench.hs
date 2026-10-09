-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

{- | Cache maintenance measurements without registry transport or parsing.
Cold fills create a store per sample. Churn and hits use prefilled stores.
Key construction builds or renders one key per sample.
-}
module Ecluse.Core.CacheBench (benchmarks) where

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Server.Cache.Store (SingleFlight, newSingleFlightWithBackend, resolveSingleFlight)
import Ecluse.Core.Server.Cache.Types (CacheKey, Source (Source), assembledKey, fullKey, renderCacheKey, versionKey)
import Ecluse.Core.Server.Pipeline.Packument (packumentETag)
import Ecluse.Core.Version (Version)
import Ecluse.Test.Package (npmVersion, pypiVersion, scopedNpm, unscopedNpm, unscopedPyPI)
import Ecluse.Test.Server.Cache (newSingleFlight)
import Test.Tasty (localOption, mkTimeout)
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnf, whnfAppIO, whnfIO)

-- | Compare whole-store fill and churn with 100,000 hot hits at three capacities, beside one key's build and rendering.
benchmarks :: IO Benchmark
benchmarks = do
    retained <- traverse capacityBench [256, 1024, 4096]
    activeOnly <- newSingleFlightWithBackend Nothing
    pure (bgroup "cache maintenance" (bench "100000 resolutions without retention" (whnfIO (replicateM_ 100000 (resolveKey activeOnly 0))) : keyConstruction : retained))

-- The key each cached read builds before it reaches a store, and its rendering, which only a byte store asks for.
keyConstruction :: Benchmark
keyConstruction = bgroup "key construction" (concatMap buildRows keySubjects <> renderRows npmSubject)

type KeySubject = (String, Text, Source, PackageName, Version)

buildRows :: KeySubject -> [Benchmark]
buildRows (label, mountBase, source@(Source origin), name, version) =
    [ bench ("full key: " <> label) (whnf (uncurry fullKey) (source, name))
    , bench ("selected-version key: " <> label) (whnf selectedKey (source, name, version))
    , bench ("assembled key: " <> label) (whnf assembledKey (packumentETag mountBase [origin] name []))
    ]

-- The whole subject arrives as the measured argument, so no part of a key is built once and shared.
selectedKey :: (Source, PackageName, Version) -> CacheKey
selectedKey (source, name, version) = versionKey source name version

-- Each row renders a key built once, outside the measurement.
renderRows :: KeySubject -> [Benchmark]
renderRows (label, mountBase, source@(Source origin), name, version) =
    [ bench ("render full key: " <> label) (whnf renderCacheKey (fullKey source name))
    , bench ("render selected-version key: " <> label) (whnf renderCacheKey (versionKey source name version))
    , bench ("render assembled key: " <> label) (whnf renderCacheKey (assembledKey (packumentETag mountBase [origin] name [])))
    ]

keySubjects :: [KeySubject]
keySubjects =
    [ npmSubject
    , ("scoped npm name", "https://proxy.example/npm", Source "https://registry.npmjs.org", scopedNpm "babel" "core", npmVersion "7.26.0")
    , ("PyPI name", "https://proxy.example/pypi", Source "https://pypi.org/simple", unscopedPyPI "requests", pypiVersion "2.32.3")
    ]

npmSubject :: KeySubject
npmSubject = ("npm name", "https://proxy.example/npm", Source "https://registry.npmjs.org", unscopedNpm "react", npmVersion "18.3.1")

capacityBench :: Int -> IO Benchmark
capacityBench capacity = do
    churnStore <- filledStore capacity
    hotStore <- filledStore capacity
    nextRange <- newIORef capacity
    pure $
        localOption (mkTimeout 5_000_000) $
            bgroup
                (show capacity)
                [ bench "cold fill" (whnfAppIO filledStore capacity)
                , bench "eligible-store churn" (whnfIO (churn churnStore nextRange capacity))
                , bench "100000 hot hits" (whnfIO (replicateM_ 100000 (resolveKey hotStore 0)))
                ]

filledStore :: Int -> IO (SingleFlight () Int Int)
filledStore capacity = do
    store <- newSingleFlight 86400 capacity capacity (const 1)
    traverse_ (resolveKey store) [0 .. capacity - 1]
    pure store

churn :: SingleFlight () Int Int -> IORef Int -> Int -> IO ()
churn store nextRange capacity = do
    firstKey <- atomicModifyIORef' nextRange (\start -> (capacity - start, start))
    traverse_ (resolveKey store) [firstKey .. firstKey + capacity - 1]

resolveKey :: SingleFlight () Int Int -> Int -> IO ()
resolveKey store key =
    void (resolveSingleFlight (const pass) (const pass) pass store key (pure (Right key)))
