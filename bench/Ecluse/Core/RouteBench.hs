{- | Work-per-request bench for request routing: the npm classifier
("Ecluse.Core.Registry.Npm.Route") that turns a request's method and path segments
into a typed 'Route' on every request, before any metadata work happens.

The input is a representative mix of the request shapes the proxy sees — bare and
scoped packuments, tarball coordinates, the @ping@ probe, search, first-party
publishes (@PUT@), and unrecognised paths — so the bench reflects the real
classifier branch distribution rather than one hot path.
-}
module Ecluse.Core.RouteBench (
    benchmarks,
) where

import Network.HTTP.Types.Method (Method, methodGet, methodPut)

import Ecluse.Core.Registry.Npm.Route (classify)
import Ecluse.Core.Server.Route (Route (Packument, Ping, Publish, Search, Tarball, Unsupported))
import Test.Tasty.Bench (Benchmark, bench, bgroup, env, whnf)

-- | The classifier bench over a mixed batch of realistic requests.
benchmarks :: Benchmark
benchmarks =
    env (pure requests) $ \reqs ->
        bgroup
            "route.classify"
            [ bench "mixed npm requests" (whnf classifyDepth reqs)
            ]

-- | A batch of realistic npm requests (method + path segments), the classifier's input.
requests :: [(Method, [Text])]
requests = concat (replicate 1000 sample)
  where
    sample =
        [ (methodGet, ["express"])
        , (methodGet, ["lodash"])
        , (methodGet, ["@babel", "core"])
        , (methodGet, ["@types", "node"])
        , (methodGet, ["express", "-", "express-4.18.2.tgz"])
        , (methodGet, ["@babel", "core", "-", "core-7.24.0.tgz"])
        , (methodGet, ["-", "ping"])
        , (methodGet, ["-", "v1", "search"])
        , (methodGet, ["favicon.ico"])
        , (methodGet, [])
        , (methodPut, ["@acme", "widget"]) -- a first-party publish (bare-package PUT)
        , (methodPut, ["express", "-", "express-4.18.2.tgz"]) -- a PUT to a non-publish path
        ]

-- | Classify every request, summing a per-route code so each result is forced.
classifyDepth :: [(Method, [Text])] -> Int
classifyDepth = foldl' (\acc (method, segments) -> acc + routeCode (classify method segments)) 0

-- | A distinct code per route shape, forcing the classifier's result to a constructor.
routeCode :: Route -> Int
routeCode = \case
    Packument{} -> 1
    Tarball{} -> 2
    Ping -> 3
    Search -> 4
    Publish{} -> 5
    Unsupported -> 6
