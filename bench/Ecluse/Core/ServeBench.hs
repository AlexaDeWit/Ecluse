{- | Work-per-request benches for the npm serve path: filtering a packument to the
surviving versions, rewriting every @dist.tarball@ onto the proxy origin
("Ecluse.Core.Registry.Npm.Filter"), re-serialising the body, and computing the own
@ETag@ over it ("Ecluse.Core.Server.Conditional") — the transform a metadata response
goes through before it is served.

A realistic micro-bench runs over @express@; a synthetic bench scales the version
count and asserts the serve transform stays linear, guarding the accidentally
quadratic class on the rewrite\/restrict\/re-serialise path. Building the filter plan
runs the engine's effectful rule sweep, so the filter+serve benches are 'IO'; the
pure rewrite-only bench stays pure.
-}
module Ecluse.Core.ServeBench (
    benchmarks,
) where

import Data.Aeson (Value, encode)
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Time (nominalDay)
import Ecluse.Bench.Corpus (
    benchEvalContext,
    benchPackageName,
    expressPackageName,
    loadExpress,
    projectInfo,
    syntheticPackumentValue,
    syntheticProxyBase,
 )
import Ecluse.Bench.Fit (notWorseThanLinearIO)
import Ecluse.Core.Package (PackageInfo)
import Ecluse.Core.Package.Filter (filterPlan)
import Ecluse.Core.Registry.Npm.Filter (
    FilterResult (Filtered, NoSurvivors),
    applyFilterPlan,
    rewriteTarballUrls,
 )
import Ecluse.Core.Rules.Types (PrecededRule, Rule (AllowIfPublishedBefore), atDefaultPrecedence)
import Ecluse.Core.Server.Conditional (ownETag, renderETag)
import Test.Tasty.Bench (Benchmark, bench, bgroup, env, whnf, whnfAppIO)

-- | The serve-transform benches: realistic over @express@, scaled over synthetic versions.
benchmarks :: Benchmark
benchmarks =
    env loadExpress $ \ ~(_, json) ->
        bgroup
            "serve (filter + url-rewrite + etag)"
            [ bench "express: rewrite + reserialise + ETag" (whnf rewriteServeDepth json)
            , bench "express: filter + serve" (whnfAppIO serveDepth (json, projectInfo expressPackageName json))
            , bench "synthetic / 2000: filter + serve" (whnfAppIO serveDepth (syntheticServeInput 2000))
            , -- A smaller upper bound than the other scaled benches: the serve op is
              -- the heaviest (filter + rewrite + a full re-serialise + a SHA-256 over the
              -- body), so each measured size costs more; this range still spans 128x,
              -- enough to fit the curve and flag a super-linear regression.
              notWorseThanLinearIO
                "scales linearly in version count"
                (32, 4096)
                syntheticServeInput
                serveDepth
            ]

{- | The transformed-body serve path: rewrite every tarball URL onto the proxy
origin, re-serialise, and compute the own @ETag@ over the bytes. Summarised to force
the whole rewritten body and its digest. Pure — no rule plan is involved.
-}
rewriteServeDepth :: Value -> Int
rewriteServeDepth value =
    let body = encode (rewriteTarballUrls syntheticProxyBase value)
     in T.length (renderETag (ownETag body)) + fromIntegral (BSL.length body)

{- | The full serve transform: build the filter plan over the versions (the engine's
effectful rule sweep), replay it onto the body (restrict to survivors + rewrite URLs),
re-serialise, and ETag the result.
-}
serveDepth :: (Value, PackageInfo) -> IO Int
serveDepth (value, info) = do
    plan <- filterPlan benchEvalContext serveRules info
    pure $ case applyFilterPlan syntheticProxyBase plan value of
        Filtered served ->
            let body = encode served
             in T.length (renderETag (ownETag body)) + fromIntegral (BSL.length body)
        NoSurvivors _ -> 0

{- | A permissive rule set: every legitimately-aged version survives, so the rewrite
and re-serialise path is exercised over the whole packument rather than short-circuited
to a no-survivors denial.
-}
serveRules :: [PrecededRule]
serveRules = [atDefaultPrecedence (AllowIfPublishedBefore nominalDay)]

-- | A synthetic packument of the given version count, paired with its projection.
syntheticServeInput :: Word -> (Value, PackageInfo)
syntheticServeInput n =
    let value = syntheticPackumentValue (fromIntegral n)
     in (value, projectInfo benchPackageName value)
