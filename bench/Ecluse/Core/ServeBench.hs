{- | Work-per-request benches for the npm serve path: deciding a packument's
survivors, merging the gated set ("Ecluse.Core.Package.Merge"), assembling the
served document with the fused tarball rewrite
("Ecluse.Core.Registry.Npm.Filter"), re-serialising the body, and computing the own
@ETag@ over it ("Ecluse.Core.Server.Conditional") -- the transform a metadata response
goes through before it is served.

The realistic benches run the full serve transform over each corpus package, so the
filter\/merge\/assemble\/re-serialise cost is reported across the real distribution
of sizes and shapes -- the re-serialise touches the whole heterogeneous body, so a
heavy packument is where its cost is felt. A synthetic bench scales the version count
and asserts the serve transform stays linear, guarding the accidentally quadratic
class on the merge\/assemble\/re-serialise path; the synthetic generator is retained
__only__ for that complexity-scaling assertion. Building the filter plan runs the
engine's effectful rule sweep, so the filter+serve benches are 'IO'.
-}
module Ecluse.Core.ServeBench (
    benchmarks,
) where

import Data.Aeson (Value, encode)
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (nominalDay)
import Ecluse.Bench.Corpus (
    LoadedEntry,
    benchEvalContext,
    benchPackageName,
    entryInfo,
    entryName,
    projectInfo,
    syntheticPackumentValue,
    syntheticProxyBase,
 )
import Ecluse.Bench.Fit (notWorseThanLinearIO)
import Ecluse.Core.Package (PackageInfo)
import Ecluse.Core.Package.Filter (filterPlan, fpSurvivors, restrictToSurvivors)
import Ecluse.Core.Package.Merge (MergePlan (mpSurvivors), Provenance (GatedSource), mergePackuments)
import Ecluse.Core.Registry.Npm.Filter (assembleMergedPackument)
import Ecluse.Core.Rules.Types (PrecededRule, Rule (AllowIfOlderThan), atDefaultPrecedence)
import Ecluse.Core.Server.Conditional (ownETag, renderETag)
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnfAppIO)

-- | The serve-transform benches: realistic over the corpus, scaled over synthetic versions.
benchmarks :: [LoadedEntry] -> Benchmark
benchmarks loaded =
    bgroup "serve (filter + merge-assemble + etag)" $
        [ bench (entryName le) (whnfAppIO serveDepth (value, entryInfo le))
        | le@(_, _, value) <- loaded
        ]
            <> [ -- A smaller upper bound than the other scaled benches: the serve op is the
                 -- heaviest (filter + rewrite + a full re-serialise + a SHA-256 over the
                 -- body), so each measured size costs more; this range still spans 128x,
                 -- enough to fit the curve and flag a super-linear regression.
                 notWorseThanLinearIO
                    "scales linearly in version count"
                    (32, 4096)
                    syntheticServeInput
                    serveDepth
               ]

{- | The full serve transform, mirroring the serve pipeline's composition: build the
filter plan over the versions (the engine's effectful rule sweep), merge the gated
survivor set, assemble the served document from the plan (each surviving version
taken from the raw body with its tarball URL rewritten in the same pass),
re-serialise, and ETag the result.
-}
serveDepth :: (Value, PackageInfo) -> IO Int
serveDepth (value, info) = do
    plan <- filterPlan benchEvalContext serveRules info
    pure $ case mergePackuments [(GatedSource, restrictToSurvivors (fpSurvivors plan) info)] of
        Just merged
            | not (Map.null (mpSurvivors merged)) ->
                let body = encode (assembleMergedPackument syntheticProxyBase (Map.singleton 0 value) merged value)
                 in T.length (renderETag (ownETag body)) + fromIntegral (BSL.length body)
        _ -> 0

{- | A permissive rule set: every legitimately-aged version survives, so the assemble
and re-serialise path is exercised over the whole packument rather than short-circuited
to a no-survivors denial.
-}
serveRules :: [PrecededRule]
serveRules = [atDefaultPrecedence (AllowIfOlderThan nominalDay)]

-- | A synthetic packument of the given version count, paired with its projection.
syntheticServeInput :: Word -> (Value, PackageInfo)
syntheticServeInput n =
    let value = syntheticPackumentValue (fromIntegral n)
     in (value, projectInfo benchPackageName value)
