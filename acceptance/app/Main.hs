{- | The live performance-acceptance harness (Context B).

For each package in the shared curated catalogue it fetches the __live__ packument
from the registry — timing the fetch (the upstream leg) — then times two slices of
Écluse's work-per-request over it:

  * the __full-packument__ transform (decode, project, rule sweep, filter, URL
    rewrite, re-serialise, ETag) that a metadata read of every version pays; and
  * the __single-version__ selective decode the cold tarball gate consults to serve
    one package version (its latest), the per-package overhead a whole-document
    decode dominates on the heavy packuments and a selective decode does not.

Each measurement is checked against the version-controlled acceptance budget
("Ecluse.Acceptance"); the run prints a summary, mirrors it to the GitHub step
summary when present, and exits non-zero __only__ on a real budget breach.

Live and non-deterministic by design: a fetch or decode failure is reported as
unavailable, never a breach, so registry flakiness does not red the run — only an
over-budget measurement does. The acceptance decision itself is pure and
unit-tested in "Ecluse.Acceptance"; this module is the live measurement shell.
-}
module Main (main) where

import Control.Exception qualified as Exception
import Data.Aeson (eitherDecode, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime, nominalDay)
import GHC.Clock (getMonotonicTime)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Ecluse.Acceptance (Sample (..), evaluate, loadCriteria, renderReport, reportBreached)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageDetails (pkgDependencies), PackageName, mkPackageName, mkScope)
import Ecluse.Core.Package.Filter (filterPlan)
import Ecluse.Core.Registry.Npm.Filter (FilterResult (Filtered, NoSurvivors), applyFilterPlan, rewriteTarballUrls)
import Ecluse.Core.Registry.Npm.Metadata (projectNpmVersion)
import Ecluse.Core.Registry.Npm.Project (Projection (NameMismatch, Projected), parsePackageInfoFromValue)
import Ecluse.Core.Rules.Types (EvalContext (EvalContext), PrecededRule, Rule (AllowIfOlderThan), atDefaultPrecedence)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Server.Conditional (ownETag, renderETag)
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Test.RegistryCapture (catBenchPins, fetchPackumentBody, loadCatalogue, parseRegistryVersions)

main :: IO ()
main = do
    criteria <- loadCriteria
    catalogue <- loadCatalogue
    manager <- newManager tlsManagerSettings
    now <- getCurrentTime
    let names = Map.keys (catBenchPins catalogue)
    inputs <- traverse (measurePackage manager now) names
    let report = evaluate criteria inputs
        rendered = renderReport report
    putText rendered
    -- In CI, mirror the summary into the GitHub step summary so a breach is visible
    -- on the pull request without the workflow shelling around the harness.
    lookupEnv "GITHUB_STEP_SUMMARY" >>= traverse_ (`appendFileText` rendered)
    when (reportBreached report) exitFailure

-- ── live measurement (the non-deterministic shell) ───────────────────────────────

{- | Fetch one package's live packument and measure Écluse's overhead over it — both
the full-packument transform and the single-version selective decode of its latest
version. A @Left (name, reason)@ marks it unavailable — a fetch, decode, or projection
failure, which is never a breach — and a @Right sample@ carries the timed legs.
-}
measurePackage :: Manager -> UTCTime -> Text -> IO (Either (Text, Text) Sample)
measurePackage manager now name = do
    let pkg = parseNpmName name
    t0 <- getMonotonicTime
    mBody <- fetchPackumentBody manager Npm name
    t1 <- getMonotonicTime
    case mBody of
        Nothing -> pure (Left (name, "registry unreachable or non-2xx"))
        Just body -> case targetVersion body of
            Nothing -> pure (Left (name, "packument exposed no versions"))
            Just version -> do
                let raw = BSL.toStrict body
                fulls <- replicateM sampleCount (measureFull now pkg body)
                single <- measureSingleVersion pkg version raw
                pure $ case (sequence fulls, single) of
                    (Just fullSecs, Just singleSec) ->
                        Right
                            Sample
                                { sampleName = name
                                , sampleVersions = maybe 0 length (parseRegistryVersions Npm body)
                                , sampleUpstreamMs = (t1 - t0) * 1000
                                , sampleFullOverheadMs = median fullSecs * 1000
                                , sampleSingleVersionOverheadMs = singleSec * 1000
                                }
                    _ -> Left (name, "packument did not decode or project")

{- | The version a single-version read targets: the latest published (the last key in
the packument's version list, the realistic install target). 'Nothing' for a
packument exposing no versions.
-}
targetVersion :: LByteString -> Maybe Version
targetVersion body = mkVersion Npm . NE.last <$> (nonEmpty =<< parseRegistryVersions Npm body)

{- | Time one pass of the full-packument transform over a body. 'Nothing' when the body
does not decode or project (an unavailable input, not a slow one). The transform's
result is forced inside the timed region so the figure reflects the real decode, filter,
rewrite, and re-serialise work.
-}
measureFull :: UTCTime -> PackageName -> LByteString -> IO (Maybe Double)
measureFull now pkg body = do
    t0 <- getMonotonicTime
    done <- runTransform now pkg body
    t1 <- getMonotonicTime
    pure (if done then Just (t1 - t0) else Nothing)

{- | Time the single-version selective decode — the cold tarball gate's read of one
version's snapshot from the raw packument, parsing only that version rather than the whole
document. 'Nothing' when the version is absent or the body does not decode; otherwise the
median of a few passes, to damp noise.

The leg is a pure, deterministic computation, so timing it needs care: replicating the
__same__ projection would let GHC share one evaluation across the passes and time nothing
on the rest. Each pass therefore runs over a __distinct__ copy of the bytes ('BS.copy', a
fresh object the compiler cannot share), made outside the timed region, so every pass is a
genuine fresh decode. The selected snapshot is forced (by 'evaluate' over a deep field)
inside the timed region so the figure reflects the real selective-decode work.
-}
measureSingleVersion :: PackageName -> Version -> ByteString -> IO (Maybe Double)
measureSingleVersion pkg version raw = do
    copies <- replicateM sampleCount (Exception.evaluate (BS.copy raw))
    passes <- traverse timePass copies
    pure $ case catMaybes passes of
        [] -> Nothing
        secs -> Just (median secs)
  where
    timePass r = do
        t0 <- getMonotonicTime
        depth <- Exception.evaluate (selectiveDepth pkg version r)
        t1 <- getMonotonicTime
        pure (if depth >= 0 then Just (t1 - t0) else Nothing)

-- The single-version selective decode reduced to an 'Int' over a deep field of the
-- selected snapshot, so forcing it runs the real projection. A non-negative result
-- means the version was found and projected; -1 marks it absent, -2 a decode failure.
selectiveDepth :: PackageName -> Version -> ByteString -> Int
selectiveDepth pkg version raw =
    case projectNpmVersion defaultLimits pkg version raw of
        Right (Just details) -> length (pkgDependencies details)
        Right Nothing -> -1
        Left _ -> -2

{- | The full-packument work-per-request transform: decode the body, project it, sweep
the rules to build the filter plan, restrict the body to the survivors, rewrite the
tarball URLs, re-serialise, and ETag the result. Returns whether the input decoded and
projected; the computed size is forced so the whole transform actually runs.
-}
runTransform :: UTCTime -> PackageName -> LByteString -> IO Bool
runTransform now pkg body =
    case eitherDecode body of
        Left _ -> pure False
        Right value -> case parsePackageInfoFromValue pkg value of
            Right (Projected info) -> do
                plan <- filterPlan (EvalContext now) serveRules info
                let size :: Int
                    size = case applyFilterPlan plan value of
                        Filtered served ->
                            let out = encode (rewriteTarballUrls proxyBase served)
                             in T.length (renderETag (ownETag out)) + fromIntegral (BSL.length out)
                        NoSurvivors _ -> 0
                size `seq` pure True
            Right (NameMismatch _) -> pure False
            Left _ -> pure False

-- The number of passes timed per package; the median is reported, to damp noise.
sampleCount :: Int
sampleCount = 5

-- A permissive rule set so the rewrite and re-serialise run over the whole packument
-- rather than short-circuiting to a denial — the full per-request cost.
serveRules :: [PrecededRule]
serveRules = [atDefaultPrecedence (AllowIfOlderThan nominalDay)]

-- A placeholder proxy origin the tarball URLs are rewritten onto.
proxyBase :: Text
proxyBase = "https://ecluse.example"

-- Parse an npm package name into a 'PackageName', recovering an @\@scope/name@ split.
parseNpmName :: Text -> PackageName
parseNpmName raw = case T.stripPrefix "@" raw of
    Just rest
        | (scope, slashName) <- T.breakOn "/" rest
        , Just bare <- T.stripPrefix "/" slashName
        , not (T.null bare) ->
            mkPackageName Npm (Just (mkScope scope)) bare
    _ -> mkPackageName Npm Nothing raw

-- The median of a list, total (0 on empty); 'sampleCount' is odd, so this is the
-- middle element of the sorted samples.
median :: [Double] -> Double
median xs = fromMaybe 0 (sort xs !!? (length xs `div` 2))
