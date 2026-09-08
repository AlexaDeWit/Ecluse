-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Live performance acceptance over the curated package catalogue.
The harness compares upstream latency with full and selective metadata processing costs.
-}
module Main (main) where

import Control.Exception qualified as Exception
import Data.Aeson (eitherDecode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime, getCurrentTime)
import GHC.Clock (getMonotonicTime)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Ecluse.Acceptance (OperatingPoint (OperatingPoint), Sample (..), evaluate, loadCriteria, renderReport, reportBreached)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (parseErrorMessage)
import Ecluse.Core.Registry.Npm.Project (parsePackageInfoFromValue, projectName)
import Ecluse.Core.Registry.WireSupport (Projection (NameMismatch, Projected))
import Ecluse.Core.Rules.Types (EvalContext (EvalContext))
import Ecluse.Core.Snapshot (Snapshot (..), digestOf)
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Test.RegistryCapture (catBenchPins, fetchPackumentBody, loadCatalogue, parseRegistryVersions)
import Ecluse.Test.Server.Transform (SelectedDepth (Depth), selectiveDepth, serveTransformSize)

main :: IO ()
main = do
    criteria <- loadCriteria
    catalogue <- loadCatalogue
    manager <- newManager tlsManagerSettings
    now <- getCurrentTime
    let names = Map.keys (catBenchPins catalogue)
    inputs <- traverse (measurePackage manager now) names
    let report = evaluate criteria inputs
        rendered = renderReport (OperatingPoint sampleCount (length names)) report
    putText rendered
    -- In CI, mirror the summary into the GitHub step summary. A breach is then
    -- visible on the pull request, without the workflow shelling around the harness.
    lookupEnv "GITHUB_STEP_SUMMARY" >>= traverse_ (`appendFileText` rendered)
    when (reportBreached report) exitFailure

{- | Measure Écluse's overhead over one package's live packument: the full transform and
the single-version selective decode. A @Left (name, reason)@ marks it unavailable, never a breach.
-}
measurePackage :: Manager -> UTCTime -> Text -> IO (Either (Text, Text) Sample)
measurePackage manager now name = case projectName name of
    Left e -> pure (Left (name, "catalogue pin is not a usable npm name: " <> parseErrorMessage e))
    Right pkg -> do
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

{- | The version a single-version read targets: the last key in the packument's version
list, the realistic install target. 'Nothing' when the packument exposes no versions.
-}
targetVersion :: LByteString -> Maybe Version
targetVersion body = mkVersion Npm . NE.last <$> (nonEmpty =<< parseRegistryVersions Npm body)

{- | Time one pass of the full-packument transform, forcing the result inside the timed
region so the figure covers the real work. 'Nothing' when the body does not decode or project.
-}
measureFull :: UTCTime -> PackageName -> LByteString -> IO (Maybe Double)
measureFull now pkg body = do
    t0 <- getMonotonicTime
    done <- runTransform now pkg body
    t1 <- getMonotonicTime
    pure (if done then Just (t1 - t0) else Nothing)

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
        depth <- Exception.evaluate (selectiveDepth pkg (r, version))
        t1 <- getMonotonicTime
        pure $ case depth of
            Depth _ -> Just (t1 - t0)
            _ -> Nothing

{- | Decode, project, and run the full serve transform, forcing the served size before the
timer stops. 'False' marks a body that did not decode or project.
-}
runTransform :: UTCTime -> PackageName -> LByteString -> IO Bool
runTransform now pkg body =
    case eitherDecode body of
        Left _ -> pure False
        Right value -> case parsePackageInfoFromValue pkg value of
            Right (Projected info) -> do
                size <- serveTransformSize (EvalContext now Nothing) (Snapshot (digestOf (toStrict body)) value, info)
                size `seq` pure True
            Right (NameMismatch _) -> pure False
            Left _ -> pure False

-- The number of passes timed per package. The harness reports their median, to damp
-- noise.
sampleCount :: Int
sampleCount = 5

-- The median of a list, total (0 on empty). 'sampleCount' is odd, so this is the
-- middle element of the sorted samples.
median :: [Double] -> Double
median xs = fromMaybe 0 (sort xs !!? (length xs `div` 2))
