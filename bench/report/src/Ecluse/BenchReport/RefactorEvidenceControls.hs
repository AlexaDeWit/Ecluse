-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | File controls exercise the same digest and identity checks used by the aggregate audit.
module Ecluse.BenchReport.RefactorEvidenceControls (evidenceControls, requireAccepted, requireRejected) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath (takeDirectory, (</>))
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.BenchReport (parseCsv)
import Ecluse.BenchReport.RefactorEvidence (Binding (..), bindingFaults, nativeReceiptPaths, orderProblems, passSchedule, receiptArtifactFaults)
import Ecluse.BenchReport.RefactorFiles (readComparisonText)
import Ecluse.Test.Contract.Manifest (digest)
import Ecluse.Test.Contract.Performance (NativeOutcome (NativePassed), NativePerformance (..), PerformanceReceipt (..))
import Ecluse.Test.Contract.Result (ExecutionIdentity (..))

-- | Missing files, changed identities and complete stale native results must fail before numeric comparison.
evidenceControls :: FilePath -> IO ()
evidenceControls manifest = withSystemTempDirectory "refactor-evidence-controls" $ \root -> do
    names <- T.lines <$> readComparisonText manifest
    let csv value = TE.encodeUtf8 ("Name,Mean (ps),2*Stdev (ps),Allocated,Copied,Peak Memory\n" <> T.unlines ["\"All." <> T.replace "\"" "\"\"" name <> "\",1,0," <> value <> ",0,1" | name <- names])
        context = Map.fromList [(key, "expected") | key <- ["source", "tree", "run", "attempt", "rts", "inputsSha256", "executableSha256", "buildPlanSha256", "overlaySha256", "workerSha256", "architecture", "kind", "pass"]]
        nativePaths = nativeReceiptPaths "benchmark" "ecosystem-npm"
        archived path = if path `elem` ["bench-results.csv", "bench-output.txt"] then "benchmark-A1" </> path else "source-inputs-A" </> path
        files =
            Map.fromList $
                [(path, "recorded bytes\n") | path <- ["before/source.txt", "after/source.txt", "before/plan.json", "after/plan.json", "before/executables.sha256", "after/executables.sha256", "listing/overlay.patch", "listing/ListingAdapter.hs", "listing/plan.json", "listing/executable.sha256", "worker.txt", "conditions.json", "events.txt", "pass.status", "residency/measure.log", "versions/probe.json", "listing/probe.json", "allocation/perf-allocation-report.md"]]
                    <> [("benchmark-A1/bench-results.csv", csv "1")]
                    <> [(archived path, "native input\n") | path <- nativePaths, path /= "bench-results.csv"]
        binding = Binding 1 context (fmap digest files)
        check = bindingFaults root context (Map.keys files)
    forM_ (Map.toList files) $ \(path, bytes) -> do
        createDirectoryIfMissing True (takeDirectory (root </> path))
        writeFileBS (root </> path) bytes
    check binding >>= requireAccepted "valid binding"
    forM_ (Map.toList files) $ \(path, bytes) -> do
        writeFileBS (root </> path) (bytes <> "changed")
        check binding >>= requireRejected ("changed artifact " <> toText path)
        removeFile (root </> path)
        check binding >>= requireRejected ("missing artifact " <> toText path)
        writeFileBS (root </> path) bytes
        check binding{bindingArtifacts = Map.delete path (bindingArtifacts binding)} >>= requireRejected "missing artifact declaration"
    forM_ (Map.keys context) $ \key ->
        check binding{bindingContext = Map.insert key "different" context} >>= requireRejected ("changed identity " <> key)
    case parseCsv (TE.decodeUtf8 (csv "2")) of
        Right rows | length rows == 343 -> pure ()
        _ -> fail "stale CSV control is not a complete native result"
    let nativePath = "benchmark-A1/bench-results.csv"
        receipt = PerformanceReceipt 1 (ExecutionIdentity "source" "not-applicable" "arm64" "plan" "run") "1" "benchmark" "ecosystem-npm" (NativePerformance NativePassed [] Map.empty) (Map.fromList [(path, digest (if path == "bench-results.csv" then csv "1" else "native input\n")) | path <- nativePaths])
    receiptArtifactFaults root "benchmark" "A1" receipt >>= requireAccepted "valid native receipt bytes"
    writeFileBS (root </> nativePath) (csv "2")
    check binding >>= requireRejected "complete stale CSV"
    receiptArtifactFaults root "benchmark" "A1" receipt >>= requireRejected "complete stale CSV against receipt"
    let rebound = binding{bindingArtifacts = Map.insert nativePath (digest (csv "2")) (bindingArtifacts binding)}
    check rebound >>= requireAccepted "rebound stale CSV fixture"
    receiptArtifactFaults root "benchmark" "A1" receipt >>= requireRejected "stale native result with a regenerated outer binding"
    let events = [[stamp (2 * index) <> " start " <> kind <> " " <> name, stamp (2 * index + 1) <> " end " <> kind <> " " <> name <> " status=0"] | (index, (kind, name)) <- zip [0 :: Int ..] passSchedule]
        stamp = T.justifyRight 4 '0' . show
    requireAccepted "valid serial order" (orderProblems (concat events) events)
    requireRejected "missing order event" (orderProblems (drop 1 (concat events)) events)
    requireRejected "reversed order" (orderProblems (reverse (concat events)) events)
    requireRejected "missing pass events" (orderProblems (concat (drop 1 events)) (drop 1 events))

-- | A positive control must reach the audit without any fault.
requireAccepted :: Text -> [Text] -> IO ()
requireAccepted label faults = unless (null faults) (fail (toString (label <> " rejected: " <> T.unlines faults)))

-- | A negative control must produce an audit fault, independently of numeric tolerances.
requireRejected :: Text -> [Text] -> IO ()
requireRejected label faults = when (null faults) (fail (toString (label <> " accepted")))
