-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Complete synthetic execution evidence tests qualification without measuring performance.
module Ecluse.BenchReport.RefactorResidencyDiagnostic.Fixture (prepareFixture) where

import Data.Aeson (object, (.=))
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Directory (copyFile, createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))

import Ecluse.BenchReport.RefactorEvidence (checksums, identityFiles)
import Ecluse.BenchReport.RefactorFiles (readComparisonText, writeComparisonText)
import Ecluse.BenchReport.RefactorResidencyDiagnostic (bindRun, writeConditions)
import Ecluse.BenchReport.RefactorResidencyDiagnostic.Inputs (Generated (..), expectedLiveHashes)
import Ecluse.BenchReport.RefactorResidencyDiagnostic.Inventory
import Ecluse.Test.Contract.Manifest (digest, digestFile, readJsonFile, writeJsonFile)

-- | The positive fixture supplies every required build, generation and child artifact.
prepareFixture :: FilePath -> IO ()
prepareFixture root = do
    let seed = "taskfiles/refactor-performance/merge-inputs"
        put path value = createDirectoryIfMissing True (takeDirectory (root </> path)) >> writeComparisonText (root </> path) value
        stamp :: Int -> Text
        stamp = T.justifyRight 4 '0' . show
        events index kind name = [stamp (2 * index) <> " start " <> kind <> " " <> name, stamp (2 * index + 1) <> " end " <> kind <> " " <> name <> " status=0"]
        identity :: Text -> Map FilePath Text
        identity side = Map.fromList [("source.txt", T.unlines [sourceFor side, treeFor side]), ("locks.sha256", ""), ("plan.json", "{}\n"), ("plan.sha256", digest "{}\n" <> "  dist-newstyle/cache/plan.json\n"), ("executables.sha256", T.unlines [digest (encodeUtf8 (side <> target)) <> "  /fixture/" <> target | target <- ["bench-report", "ecluse-bench", "perf-acceptance", "ecluse-residency"]]), ("toolchain.txt", "fixture GHC and Cabal\n")]
    put "controller.txt" (T.unlines (replicate 2 (T.replicate 40 "c")))
    put "worker.txt" "run=run-7\nattempt=2\nrunner=fixture\nimage=fixture\naarch64\n"
    writeJsonFile (root </> "run.json") (Map.fromList [("run", "run-7"), ("attempt", "2"), ("runner", "fixture"), ("image", "fixture"), ("architecture", "aarch64"), ("controllerSource", T.replicate 40 "c"), ("controllerTree", T.replicate 40 "c"), ("sourceA", sourceFor "A"), ("treeA", treeFor "A"), ("sourceB", sourceFor "B"), ("treeB", treeFor "B")] :: Map Text Text)
    forM_ ["controller-build.status", "controls.status", "capture-check.status", "generation-check.status"] (`put` "0\n")
    forM_ ["controller-build.log", "controls.log", "capture-check.log", "generation-check.log", "bench-report", "overlay/controller-task.yml", "overlay/controller-workflow.yml", "source-A.tar.gz", "source-B.tar.gz"] (`put` "fixture\n")
    put "controller-command.txt" "cabal\nbuild\nexe:bench-report\n"
    createDirectoryIfMissing True (root </> "overlay")
    copyFile (seed </> "Main.hs") (root </> "overlay/Main.hs")
    corpus <- checksums (seed </> "expected-corpus.sha256")
    overlays <- readJsonFile (seed </> "expected-overlays.json") :: IO (Map Text Text)
    forM_ ["A", "B"] $ \side -> do
        let build = "build-" <> toString side
            helper = "helper-build-" <> toString side
        forM_ (Map.toList (identity side)) $ \(path, value) -> put (build </> path) value
        copyFile (seed </> "expected-corpus.sha256") (root </> build </> "corpus.sha256")
        forM_ (Map.keys corpus) $ \path -> do
            createDirectoryIfMissing True (takeDirectory (root </> "source-inputs-" <> toString side </> path))
            copyFile path (root </> "source-inputs-" <> toString side </> path)
        put ("source-inputs-" <> toString side </> "acceptance/criteria.json") "fixture criteria\n"
        forM_ [build, helper] $ \directory -> do
            forM_ ["ecluse.cabal", "cabal.project", "cabal.project.freeze", "flake.lock"] $ \path -> put (directory </> "config" </> path) "fixture config\n"
            put (directory </> "status") "0\n"
            put (directory </> "output.txt") "fixture build\n"
        put (build </> "locks.sha256") (T.unlines [digest "fixture config\n" <> "  " <> path | path <- ["cabal.project", "cabal.project.freeze", "flake.lock"]])
        put (build </> "command.txt") (T.unlines (nativeBuildArguments root side))
        put (build </> "binary-path.txt") ("/fixture/" <> side <> "/ecluse-residency\n")
        put (build </> "ecluse-residency") (side <> "ecluse-residency")
        forM_ ["source.txt", "toolchain.txt"] $ \path -> copyFile (root </> build </> path) (root </> helper </> path)
        put (helper </> "plan.json") "{\"fixture-helper\":true}\n"
        put (helper </> "command.txt") "cabal\nbuild\ntest:ecluse-residency\n"
        put (helper </> "executable.txt") ("/fixture/helper-" <> side <> "/ecluse-residency\n")
        put (helper </> "ecluse-residency") ("helper-" <> side)
        put (helper </> "executable.sha256") (digest (encodeUtf8 ("helper-" <> side)) <> "  /fixture/helper-" <> side <> "/ecluse-residency\n")
        copyFile (seed </> "Main.hs") (root </> helper </> "Main.hs")
        overlay <- maybe (fail "missing exact helper overlay fixture") pure (Map.lookup side overlays)
        put (helper </> "overlay.patch") overlay
    setupEvents <- forM (zip [0 :: Int ..] [("setup", "native-A"), ("setup", "native-B"), ("setup", "helper-A"), ("setup", "helper-B")]) $ \(index, (kind, name)) -> pure (events index kind name)
    generationEvents <- forM (zip [4 :: Int ..] [(name, probe) | name <- passes, probe <- merges]) $ \(index, (name, probe)) -> do
        let directory = generationDirectory name probe
            helper = "helper-build-" <> toString (sideOf name)
        executable <- T.strip <$> readComparisonText (root </> helper </> "executable.txt")
        put (directory </> "command.txt") (T.unlines (executable : helperArguments root name probe))
        put (directory </> "status") "0\n"
        records <- forM ["private", "public"] $ \role -> do
            let original = root </> directory </> "live" </> toString role <> ".json"
                archive = directory </> toString role <> ".json"
                bytes = encodeUtf8 (probeId probe <> "-" <> role)
            put archive (decodeUtf8 bytes)
            put (directory </> toString role <> ".original") (decodeUtf8 bytes)
            helperDigest <- digestFile (root </> helper </> "ecluse-residency")
            pure (Generated role (probeShape probe) (probePackage probe) (sourceFor name) helperDigest original archive (BS.length bytes) (digest bytes))
        writeJsonFile (root </> directory </> "manifest.json") records
        put (directory </> "returned.txt") (T.unlines (map (toText . generatedOriginal) records))
        forM_ ["helper-before.sha256", "helper-after.sha256"] $ \file -> copyFile (root </> helper </> "executable.sha256") (root </> directory </> file)
        forM_ ["overlay-before.patch", "overlay-after.patch"] $ \file -> copyFile (root </> helper </> "overlay.patch") (root </> directory </> file)
        let recorded = events index "setup" ("generate-" <> name <> "-" <> probeId probe)
        put (directory </> "events.txt") (T.unlines recorded)
        pure recorded
    put "setup-order.txt" (T.unlines (concat (setupEvents <> generationEvents)))
    childEvents <- forM (zip [12 :: Int ..] schedule) $ \(index, (name, probe)) -> do
        let directory = childDirectory name probe
            build = "build-" <> toString (sideOf name)
        createDirectoryIfMissing True (root </> directory)
        writeConditions root name (probeId probe)
        returned <- if probeShape probe == "Shared" then pure [] else T.lines <$> readComparisonText (root </> generationDirectory name probe </> "returned.txt")
        executable <- T.strip <$> readComparisonText (root </> build </> "binary-path.txt")
        put (directory </> "command.txt") (T.unlines (executable : nativeArguments probe returned))
        put (directory </> "environment.txt") "GHCRTS=\nscheduling=serial\ntimeout=600\nkill-after=30\n"
        put (directory </> "native.status") "0\n"
        forM_ ["before", "after"] $ \phase -> forM_ identityFiles $ \file -> do
            createDirectoryIfMissing True (root </> directory </> phase)
            copyFile (root </> build </> file) (root </> directory </> phase </> file)
        hashes <- expectedLiveHashes root name probe
        forM_ ["inputs-before.sha256", "inputs-after.sha256"] $ \file -> put (directory </> file) (T.unlines [hash <> "  " <> toText path | (path, hash) <- Map.toList hashes])
        sourceBytes <-
            if probeShape probe == "Shared"
                then BS.length <$> readFileBS (root </> "source-inputs-" <> toString (sideOf name) </> probePath probe)
                else sum <$> traverse (\role -> BS.length <$> readFileBS (root </> generationDirectory name probe </> role <> ".original")) ["private", "public"]
        let metrics :: [(Text, Integer)]
            metrics = if probeShape probe == "Shared" then [("wireBytes", toInteger sourceBytes), ("versions", 2), ("baselineLive", 500), ("heldLive", 600), ("releasedLive", 500), ("preparationAllocated", 1000), ("preparationMaxLive", 1500)] else [("listingSourceBytes", toInteger sourceBytes), ("listingBasisBytes", 100), ("listingServedBytes", 100), ("listingBaseline", 500), ("listingReadPeak", 600), ("listingEntryLive", 600), ("listingPeak", 1500)]
        writeJsonFile (root </> directory </> "native.json") (object [fromString (toString key) .= value | (key, value) <- metrics])
        let recorded = events index "child" (name <> "-" <> probeId probe)
        put (directory </> "events.txt") (T.unlines recorded)
        pure recorded
    put "order.txt" (T.unlines (concat childEvents))
    bindRun root
