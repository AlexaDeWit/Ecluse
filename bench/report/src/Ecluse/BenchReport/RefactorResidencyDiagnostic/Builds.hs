-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Qualify exact sources and completed native/helper builds before measuring children.
module Ecluse.BenchReport.RefactorResidencyDiagnostic.Builds (auditBuild, auditBuilds) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.FilePath (takeDirectory, (</>))

import Ecluse.BenchReport.RefactorEvidence (checksums, equalFile, executableDigests, safePath, zeroStatus)
import Ecluse.BenchReport.RefactorFiles (captureProblems, readComparisonText)
import Ecluse.BenchReport.RefactorResidencyDiagnostic.Inventory
import Ecluse.BenchReport.RefactorResidencyDiagnostic.Plans (auditPlan, comparePlans, planBinaryPath)
import Ecluse.Test.Contract.Manifest (digest, digestFile)
import Ecluse.Test.Contract.Result (auditExactIds)

-- | Both builds and their external plans must qualify before helpers generate inputs.
auditBuilds :: FilePath -> IO [Text]
auditBuilds root = do
    builds <- concat <$> traverse (captureProblems . auditBuild root) ["A", "B"]
    plans <- captureProblems (comparePlans root)
    pure (builds <> plans)

-- | Authenticate source archives, configuration, corpus inventories and residency component plans.
auditBuild :: FilePath -> Text -> IO [Text]
auditBuild root side = do
    let build = "build-" <> toString side
        helper = "helper-build-" <> toString side
        source = takeDirectory root </> "source-" <> toString side
        helperSourceDirectory = takeDirectory root </> "helper-" <> toString side
    corpus <- authenticatedCorpus (root </> build </> "corpus.sha256")
    locks <- checksums (root </> build </> "locks.sha256")
    requireDiagnostic (all safePath (Map.keys locks)) "dependency lock path escapes archive"
    requireDiagnostic (null (auditExactIds "dependency lock" ["flake.lock", "cabal.project", "cabal.project.freeze"] (map toText (Map.keys locks)))) "dependency lock inventory differs"
    archive <- digestFile (root </> "source-" <> toString side <> ".tar")
    requireDiagnostic (archive == archiveDigest side) "exact source archive differs"
    archiveCommand <- T.lines <$> readComparisonText (root </> "source-" <> toString side <> ".archive-command.txt")
    requireDiagnostic (archiveCommand == sourceArchiveCommand side) "source archive command differs"
    forM_ (Map.toList (configurationDigests side)) $ \(path, expected) ->
        forM_ [build </> "config", helper </> "config", "source-configs-" <> toString side] $ \directory -> do
            actual <- digestFile (root </> directory </> path)
            requireDiagnostic (actual == expected) "exact source configuration differs"
    nativePlan <- auditPlan (root </> build) source
    helperPlan <- auditPlan (root </> helper) helperSourceDirectory
    nativeSource <- T.lines <$> readComparisonText (root </> build </> "source.txt")
    helperSource <- T.lines <$> readComparisonText (root </> helper </> "source.txt")
    nativeDigests <- executableDigests (root </> build </> "executables.sha256")
    helperDigests <- executableDigests (root </> helper </> "executable.sha256")
    nativeBinary <- digestFile (root </> build </> "ecluse-residency")
    helperBinary <- digestFile (root </> helper </> "ecluse-residency")
    nativePath <- T.strip <$> readComparisonText (root </> build </> "binary-path.txt")
    helperPath <- T.strip <$> readComparisonText (root </> helper </> "executable.txt")
    requireDiagnostic (nativePath == toText (planBinaryPath source) && helperPath == toText (planBinaryPath helperSourceDirectory)) "recorded executable path differs from residency plan"
    helperRecord <- checksums (root </> helper </> "executable.sha256")
    nativeRecord <- checksums (root </> build </> "executables.sha256")
    requireDiagnostic (helperRecord == Map.singleton (planBinaryPath helperSourceDirectory) helperBinary && Map.lookup (planBinaryPath source) nativeRecord == Just nativeBinary) "recorded executable path or digest differs"
    overlay <- readComparisonText (root </> helper </> "overlay.patch")
    mainDigest <- digestFile (root </> helper </> "Main.hs")
    let overlayDigest = digest (TE.encodeUtf8 (T.unlines (filter (not . T.isPrefixOf "index ") (T.lines overlay))))
        expectedOverlay = if side == "A" then "0a388db3d3c5745d86aaf7546713f2af4956206888ba0bfe7966a8ba107de148" else "1d9bc15a64e600aa1f53780427caf29284f3f83fc31f835a5bcf9c9f8bbf4c79"
    sameHelper <- equalFile root "overlay" helper "Main.hs"
    toolchain <- equalFile root build helper "toolchain.txt"
    plan <- digestFile (root </> build </> "plan.json")
    planRecord <- checksums (root </> build </> "plan.sha256")
    corpusFaults <-
        concat
            <$> forM
                (Map.toList corpus)
                ( \(path, expected) -> do
                    actual <- digestFile (root </> "source-inputs-" <> toString side </> path)
                    pure ["archived corpus hash differs" | actual /= expected]
                )
    lockFaults <-
        concat
            <$> forM
                (Map.toList locks)
                ( \(path, expected) -> do
                    actual <- digestFile (root </> build </> "config" </> path)
                    pure ["dependency lock hash differs" | actual /= expected]
                )
    status <- concat <$> traverse (zeroStatus root . (</> "status")) [build, helper]
    commands <- forM [(build, nativeBuildArguments root side), (helper, ["cabal", "build", "test:ecluse-residency"])] $ \(directory, expected) -> do
        actual <- T.lines <$> readComparisonText (root </> directory </> "command.txt")
        pure ["build command differs: " <> toText directory | actual /= expected]
    pure
        ( nativePlan
            <> helperPlan
            <> sameHelper
            <> toolchain
            <> corpusFaults
            <> lockFaults
            <> status
            <> concat commands
            <> ["helper entrypoint differs from controller contract" | mainDigest /= "c9d9d3c16a99f35be31fca40a36d6e527b45163382253f631b1aa65df38b7392"]
            <> ["helper overlay differs from exact product Main" | overlayDigest /= expectedOverlay]
            <> ["native or helper source differs" | nativeSource /= [sourceFor side, treeFor side] || helperSource /= nativeSource]
            <> ["native executable differs" | Map.lookup "ecluse-residency" nativeDigests /= Just nativeBinary]
            <> auditExactIds "native binary" ["bench-report", "ecluse-bench", "perf-acceptance", "ecluse-residency"] (map toText (Map.keys nativeDigests))
            <> ["helper executable differs" | helperDigests /= Map.singleton "ecluse-residency" helperBinary]
            <> ["native build plan hash differs" | planRecord /= Map.singleton "dist-newstyle/cache/plan.json" plan]
            <> ["helper overlay changes another file" | filter (T.isPrefixOf "diff --git ") (T.lines overlay) /= ["diff --git a/test/residency/Main.hs b/test/residency/Main.hs"]]
        )
