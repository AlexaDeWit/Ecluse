-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Fixed evidence obligations for the temporary allocation diagnostic.
module Ecluse.BenchReport.AllocationDiagnostic.Inventory (
    Recipe (..),
    scheduleFor,
    overlayFilesFor,
    dependencyFilesFor,
    sharedPathsFor,
    passPathsFor,
    runPathsFor,
    passForRecipe,
    scopeFor,
    Pass (..),
    schedule,
    builds,
    reactRow,
    sourceFor,
    treeFor,
    identityFiles,
    buildCommand,
    overlayFiles,
    dependencyFiles,
    inputFiles,
    sharedPaths,
    buildPaths,
    passPaths,
    runPaths,
    passFor,
    bindingFor,
    requireDiagnostic,
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.FilePath ((</>))

import Ecluse.BenchReport.FullHistory.Trust (trustedPaths)
import Ecluse.BenchReport.RefactorEvidence (Binding (Binding))
import Ecluse.Test.Contract.Manifest (digestFile)
import Ecluse.Test.Corpus (corpusPackages, cpPath, pypiCorpusPackages)

-- | Two fixed diagnostic recipes share provenance checks.
data Recipe = Focused | FullHistory deriving stock (Eq)

-- | The full suite uses original controls, shared marker controls and recorded repeats.
scheduleFor :: Recipe -> [Pass]
scheduleFor Focused = schedule
scheduleFor FullHistory = [Pass "C-A1" "C-A" "native", Pass "C-B1" "C-B" "native", Pass "M-B1" "I-B" "marker-only", Pass "M-A1" "I-A" "marker-only", Pass "I-A1" "I-A" "record", Pass "I-B1" "I-B" "record", Pass "I-B2" "I-B" "record", Pass "I-A2" "I-A" "record"]

-- | Neither recipe authorises a full-suite acceptance receipt.
scopeFor :: Recipe -> Text
scopeFor Focused = "focused diagnostic, not acceptance"
scopeFor FullHistory = "full-history diagnostic, not acceptance"

-- | A fixed pass names its build and distinguishes real work from empty controls.
data Pass = Pass Text Text Text

-- | Every required launch, in its prescribed serial order.
schedule :: [Pass]
schedule =
    [ Pass "C-A1" "C-A" "native"
    , Pass "C-B1" "C-B" "native"
    , Pass "C-B2" "C-B" "native"
    , Pass "C-A2" "C-A" "native"
    , Pass "I-A1" "I-A" "record"
    , Pass "I-B1" "I-B" "record"
    , Pass "I-B2" "I-B" "record"
    , Pass "I-A2" "I-A" "record"
    , Pass "O-A-native" "I-A" "empty-native"
    , Pass "O-A-record" "I-A" "empty-record"
    , Pass "O-B-native" "I-B" "empty-native"
    , Pass "O-B-record" "I-B" "empty-record"
    ]

-- | Both source revisions require an original and an instrumented executable.
builds :: [Text]
builds = ["C-A", "C-B", "I-A", "I-B"]

-- | Exact native CSV identity and Tasty selection.
reactRow :: Text
reactRow = "All.ecluse-core (work-per-request).ecosystem: npm.single-version metadata (per package).react (large).selective decode"

-- | Fixed source commit for a build in the schedule.
sourceFor :: Text -> Text
sourceFor build
    | "-A" `T.isSuffixOf` build = "690a4cd522f3146fc04ae42d6abac684a44a422c"
    | otherwise = "ab5f4af6e55b90ae79392f3d10a9505295f1239f"

-- | Fixed source tree, independent of archived declarations.
treeFor :: Text -> Text
treeFor build
    | "-A" `T.isSuffixOf` build = "4fca08ce4b4d44e4903c58771a4f5cccd93b4958"
    | otherwise = "d20be98f858e0f6c030a4b312e2ade924d3105f7"

-- | Files that must match the build before and after each process.
identityFiles :: [FilePath]
identityFiles = ["source.txt", "corpus.sha256", "locks.sha256", "cabal.sha256", "project-local.sha256", "plan.json", "binary.sha256", "toolchain.txt", "dependency.sha256"]

-- | The benchmark build options remain identical to the qualified native run.
buildCommand :: [Text]
buildCommand = ["cabal", "build", "bench:ecluse-bench", "--enable-optimization=1", "--disable-profiling"]

-- | All controller overlay files are required, even when the binding omits them.
overlayFiles :: [FilePath]
overlayFiles = ["Diagnostic.hs", "LICENSE.tasty-bench", "README.md", "tasty-bench.patch", "controller-task.yml", "controller-workflow.yml"]

-- | The original archive and revised Cabal file accompany the complete compiled dependency inputs.
dependencyFiles :: [FilePath]
dependencyFiles = ["tasty-bench-0.4.1.tar.gz", "revision-1.cabal", "tasty-bench-0.4.1/LICENSE", "tasty-bench-0.4.1/tasty-bench.cabal", "tasty-bench-0.4.1/src/Test/Tasty/Bench.hs", "tasty-bench-0.4.1/src/Test/Tasty/Bench/Diagnostic.hs"]

-- | Every registered capture and its pins must appear in the input archive.
inputFiles :: [FilePath]
inputFiles = "corpus/pins.json" : [toString (fromMaybe (toText path) (T.stripPrefix "bench/" (toText path))) | package <- corpusPackages <> pypiCorpusPackages, let path = cpPath package]

-- | Shared proof artifacts must exist independently of any supplied binding map.
sharedPaths :: [FilePath]
sharedPaths =
    ["controller.txt", "worker.txt", "bench-report", "controller-command.txt", "controller-build.status", "controller-build.log", "controls.status", "controls.log", "source-A.tar.gz", "source-B.tar.gz"]
        <> map ("overlay" </>) overlayFiles
        <> map ("dependency" </>) dependencyFiles
        <> map ("inputs" </>) inputFiles

-- | Complete per-build inventory, with the local dependency selector only for instrumented builds.
buildPaths :: Text -> [FilePath]
buildPaths build =
    map (directory </>) (["status", "build.log", "command.txt", "binary-path.txt", "ecluse-bench"] <> map ("identity" </>) identityFiles <> map ("config" </>) configs)
  where
    directory = "build-" <> toString build
    configs = ["ecluse.cabal", "cabal.project", "cabal.project.freeze", "flake.lock"] <> ["cabal.project.local" | "I-" `T.isPrefixOf` build]

-- | Complete native output and before/after identity inventory for one scheduled pass.
passPaths :: Pass -> [FilePath]
passPaths (Pass name _ mode) =
    sharedPaths
        <> map (toString name </>) (["conditions.json", "command.txt", "environment.txt", "native.csv", "native.status", "native.log", "measure.status", "measure.log"] <> ["batches.csv" | mode `elem` ["record", "empty-record"]])
        <> [toString name </> phase </> file | phase <- ["before", "after"], file <- identityFiles]

-- | The final binding covers every obligation plus the complete serial order and child bindings.
runPaths :: [FilePath]
runPaths = ordNub (sharedPaths <> ["order.txt"] <> concatMap buildPaths builds <> concatMap passPaths schedule <> ["build-" <> toString build </> "binding.json" | build <- builds] <> [toString name </> "binding.json" | Pass name _ _ <- schedule])

-- | Refuse an undeclared pass before creating evidence.
passFor :: Text -> IO Pass
passFor name = maybe (fail "unknown diagnostic pass") pure (find (\(Pass key _ _) -> name == key) schedule)

-- | Hash the independently supplied obligation list, never a discovered subset of existing files.
bindingFor :: FilePath -> Map Text Text -> [FilePath] -> IO Binding
bindingFor root declared paths = Binding 1 declared . Map.fromList <$> traverse (\path -> (path,) <$> digestFile (root </> path)) paths

-- | Refuse one named diagnostic proof failure.
requireDiagnostic :: Bool -> Text -> IO ()
requireDiagnostic condition problem = unless condition (fail (toString problem))

-- | Each recipe carries its own fixed dependency patch and literal inventories.
overlayFilesFor :: Recipe -> [FilePath]
overlayFilesFor Focused = overlayFiles
overlayFilesFor FullHistory = overlayFiles <> ["FullHistory.hs", "expected-tree.txt", "expected-csv.txt", "trusted.json", "source-audit.status", "source-audit.log"]

-- | Full-history marking adds one private dependency module.
dependencyFilesFor :: Recipe -> [FilePath]
dependencyFilesFor Focused = dependencyFiles
dependencyFilesFor FullHistory = dependencyFiles <> ["tasty-bench-0.4.1/src/Test/Tasty/Bench/FullHistory.hs"]

-- | Mandatory inventories never come from the observed binding map.
sharedPathsFor :: Recipe -> [FilePath]
sharedPathsFor Focused = sharedPaths
sharedPathsFor FullHistory = ordNub (["controller-source.tar.gz"] <> trustedPaths <> sharedPaths <> map ("overlay" </>) (drop (length overlayFiles) (overlayFilesFor FullHistory)) <> ["dependency/tasty-bench-0.4.1/src/Test/Tasty/Bench/FullHistory.hs"])

-- | Marked launches retain both traversals and the exact provider row binding.
passPathsFor :: Recipe -> Pass -> [FilePath]
passPathsFor Focused scheduled = passPaths scheduled
passPathsFor FullHistory scheduled@(Pass name build _) = ordNub (passPaths scheduled <> sharedPathsFor FullHistory <> [toString name </> file | "I-" `T.isPrefixOf` build, file <- ["tree-before.txt", "tree-after.txt", "mark.txt", "row-binding.txt", "precision.txt"]])

-- | The full run seals exactly its eight launches and four builds.
runPathsFor :: Recipe -> [FilePath]
runPathsFor Focused = runPaths
runPathsFor FullHistory = ordNub (sharedPathsFor FullHistory <> ["order.txt"] <> concatMap buildPaths builds <> concatMap (passPathsFor FullHistory) (scheduleFor FullHistory) <> ["build-" <> toString build </> "binding.json" | build <- builds] <> [toString name </> "binding.json" | Pass name _ _ <- scheduleFor FullHistory])

-- | Refuse launches outside the selected fixed recipe.
passForRecipe :: Recipe -> Text -> IO Pass
passForRecipe recipe name = maybe (fail "unknown diagnostic pass") pure (find (\(Pass key _ _) -> name == key) (scheduleFor recipe))
