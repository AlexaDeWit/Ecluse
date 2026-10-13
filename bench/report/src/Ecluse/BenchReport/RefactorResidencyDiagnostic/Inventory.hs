-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Fixed obligations for the supplementary three-probe serial campaign.
module Ecluse.BenchReport.RefactorResidencyDiagnostic.Inventory (
    Probe (..),
    probes,
    passes,
    schedule,
    merges,
    sourceFor,
    treeFor,
    sideOf,
    childDirectory,
    generationDirectory,
    helperFiles,
    childFiles,
    generatedFiles,
    inventory,
    nativeBuildArguments,
    nativeArguments,
    helperArguments,
    requireDiagnostic,
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.FilePath (takeDirectory, (</>))

import Ecluse.BenchReport.RefactorEvidence (checksums, identityFiles)

-- | Each probe fixes its package, committed capture, shape and native command.
data Probe = Probe
    { probeId :: Text
    , probePackage :: Text
    , probePath :: FilePath
    , probeShape :: Text
    }
    deriving stock (Eq, Show)

-- | The catalogue is independent of recorded output.
probes :: [Probe]
probes =
    [ Probe "s3-heavy" "@aws-sdk/client-s3" "bench/corpus/npm/aws-sdk-client-s3.full.json" "HeavyBase"
    , Probe "webpack-shared" "webpack" "bench/corpus/npm/webpack.full.json" "Shared"
    , Probe "lodash-publish" "lodash" "bench/corpus/npm/lodash.full.json" "PublishOrder"
    ]

-- | Repetitions run A1, B1, A2 and B2 on one worker.
passes :: [Text]
passes = ["A1", "B1", "A2", "B2"]

-- | Twelve child observations define the order before any execution.
schedule :: [(Text, Probe)]
schedule = [(name, probe) | name <- passes, probe <- probes]

-- | Exactly two merge helpers run for each repetition.
merges :: [Probe]
merges = filter ((/= "Shared") . probeShape) probes

-- | Product sources are fixed independently of the run record.
sourceFor :: Text -> Text
sourceFor side = if sideOf side == "A" then "690a4cd522f3146fc04ae42d6abac684a44a422c" else "ab5f4af6e55b90ae79392f3d10a9505295f1239f"

-- | Product trees authenticate the checkout snapshots.
treeFor :: Text -> Text
treeFor side = if sideOf side == "A" then "4fca08ce4b4d44e4903c58771a4f5cccd93b4958" else "d20be98f858e0f6c030a4b312e2ade924d3105f7"

-- | A repetition carries its product side in its first character.
sideOf :: Text -> Text
sideOf = T.take 1

-- | Child artifacts have a fixed directory for each obligation.
childDirectory :: Text -> Probe -> FilePath
childDirectory name probe = toString ("child-" <> name <> "-" <> probeId probe)

-- | Generation artifacts remain separate from native sources.
generationDirectory :: Text -> Probe -> FilePath
generationDirectory name probe = toString ("generate-" <> name <> "-" <> probeId probe)

-- | Helper builds preserve their overlay and executable bytes.
helperFiles :: [FilePath]
helperFiles = ["source.txt", "plan.json", "toolchain.txt", "overlay.patch", "Main.hs", "executable.txt", "executable.sha256", "ecluse-residency", "status", "output.txt", "command.txt"] <> ["config" </> path | path <- ["ecluse.cabal", "cabal.project", "cabal.project.freeze", "flake.lock"]]

-- | Snapshots and live input hashes surround every child.
childFiles :: [FilePath]
childFiles = ["conditions.json", "command.txt", "environment.txt", "native.status", "native.json", "events.txt", "inputs-before.sha256", "inputs-after.sha256"] <> [phase </> path | phase <- ["before", "after"], path <- identityFiles]

-- | Preserve both original returned byte sequences, even when one names a committed capture.
generatedFiles :: [FilePath]
generatedFiles = ["command.txt", "status", "returned.txt", "manifest.json", "private.original", "public.original", "private.json", "public.json", "helper-before.sha256", "helper-after.sha256", "overlay-before.patch", "overlay-after.patch", "events.txt"]

-- | The mandatory inventory comes from the fixed schedule and native corpus identity.
inventory :: FilePath -> IO [FilePath]
inventory root = do
    corpus <- checksums (root </> "build-A/corpus.sha256")
    pure $
        ["controller.txt", "worker.txt", "run.json", "bench-report", "controller-command.txt", "controller-build.status", "controller-build.log", "controls.status", "controls.log", "capture-check.status", "capture-check.log", "generation-check.status", "generation-check.log", "setup-order.txt", "order.txt", "overlay/Main.hs", "overlay/controller-task.yml", "overlay/controller-workflow.yml"]
            <> ["source-" <> side <> ".tar.gz" | side <- ["A", "B"]]
            <> ["build-" <> side </> path | side <- ["A", "B"], path <- identityFiles <> ["status", "output.txt", "command.txt", "binary-path.txt", "ecluse-residency"] <> ["config" </> file | file <- ["ecluse.cabal", "cabal.project", "cabal.project.freeze", "flake.lock"]]]
            <> ["source-inputs-" <> side </> path | side <- ["A", "B"], path <- "acceptance/criteria.json" : Map.keys corpus]
            <> ["helper-build-" <> side </> path | side <- ["A", "B"], path <- helperFiles]
            <> [generationDirectory name probe </> file | name <- passes, probe <- merges, file <- generatedFiles]
            <> [childDirectory name probe </> file | (name, probe) <- schedule, file <- childFiles]

-- | Record every forwarded variable in the reused native build invocation.
nativeBuildArguments :: FilePath -> Text -> [Text]
nativeBuildArguments root side =
    [ "task"
    , "--exit-code"
    , "performance:comparison:build"
    , "SIDE=" <> side
    , "COMPARISON_ROOT=" <> toText (takeDirectory root)
    , "EVIDENCE=" <> toText root
    , "BASELINE=" <> sourceFor "A"
    , "CANDIDATE=" <> sourceFor "B"
    , "SOURCE=" <> sourceFor side
    , "SOURCE_DIR=" <> toText (takeDirectory root </> "source-" <> toString side)
    ]

-- | Keep both RTS blocks and their exact original positions for merge children.
nativeArguments :: Probe -> [Text] -> [Text]
nativeArguments probe returned
    | probeShape probe == "Shared" = ["--metadata-probe", "Shared", toText (probePath probe), "+RTS", "-T", "-N1", "-RTS"]
    | otherwise = ["--metadata-merge-probe"] <> returned <> ["+RTS", "-F1", "-A128k", "-RTS", toText (probePath probe), "+RTS", "-T", "-N1", "-RTS"]

-- | The helper only generates one fixed native shape per call.
helperArguments :: FilePath -> Text -> Probe -> [Text]
helperArguments root name probe = [probeShape probe, toText (probePath probe), toText (root </> generationDirectory name probe </> "live")]

-- | Evidence faults stop qualification before numeric attribution.
requireDiagnostic :: Bool -> Text -> IO ()
requireDiagnostic valid message = unless valid (fail (toString message))
