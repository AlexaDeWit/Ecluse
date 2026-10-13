-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Supplementary isolated native observations retain the original residency field rules.
module Ecluse.BenchReport.RefactorResidencyDiagnostic (
    writeConditions,
    bindRun,
    qualifyInputs,
    qualifyBuilds,
    preparationFaults,
    auditFaults,
    audit,
) where

import Data.Aeson (Value (Object), object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.FilePath ((</>))
import UnliftIO.Exception (tryAny)

import Ecluse.BenchReport.RefactorEvidence (Binding, bindingFaults, checksums, contextForTarget, equalFile, identityFiles, orderProblemsFor, required, writeBinding, zeroStatus)
import Ecluse.BenchReport.RefactorFiles (captureProblems, readComparisonText, writeComparisonText)
import Ecluse.BenchReport.RefactorInputAudit (expectedProbeInputs)
import Ecluse.BenchReport.RefactorInputs (Input (..))
import Ecluse.BenchReport.RefactorMetrics (Observation, comparePasses, comparisonPairs, observationId, residency)
import Ecluse.BenchReport.RefactorResidencyDiagnostic.Builds (auditBuilds)
import Ecluse.BenchReport.RefactorResidencyDiagnostic.Inputs
import Ecluse.BenchReport.RefactorResidencyDiagnostic.Inventory
import Ecluse.Test.Contract.Manifest (digestFile, jsonLine, readJsonFile, writeJsonFile)
import Ecluse.Test.Contract.Result (auditExactIds)

context :: FilePath -> Text -> Probe -> IO (Map Text Text)
context root name probe = do
    let rts = if probeShape probe == "Shared" then "-T -N1" else "-F1 -A128k, then -T -N1"
        paths = if probeShape probe == "Shared" then ["source-inputs-" <> toString (sideOf name) </> probePath probe] else [generationDirectory name probe </> "manifest.json"]
    shared <- contextForTarget root "residency-diagnostic" name ("build-" <> toString (sideOf name)) "ecluse-residency" Nothing paths rts
    pure (Map.union (Map.fromList [("scope", "supplementary isolated children, not acceptance"), ("probe", probeId probe), ("shape", probeShape probe), ("package", probePackage probe), ("corpus", toText (probePath probe))]) shared)

-- | Declare the exact child and input identity before execution.
writeConditions :: FilePath -> Text -> Text -> IO ()
writeConditions root name identifier = do
    probe <- resolveProbe identifier
    requireDiagnostic ((name, probe) `elem` schedule) "unknown scheduled child"
    context root name probe >>= writeJsonFile (root </> childDirectory name probe </> "conditions.json")

runContext :: FilePath -> IO (Map Text Text)
runContext root = readJsonFile (root </> "run.json")

-- | Seal the mandatory inventory after all twelve children finish.
bindRun :: FilePath -> IO ()
bindRun root = do
    declared <- runContext root
    paths <- inventory root
    writeBinding root "binding.json" declared paths

-- | A mismatched generated corpus stops the campaign before any measured child starts.
qualifyInputs :: FilePath -> IO ()
qualifyInputs root = do
    faults <- preparationFaults root
    writeJsonFile (root </> "generated-input-faults.json") faults
    requireDiagnostic (null faults) (T.unlines faults)

-- | Exact builds and configurations must qualify before any helper call starts.
qualifyBuilds :: FilePath -> IO ()
qualifyBuilds root = auditBuilds root >>= \faults -> requireDiagnostic (null faults) (T.unlines faults)

-- | No measured child is needed to qualify completed preparation.
preparationFaults :: FilePath -> IO [Text]
preparationFaults root = do
    builds <- auditBuilds root
    inputs <- captureProblems (matchedInputs root)
    generations <- concat <$> traverse (\(name, probe) -> captureProblems (auditGeneration root name probe)) [(name, probe) | name <- passes, probe <- merges]
    pure (builds <> inputs <> generations)

-- | Complete evidence qualification takes externally supplied CI run and attempt identities.
auditFaults :: FilePath -> Text -> Text -> IO [Text]
auditFaults root runId attempt = do
    run <- captureProblems $ do
        declared <- runContext root
        expectedPaths <- inventory root
        binding <- readJsonFile (root </> "binding.json") :: IO Binding
        faults <- bindingFaults root declared expectedPaths binding
        worker <- T.lines <$> readComparisonText (root </> "worker.txt")
        controller <- T.lines <$> readComparisonText (root </> "controller.txt")
        controllerExpected <- traverse (`required` declared) ["controllerSource", "controllerTree"]
        runner <- required "runner" declared
        image <- required "image" declared
        pure
            ( faults
                <> ["run identity differs: " <> key | (key, value) <- [("run", runId), ("attempt", attempt), ("architecture", "aarch64"), ("sourceA", sourceFor "A"), ("treeA", treeFor "A"), ("sourceB", sourceFor "B"), ("treeB", treeFor "B")], Map.lookup key declared /= Just value]
                <> ["controller identity differs" | controller /= controllerExpected || any (\value -> T.length value /= 40 || not (T.all (`elem` ("0123456789abcdef" :: String)) value)) controller]
                <> ["worker identity differs" | T.null runner || T.null image || not ("aarch64" `T.isInfixOf` T.unlines worker) || any (\field -> length (filter (== field) worker) /= 1) ["run=" <> runId, "attempt=" <> attempt, "runner=" <> runner, "image=" <> image]]
            )
    preparation <- preparationFaults root
    children <- concat <$> traverse (\(name, probe) -> captureProblems (auditChild root name probe)) schedule
    order <- captureProblems (auditOrder root)
    shared <- captureProblems $ do
        equality <- concat <$> traverse (equalFile root "build-A" "build-B") ["locks.sha256", "corpus.sha256", "toolchain.txt"]
        status <- concat <$> traverse (zeroStatus root) ["controller-build.status", "controls.status", "capture-check.status", "build-check.status", "generation-check.status"]
        command <- T.lines <$> readComparisonText (root </> "controller-command.txt")
        pure (equality <> status <> ["controller command differs" | command /= ["cabal", "build", "exe:bench-report"]])
    pure (run <> preparation <> children <> order <> shared)

auditGeneration :: FilePath -> Text -> Probe -> IO [Text]
auditGeneration root name probe = do
    let directory = generationDirectory name probe
        helper = "helper-build-" <> toString (sideOf name)
    executable <- T.strip <$> readComparisonText (root </> helper </> "executable.txt")
    command <- T.lines <$> readComparisonText (root </> directory </> "command.txt")
    hashes <- checksums (root </> helper </> "executable.sha256")
    binaries <-
        concat
            <$> forM
                ["helper-before.sha256", "helper-after.sha256"]
                ( \file -> do
                    actual <- checksums (root </> directory </> file)
                    pure ["helper changed during generation" | actual /= hashes]
                )
    overlays <-
        concat
            <$> traverse
                ( \file -> do
                    expected <- digestFile (root </> helper </> "overlay.patch")
                    actual <- digestFile (root </> directory </> file)
                    pure ["helper overlay changed during generation" | actual /= expected]
                )
                ["overlay-before.patch", "overlay-after.patch"]
    status <- zeroStatus root (directory </> "status")
    pure (status <> binaries <> overlays <> ["helper command differs" | command /= executable : helperArguments root name probe])

auditChild :: FilePath -> Text -> Probe -> IO [Text]
auditChild root name probe = do
    let directory = childDirectory name probe
        build = "build-" <> toString (sideOf name)
    declared <- readJsonFile (root </> directory </> "conditions.json")
    expected <- context root name probe
    executable <- T.strip <$> readComparisonText (root </> build </> "binary-path.txt")
    returned <- inputPaths root name probe
    command <- T.lines <$> readComparisonText (root </> directory </> "command.txt")
    environment <- T.lines <$> readComparisonText (root </> directory </> "environment.txt")
    snapshots <- concat <$> sequence [equalFile root build (directory </> phase) file | phase <- ["before", "after"], file <- identityFiles]
    liveExpected <- expectedLiveHashes root name probe
    liveFaults <-
        concat
            <$> forM
                ["inputs-before.sha256", "inputs-after.sha256"]
                ( \file -> do
                    actual <- checksums (root </> directory </> file)
                    pure ["live native input hashes differ" | actual /= liveExpected]
                )
    status <- zeroStatus root (directory </> "native.status")
    observations <- readObservation root name probe
    pure
        ( snapshots
            <> liveFaults
            <> status
            <> ["child conditions differ" | declared /= expected]
            <> ["native command differs" | command /= executable : nativeArguments probe (if probeShape probe == "Shared" then [] else returned)]
            <> ["native environment differs" | environment /= ["GHCRTS=", "scheduling=serial", "timeout=600", "kill-after=30"]]
            <> auditExactIds "child observation" [observationName probe] (map observationId observations)
        )

observationName :: Probe -> Text
observationName probe = (if probeShape probe == "Shared" then "metadata-residency" else "metadata-merge") <> "/" <> probePackage probe <> "/" <> probeShape probe

readObservation :: FilePath -> Text -> Probe -> IO [Observation]
readObservation root name probe = do
    decoded <- tryAny (readJsonFile (root </> childDirectory name probe </> "native.json") :: IO Value)
    raw <- either (const (fail "native child must contain exactly one observation")) pure decoded
    expectedSource <-
        if probeShape probe == "Shared"
            then do
                captures <- expectedProbeInputs root "versions"
                input <- maybe (fail "missing trusted Shared capture") pure (find ((== probePath probe) . inputPath) captures)
                pure (inputBytes input)
            else do
                records <- readJsonFile (root </> generationDirectory name probe </> "manifest.json")
                pure (sum (map generatedBytes records))
    let sourceField = if probeShape probe == "Shared" then "wireBytes" else "listingSourceBytes"
    case raw of
        Object fields -> requireDiagnostic (KeyMap.lookup sourceField fields == Just (toJSON expectedSource)) "native source byte count differs from authenticated inputs"
        _ -> fail "native child must emit one object"
    let kind = if probeShape probe == "Shared" then "metadata-residency" else "metadata-merge"
        field = if probeShape probe == "Shared" then "measurement" else "peaks"
        wrapped = object ["package" .= probePackage probe, "shape" .= probeShape probe, field .= raw]
    either (fail . toString) pure (residency Map.empty (kind <> " " <> decodeUtf8 (jsonLine wrapped)))

auditOrder :: FilePath -> IO [Text]
auditOrder root = do
    global <- T.lines <$> readComparisonText (root </> "order.txt")
    children <- traverse (\(name, probe) -> T.lines <$> readComparisonText (root </> childDirectory name probe </> "events.txt")) schedule
    preparation <- T.lines <$> readComparisonText (root </> "setup-order.txt")
    generated <- traverse (\(name, probe) -> T.lines <$> readComparisonText (root </> generationDirectory name probe </> "events.txt")) [(name, probe) | name <- passes, probe <- merges]
    let setupSchedule = [("setup", "native-A"), ("setup", "native-B"), ("setup", "helper-A"), ("setup", "helper-B")] <> [("setup", "generate-" <> name <> "-" <> probeId probe) | name <- passes, probe <- merges]
        setupEvents = chunks preparation
        allTimes = mapMaybe (listToMaybe . T.words) (preparation <> global)
    pure
        ( orderProblemsFor [("child", name <> "-" <> probeId probe) | (name, probe) <- schedule] global children
            <> orderProblemsFor setupSchedule preparation setupEvents
            <> ["helper events differ from setup order" | generated /= drop 4 setupEvents]
            <> ["builds or generation overlap measured children" | not (and (zipWith (<=) allTimes (drop 1 allTimes)))]
        )
  where
    chunks [] = []
    chunks events = take 2 events : chunks (drop 2 events)

-- | Publish every signed pair and retain adverse measurements without issuing acceptance.
audit :: FilePath -> Text -> Text -> IO ()
audit root runId attempt = do
    faults <- auditFaults root runId attempt
    writeJsonFile (root </> "audit-faults.json") faults
    comparisons <- captureProblems $ do
        linesByProbe <- forM probes $ \probe -> do
            observations <- traverse (\name -> (name,) <$> readObservation root name probe) ["A1", "A2", "B1", "B2"]
            concat <$> traverse (uncurry (comparePasses "residency-diagnostic")) (comparisonPairs observations)
        writeComparisonText (root </> "comparison.tsv") (T.unlines ("scope\tkind\tbefore-pass\tafter-pass\trow\tmetric\tbefore\tafter\tdelta\trule\tresult" : map fst (concat linesByProbe)))
        pure []
    writeComparisonText (root </> "scope.txt") "Supplementary isolated native children only. Each probe retains all six signed pairs and the original live-field tolerance of 1024 bytes. Preparation allocation is informational. This campaign does not establish whole-suite parity or waive native breaches. R9b listing recovery is separately qualified. Archived binaries and source snapshots do not attest the worker or scheduler. Unmeasured component binaries retain hashes only.\n"
    requireDiagnostic (null (faults <> comparisons)) (T.unlines (faults <> comparisons))
