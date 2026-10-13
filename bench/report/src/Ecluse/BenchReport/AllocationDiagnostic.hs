-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Bind focused React diagnostics without issuing full-suite acceptance receipts.
module Ecluse.BenchReport.AllocationDiagnostic (recordBuild, writeConditions, bindPass, bindRun, audit) where

import Data.Aeson (Object, Value (Object), object, toJSON, withObject, (.:), (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Directory (doesFileExist, listDirectory)
import System.FilePath (isAbsolute, splitDirectories, (</>))

import Ecluse.BenchReport (BenchRow (..), parseCsv, splitName)
import Ecluse.BenchReport.AllocationDiagnostic.Batches (auditBatches)
import Ecluse.BenchReport.AllocationDiagnostic.Inventory
import Ecluse.BenchReport.RefactorEvidence (Binding (..))
import Ecluse.BenchReport.RefactorFiles (captureProblems, readComparisonText, writeComparisonText)
import Ecluse.Test.Contract.Manifest (digestFile, readJsonFile, writeJsonFile)
import Ecluse.Test.Contract.Result (auditExactIds)

-- | Refuse an unexpected source or dependency before any measurement starts.
recordBuild :: FilePath -> Text -> Text -> IO ()
recordBuild root build source = do
    requireDiagnostic (build `elem` builds) "unknown build"
    requireDiagnostic (source == sourceFor build) "build source differs"
    let directory = "build-" <> toString build
    sourceRecord <- readComparisonText (root </> directory </> "identity/source.txt")
    requireDiagnostic (T.lines sourceRecord == [source, treeFor build]) "build checkout differs"
    binary <- digestFile (root </> directory </> "ecluse-bench")
    live <- T.strip <$> readComparisonText (root </> directory </> "identity/binary.sha256")
    requireDiagnostic (binary == live) "archived executable differs from live executable"
    checkPlan root build
    let paths = buildPaths build
    bindingFor root (Map.fromList [("build", build), ("source", source), ("binary", binary)]) paths >>= writeJsonFile (root </> directory </> "binding.json")

checkPlan :: FilePath -> Text -> IO ()
checkPlan root build = do
    units <- readPlanUnits root build
    (benchmark, unit) <- benchmarkLibrary units
    fit <- fitLibrary units benchmark unit
    validateFit build fit
    requireDiagnostic (KeyMap.lookup "pkg-version" unit == Just (toJSON ("0.4.1" :: Text))) "tasty-bench version differs"
    requireDiagnostic (KeyMap.lookup "flags" unit == Just (object ["tasty" .= True])) "tasty-bench flags differ"
    if "C-" `T.isPrefixOf` build
        then do
            requireDiagnostic (KeyMap.lookup "pkg-src-sha256" unit == Just (toJSON tarDigest)) "native tasty-bench source differs"
            requireDiagnostic (KeyMap.lookup "pkg-cabal-sha256" unit == Just (toJSON cabalDigest)) "native tasty-bench Cabal revision differs"
        else do
            requireDiagnostic (KeyMap.lookup "style" unit == Just (toJSON ("local" :: Text))) "diagnostic tasty-bench is not local"
            case KeyMap.lookup "pkg-src" unit of
                Just (Object origin) -> requireDiagnostic (KeyMap.lookup "path" origin == Just (toJSON (root </> "dependency/tasty-bench-0.4.1"))) "overlay source path differs"
                _ -> fail "overlay source identity is missing"

cabalDigest :: Text
cabalDigest = "c38a77747f14e8d26ca99869b879ee56099b04e5381a74a62255dcbab85aab84"

tarDigest :: Text
tarDigest = "315c77e1f0f5fddf3b8465060867bb30c6d6562867b8609afdc7f6e14e295777"

-- | Declare the exact row, mode and archived build before launching its process.
writeConditions :: FilePath -> Text -> IO ()
writeConditions root name = do
    Pass _ build mode <- passFor name
    run <- lookupEnv "GITHUB_RUN_ID" >>= maybe (fail "missing run identity") (pure . toText)
    attempt <- lookupEnv "GITHUB_RUN_ATTEMPT" >>= maybe (fail "missing attempt identity") (pure . toText)
    worker <- digestFile (root </> "worker.txt")
    buildDigest <- digestFile (root </> "build-" <> toString build </> "binding.json")
    controller <- digestFile (root </> "controller.txt")
    let declared :: Map Text Text
        declared = Map.fromList [("schema", "1"), ("scope", "focused diagnostic, not acceptance"), ("row", reactRow), ("pass", name), ("build", build), ("mode", mode), ("source", sourceFor build), ("run", run), ("attempt", attempt), ("worker", worker), ("buildBinding", buildDigest), ("controller", controller), ("rts", "-T"), ("stdev", "10"), ("optimisation", "-O1"), ("profiling", "off")]
    writeJsonFile (root </> toString name </> "conditions.json") declared

-- | Bind the complete pass inventory without assigning an acceptance verdict.
bindPass :: FilePath -> Text -> IO ()
bindPass root name = do
    scheduled <- passFor name
    declared <- readJsonFile (root </> toString name </> "conditions.json")
    bindingFor root declared (passPaths scheduled) >>= writeJsonFile (root </> toString name </> "binding.json")

-- | Seal the complete run after all serial passes finish.
bindRun :: FilePath -> IO ()
bindRun root = runContext root >>= \declared -> bindingFor root declared runPaths >>= writeJsonFile (root </> "binding.json")

runContext :: FilePath -> IO (Map Text Text)
runContext root = do
    controller <- digestFile (root </> "controller.txt")
    worker <- digestFile (root </> "worker.txt")
    pure (Map.fromList [("scope", "focused diagnostic, not acceptance"), ("controller", controller), ("worker", worker)])

auditRun :: FilePath -> IO ()
auditRun root = do
    binding <- auditBinding root "binding.json" runPaths
    expected <- runContext root
    requireDiagnostic (bindingContext binding == expected) "run binding context differs"
    status <- T.strip <$> readComparisonText (root </> "controller-build.status")
    requireDiagnostic (status == "0") "controller build failed"
    command <- T.lines <$> readComparisonText (root </> "controller-command.txt")
    requireDiagnostic (command == ["cabal", "build", "exe:bench-report"]) "controller build command differs"

-- | Audit complete evidence against independently supplied CI run and attempt identities.
audit :: FilePath -> Text -> Text -> IO ()
audit root expectedRun expectedAttempt = do
    run <- captureProblems (auditRun root >> pure [])
    buildFaults <- concat <$> traverse (captureProblems . auditBuild root) builds
    passes <- concat <$> traverse (captureProblems . auditPass root expectedRun expectedAttempt) schedule
    other <- captureProblems (auditOrder root >> auditDependency root >> auditPlans root >> auditInputs root >> pure [])
    let faults = run <> buildFaults <> passes <> other
    writeJsonFile (root </> "audit-faults.json") faults
    when (null faults) (writeObservations root)
    writeComparisonText (root </> "scope.txt") "Focused React diagnostics only. Original full startup remains. Preceding benchmark history differs. Empty controls quantify overhead without subtraction. No acceptance receipt or tolerance is implied.\n"
    requireDiagnostic (null faults) (T.unlines faults)

auditBinding :: FilePath -> FilePath -> [FilePath] -> IO Binding
auditBinding root path required = do
    binding <- readJsonFile (root </> path)
    requireDiagnostic (bindingSchema binding == 1) "binding schema differs"
    let inventoryFaults = auditExactIds "bound artifact" (map toText required) (map toText (Map.keys (bindingArtifacts binding)))
    requireDiagnostic (null inventoryFaults) ("binding inventory differs: " <> toText path)
    forM_ required $ \artifact -> do
        exists <- doesFileExist (root </> artifact)
        requireDiagnostic exists ("required artifact missing: " <> toText artifact)
    forM_ (Map.toList (bindingArtifacts binding)) $ \(artifact, expected) -> do
        requireDiagnostic (not (isAbsolute artifact) && ".." `notElem` splitDirectories artifact) "artifact path escapes evidence"
        actual <- digestFile (root </> artifact)
        requireDiagnostic (actual == expected) ("artifact digest differs: " <> toText artifact)
    pure binding

auditBuild :: FilePath -> Text -> IO [Text]
auditBuild root build = do
    let directory = "build-" <> toString build
    binding <- auditBinding root (directory </> "binding.json") (buildPaths build)
    requireDiagnostic (Map.lookup "source" (bindingContext binding) == Just (sourceFor build)) "bound source differs"
    status <- T.strip <$> readComparisonText (root </> directory </> "status")
    requireDiagnostic (status == "0") "build status failed"
    binary <- digestFile (root </> directory </> "ecluse-bench")
    requireDiagnostic (Map.lookup "binary" (bindingContext binding) == Just binary) "build binary identity differs"
    sourceLines <- T.lines <$> readComparisonText (root </> directory </> "identity/source.txt")
    requireDiagnostic (sourceLines == [sourceFor build, treeFor build]) "build source snapshot differs"
    live <- T.strip <$> readComparisonText (root </> directory </> "identity/binary.sha256")
    requireDiagnostic (live == binary) "live binary identity differs"
    command <- T.lines <$> readComparisonText (root </> directory </> "command.txt")
    requireDiagnostic (command == buildCommand) "build command differs"
    checkPlan root build
    pure []

auditPass :: FilePath -> Text -> Text -> Pass -> IO [Text]
auditPass root expectedRun expectedAttempt scheduled@(Pass name build mode) = do
    let directory = toString name
        buildDirectory = "build-" <> toString build
    binding <- auditBinding root (directory </> "binding.json") (passPaths scheduled)
    declared <- readJsonFile (root </> directory </> "conditions.json")
    requireDiagnostic (bindingContext binding == declared) "pass declaration differs from binding"
    forM_ [("row", reactRow), ("pass", name), ("build", build), ("mode", mode), ("source", sourceFor build)] $ \(key, expected) ->
        requireDiagnostic (Map.lookup key declared == Just expected) ("pass context differs: " <> key)
    requireDiagnostic (Map.lookup "run" declared == Just expectedRun && Map.lookup "attempt" declared == Just expectedAttempt) "run or attempt differs from trusted invocation"
    worker <- digestFile (root </> "worker.txt")
    buildDigest <- digestFile (root </> buildDirectory </> "binding.json")
    controller <- digestFile (root </> "controller.txt")
    requireDiagnostic (Map.lookup "controller" declared == Just controller) "controller identity differs"
    requireDiagnostic (Map.lookup "worker" declared == Just worker && Map.lookup "buildBinding" declared == Just buildDigest) "pass worker or build differs"
    workerFields <- T.lines <$> readComparisonText (root </> "worker.txt")
    forM_ ["run", "attempt"] $ \key -> do
        let values = mapMaybe (T.stripPrefix (key <> "=")) workerFields
        requireDiagnostic (values == maybeToList (Map.lookup key declared) && length values == 1) "run or attempt differs"
    forM_ [("scope", "focused diagnostic, not acceptance"), ("rts", "-T"), ("stdev", "10"), ("optimisation", "-O1"), ("profiling", "off")] $ \(key, value) ->
        requireDiagnostic (Map.lookup key declared == Just value) ("measurement condition differs: " <> key)
    command <- T.lines <$> readComparisonText (root </> directory </> "command.txt")
    executable <- T.strip <$> readComparisonText (root </> buildDirectory </> "binary-path.txt")
    let expectedCommand = [executable, "--csv", toText (root </> directory </> "native.csv"), "--stdev", "10", "-p", "$0 == \"" <> reactRow <> "\"", "+RTS", "-T", "-RTS"]
    requireDiagnostic (command == expectedCommand) "native command differs"
    environment <- T.lines <$> readComparisonText (root </> directory </> "environment.txt")
    requireDiagnostic (environment == ["ECLUSE_BATCH_MODE=" <> mode, "ECLUSE_BATCH_OUTPUT=" <> toText (root </> directory </> "batches.csv"), "GHCRTS=", "timeout=600", "kill-after=30"]) "native environment differs"
    forM_ ["native.status", "measure.status"] $ \file -> do
        status <- T.strip <$> readComparisonText (root </> directory </> file)
        requireDiagnostic (status == "0") ("failed native or outer status: " <> name)
    forM_ identityFiles $ \file -> do
        expected <- digestFile (root </> buildDirectory </> "identity" </> file)
        forM_ ["before", "after"] $ \phase -> do
            actual <- digestFile (root </> directory </> phase </> file)
            requireDiagnostic (actual == expected) ("pass identity changed: " <> name <> "/" <> toText file)
    csv <- readComparisonText (root </> directory </> "native.csv") >>= either (fail . toString) pure . parseCsv
    row <- case csv of
        [value] | (rowGroup value, rowBench value) == splitName reactRow -> pure value
        _ -> fail "native CSV must contain exactly the React row"
    requireDiagnostic (isJust (rowAllocatedBytes row)) "native allocation is missing"
    if mode `elem` ["record", "empty-record"]
        then auditBatches root directory row
        else do
            exists <- doesFileExist (root </> directory </> "batches.csv")
            requireDiagnostic (not exists) "unexpected batch records for native accounting"
    pure []

auditOrder :: FilePath -> IO ()
auditOrder root = do
    events <- map T.words . T.lines <$> readComparisonText (root </> "order.txt")
    let actual = [wordsAfterTime | _ : wordsAfterTime <- events]
        expected = concatMap (\(Pass name _ _) -> [["start", name], ["end", name]]) schedule
    requireDiagnostic (actual == expected && length events == length expected) "serial pass order differs"
    entries <- listDirectory root
    let passNames = filter (\name -> any (`T.isPrefixOf` toText name) ["C-", "I-", "O-"]) entries
    requireDiagnostic (sort passNames == sort [toString name | Pass name _ _ <- schedule]) "pass inventory differs"

auditDependency :: FilePath -> IO ()
auditDependency root = do
    status <- T.strip <$> readComparisonText (root </> "controls.status")
    requireDiagnostic (status == "0") "negative controls failed"
    tar <- digestFile (root </> "dependency/tasty-bench-0.4.1.tar.gz")
    cabal <- digestFile (root </> "dependency/revision-1.cabal")
    requireDiagnostic (tar == tarDigest) "upstream archive hash differs"
    requireDiagnostic (cabal == cabalDigest) "dependency Cabal revision differs"
    auditManifest (root </> "build-I-A/identity/dependency.sha256") "./" (root </> "dependency")

auditPlans :: FilePath -> IO ()
auditPlans root = do
    native <- externalPlan "C-A"
    forM_ ["C-B", "I-A", "I-B"] $ \build -> do
        actual <- externalPlan build
        requireDiagnostic (actual == native) ("external dependency plan differs: " <> build)
  where
    externalPlan :: Text -> IO [Value]
    externalPlan build = do
        units <- readPlanUnits root build
        (benchmark, library) <- benchmarkLibrary units
        fit <- fitLibrary units benchmark library
        libraryId <- unitId library
        fitId <- unitId fit
        validateFit build fit
        traverse (normaliseExternal libraryId fitId) [unit | unit <- units, KeyMap.lookup "pkg-name" unit `notElem` map (Just . toJSON) (["ecluse", "tasty-bench"] :: [Text])]

readPlanUnits :: FilePath -> Text -> IO [Object]
readPlanUnits root build = do
    plan <- readJsonFile (root </> "build-" <> toString build </> "identity/plan.json")
    either fail pure (parseEither (withObject "plan" (.: "install-plan")) plan :: Either String [Object])

uniqueComponent :: Text -> Text -> [Object] -> IO Object
uniqueComponent package component units = case [unit | unit <- units, KeyMap.lookup "pkg-name" unit == Just (toJSON package), KeyMap.lookup "component-name" unit == Just (toJSON component)] of
    [unit] -> pure unit
    _ -> fail (toString ("missing or duplicate plan component: " <> package <> "/" <> component))

benchmarkLibrary :: [Object] -> IO (Object, Object)
benchmarkLibrary units = do
    benchmark <- uniqueComponent "ecluse" "bench:ecluse-bench" units
    library <- uniqueComponent "tasty-bench" "lib" units
    identifiers <- traverse unitId units
    requireDiagnostic (length identifiers == length (ordNub identifiers)) "duplicate plan unit identifier"
    requireDependency "benchmark does not use exact tasty-bench library" benchmark library
    pure (benchmark, library)

fitLibrary :: [Object] -> Object -> Object -> IO Object
fitLibrary units benchmark library = do
    fit <- uniqueComponent "tasty-bench-fit" "lib" units
    requireDependency "benchmark does not use exact tasty-bench-fit library" benchmark fit
    requireDependency "tasty-bench-fit does not use exact tasty-bench library" fit library
    pure fit

unitId :: Object -> IO Text
unitId unit = either fail pure (parseEither (.: "id") unit)

unitDependencies :: Object -> IO [Text]
unitDependencies unit = either fail pure (parseEither (.: "depends") unit)

requireDependency :: Text -> Object -> Object -> IO ()
requireDependency problem consumer library = do
    dependencies <- unitDependencies consumer
    libraryId <- unitId library
    requireDiagnostic (length (filter (== libraryId) dependencies) == 1) problem

validateFit :: Text -> Object -> IO ()
validateFit build unit = do
    let expectedStyle :: Text
        expectedStyle = if "C-" `T.isPrefixOf` build then "global" else "inplace"
    requireDiagnostic (KeyMap.lookup "style" unit == Just (toJSON expectedStyle)) "tasty-bench-fit build style differs"
    requireDiagnostic (KeyMap.lookup "pkg-version" unit == Just (toJSON ("0.1.1" :: Text))) "tasty-bench-fit version differs"
    requireDiagnostic (KeyMap.lookup "flags" unit == Just (object ["debug" .= False])) "tasty-bench-fit flags differ"
    requireDiagnostic (KeyMap.lookup "pkg-src-sha256" unit == Just (toJSON ("3b26a892b3080be7d0298862a85d3fb116cabf6e3508ad912354cb7247162ef4" :: Text))) "tasty-bench-fit source differs"
    requireDiagnostic (KeyMap.lookup "pkg-cabal-sha256" unit == Just (toJSON ("762cb6ee6da109f9c36d3e3a0835ccdac356939c9733a0dc501ddf24102615d2" :: Text))) "tasty-bench-fit Cabal revision differs"

normaliseExternal :: Text -> Text -> Object -> IO Value
normaliseExternal libraryId fitId unit = do
    identifier <- unitId unit
    let comparable = foldr KeyMap.delete unit ["pkg-src", "dist-dir", "build-info"]
    if identifier /= fitId
        then pure (Object comparable)
        else do
            dependencies <- unitDependencies unit
            source <- maybe (fail "tasty-bench-fit source record is missing") pure (KeyMap.lookup "pkg-src" unit)
            let stableDependencies = map (\dependency -> if dependency == libraryId then "diagnostic:tasty-bench:lib" else dependency) dependencies
                normalised = KeyMap.insert "pkg-src" source (KeyMap.insert "depends" (toJSON stableDependencies) (foldr KeyMap.delete comparable ["id", "style"]))
            pure (Object normalised)

auditInputs :: FilePath -> IO ()
auditInputs root = auditManifest (root </> "build-C-A/identity/corpus.sha256") "bench/" (root </> "inputs")

auditManifest :: FilePath -> Text -> FilePath -> IO ()
auditManifest manifest prefix archive = do
    entries <- map T.words . T.lines <$> readComparisonText manifest
    requireDiagnostic (not (null entries)) "empty file digest manifest"
    forM_ entries $ \case
        [expected, path] | Just relative <- T.stripPrefix prefix path -> do
            requireDiagnostic (not (isAbsolute (toString relative)) && ".." `notElem` splitDirectories (toString relative)) "manifest path escapes archive"
            actual <- digestFile (archive </> toString relative)
            requireDiagnostic (actual == expected) ("archived file differs: " <> path)
        _ -> fail "invalid file digest manifest"

writeObservations :: FilePath -> IO ()
writeObservations root = do
    observations <- forM schedule $ \(Pass name _ mode) -> do
        rows <- readComparisonText (root </> toString name </> "native.csv") >>= either (fail . toString) pure . parseCsv
        case rows of
            [row] -> pure (name, mode, row)
            _ -> fail "unexpected rows while writing observations"
    writeComparisonText (root </> "observations.tsv") (T.unlines ("pass\tmode\tallocatedBytes\tmeanPs" : [T.intercalate "\t" [name, mode, maybe "missing" show (rowAllocatedBytes row), show (rowMeanPs row)] | (name, mode, row) <- observations]))
    let pair side = do
            (_, _, native) <- find (\(name, _, _) -> name == "O-" <> side <> "-native") observations
            (_, _, recorded) <- find (\(name, _, _) -> name == "O-" <> side <> "-record") observations
            before <- rowAllocatedBytes native
            after <- rowAllocatedBytes recorded
            pure (T.intercalate "\t" [side, show before, show after, show (after - before), show (rowMeanPs recorded - rowMeanPs native)])
    overhead <- maybe (fail "missing overhead pair") pure (traverse pair ["A", "B"])
    writeComparisonText (root </> "empty-overhead.tsv") (T.unlines ("source\tnativeBytes\trecordedBytes\tdeltaBytes\tdeltaMeanPs" : overhead))
