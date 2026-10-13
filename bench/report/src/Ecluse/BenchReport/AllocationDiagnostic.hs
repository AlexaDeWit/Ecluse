-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Bind focused React diagnostics without issuing full-suite acceptance receipts.
module Ecluse.BenchReport.AllocationDiagnostic (recordBuild, writeConditions, bindPass, audit, controls) where

import Data.Aeson (Value (Object), object, toJSON, withObject, (.:), (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Word (Word64)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath (isAbsolute, splitDirectories, (</>))
import UnliftIO.Exception (tryAny)

import Ecluse.BenchReport (BenchRow (..), parseCsv, splitName)
import Ecluse.BenchReport.RefactorEvidence (Binding (..))
import Ecluse.BenchReport.RefactorFiles (captureProblems, readComparisonText, writeComparisonText)
import Ecluse.Test.Contract.Manifest (digestFile, readJsonFile, writeJsonFile)

data Pass = Pass Text Text Text

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

reactRow :: Text
reactRow = "All.ecluse-core (work-per-request).ecosystem: npm.single-version metadata (per package).react (large).selective decode"

sourceFor :: Text -> Text
sourceFor build
    | "-A" `T.isSuffixOf` build = "690a4cd522f3146fc04ae42d6abac684a44a422c"
    | otherwise = "ab5f4af6e55b90ae79392f3d10a9505295f1239f"

treeFor :: Text -> Text
treeFor build
    | "-A" `T.isSuffixOf` build = "4fca08ce4b4d44e4903c58771a4f5cccd93b4958"
    | otherwise = "d20be98f858e0f6c030a4b312e2ade924d3105f7"

identityFiles :: [FilePath]
identityFiles = ["source.txt", "corpus.sha256", "locks.sha256", "cabal.sha256", "project-local.sha256", "plan.json", "binary.sha256", "toolchain.txt", "dependency.sha256"]

passFor :: Text -> IO Pass
passFor name = maybe (fail "unknown diagnostic pass") pure (find (\(Pass key _ _) -> name == key) schedule)

bindingFor :: FilePath -> Map Text Text -> [FilePath] -> IO Binding
bindingFor root declared paths = Binding 1 declared . Map.fromList <$> traverse (\path -> (path,) <$> digestFile (root </> path)) paths

check :: Bool -> Text -> IO ()
check condition problem = unless condition (fail (toString problem))

-- | Refuse an unexpected source or dependency before any measurement starts.
recordBuild :: FilePath -> Text -> Text -> IO ()
recordBuild root build source = do
    check (build `elem` ["C-A", "C-B", "I-A", "I-B"]) "unknown build"
    check (source == sourceFor build) "build source differs"
    let directory = "build-" <> toString build
    identity <- readComparisonText (root </> directory </> "identity/source.txt")
    check (T.lines identity == [source, treeFor build]) "build checkout differs"
    binary <- digestFile (root </> directory </> "ecluse-bench")
    live <- T.strip <$> readComparisonText (root </> directory </> "identity/binary.sha256")
    check (binary == live) "archived executable differs from live executable"
    checkPlan root build
    paths <- filesBelow root directory
    bindingFor root (Map.fromList [("build", build), ("source", source), ("binary", binary)]) paths >>= writeJsonFile (root </> directory </> "binding.json")

checkPlan :: FilePath -> Text -> IO ()
checkPlan root build = do
    plan <- readJsonFile (root </> "build-" <> toString build </> "identity/plan.json")
    units <- either fail pure (parseEither (withObject "plan" (.: "install-plan")) plan :: Either String [Value])
    let matches = [unit | Object unit <- units, KeyMap.lookup "pkg-name" unit == Just (toJSON ("tasty-bench" :: Text))]
    case matches of
        [unit] -> do
            check (KeyMap.lookup "pkg-version" unit == Just (toJSON ("0.4.1" :: Text))) "tasty-bench version differs"
            check (KeyMap.lookup "flags" unit == Just (object ["tasty" .= True])) "tasty-bench flags differ"
            if "C-" `T.isPrefixOf` build
                then do
                    check (KeyMap.lookup "pkg-src-sha256" unit == Just (toJSON tarDigest)) "native tasty-bench source differs"
                    check (KeyMap.lookup "pkg-cabal-sha256" unit == Just (toJSON cabalDigest)) "native tasty-bench Cabal revision differs"
                else do
                    check (KeyMap.lookup "style" unit == Just (toJSON ("local" :: Text))) "diagnostic tasty-bench is not local"
                    case KeyMap.lookup "pkg-src" unit of
                        Just (Object origin) -> check (KeyMap.lookup "path" origin == Just (toJSON (root </> "dependency/tasty-bench-0.4.1"))) "overlay source path differs"
                        _ -> fail "overlay source identity is missing"
        _ -> fail "missing or duplicate tasty-bench dependency"

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
    let declared = Map.fromList [("schema", "1"), ("scope", "focused diagnostic, not acceptance"), ("row", reactRow), ("pass", name), ("build", build), ("mode", mode), ("source", sourceFor build), ("run", run), ("attempt", attempt), ("worker", worker), ("buildBinding", buildDigest), ("controller", controller), ("rts", "-T"), ("stdev", "10"), ("optimisation", "-O1"), ("profiling", "off")]
    writeJsonFile (root </> toString name </> "conditions.json") declared

-- | Bind raw files, including partial failed output, without assigning an acceptance verdict.
bindPass :: FilePath -> Text -> IO ()
bindPass root name = do
    _ <- passFor name
    declared <- readJsonFile (root </> toString name </> "conditions.json")
    paths <- filesBelow root (toString name)
    let shared = ["controller.txt", "worker.txt", "bench-report", "controls.status", "controls.log", "source-A.tar.gz", "source-B.tar.gz"]
    bindingFor root declared (shared <> paths) >>= writeJsonFile (root </> toString name </> "binding.json")

filesBelow :: FilePath -> FilePath -> IO [FilePath]
filesBelow root directory = do
    entries <- sort <$> listDirectory (root </> directory)
    concat <$> traverse descend entries
  where
    descend entry = do
        let path = directory </> entry
        file <- doesFileExist (root </> path)
        if file then pure [path] else filesBelow root path

-- | Reject incomplete identities, execution or batch arithmetic. Preserve all raw observations.
audit :: FilePath -> IO ()
audit root = do
    builds <- concat <$> traverse (captureProblems . auditBuild root) ["C-A", "C-B", "I-A", "I-B"]
    passes <- concat <$> traverse (captureProblems . auditPass root) schedule
    other <- captureProblems (auditOrder root >> auditDependency root >> auditPlans root >> auditInputs root >> pure [])
    let faults = builds <> passes <> other
    writeJsonFile (root </> "audit-faults.json") faults
    when (null faults) (writeObservations root)
    writeComparisonText (root </> "scope.txt") "Focused React diagnostics only. Original full startup remains. Preceding benchmark history differs. Empty controls quantify overhead without subtraction. No acceptance receipt or tolerance is implied.\n"
    check (null faults) (T.unlines faults)

auditBinding :: FilePath -> FilePath -> IO Binding
auditBinding root path = do
    binding <- readJsonFile (root </> path)
    check (bindingSchema binding == 1) "binding schema differs"
    forM_ (Map.toList (bindingArtifacts binding)) $ \(artifact, expected) -> do
        check (not (isAbsolute artifact) && ".." `notElem` splitDirectories artifact) "artifact path escapes evidence"
        actual <- digestFile (root </> artifact)
        check (actual == expected) ("artifact digest differs: " <> toText artifact)
    pure binding

auditBuild :: FilePath -> Text -> IO [Text]
auditBuild root build = do
    let directory = "build-" <> toString build
    binding <- auditBinding root (directory </> "binding.json")
    check (Map.lookup "source" (bindingContext binding) == Just (sourceFor build)) "bound source differs"
    status <- T.strip <$> readComparisonText (root </> directory </> "status")
    check (status == "0") "build status failed"
    binary <- digestFile (root </> directory </> "ecluse-bench")
    check (Map.lookup "binary" (bindingContext binding) == Just binary) "build binary identity differs"
    checkPlan root build
    pure []

auditPass :: FilePath -> Pass -> IO [Text]
auditPass root (Pass name build mode) = do
    let directory = toString name
        buildDirectory = "build-" <> toString build
    binding <- auditBinding root (directory </> "binding.json")
    declared <- readJsonFile (root </> directory </> "conditions.json")
    check (bindingContext binding == declared) "pass declaration differs from binding"
    forM_ [("row", reactRow), ("pass", name), ("build", build), ("mode", mode), ("source", sourceFor build)] $ \(key, expected) ->
        check (Map.lookup key declared == Just expected) ("pass context differs: " <> key)
    worker <- digestFile (root </> "worker.txt")
    buildDigest <- digestFile (root </> buildDirectory </> "binding.json")
    controller <- digestFile (root </> "controller.txt")
    check (Map.lookup "controller" declared == Just controller) "controller identity differs"
    check (Map.lookup "worker" declared == Just worker && Map.lookup "buildBinding" declared == Just buildDigest) "pass worker or build differs"
    workerFields <- T.lines <$> readComparisonText (root </> "worker.txt")
    forM_ ["run", "attempt"] $ \key -> do
        let values = mapMaybe (T.stripPrefix (key <> "=")) workerFields
        check (values == maybeToList (Map.lookup key declared) && length values == 1) "run or attempt differs"
    forM_ [("scope", "focused diagnostic, not acceptance"), ("rts", "-T"), ("stdev", "10"), ("optimisation", "-O1"), ("profiling", "off")] $ \(key, value) ->
        check (Map.lookup key declared == Just value) ("measurement condition differs: " <> key)
    command <- T.lines <$> readComparisonText (root </> directory </> "command.txt")
    executable <- T.strip <$> readComparisonText (root </> buildDirectory </> "binary-path.txt")
    let expectedCommand = [executable, "--csv", toText (root </> directory </> "native.csv"), "--stdev", "10", "-p", "$0 == \"" <> reactRow <> "\"", "+RTS", "-T", "-RTS"]
    check (command == expectedCommand) "native command differs"
    environment <- T.lines <$> readComparisonText (root </> directory </> "environment.txt")
    check (environment == ["ECLUSE_BATCH_MODE=" <> mode, "ECLUSE_BATCH_OUTPUT=" <> toText (root </> directory </> "batches.csv"), "GHCRTS=", "timeout=600", "kill-after=30"]) "native environment differs"
    forM_ ["native.status", "measure.status"] $ \file -> do
        status <- T.strip <$> readComparisonText (root </> directory </> file)
        check (status == "0") ("failed native or outer status: " <> name)
    forM_ identityFiles $ \file -> do
        expected <- digestFile (root </> buildDirectory </> "identity" </> file)
        forM_ ["before", "after"] $ \phase -> do
            actual <- digestFile (root </> directory </> phase </> file)
            check (actual == expected) ("pass identity changed: " <> name <> "/" <> toText file)
    csv <- readComparisonText (root </> directory </> "native.csv") >>= either (fail . toString) pure . parseCsv
    row <- case csv of
        [value] | (rowGroup value, rowBench value) == splitName reactRow -> pure value
        _ -> fail "native CSV must contain exactly the React row"
    check (isJust (rowAllocatedBytes row)) "native allocation is missing"
    if mode `elem` ["record", "empty-record"]
        then auditBatches root directory row
        else do
            exists <- doesFileExist (root </> directory </> "batches.csv")
            check (not exists) "unexpected batch records for native accounting"
    pure []

data Batch = Batch Integer Integer Integer Integer Integer Integer Integer Integer Text

readBatch :: Text -> Either Text Batch
readBatch line = case T.splitOn "," line of
    [sequenceNumber, n, startTime, startGlobal, endTime, endGlobal, startThread, endThread, thread] ->
        Batch <$> unsigned sequenceNumber <*> unsigned n <*> unsigned startTime <*> unsigned startGlobal <*> unsigned endTime <*> unsigned endGlobal <*> signed startThread <*> signed endThread <*> pure thread
    _ -> Left "malformed batch record"
  where
    unsigned = bounded 0 (toInteger (maxBound :: Word64))
    signed = bounded (toInteger (minBound :: Int64)) (toInteger (maxBound :: Int64))
    bounded lower upper field = case readMaybe (toString field) of
        Just value | value >= lower && value <= upper -> Right value
        _ -> Left "batch integer exceeds its native bounds"

auditBatches :: FilePath -> FilePath -> BenchRow -> IO ()
auditBatches root directory row = do
    contents <- T.lines <$> readComparisonText (root </> directory </> "batches.csv")
    batches <- case contents of
        header : rest | header == "sequence,n,startTimePs,startGlobalBytes,endTimePs,endGlobalBytes,startThreadBytes,endThreadBytes,thread" -> either (fail . toString) pure (traverse readBatch rest)
        _ -> fail "batch header differs"
    validateBatches (rowAllocatedBytes row) batches

validateBatches :: Maybe Integer -> [Batch] -> IO ()
validateBatches allocated batches = do
    check (length batches >= 2 && length batches <= 256) "invalid batch record count"
    forM_ (zip [0 :: Integer ..] batches) $ \(index, Batch sequenceNumber n startTime startGlobal endTime endGlobal startThread endThread thread) -> do
        check (sequenceNumber == index && n == 2 ^ index) "batch sequence differs or duplicates exist"
        check (endTime >= startTime && endGlobal >= startGlobal && startThread >= endThread) "batch counters move in the wrong direction"
        check (not (T.null thread)) "missing measurement thread"
    check (length (ordNub [thread | Batch _ _ _ _ _ _ _ _ thread <- batches]) == 1) "measurement thread changed"
    case reverse batches of
        Batch _ _ _ endStart _ endFinish _ _ _ : Batch _ n _ previousStart _ previousFinish _ _ _ : _ ->
            check (allocated == Just (((previousFinish - previousStart) `quot` 5 + 2 * ((endFinish - endStart) `quot` 5)) `quot` n)) "native allocation estimator differs from raw batches"
        _ -> fail "missing final batch pair"

auditOrder :: FilePath -> IO ()
auditOrder root = do
    events <- map T.words . T.lines <$> readComparisonText (root </> "order.txt")
    let actual = [wordsAfterTime | _ : wordsAfterTime <- events]
        expected = concatMap (\(Pass name _ _) -> [["start", name], ["end", name]]) schedule
    check (actual == expected && length events == length expected) "serial pass order differs"
    entries <- listDirectory root
    let passNames = filter (\name -> any (`T.isPrefixOf` toText name) ["C-", "I-", "O-"]) entries
    check (sort passNames == sort [toString name | Pass name _ _ <- schedule]) "pass inventory differs"

auditDependency :: FilePath -> IO ()
auditDependency root = do
    status <- T.strip <$> readComparisonText (root </> "controls.status")
    check (status == "0") "negative controls failed"
    tar <- digestFile (root </> "dependency/tasty-bench-0.4.1.tar.gz")
    cabal <- digestFile (root </> "dependency/revision-1.cabal")
    check (tar == tarDigest) "upstream archive hash differs"
    check (cabal == cabalDigest) "dependency Cabal revision differs"
    auditManifest (root </> "build-I-A/identity/dependency.sha256") "./" (root </> "dependency")

auditPlans :: FilePath -> IO ()
auditPlans root = do
    native <- externalPlan "C-A"
    forM_ ["C-B", "I-A", "I-B"] $ \build -> do
        actual <- externalPlan build
        check (actual == native) ("external dependency plan differs: " <> build)
  where
    externalPlan build = do
        plan <- readJsonFile (root </> "build-" <> toString build </> "identity/plan.json")
        units <- either fail pure (parseEither (withObject "plan" (.: "install-plan")) plan :: Either String [Value])
        pure [Object (foldr KeyMap.delete unit ["pkg-src", "dist-dir", "build-info"]) | Object unit <- units, KeyMap.lookup "pkg-name" unit `notElem` map (Just . toJSON) (["ecluse", "tasty-bench"] :: [Text])]

auditInputs :: FilePath -> IO ()
auditInputs root = auditManifest (root </> "build-C-A/identity/corpus.sha256") "bench/" (root </> "inputs")

auditManifest :: FilePath -> Text -> FilePath -> IO ()
auditManifest manifest prefix archive = do
    entries <- map T.words . T.lines <$> readComparisonText manifest
    check (not (null entries)) "empty file digest manifest"
    forM_ entries $ \case
        [expected, path] | Just relative <- T.stripPrefix prefix path -> do
            check (not (isAbsolute (toString relative)) && ".." `notElem` splitDirectories (toString relative)) "manifest path escapes archive"
            actual <- digestFile (archive </> toString relative)
            check (actual == expected) ("archived file differs: " <> path)
        _ -> fail "invalid file digest manifest"

-- | Exercise malformed counters, missing batches and estimator mismatches before remote builds.
controls :: IO ()
controls = do
    let first = Batch 0 1 0 0 10 100 1000 900 "ThreadId 1"
        second = Batch 1 2 10 100 30 300 900 700 "ThreadId 1"
        good = [first, second]
    validateBatches (Just 100) good
    forM_ [[], [first], [first, first], [second, first], replicate 257 first] $ \bad -> reject (validateBatches (Just 100) bad)
    reject (validateBatches (Just 101) good)
    reject (validateBatches Nothing good)
    reject (validateBatches (Just 100) [first, Batch 1 2 10 100 30 300 900 700 "ThreadId 2"])
    reject (validateBatches (Just 100) [first, Batch 1 2 10 100 30 300 900 1000 "ThreadId 1"])
    forM_ ["0,-1,0,0,0,0,0,0,thread", "0,1,0,0,0,0,9223372036854775808,0,thread", "0,1,0,0,0,0,-9223372036854775809,0,thread"] $ \bad ->
        check (isLeft (readBatch bad)) "out-of-range counter accepted"
  where
    reject action = tryAny action >>= \result -> check (isLeft result) "diagnostic negative control accepted"

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
