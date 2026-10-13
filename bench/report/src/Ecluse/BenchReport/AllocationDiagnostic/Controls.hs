-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Each negative control reaches the complete audit from an otherwise accepted fixture.
module Ecluse.BenchReport.AllocationDiagnostic.Controls (controls) where

import Data.Aeson (Object, Value (Array, Object), toJSON)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Directory (copyFile, createDirectoryIfMissing, removeFile, renameDirectory)
import System.FilePath ((</>))
import UnliftIO.Exception (finally, tryAny)
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.BenchReport.AllocationDiagnostic (audit, bindRun)
import Ecluse.BenchReport.AllocationDiagnostic.Batches (batchControls)
import Ecluse.BenchReport.AllocationDiagnostic.Fixture
import Ecluse.BenchReport.AllocationDiagnostic.Inventory
import Ecluse.BenchReport.RefactorEvidence (Binding (..))
import Ecluse.BenchReport.RefactorEvidenceControls (requireAccepted)
import Ecluse.BenchReport.RefactorFiles (readComparisonText, writeComparisonText)
import Ecluse.Test.Contract.Manifest (digestFile, readJsonFile, writeJsonFile)

data Mutation = Mutation Text [Text] (FilePath -> IO ())

-- | Run complete positive, targeted negative and restored positive controls before measurements.
controls :: FilePath -> IO ()
controls evidence = do
    batchControls
    withSystemTempDirectory "allocation-evidence-controls" $ \root -> do
        prepareFixture evidence root
        accept root "initial fixture"
        original <- snapshot root
        forM_ mutations $ \(Mutation name expected change) -> do
            accept root (name <> " before mutation")
            ( do
                    change root
                    result <- tryAny (audit root fixtureRun fixtureAttempt)
                    requireDiagnostic (isLeft result) (name <> " unexpectedly passed the full audit")
                    faults <- readJsonFile (root </> "audit-faults.json")
                    requireDiagnostic (not (null faults)) (name <> " did not produce audit faults")
                    requireDiagnostic (all (\fault -> any (`T.isInfixOf` fault) expected) faults) (name <> " failed at an unrelated boundary: " <> T.unlines faults)
                    putTextLn (name <> ": rejected by the intended full-audit boundary")
                )
                `finally` restore root original
            accept root (name <> " restored fixture")
  where
    accept root name = do
        audit root fixtureRun fixtureAttempt
        readJsonFile (root </> "audit-faults.json") >>= requireAccepted name

mutations :: [Mutation]
mutations =
    [ semantic "source declaration" ["bound source differs"] (alterBinding "build-C-A/binding.json" (\binding -> binding{bindingContext = Map.insert "source" "different" (bindingContext binding)}))
    , semantic "source snapshot" ["build source snapshot differs"] (changeSnapshot "C-A" "source.txt" "wrong source\nwrong tree\n")
    , semantic "archived binary" ["build binary identity differs"] (writeFixtureText "build-C-A/ecluse-bench" "different executable\n")
    , semantic "worker identity" ["pass worker or build differs"] (alterCondition "C-A1" (Map.insert "worker" "different"))
    , semantic "run identity" ["run or attempt differs from trusted invocation"] (alterCondition "C-A1" (Map.insert "run" "different"))
    , semantic "coherent stale run" ["run or attempt differs from trusted invocation"] staleRun
    , semantic "attempt identity" ["run or attempt differs from trusted invocation"] (alterCondition "C-A1" (Map.insert "attempt" "2"))
    , semantic "measurement mode" ["pass context differs: mode"] (alterCondition "C-A1" (Map.insert "mode" "empty-native"))
    , semantic "native command" ["native command differs"] (writeFixtureText "C-A1/command.txt" "different command\n")
    , semantic "build command" ["build command differs"] (writeFixtureText "build-C-A/command.txt" "different build\n")
    , semantic "controller command" ["controller build command differs"] (writeFixtureText "controller-command.txt" "different controller build\n")
    , semantic "dependency source identity" ["native tasty-bench source differs"] changePlan
    , semantic "missing benchmark library" ["missing or duplicate plan component: tasty-bench/lib"] (mutatePlan "I-A" (filter (not . isComponent "tasty-bench" "lib")))
    , semantic "duplicate benchmark library" ["missing or duplicate plan component: tasty-bench/lib"] (mutatePlan "I-A" (\units -> units <> filter (isComponent "tasty-bench" "lib") units))
    , semantic "missing benchmark component" ["missing or duplicate plan component: ecluse/bench:ecluse-bench"] (mutatePlan "I-A" (filter (not . isComponent "ecluse" "bench:ecluse-bench")))
    , semantic "wrong benchmark library edge" ["benchmark does not use exact tasty-bench library"] (mutatePlan "I-A" (map (changeComponent "ecluse" "bench:ecluse-bench" (KeyMap.insert "depends" (toJSON (["tasty-bench-0.4.1-inplace-bench-fibo", "tasty-bench-fit-0.1.1-inplace"] :: [Text]))))))
    , semantic "duplicate benchmark library edge" ["benchmark does not use exact tasty-bench library"] (mutatePlan "I-A" (map (changeComponent "ecluse" "bench:ecluse-bench" (KeyMap.insert "depends" (toJSON (["tasty-bench-0.4.1-inplace", "tasty-bench-0.4.1-inplace", "tasty-bench-fit-0.1.1-inplace"] :: [Text]))))))
    , semantic "fit source identity" ["tasty-bench-fit source differs"] (mutatePlan "I-A" (map (changeComponent "tasty-bench-fit" "lib" (KeyMap.insert "pkg-src-sha256" (toJSON ("different" :: Text))))))
    , semantic "fit unrelated dependency" ["external dependency plan differs: I-A"] (mutatePlan "I-A" (map (changeComponent "tasty-bench-fit" "lib" (KeyMap.insert "depends" (toJSON (["different-base", "tasty-bench-0.4.1-inplace"] :: [Text]))))))
    , semantic "fit wrong overlay edge" ["tasty-bench-fit does not use exact tasty-bench library"] (mutatePlan "I-A" (map (changeComponent "tasty-bench-fit" "lib" (KeyMap.insert "depends" (toJSON (["fixture-base", "wrong-library"] :: [Text]))))))
    , semantic "dependency archive identity" ["upstream archive hash differs"] (writeFixtureText "dependency/tasty-bench-0.4.1.tar.gz" "different archive\n")
    , semantic "input identity" ["archived file differs: bench/corpus/pins.json"] (writeFixtureText "inputs/corpus/pins.json" "different input\n")
    , Mutation "missing pass" ["required artifact missing: C-A1/", "C-A1/binding.json", "pass inventory differs"] (\root -> renameDirectory (root </> "C-A1") (root </> "absent-pass"))
    , Mutation "duplicate pass" ["pass inventory differs"] (\root -> createDirectoryIfMissing True (root </> "C-A3"))
    , semantic "reordered passes" ["serial pass order differs"] (rewrite "order.txt" (T.unlines . reverse . T.lines))
    , semantic "CSV selection" ["native CSV must contain exactly the React row"] (writeFixtureText "C-A1/native.csv" (nativeCsv "different row"))
    , semantic "duplicate CSV row" ["native CSV must contain exactly the React row"] (writeFixtureText "C-A1/native.csv" (nativeCsv reactRow <> T.unlines (drop 1 (T.lines (nativeCsv reactRow)))))
    , semantic "timeout status" ["failed native or outer status: C-A1"] (writeFixtureText "C-A1/native.status" "124\n")
    , semantic "native failure" ["failed native or outer status: C-A1"] (writeFixtureText "C-A1/native.status" "1\n")
    , semantic "outer failure" ["failed native or outer status: C-A1"] (writeFixtureText "C-A1/measure.status" "1\n")
    , Mutation "empty run binding map" ["binding inventory differs: binding.json"] (alterBinding "binding.json" (\binding -> binding{bindingArtifacts = Map.empty}))
    , Mutation "empty build binding map" ["binding inventory differs: build-C-A/binding.json"] emptyBuild
    , Mutation "empty pass binding map" ["binding inventory differs: C-A1/binding.json"] (omitPassEntries (const Map.empty))
    , Mutation "unsafe binding path" ["binding inventory differs: binding.json"] (alterBinding "binding.json" (\binding -> binding{bindingArtifacts = Map.insert "../escape" "digest" (bindingArtifacts binding)}))
    ]
        <> [Mutation ("changed artifact " <> toText path) ["artifact digest differs: " <> toText path] (writeFixtureText path "changed bytes\n") | path <- ["source-A.tar.gz", "bench-report", "overlay/Diagnostic.hs"]]
        <> [Mutation ("missing artifact " <> toText path) [toText path] (\root -> removeFile (root </> path)) | path <- omissionFiles]
        <> [Mutation ("removed run entry " <> toText path) ["binding inventory differs: binding.json"] (alterBinding "binding.json" (\binding -> binding{bindingArtifacts = Map.delete path (bindingArtifacts binding)})) | path <- omissionFiles]
        <> [Mutation ("removed pass entry " <> toText path) ["binding inventory differs: C-A1/binding.json"] (omitPassEntries (Map.delete path)) | path <- ["source-A.tar.gz", "overlay/Diagnostic.hs", "bench-report", "controls.log", "C-A1/command.txt", "C-A1/before/plan.json"]]

omissionFiles :: [FilePath]
omissionFiles = ["source-A.tar.gz", "source-B.tar.gz", "bench-report", "controls.status", "controls.log", "overlay/Diagnostic.hs", "overlay/LICENSE.tasty-bench", "overlay/README.md", "overlay/tasty-bench.patch", "overlay/controller-task.yml", "overlay/controller-workflow.yml", "build-C-A/command.txt", "C-A1/command.txt", "C-A1/before/plan.json", "C-A1/binding.json", "C-A1/native.csv", "C-A1/native.status", "C-A1/conditions.json", "build-C-A/ecluse-bench", "build-C-A/config/ecluse.cabal"]

semantic :: Text -> [Text] -> (FilePath -> IO ()) -> Mutation
semantic name expected change = Mutation name expected (\root -> change root >> refreshBindings root)

writeFixtureText :: FilePath -> Text -> FilePath -> IO ()
writeFixtureText path value root = writeComparisonText (root </> path) value

rewrite :: FilePath -> (Text -> Text) -> FilePath -> IO ()
rewrite path change root = readComparisonText (root </> path) >>= writeComparisonText (root </> path) . change

alterBinding :: FilePath -> (Binding -> Binding) -> FilePath -> IO ()
alterBinding path change root = readJsonFile (root </> path) >>= writeJsonFile (root </> path) . change

alterCondition :: Text -> (Map Text Text -> Map Text Text) -> FilePath -> IO ()
alterCondition name change root = readJsonFile path >>= writeJsonFile path . change
  where
    path = root </> toString name </> "conditions.json"

staleRun :: FilePath -> IO ()
staleRun root = do
    writeFixtureText "worker.txt" "run=other-run\nattempt=1\nrunner=fixture\n" root
    worker <- digestFile (root </> "worker.txt")
    forM_ schedule $ \(Pass name _ _) -> alterCondition name (Map.insert "worker" worker . Map.insert "run" "other-run") root

changeSnapshot :: Text -> FilePath -> Text -> FilePath -> IO ()
changeSnapshot build file value root = do
    writeComparisonText (root </> "build-" <> toString build </> "identity" </> file) value
    mirrorSnapshot root build file

mirrorSnapshot :: FilePath -> Text -> FilePath -> IO ()
mirrorSnapshot root build file =
    forM_ [name | Pass name actual _ <- schedule, actual == build] $ \name ->
        forM_ ["before", "after"] $ \phase -> copyFile (root </> "build-" <> toString build </> "identity" </> file) (root </> toString name </> phase </> file)

changePlan :: FilePath -> IO ()
changePlan = mutatePlan "C-A" (map (changeComponent "tasty-bench" "lib" (KeyMap.insert "pkg-src-sha256" (toJSON ("different" :: Text)))))

mutatePlan :: Text -> ([Value] -> [Value]) -> FilePath -> IO ()
mutatePlan build change root = do
    let path = root </> "build-" <> toString build </> "identity/plan.json"
    plan <- readJsonFile path
    changed <- case plan of
        Object fields -> case KeyMap.lookup "install-plan" fields of
            Just (Array units) -> pure (Object (KeyMap.insert "install-plan" (toJSON (change (toList units))) fields))
            _ -> fail "fixture plan lacks units"
        _ -> fail "fixture plan is not an object"
    writeJsonFile path changed
    mirrorSnapshot root build "plan.json"

isComponent :: Text -> Text -> Value -> Bool
isComponent package component (Object fields) = KeyMap.lookup "pkg-name" fields == Just (toJSON package) && KeyMap.lookup "component-name" fields == Just (toJSON component)
isComponent _ _ _ = False

changeComponent :: Text -> Text -> (Object -> Object) -> Value -> Value
changeComponent package component change unit@(Object fields)
    | isComponent package component unit = Object (change fields)
changeComponent _ _ _ unit = unit

omitPassEntries :: (Map FilePath Text -> Map FilePath Text) -> FilePath -> IO ()
omitPassEntries change root = do
    alterBinding "C-A1/binding.json" (\binding -> binding{bindingArtifacts = change (bindingArtifacts binding)}) root
    bindRun root

emptyBuild :: FilePath -> IO ()
emptyBuild root = do
    alterBinding "build-C-A/binding.json" (\binding -> binding{bindingArtifacts = Map.empty}) root
    checksum <- digestFile (root </> "build-C-A/binding.json")
    forM_ [name | Pass name build _ <- schedule, build == "C-A"] $ \name -> do
        alterCondition name (Map.insert "buildBinding" checksum) root
        declared <- readJsonFile (root </> toString name </> "conditions.json")
        scheduled <- passFor name
        bindingFor root declared (passPaths scheduled) >>= writeJsonFile (root </> toString name </> "binding.json")
    bindRun root
