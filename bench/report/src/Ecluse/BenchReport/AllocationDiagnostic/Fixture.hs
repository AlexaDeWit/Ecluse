-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Small complete evidence fixtures use real pinned dependency bytes and opaque build payloads.
module Ecluse.BenchReport.AllocationDiagnostic.Fixture (prepareFixture, refreshBindings, snapshot, restore, fixtureRun, fixtureAttempt, nativeCsv) where

import Data.Aeson (Value, object, (.=))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, listDirectory, removePathForcibly)
import System.FilePath (takeDirectory, (</>))

import Ecluse.BenchReport.AllocationDiagnostic (bindPass, bindRun, recordBuild, writeConditions)
import Ecluse.BenchReport.AllocationDiagnostic.Inventory
import Ecluse.BenchReport.RefactorEvidence (Binding (..))
import Ecluse.BenchReport.RefactorFiles (writeComparisonText)
import Ecluse.Test.Contract.Manifest (digestFile, readJsonFile, writeJsonFile)
import Ecluse.Test.Env (withEnvOverrides)

-- | The full audit receives this identity independently of fixture declarations.
fixtureRun :: Text
fixtureRun = "allocation-fixture"

-- | The trusted fixture attempt never comes from an archived file.
fixtureAttempt :: Text
fixtureAttempt = "1"

-- | Produce one native row with an allocation estimate matching the two synthetic batches.
nativeCsv :: Text -> Text
nativeCsv label = "Name,Mean (ps),2*Stdev (ps),Allocated,Copied,Peak Memory\n\"" <> label <> "\",10,0,100,0,0\n"

-- | Populate every obligation and call production writers to bind an initially valid fixture.
prepareFixture :: FilePath -> FilePath -> IO ()
prepareFixture evidence root = do
    forM_ runPaths $ \path -> writeFixtureText path ("fixture: " <> toText path <> "\n")
    forM_ dependencyFiles $ \path -> copyFile (evidence </> "dependency" </> path) (root </> "dependency" </> path)
    forM_ overlayFiles $ \path -> copyFile (evidence </> "overlay" </> path) (root </> "overlay" </> path)
    writeFixtureText "worker.txt" ("run=" <> fixtureRun <> "\nattempt=" <> fixtureAttempt <> "\nrunner=fixture\n")
    writeFixtureText "controller-command.txt" "cabal\nbuild\nexe:bench-report\n"
    forM_ ["controls.status", "controller-build.status"] $ \path -> writeFixtureText path "0\n"
    corpus <- checksumLines root "inputs" "bench/" inputFiles
    dependency <- checksumLines root "dependency" "./" dependencyFiles
    forM_ builds $ \build -> do
        let directory = "build-" <> toString build
            identityDirectory = directory </> "identity"
        writeFixtureText (directory </> "status") "0\n"
        writeFixtureText (directory </> "command.txt") (T.unlines buildCommand)
        writeFixtureText (directory </> "binary-path.txt") (toText (root </> directory </> "ecluse-bench") <> "\n")
        writeFixtureText (identityDirectory </> "source.txt") (T.unlines [sourceFor build, treeFor build])
        binary <- digestFile (root </> directory </> "ecluse-bench")
        writeFixtureText (identityDirectory </> "binary.sha256") (binary <> "\n")
        writeFixtureText (identityDirectory </> "corpus.sha256") corpus
        writeFixtureText (identityDirectory </> "dependency.sha256") dependency
        writeJsonFile (root </> identityDirectory </> "plan.json") (fixturePlan root build)
        recordBuild root build (sourceFor build)
    withEnvOverrides [("GITHUB_RUN_ID", toString fixtureRun), ("GITHUB_RUN_ATTEMPT", toString fixtureAttempt)] $
        forM_ schedule $ \(Pass name build mode) -> do
            let directory = toString name
                executable = toText (root </> "build-" <> toString build </> "ecluse-bench")
            forM_ ["before", "after"] $ \phase -> forM_ identityFiles $ \file ->
                copyFile (root </> "build-" <> toString build </> "identity" </> file) (root </> directory </> phase </> file)
            writeConditions root name
            writeFixtureText (directory </> "native.csv") (nativeCsv reactRow)
            writeFixtureText (directory </> "command.txt") (T.unlines [executable, "--csv", toText (root </> directory </> "native.csv"), "--stdev", "10", "-p", "$0 == \"" <> reactRow <> "\"", "+RTS", "-T", "-RTS"])
            writeFixtureText (directory </> "environment.txt") (T.unlines ["ECLUSE_BATCH_MODE=" <> mode, "ECLUSE_BATCH_OUTPUT=" <> toText (root </> directory </> "batches.csv"), "GHCRTS=", "timeout=600", "kill-after=30"])
            forM_ ["native.status", "measure.status"] $ \path -> writeFixtureText (directory </> path) "0\n"
            when (mode `elem` ["record", "empty-record"]) (writeFixtureText (directory </> "batches.csv") batchFixture)
            bindPass root name
    writeFixtureText "order.txt" (T.unlines (concatMap (\(Pass name _ _) -> ["0 start " <> name, "1 end " <> name]) schedule))
    bindRun root
  where
    writeFixtureText path value = createDirectoryIfMissing True (takeDirectory (root </> path)) >> writeComparisonText (root </> path) value

fixturePlan :: FilePath -> Text -> Value
fixturePlan root build = object ["install-plan" .= units]
  where
    instrumented = "I-" `T.isPrefixOf` build
    libraryId :: Text
    libraryId = if instrumented then "tasty-bench-0.4.1-inplace" else "native-tasty-bench"
    fitId :: Text
    fitId = if instrumented then "tasty-bench-fit-0.1.1-inplace" else "native-tasty-bench-fit"
    units :: [Value]
    units =
        [ object ["id" .= ("fixture-benchmark" :: Text), "pkg-name" .= ("ecluse" :: Text), "component-name" .= ("bench:ecluse-bench" :: Text), "depends" .= [libraryId, fitId]]
        , object (common <> selection)
        , fit
        , object ["id" .= ("fixture-base" :: Text), "pkg-name" .= ("base" :: Text), "component-name" .= ("lib" :: Text), "depends" .= ([] :: [Text])]
        ]
            <> [object ["id" .= ("tasty-bench-0.4.1-inplace-bench-fibo" :: Text), "pkg-name" .= ("tasty-bench" :: Text), "pkg-version" .= ("0.4.1" :: Text), "component-name" .= ("bench:bench-fibo" :: Text), "depends" .= [libraryId]] | instrumented]
    common = ["id" .= libraryId, "component-name" .= ("lib" :: Text), "depends" .= (["fixture-base"] :: [Text]), "pkg-name" .= ("tasty-bench" :: Text), "pkg-version" .= ("0.4.1" :: Text), "flags" .= object ["tasty" .= True]]
    selection
        | not instrumented = ["pkg-src-sha256" .= ("315c77e1f0f5fddf3b8465060867bb30c6d6562867b8609afdc7f6e14e295777" :: Text), "pkg-cabal-sha256" .= ("c38a77747f14e8d26ca99869b879ee56099b04e5381a74a62255dcbab85aab84" :: Text)]
        | otherwise = ["style" .= ("local" :: Text), "pkg-src" .= object ["path" .= (root </> "dependency/tasty-bench-0.4.1")]]
    fit :: Value
    fit =
        object
            [ "id" .= fitId
            , "pkg-name" .= ("tasty-bench-fit" :: Text)
            , "pkg-version" .= ("0.1.1" :: Text)
            , "component-name" .= ("lib" :: Text)
            , "flags" .= object ["debug" .= False]
            , "style" .= (if instrumented then "inplace" else "global" :: Text)
            , "pkg-src" .= object ["type" .= ("repo-tar" :: Text), "repo" .= object ["type" .= ("secure-repo" :: Text), "uri" .= ("http://hackage.haskell.org/" :: Text)]]
            , "pkg-src-sha256" .= ("3b26a892b3080be7d0298862a85d3fb116cabf6e3508ad912354cb7247162ef4" :: Text)
            , "pkg-cabal-sha256" .= ("762cb6ee6da109f9c36d3e3a0835ccdac356939c9733a0dc501ddf24102615d2" :: Text)
            , "depends" .= ["fixture-base", libraryId]
            ]

checksumLines :: FilePath -> FilePath -> Text -> [FilePath] -> IO Text
checksumLines root directory prefix paths = T.unlines <$> traverse line paths
  where
    line path = do
        checksum <- digestFile (root </> directory </> path)
        pure (checksum <> "  " <> prefix <> toText path)

batchFixture :: Text
batchFixture = T.unlines ["sequence,n,startTimePs,startGlobalBytes,endTimePs,endGlobalBytes,startThreadBytes,endThreadBytes,thread", "0,1,0,0,10,100,1000,900,ThreadId 1", "1,2,10,100,30,300,900,700,ThreadId 1"]

-- | Refresh digests after a semantic mutation without repairing the mutated declaration.
refreshBindings :: FilePath -> IO ()
refreshBindings root = do
    forM_ builds $ \build -> do
        let path = "build-" <> toString build </> "binding.json"
        existing <- readJsonFile (root </> path)
        bindingFor root (bindingContext existing) (buildPaths build) >>= writeJsonFile (root </> path)
    forM_ schedule $ \(Pass name build _) -> do
        let path = toString name </> "conditions.json"
        declared <- readJsonFile (root </> path)
        checksum <- digestFile (root </> "build-" <> toString build </> "binding.json")
        writeJsonFile (root </> path) (Map.insert "buildBinding" checksum (declared :: Map Text Text))
        bindPass root name
    bindRun root

-- | Keep a byte snapshot so each mutation starts from exactly the same accepted evidence.
snapshot :: FilePath -> IO [(FilePath, ByteString)]
snapshot root = go ""
  where
    go directory = do
        entries <- sort <$> listDirectory (root </> directory)
        concat <$> traverse (readEntry directory) entries
    readEntry directory entry = do
        let path = directory </> entry
        file <- doesFileExist (root </> path)
        if file then (: []) . (path,) <$> readFileBS (root </> path) else go path

-- | Restore every original byte and remove extra artifacts introduced by a control.
restore :: FilePath -> [(FilePath, ByteString)] -> IO ()
restore root files = do
    removePathForcibly root
    forM_ files $ \(path, bytes) -> do
        createDirectoryIfMissing True (takeDirectory (root </> path))
        writeFileBS (root </> path) bytes
