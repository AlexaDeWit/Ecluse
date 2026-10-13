-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | Semantic full-history controls preserve the outer bindings and target each new obligation.
module Ecluse.BenchReport.FullHistory.Controls (controls) where

import Codec.Compression.GZip qualified as GZip
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.FilePath ((</>))
import UnliftIO.Exception (finally, tryAny)
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.BenchReport.AllocationDiagnostic (auditFor)
import Ecluse.BenchReport.AllocationDiagnostic.Controls qualified as Focused
import Ecluse.BenchReport.AllocationDiagnostic.Fixture
import Ecluse.BenchReport.AllocationDiagnostic.Inventory
import Ecluse.BenchReport.RefactorFiles (readComparisonText, writeComparisonText)
import Ecluse.Test.Contract.Manifest (digestFile, readJsonFile, writeJsonFile)

-- | Reuse complete focused controls, then test the extra full-suite proof obligations.
controls :: FilePath -> IO ()
controls evidence = do
    Focused.controls evidence
    withSystemTempDirectory "full-history-controls" $ \root -> do
        prepareFixtureFor FullHistory evidence root
        accept root
        original <- snapshot root
        forM_ mutations $ \(name, boundary, change) -> do
            ( do
                    change root
                    refreshBindingsFor FullHistory root
                    rejected <- tryAny (auditFor FullHistory root fixtureRun fixtureAttempt)
                    requireDiagnostic (isLeft rejected) (name <> " unexpectedly passed")
                    faults <- readJsonFile (root </> "audit-faults.json") :: IO [Text]
                    requireDiagnostic (any (T.isInfixOf boundary) faults) (name <> " missed its intended boundary: " <> T.unlines faults)
                    putTextLn (name <> ": rejected at " <> boundary)
                )
                `finally` restore root original
            accept root
  where
    accept root = auditFor FullHistory root fixtureRun fixtureAttempt

mutations :: [(Text, Text, FilePath -> IO ())]
mutations =
    [ ("absent mark", "full React mark differs", write "I-A1/mark.txt" (reactRow <> "\n0\n"))
    , ("duplicate mark", "full React mark differs", write "I-A1/mark.txt" (reactRow <> "\n2\n"))
    , ("changed tree", "full tree inventory differs", rewrite "I-A1/tree-after.txt" (T.replace "selective decode" "renamed decode"))
    , ("omitted CSV row", "full native CSV inventory differs", rewrite "C-A1/native.csv" (T.unlines . filter (not . T.isInfixOf "selective decode") . T.lines))
    , ("changed CSV name", "full native CSV inventory differs", rewrite "C-A1/native.csv" (T.replace "selective decode" "renamed decode"))
    , ("omitted non-CSV outcome", "full native outcomes differ", rewrite "C-A1/native.log" (T.unlines . filter (not . T.isInfixOf "decodes every generated release") . T.lines))
    , ("failed non-CSV assertion", "full native outcomes differ", rewrite "C-A1/native.log" (T.replace "decodes every generated release: OK" "decodes every generated release: FAIL"))
    , ("wrong row binding", "raw row or mode binding differs", write "I-A1/row-binding.txt" "other-row\nrecord\n")
    , ("wrong marked mode", "raw row or mode binding differs", write "M-A1/row-binding.txt" (reactRow <> "\nrecord\n"))
    , ("malformed counters", "batch integer exceeds its native bounds", rewrite "I-A1/batches.csv" (T.replace "0,1,0,0,10" "0,-1,0,0,10"))
    ,
        ( "altered product source"
        , "trusted source bytes differ"
        , \root -> do
            let path = root </> "source-A.tar.gz"
            bytes <- GZip.decompress <$> LBS.readFile path
            LBS.writeFile path (GZip.compress (bytes <> "changed source"))
        )
    , ("altered literal inventory", "trusted source bytes differ", rewrite "overlay/expected-tree.txt" (T.replace "selective decode" "renamed decode"))
    , ("wrong dependency bytes", "trusted source bytes differ", changeDependency)
    ,
        ( "wrong condition mode"
        , "pass context differs: mode"
        , \root -> do
            let path = root </> "M-A1/conditions.json"
            fields <- readJsonFile path :: IO (Map Text Text)
            writeJsonFile path (Map.insert "mode" "record" fields)
        )
    , ("nonmonotone serial envelope", "serial timestamps are not monotone", rewrite "order.txt" (T.replace "00:00:02" "00:00:00"))
    ]

write :: FilePath -> Text -> FilePath -> IO ()
write path contents root = writeComparisonText (root </> path) contents

rewrite :: FilePath -> (Text -> Text) -> FilePath -> IO ()
rewrite path change root = readComparisonText (root </> path) >>= writeComparisonText (root </> path) . change

changeDependency :: FilePath -> IO ()
changeDependency root = do
    let path = "tasty-bench-0.4.1/src/Test/Tasty/Bench/FullHistory.hs"
    write ("dependency" </> path) "changed source\n" root
    changed <- digestFile (root </> "dependency" </> path)
    let manifest value = T.unlines [if ("  ./" <> toText path) `T.isSuffixOf` line then changed <> "  ./" <> toText path else line | line <- T.lines value]
    forM_ builds $ \build -> rewrite ("build-" <> toString build </> "identity/dependency.sha256") manifest root
    forM_ (scheduleFor FullHistory) $ \(Pass name _ _) ->
        forM_ ["before", "after"] $ \phase -> rewrite (toString name </> phase </> "dependency.sha256") manifest root
