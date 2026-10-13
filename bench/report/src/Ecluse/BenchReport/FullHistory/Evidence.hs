-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | Full-suite obligations supplement the shared fixed diagnostic provenance checks.
module Ecluse.BenchReport.FullHistory.Evidence (auditOutcomes, auditTimes, auditTrusted, writeObservations, fixtureOutputs, auditSources, auditInvocation) where

import Codec.Compression.GZip qualified as GZip
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import System.FilePath ((</>))
import System.Process.Typed (proc, readProcessStdout_)

import Ecluse.BenchReport (BenchRow (..), parseCsv, splitName)
import Ecluse.BenchReport.AllocationDiagnostic.Batches (batchFits)
import Ecluse.BenchReport.AllocationDiagnostic.Inventory
import Ecluse.BenchReport.FullHistory.Catalogue (csvPaths, treePaths)
import Ecluse.BenchReport.FullHistory.Trust (trustedDigest)
import Ecluse.BenchReport.RefactorFiles (readComparisonText, writeComparisonText)
import Ecluse.Test.Contract.Manifest (digest, digestFile, readJsonFile)

-- | Require all native outcomes, including assertions which have no CSV row.
auditOutcomes :: FilePath -> Pass -> [BenchRow] -> IO ()
auditOutcomes root (Pass name build mode) rows = do
    requireDiagnostic (map (\row -> (rowGroup row, rowBench row)) rows == map splitName csvPaths) "full native CSV inventory differs"
    requireDiagnostic (all (isJust . rowAllocatedBytes) rows) "full native allocation is missing"
    console <- readComparisonText (directory </> "native.log")
    requireDiagnostic (consoleOutcomes console == map (,"OK") treePaths) "full native outcomes differ or an assertion failed"
    requireDiagnostic (length (filter (T.isPrefixOf "All 361 tests passed (") (T.lines console)) == 1) "full native success summary differs"
    when ("I-" `T.isPrefixOf` build) $ do
        before <- T.lines <$> readComparisonText (directory </> "tree-before.txt")
        after <- T.lines <$> readComparisonText (directory </> "tree-after.txt")
        requireDiagnostic (before == treePaths && after == before) "full tree inventory differs"
        mark <- T.lines <$> readComparisonText (directory </> "mark.txt")
        requireDiagnostic (mark == [reactRow, "1"]) "full React mark differs"
        bound <- T.lines <$> readComparisonText (directory </> "row-binding.txt")
        requireDiagnostic (bound == [reactRow, mode]) "raw row or mode binding differs"
  where
    directory = root </> toString name

consoleOutcomes :: Text -> [(Text, Text)]
consoleOutcomes = snd . foldl' step ([], []) . T.lines
  where
    step (parents, outcomes) line
        | line == "All" = ([(0, "All")], outcomes)
        | null parents || T.null trimmed = (parents, outcomes)
        | indentation == 0 = (parents, outcomes)
        | Just (label, status) <- outcome trimmed =
            let above = filter ((< indentation) . fst) parents
             in (above, outcomes <> [(T.intercalate "." (map snd above <> [label]), status)])
        | " ± " `T.isInfixOf` trimmed = (parents, outcomes)
        | otherwise = (filter ((< indentation) . fst) parents <> [(indentation, trimmed)], outcomes)
      where
        trimmed = T.strip line
        indentation = T.length line - T.length (T.stripStart line)
    outcome line = do
        let (label, ending) = T.breakOnEnd ":" line
        status <- listToMaybe (T.words ending)
        guard (status `elem` ["OK", "FAIL", "SKIP"] && not (T.null label))
        pure (T.dropEnd 1 label, status)

-- | Parse timestamps and require a monotone serial envelope.
auditTimes :: [Text] -> IO ()
auditTimes stamps = do
    times <- traverse parse stamps
    requireDiagnostic (and (zipWith (<=) times (drop 1 times))) "serial timestamps are not monotone"
  where
    parse value = maybe (fail "invalid serial timestamp") pure (parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%z" (toString value) :: Maybe UTCTime)

-- | Authenticate fixed source, configuration, input and complete dependency bytes.
auditTrusted :: FilePath -> IO ()
auditTrusted root =
    auditCatalogue root (const True) >> do
        forM_ [("expected-tree.txt", treePaths), ("expected-csv.txt", csvPaths)] $ \(path, expected) -> do
            actual <- T.lines <$> readComparisonText (root </> "overlay" </> path)
            requireDiagnostic (actual == expected) "literal full inventory differs"
        catalogue <- readJsonFile (root </> "overlay/trusted.json") :: IO (Map FilePath Text)
        let corpus = [expected <> "  bench/" <> relative | (path, expected) <- Map.toList catalogue, Just relative <- [T.stripPrefix "inputs/" (toText path)]]
        forM_ builds $ \build -> do
            actual <- T.lines <$> readComparisonText (root </> "build-" <> toString build </> "identity/corpus.sha256")
            requireDiagnostic (actual == corpus) "full corpus snapshot inventory differs"
        status <- T.strip <$> readComparisonText (root </> "overlay/source-audit.status")
        requireDiagnostic (status == "0") "prelaunch source qualification failed"

-- | Source qualification finishes before any benchmark executable launches.
auditSources :: FilePath -> IO ()
auditSources root = auditCatalogue root (\path -> not ("build-" `T.isPrefixOf` toText path || "inputs/" `T.isPrefixOf` toText path))

-- | The workflow supplies worker and controller identities independently of evidence.
auditInvocation :: FilePath -> Text -> Text -> IO ()
auditInvocation root worker controller = do
    fields <- T.lines <$> readComparisonText (root </> "worker.txt")
    requireDiagnostic (filter (T.isPrefixOf "runner=") fields == ["runner=" <> worker]) "worker differs from trusted invocation"
    source <- T.lines <$> readComparisonText (root </> "controller.txt")
    requireDiagnostic (listToMaybe source == Just controller && length source == 2) "controller differs from trusted invocation"
    trustedTree <- readProcessStdout_ (proc "git" ["rev-parse", toString controller <> "^{tree}"])
    requireDiagnostic (source == [controller, T.strip (TE.decodeUtf8 (LBS.toStrict trustedTree))]) "controller tree differs from trusted git object"
    trustedArchive <- readProcessStdout_ (proc "git" ["archive", toString controller])
    actualArchive <- GZip.decompress <$> LBS.readFile (root </> "controller-source.tar.gz")
    requireDiagnostic (digest (LBS.toStrict actualArchive) == digest (LBS.toStrict trustedArchive)) "controller source archive differs from trusted git object"

auditCatalogue :: FilePath -> (FilePath -> Bool) -> IO ()
auditCatalogue root selected = do
    trusted <- digestFile (root </> "overlay/trusted.json")
    requireDiagnostic (trusted == trustedDigest) "trusted source catalogue differs"
    catalogue <- readJsonFile (root </> "overlay/trusted.json") :: IO (Map FilePath Text)
    forM_ (filter (selected . fst) (Map.toList catalogue)) $ \(path, expected) -> do
        actual <-
            if path `elem` ["source-A.tar.gz", "source-B.tar.gz"]
                then digest . LBS.toStrict . GZip.decompress <$> LBS.readFile (root </> path)
                else digestFile (root </> path)
        requireDiagnostic (actual == expected) ("trusted source bytes differ: " <> toText path)

-- | Keep every signed native pair after qualification, without allowances or subtraction.
writeObservations :: FilePath -> IO ()
writeObservations root = do
    observations <- forM (scheduleFor FullHistory) $ \(Pass name _ mode) -> do
        rows <- readComparisonText (root </> toString name </> "native.csv") >>= either (fail . toString) pure . parseCsv
        case find (\row -> (rowGroup row, rowBench row) == splitName reactRow) rows of
            Just row -> pure (name, mode, row)
            Nothing -> fail "React observation missing"
    writeComparisonText (root </> "observations.tsv") (T.unlines ("pass\tmode\tallocatedBytes\tmeanPs" : [T.intercalate "\t" [name, mode, maybe "missing" show (rowAllocatedBytes row), show (rowMeanPs row)] | (name, mode, row) <- observations]))
    let difference getter before after = maybe "missing" show ((-) <$> getter after <*> getter before)
        pairs = [T.intercalate "\t" [before, after, modeA, modeB, show (b - a), show (rowMeanPs rowB - rowMeanPs rowA), show (rowStdev2Ps rowB - rowStdev2Ps rowA), difference rowCopiedBytes rowA rowB, difference rowPeakBytes rowA rowB] | (before, modeA, rowA) <- observations, (after, modeB, rowB) <- observations, before /= after, Just a <- [rowAllocatedBytes rowA], Just b <- [rowAllocatedBytes rowB]]
    writeComparisonText (root </> "all-signed-native-pairs.tsv") (T.unlines ("before\tafter\tbeforeMode\tafterMode\tdeltaBytes\tdeltaMeanPs\tdeltaStdev2Ps\tdeltaCopiedBytes\tdeltaPeakBytes" : pairs))
    fits <- forM [name | Pass name _ mode <- scheduleFor FullHistory, mode == "record"] $ \name -> do
        precisionText <- T.strip <$> readComparisonText (root </> toString name </> "precision.txt")
        precision <- maybe (fail "invalid native timing precision") pure (readMaybe (toString precisionText))
        (global, thread, meanTime, deviation) <- batchFits root (toString name) precision
        pure (T.intercalate "\t" [name, show global, show thread, show (global - thread), show meanTime, show deviation])
    writeComparisonText (root </> "batch-fits.tsv") (T.unlines ("pass\tglobalBytes\tthreadBytes\tglobalMinusThreadBytes\tmeanPs\tstdev2Ps" : fits))

-- | Small fixtures retain the complete literal outcome inventory.
fixtureOutputs :: FilePath -> Pass -> IO ()
fixtureOutputs root (Pass name build mode) = do
    write "native.csv" ("Name,Mean (ps),2*Stdev (ps),Allocated,Copied,Peak Memory\n" <> T.concat ["\"" <> path <> "\",10000000000,2683281572,100,0,0\n" | path <- csvPaths])
    write "native.log" (T.unlines (concatMap console treePaths <> ["All 361 tests passed (1.00s)"]))
    when ("I-" `T.isPrefixOf` build) $ do
        write "tree-before.txt" (T.unlines treePaths)
        write "tree-after.txt" (T.unlines treePaths)
        write "precision.txt" "1000000000\n"
        write "mark.txt" (T.unlines [reactRow, "1"])
        write "row-binding.txt" (T.unlines [reactRow, mode])
  where
    write file = writeComparisonText (root </> toString name </> file)
    console path = case reverse (zip [0 :: Int ..] (T.splitOn "." path)) of
        (depth, leaf) : groups -> [T.replicate (index * 2) " " <> parentName | (index, parentName) <- reverse groups] <> [T.replicate (depth * 2) " " <> leaf <> ": OK"]
        [] -> []
