-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Identities and completeness checks for temporary experiment artifacts.
module Ecluse.OsvIndex.Evidence (writeManifest, auditExperiment, cases, trialOrder) where

import Data.Aeson (FromJSON (parseJSON), Value (Null), object, withObject, (.:), (.=))
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Database.SQLite.Simple (Only (fromOnly), query_, withConnection)
import GHC.Conc (getNumCapabilities, getNumProcessors)
import System.Directory (doesFileExist, getFileSize, listDirectory)
import System.FilePath ((</>))
import System.IO (withBinaryFile)

import Ecluse.OsvIndex.Artifact (Snapshot (..))
import Ecluse.Test.Contract.Manifest (digestFile, failRunner, jsonLine, readJsonFile, writeJsonFile)

cases :: [FilePath]
cases = ["npm-corpus", "pypi-corpus", "npm-full", "pypi-full"]

trialOrder :: [FilePath]
trialOrder = ["warm-A", "warm-B", "1-A", "1-B", "2-B", "2-A", "3-A", "3-B", "4-B", "4-A", "5-A", "5-B", "6-B", "6-A"]

-- | Bind the runner, source variants, binaries and acquired bytes before timing starts.
writeManifest :: FilePath -> IO ()
writeManifest root = do
    sourceFiles <- T.lines <$> readFileText (root </> "sources/tracked.txt")
    trees <- forM ["A", "B"] $ \variant -> do
        hashes <- forM sourceFiles $ \relative -> do
            hash <- digestFile (root </> "sources" </> variant </> toString relative)
            pure (relative, hash)
        writeJsonFile (root </> (variant <> "-source-files.json")) (Map.fromList hashes)
        treeHash <- digestFile (root </> (variant <> "-source-files.json"))
        binaryHash <- digestFile (root </> "bin" </> variant)
        pure (object ["variant" .= variant, "fileManifestSha256" .= treeHash, "binarySha256" .= binaryHash])
    inputNames <- sort <$> listDirectory (root </> "inputs")
    inputs <- forM inputNames $ \name -> do
        let path = root </> "inputs" </> name
        hash <- digestFile path
        bytes <- getFileSize path
        pure (object ["path" .= ("inputs" </> name), "bytes" .= bytes, "sha256" .= hash])
    environment <- forM ["GITHUB_SHA", "GITHUB_RUN_ID", "GITHUB_RUN_ATTEMPT", "RUNNER_ARCH"] $ \key -> do
        value <- lookupEnv key >>= maybe (failRunner ("missing " <> toText key)) (pure . toText)
        pure (toText key, value)
    inheritedRts <- lookupEnv "GHCRTS"
    capabilities <- getNumCapabilities
    processors <- getNumProcessors
    sqlite <- withConnection ":memory:" $ \conn -> map fromOnly <$> (query_ conn "SELECT sqlite_version()" :: IO [Only Text])
    flake <- digestFile "flake.lock"
    patch <- digestFile (root </> "sources/source-diff.patch")
    writeJsonFile (root </> "manifest.json") $
        object
            [ "productionBase" .= ("690a4cd522f3146fc04ae42d6abac684a44a422c" :: Text)
            , "execution" .= Map.fromList environment
            , "sourceVariants" .= trees
            , "inputs" .= inputs
            , "flakeLockSha256" .= flake
            , "sourceDiffSha256" .= patch
            , "inheritedGhcrts" .= inheritedRts
            , "capabilities" .= capabilities
            , "processors" .= processors
            , "sqlite" .= sqlite
            , "limits" .= (["RSS includes retained local HTTP fixture bytes", "fresh connections do not imply an empty OS page cache", "Python uses the existing pinned shell dependency only for per-process getrusage", "no schema epoch change or production index removal is authorised"] :: [Text])
            ]
    writeJsonFile (root </> "trial-order.json") (object ["cases" .= cases, "order" .= trialOrder, "queryBatchesPerMode" .= (30 :: Int), "queryModes" .= (["reused", "fresh"] :: [Text])])

data Resource = Resource
    { rsExit :: Int
    , rsWall :: Double
    , rsUser :: Double
    , rsSystem :: Double
    , rsRss :: Double
    }

instance FromJSON Resource where
    parseJSON = withObject "sample resources" $ \o -> Resource <$> o .: "exitCode" <*> o .: "wallSeconds" <*> o .: "userSeconds" <*> o .: "systemSeconds" <*> o .: "peakRssKiB"

-- | Missing or failed observations prevent a complete result and a removal recommendation.
auditExperiment :: FilePath -> IO ()
auditExperiment root = do
    writeCompilationRows root
    failures <- traverse (auditRequired root) required
    rows <- fmap concat $ forM cases $ \input -> forM ["A", "B"] $ \variant -> do
        measurements <- fmap catMaybes $ forM [1 :: Int .. 6] $ \pair -> do
            let directory = root </> "samples" </> input </> (show pair <> "-" <> variant)
            exists <- doesFileExist (directory </> "resource.json")
            if not exists
                then pure Nothing
                else do
                    sample <- readJsonFile (directory </> "resource.json")
                    snapshotExists <- doesFileExist (directory </> "snapshot.json")
                    snapshot <- if snapshotExists then Just <$> readJsonFile (directory </> "snapshot.json") else pure Nothing
                    pure (Just (sample, snapshot))
        pure (summaryRow input variant measurements)
    queryRows <- querySummaries root
    let faults = concat failures
    writeJsonFile (root </> "audit.json") (object ["complete" .= null faults, "faults" .= faults, "compilation" .= rows, "queries" .= queryRows, "recommendation" .= ("No automatic removal recommendation. Review query distributions, adverse rows and same-epoch migration policy." :: Text)])
    writeFileText (root </> "summary.md") (T.unlines (["# Range-index experiment", "", if null faults then "Required observations and status files are present." else "The experiment is incomplete. No removal recommendation is supported.", "", "Raw failed and partial samples remain in the artifact. Compilation summaries use successful samples only and state their count.", "RSS includes the local HTTP fixtures. Fresh-process wall time includes reader acceptance and startup. Internal query time reuses a connection.", "The OS page cache was not flushed. No cold-disk claim is supported.", "A compatible storage reduction keeps the schema epoch. Issue #1234 requires an explicit migration override.", "", "Failures:"] <> map ("- " <>) faults))
    unless (null faults) (failRunner "experiment audit failed")
  where
    required =
        ["manifest.json", "trial-order.json", "conditions.txt"]
            <> ["samples" </> input </> sample </> file | input <- cases, sample <- trialOrder, file <- ["resource.json", "compile.status", "snapshot.json", "inspect.status", "comparison.json", "compare.status"]]
            <> ["readers" </> input </> file | input <- cases, file <- ["reader.jsonl", "reader.status", "edges.jsonl", "edges.status", "selection-A.json", "selection-B.json", "select-A.status", "select-B.status"]]
            <> ["queries" </> input </> mode </> (show batch <> "-" <> variant) </> file | input <- cases, mode <- ["fresh", "reused"], batch <- [1 :: Int .. 30], variant <- ["A", "B"], file <- ["resource.json", "result.json", "query.status"]]

summaryRow :: FilePath -> FilePath -> [(Resource, Maybe Snapshot)] -> Value
summaryRow input variant samples =
    object
        [ "case" .= input
        , "variant" .= variant
        , "observed" .= length samples
        , "successful" .= length successful
        , "wallSeconds" .= spread (map (rsWall . fst) successful)
        , "userSeconds" .= spread (map (rsUser . fst) successful)
        , "systemSeconds" .= spread (map (rsSystem . fst) successful)
        , "peakRssKiB" .= spread (map (rsRss . fst) successful)
        , "databaseBytes" .= spread [fromInteger (snBytes snapshot) | (_, Just snapshot) <- successful]
        ]
  where
    successful = filter ((== 0) . rsExit . fst) samples

spread :: [Double] -> Value
spread values = case sort values of
    [] -> object ["count" .= (0 :: Int)]
    firstValue : rest ->
        let count = fromIntegral (length values)
            mean = sum values / count
            variance = if count > 1 then sum [(x - mean) ^ (2 :: Int) | x <- values] / (count - 1) else 0
         in object ["count" .= length values, "minimum" .= firstValue, "maximum" .= foldl' max firstValue rest, "mean" .= mean, "sampleStdev" .= sqrt variance]

data Operation = Operation
    { opName :: Text
    , opElapsed :: Double
    , opIterations :: Int
    }

instance FromJSON Operation where
    parseJSON = withObject "operation" $ \o -> Operation <$> o .: "operation" <*> o .: "elapsedNs" <*> o .: "iterations"

newtype QueryReport = QueryReport [Operation]

instance FromJSON QueryReport where
    parseJSON = withObject "query report" $ \o -> QueryReport <$> o .: "operations"

querySummaries :: FilePath -> IO [Value]
querySummaries root = forM [(input, mode, variant) | input <- cases, mode <- ["fresh", "reused"], variant <- ["A", "B"]] $ \(input, mode, variant) -> do
    observed <- fmap catMaybes $ forM [1 :: Int .. 30] $ \batch -> do
        let directory = root </> "queries" </> input </> mode </> (show batch <> "-" <> variant)
        exists <- (&&) <$> doesFileExist (directory </> "resource.json") <*> doesFileExist (directory </> "result.json")
        if not exists
            then pure Nothing
            else do
                resource <- readJsonFile (directory </> "resource.json")
                QueryReport operations <- readJsonFile (directory </> "result.json")
                unless (all (\op -> opIterations op > 0 && opElapsed op >= 20000000) operations) (failRunner "query batch fell below its timing floor")
                pure (Just (resource, operations))
    let successful = filter ((== 0) . rsExit . fst) observed
        byOperation = Map.fromListWith (<>) [(opName op, [opElapsed op / fromIntegral (opIterations op)]) | (_, operations) <- successful, op <- operations]
    pure (object ["case" .= input, "mode" .= mode, "variant" .= variant, "observed" .= length observed, "successful" .= length successful, "freshProcessWallSeconds" .= spread (map (rsWall . fst) successful), "reusedConnectionNsPerOperation" .= Map.map spread byOperation])

auditRequired :: FilePath -> FilePath -> IO [Text]
auditRequired root relative = do
    let path = root </> relative
        label = toText relative
    exists <- doesFileExist path
    if not exists
        then pure [label <> ": missing"]
        else
            if ".status" `T.isSuffixOf` label
                then do
                    status <- T.strip <$> readFileText path
                    pure [label <> ": exit " <> status | status /= "0"]
                else
                    if "resource.json" `T.isSuffixOf` label
                        then do
                            resource <- readJsonFile path
                            pure [label <> ": child failed" | rsExit resource /= 0]
                        else
                            if "snapshot.json" `T.isSuffixOf` label
                                then do
                                    snapshot <- readJsonFile path
                                    let present = any (\(_, name, _) -> name == "uq_ranges_segment") (snSchema snapshot)
                                        expected = "-A/" `T.isInfixOf` label
                                    pure [label <> ": wrong candidate index state" | present /= expected]
                                else pure []

writeCompilationRows :: FilePath -> IO ()
writeCompilationRows root = withBinaryFile (root </> "samples.jsonl") WriteMode $ \handle ->
    for_ [(input, sample) | input <- cases, sample <- trialOrder] $ \(input, sample) -> do
        let directory = "samples" </> input </> sample
        resource <- optionalJson (root </> directory </> "resource.json")
        snapshot <- optionalJson (root </> directory </> "snapshot.json")
        BS.hPut handle (jsonLine (object ["case" .= input, "sample" .= sample, "path" .= directory, "resource" .= resource, "snapshot" .= snapshot]))

optionalJson :: FilePath -> IO Value
optionalJson path = doesFileExist path >>= \exists -> if exists then readJsonFile path else pure Null
