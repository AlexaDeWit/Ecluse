-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Temporary measurement entry point. Task owns preparation and sample ordering.
module Main (main) where

import Data.Aeson (object, (.=))
import Data.ByteString.Lazy qualified as LBS
import Database.SQLite.Simple (executeMany, execute_, query_, withConnection)
import System.Directory (copyFile, createDirectory, createDirectoryIfMissing, renameFile)
import System.FilePath ((</>))

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.OsvIndex.Artifact (compareSnapshots, inspectArtifact, selectedNames)
import Ecluse.OsvIndex.Evidence (auditExperiment, writeManifest)
import Ecluse.OsvIndex.Reader (checkReaders, readerBatch)
import Ecluse.Test.Contract.Manifest (failRunner, writeJsonFile)
import Ecluse.Test.Corpus.Advisories (AdvisoryInputs (..), compileAdvisoryInputs, corpusAdvisories)
import Ecluse.Test.Osv (RangeRow)

-- | Execute one named leaf operation.
main :: IO ()
main =
    getArgs >>= \case
        ["corpus", eco, root] ->
            parseEco eco >>= \ecosystem -> do
                inputs <- corpusAdvisories ecosystem
                LBS.writeFile (root </> (eco <> "-corpus.zip")) (aiOsvZip inputs)
                LBS.writeFile (root </> (eco <> "-corpus-epss.gz")) (aiEpssFeed inputs)
        ["compile", eco, zipPath, epssPath, output] ->
            parseEco eco >>= \ecosystem -> do
                createDirectory output
                inputs <- AdvisoryInputs <$> readFileLBS zipPath <*> readFileLBS epssPath
                artifact <- compileAdvisoryInputs ecosystem output inputs
                renameFile artifact (output </> "artifact.db")
                writeJsonFile (output </> "compiled.json") (object ["artifact" .= ("artifact.db" :: Text)])
        ["inspect", db, output] -> inspectArtifact db output
        ["compare", left, right, output] -> compareSnapshots left right output
        ["select", db, output, capture] -> selectedNames db output (toText capture)
        ["reader-check", eco, left, right, selection, output] -> parseEco eco >>= \ecosystem -> checkReaders ecosystem left right selection output
        ["reader-batch", eco, db, selection, output] -> parseEco eco >>= \ecosystem -> readerBatch ecosystem db selection output
        ["edges", eco, left, right, directory, output] -> parseEco eco >>= \ecosystem -> checkEdges ecosystem left right directory output
        ["manifest", root] -> writeManifest root
        ["audit", root] -> auditExperiment root
        _ -> failRunner "expected corpus, compile, inspect, compare, select, reader-check, reader-batch, edges, manifest or audit"

parseEco :: String -> IO Ecosystem
parseEco = \case
    "npm" -> pure Npm
    "pypi" -> pure PyPI
    _ -> failRunner "expected npm or pypi"

checkEdges :: Ecosystem -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
checkEdges eco left right directory output = do
    createDirectoryIfMissing True directory
    let a = directory </> "A.db"
        b = directory </> "B.db"
        selection = directory </> "selection.json"
    copyFile left a
    copyFile right b
    for_ [a, b] $ \path -> withConnection path $ \conn -> do
        executeMany conn "INSERT OR IGNORE INTO package_vulnerability_ranges (package_name, cve_id, introduced_version, fixed_version, last_affected_version, severity, epss_score) VALUES (?, ?, ?, ?, ?, ?, ?)" edgeRows
        stored <- query_ conn "SELECT package_name, cve_id, introduced_version, fixed_version, last_affected_version, severity, epss_score FROM package_vulnerability_ranges WHERE package_name IN ('ecluse-index-edge-fixed', 'ecluse-index-edge-last', 'ecluse-index-edge-unbounded', 'ecluse-index-edge-invalid')" :: IO [RangeRow]
        unless (sort stored == sort edgeRows) (failRunner "injected edge rows lost values or duplicate copies")
        execute_ conn "UPDATE meta SET value = CAST((SELECT COUNT(*) FROM package_vulnerability_ranges) AS TEXT) WHERE key = 'row_count'"
    writeJsonFile selection (object ["names" .= (["ecluse-index-edge-fixed", "ecluse-index-edge-last", "ecluse-index-edge-unbounded", "ecluse-index-edge-invalid", "ecluse-index-edge-missing"] :: [Text])])
    checkReaders eco a b selection output

edgeRows :: [RangeRow]
edgeRows =
    [ fixed
    , fixed
    , ("ecluse-index-edge-last", "CVE-last", Just "0", Nothing, Just "1.0.0", Just 8, Just 0.7)
    , ("ecluse-index-edge-unbounded", "CVE-unbounded", Nothing, Nothing, Nothing, Nothing, Nothing)
    , ("ecluse-index-edge-invalid", "CVE-invalid", Just "invalid", Just "invalid-fix", Nothing, Just 10, Just 1)
    ]
  where
    fixed = ("ecluse-index-edge-fixed", "CVE-fixed", Just "0", Just "1.0.0", Nothing, Just 8, Just 0.7)
