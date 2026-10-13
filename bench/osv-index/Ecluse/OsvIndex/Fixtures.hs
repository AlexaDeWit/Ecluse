-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Fixed additional rows exercise nullable bounds and duplicate persistence.
module Ecluse.OsvIndex.Fixtures (prepareEdges, edgePackages, edgeRowsFaults) where

import Database.SQLite.Simple (executeMany, execute_, query_, withConnection)
import System.Directory (copyFile, createDirectoryIfMissing)
import System.FilePath ((</>))

import Ecluse.OsvIndex.Artifact (Selection (Selection))
import Ecluse.OsvIndex.Contract (Selected (Selected), SelectionRole (..))
import Ecluse.Test.Contract.Manifest (digestFile, failRunner, writeJsonFile)
import Ecluse.Test.Osv (RangeRow)

-- | Five roles exercise each bound shape and an absent package.
edgePackages :: [Selected]
edgePackages =
    [ Selected Absent "ecluse-index-edge-missing" 0
    , Selected Corpus "ecluse-index-edge-fixed" 2
    , Selected Median "ecluse-index-edge-last" 1
    , Selected P95 "ecluse-index-edge-unbounded" 1
    , Selected Maximum "ecluse-index-edge-invalid" 1
    ]

-- | Mutate only copied artifacts, retaining each variant's original indexes.
prepareEdges :: FilePath -> FilePath -> FilePath -> IO ()
prepareEdges left right directory = do
    createDirectoryIfMissing True directory
    let a = directory </> "A.db"
        b = directory </> "B.db"
    copyFile left a
    copyFile right b
    for_ [a, b] $ \path -> do
        withConnection path $ \conn -> do
            executeMany conn "INSERT OR IGNORE INTO package_vulnerability_ranges (package_name, cve_id, introduced_version, fixed_version, last_affected_version, severity, epss_score) VALUES (?, ?, ?, ?, ?, ?, ?)" edgeRows
            execute_ conn "UPDATE meta SET value = CAST((SELECT COUNT(*) FROM package_vulnerability_ranges) AS TEXT) WHERE key = 'row_count'"
        faults <- edgeRowsFaults path
        unless (null faults) (failRunner (show faults))
    hash <- digestFile a
    writeJsonFile (directory </> "selection.json") (Selection edgePackages [] hash)

-- | Assert exact injected rows rather than accepting equal losses in both variants.
edgeRowsFaults :: FilePath -> IO [Text]
edgeRowsFaults path = withConnection path $ \conn -> do
    stored <- query_ conn "SELECT package_name, cve_id, introduced_version, fixed_version, last_affected_version, severity, epss_score FROM package_vulnerability_ranges WHERE package_name IN ('ecluse-index-edge-fixed', 'ecluse-index-edge-last', 'ecluse-index-edge-unbounded', 'ecluse-index-edge-invalid')" :: IO [RangeRow]
    pure ["injected edge rows lost values or duplicate copies" | sort stored /= sort edgeRows]

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
