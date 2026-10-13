-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | SQLite evidence for the temporary range-index experiment.
module Ecluse.OsvIndex.Artifact (Snapshot (..), inspectArtifact, compareSnapshots, selectedNames) where

import Crypto.Hash (Context, SHA256, hashFinalize, hashInit, hashUpdate)
import Data.Aeson (FromJSON, ToJSON, object, (.=))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Database.SQLite.Simple (Connection, Only (Only, fromOnly), Query (Query), fold_, query, query_, withConnection)
import System.Directory (getFileSize)

import Ecluse.Test.Contract.Manifest (failRunner, jsonLine, readJsonFile, writeJsonFile)
import Ecluse.Test.Osv (RangeRow)

-- | Stored evidence retains the complete metadata and the typed row multiset digest.
data Snapshot = Snapshot
    { snRows :: Int
    , snRowsDigest :: Text
    , snMetadata :: Map Text Text
    , snSchema :: [(Text, Text, Maybe Text)]
    , snEpoch :: [Int]
    , snIntegrity :: [Text]
    , snBytes :: Integer
    , snPages :: [Int]
    , snPageSize :: [Int]
    , snFreePages :: [Int]
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON Snapshot
instance FromJSON Snapshot

-- | Hash every sorted typed row, including repeated copies, without retaining the table.
inspectArtifact :: FilePath -> FilePath -> IO ()
inspectArtifact db output = withConnection db $ \conn -> do
    (count, context) <- fold_ conn rowsQuery (0 :: Int, hashInit :: Context SHA256) addRow
    snapshot <-
        Snapshot count (show (hashFinalize context)) . Map.fromList
            <$> query_ conn "SELECT key, value FROM meta ORDER BY key"
            <*> query_ conn "SELECT type, name, sql FROM sqlite_schema ORDER BY type, name"
            <*> pragma conn "PRAGMA user_version"
            <*> (map fromOnly <$> (query_ conn "PRAGMA integrity_check" :: IO [Only Text]))
            <*> getFileSize db
            <*> pragma conn "PRAGMA page_count"
            <*> pragma conn "PRAGMA page_size"
            <*> pragma conn "PRAGMA freelist_count"
    writeJsonFile output snapshot
  where
    addRow (count, context) (row :: RangeRow) = do
        let !nextCount = count + 1
            !nextContext = hashUpdate context (jsonLine row)
        pure (nextCount, nextContext)
    rowsQuery = "SELECT package_name, cve_id, introduced_version, fixed_version, last_affected_version, severity, epss_score FROM package_vulnerability_ranges ORDER BY package_name, cve_id, introduced_version, fixed_version, last_affected_version, severity, epss_score"

pragma :: Connection -> Query -> IO [Int]
pragma conn statement = map fromOnly <$> query_ conn statement

-- | Refuse every difference except the candidate index and listed compilation provenance.
compareSnapshots :: FilePath -> FilePath -> FilePath -> IO ()
compareSnapshots left right output = do
    a <- readJsonFile left
    b <- readJsonFile right
    let faults = snapshotFaults a b
    writeJsonFile output (object ["faults" .= faults, "metadataExclusions" .= exclusions, "a" .= a, "b" .= b])
    unless (null faults) (failRunner (T.unlines faults))

exclusions :: [Text]
exclusions = ["built_at", "source_url", "epss_source_url", "osv_source", "epss_source"]

snapshotFaults :: Snapshot -> Snapshot -> [Text]
snapshotFaults a b =
    [ label
    | (label, equal) <-
        [ ("typed row multiset differs", (snRows a, snRowsDigest a) == (snRows b, snRowsDigest b))
        , ("semantic metadata differs", normalMetadata a == normalMetadata b)
        , ("schema differs beyond candidate index", normalSchema a == normalSchema b)
        , ("schema epoch differs", snEpoch a == snEpoch b)
        , ("integrity check failed", snIntegrity a == ["ok"] && snIntegrity b == ["ok"])
        , ("required lookup index missing", all required [a, b])
        , ("unexpected source URL metadata", all validSource [a, b])
        , ("empty range table", snRows a > 0 && snRows b > 0)
        ]
    , not equal
    ]
  where
    normalMetadata = Map.mapWithKey normalise . Map.delete "built_at" . snMetadata
    normalise key value
        | key `elem` drop 1 exclusions = normalUrl value
        | otherwise = value
    normalSchema = filter (\(_, name, _) -> name /= "uq_ranges_segment") . snSchema
    required s = all (`elem` [name | (kind, name, _) <- snSchema s, kind == "index"]) ["idx_package_name", "idx_package_fixed"]
    validSource s = all (T.isPrefixOf "http://127.0.0.1:" . snd) (filter ((`elem` drop 1 exclusions) . fst) (Map.toList (snMetadata s)))

normalUrl :: Text -> Text
normalUrl value = case T.stripPrefix "http://127.0.0.1:" value of
    Just rest -> "http://127.0.0.1:<port>" <> T.dropWhile (/= '/') rest
    Nothing -> value

-- | Choose fixed fan-out ranks and capture the exact production SQL query plans.
selectedNames :: FilePath -> FilePath -> Text -> IO ()
selectedNames db output corpusName = withConnection db $ \conn -> do
    counts <- query_ conn "SELECT package_name, COUNT(*) FROM package_vulnerability_ranges GROUP BY package_name ORDER BY COUNT(*), package_name" :: IO [(Text, Int)]
    let n = length counts
        ranked = mapMaybe (counts !!?) [n `div` 2, max 0 (95 * n `div` 100), max 0 (n - 1)]
        names = ordNub (["ecluse-index-absent-1331", corpusName] <> map fst ranked)
    let fanout = Map.fromList counts
    unless (Map.notMember "ecluse-index-absent-1331" fanout) (failRunner "the designated absent package is present")
    source <- readFileText "core/src/Ecluse/Core/Cve/Internal.hs"
    let sqlLines = [(number, line) | (number, line) <- zip [1 :: Int ..] (T.lines source), "SELECT DISTINCT package_name FROM" `T.isInfixOf` line || "SELECT cve_id, introduced_version, fixed_version," `T.isInfixOf` line]
    unless (length sqlLines == 2) (failRunner "production SQL extraction did not find exactly two reader queries")
    plans <- forM sqlLines $ \(number, line) -> do
        let sql = T.takeWhile (/= '"') (T.drop 1 (T.dropWhile (/= '"') line))
            statement = Query ("EXPLAIN QUERY PLAN " <> sql)
        observations <-
            if "?" `T.isInfixOf` sql
                then forM names $ \name -> do
                    plan <- query conn statement (Only name) :: IO [(Int, Int, Int, Text)]
                    pure (object ["name" .= name, "plan" .= plan])
                else do
                    plan <- query_ conn statement :: IO [(Int, Int, Int, Text)]
                    pure [object ["plan" .= plan]]
        pure (object ["source" .= ("core/src/Ecluse/Core/Cve/Internal.hs" :: Text), "line" .= number, "sql" .= sql, "observations" .= observations])
    writeJsonFile output (object ["names" .= names, "rankedFanout" .= ranked, "selectedFanout" .= [(name, Map.findWithDefault 0 name fanout) | name <- names], "plans" .= plans, "fixedVersion" .= ("in-memory packageAdvisories/fixedAt, no production SQL" :: Text)])
