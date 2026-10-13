-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | SQLite evidence for the temporary range-index experiment.
module Ecluse.OsvIndex.Artifact (
    Snapshot (..),
    ArtifactComparison (..),
    Selection (..),
    QueryPlan (..),
    PlanObservation (..),
    inspectArtifact,
    snapshotArtifact,
    compareSnapshots,
    makeComparison,
    selectedNames,
    selectArtifact,
    fanoutOf,
) where

import Crypto.Hash (Context, SHA256, hashFinalize, hashInit, hashUpdate)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Database.SQLite.Simple (Connection, Only (Only, fromOnly), Query (Query), fold_, query, query_, withConnection)
import System.Directory (getFileSize)

import Ecluse.OsvIndex.Contract (Selected (..), SelectionRole (..))
import Ecluse.Test.Contract.Manifest (digestFile, failRunner, jsonLine, readJsonFile, writeJsonFile)
import Ecluse.Test.Osv (RangeRow)

-- | Stored evidence retains the complete metadata and the typed row multiset digest.
data Snapshot = Snapshot
    { snFileHash :: Text
    , snRows :: Int
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
inspectArtifact db output = snapshotArtifact db >>= writeJsonFile output

-- | Read fresh SQLite evidence independently of a stored sample report.
snapshotArtifact :: FilePath -> IO Snapshot
snapshotArtifact db = withConnection db $ \conn -> do
    fileHash <- digestFile db
    (count, context) <- fold_ conn rowsQuery (0 :: Int, hashInit :: Context SHA256) addRow
    Snapshot fileHash count (show (hashFinalize context)) . Map.fromList
        <$> query_ conn "SELECT key, value FROM meta ORDER BY key"
        <*> query_ conn "SELECT type, name, sql FROM sqlite_schema ORDER BY type, name"
        <*> pragma conn "PRAGMA user_version"
        <*> (map fromOnly <$> (query_ conn "PRAGMA integrity_check" :: IO [Only Text]))
        <*> getFileSize db
        <*> pragma conn "PRAGMA page_count"
        <*> pragma conn "PRAGMA page_size"
        <*> pragma conn "PRAGMA freelist_count"
  where
    addRow (count, context) (row :: RangeRow) = do
        let !nextCount = count + 1
            !nextContext = hashUpdate context (jsonLine row)
        pure (nextCount, nextContext)
    rowsQuery = "SELECT package_name, cve_id, introduced_version, fixed_version, last_affected_version, severity, epss_score FROM package_vulnerability_ranges ORDER BY package_name, cve_id, introduced_version, fixed_version, last_affected_version, severity, epss_score"

pragma :: Connection -> Query -> IO [Int]
pragma conn statement = map fromOnly <$> query_ conn statement

-- | Keep both complete snapshots beside the semantic outcome.
data ArtifactComparison = ArtifactComparison {coFaults :: [Text], coExclusions :: [Text], coA :: Snapshot, coB :: Snapshot}
    deriving stock (Eq, Show, Generic)

instance FromJSON ArtifactComparison
instance ToJSON ArtifactComparison

-- | Refuse every difference except the candidate index and listed compilation provenance.
compareSnapshots :: FilePath -> FilePath -> FilePath -> IO ()
compareSnapshots left right output = do
    a <- readJsonFile left
    b <- readJsonFile right
    let comparison = makeComparison a b
    writeJsonFile output comparison
    unless (null (coFaults comparison)) (failRunner (T.unlines (coFaults comparison)))

-- | The auditor recomputes this result instead of trusting a reported fault list.
makeComparison :: Snapshot -> Snapshot -> ArtifactComparison
makeComparison a b = ArtifactComparison (snapshotFaults a b) exclusions a b

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

-- | Each selected role keeps its own SQL-plan observation.
data PlanObservation = PlanObservation {poRole :: Maybe SelectionRole, poName :: Maybe Text, poPlan :: [(Int, Int, Int, Text)]}
    deriving stock (Eq, Show, Generic)

instance FromJSON PlanObservation
instance ToJSON PlanObservation

-- | Preserve the current production SQL and its source location.
data QueryPlan = QueryPlan {qpSource :: FilePath, qpLine :: Int, qpSql :: Text, qpObservations :: [PlanObservation]}
    deriving stock (Eq, Show, Generic)

instance FromJSON QueryPlan
instance ToJSON QueryPlan

-- | The retained database hash binds selections and plans to a concrete artifact.
data Selection = Selection {sePackages :: [Selected], sePlans :: [QueryPlan], seDatabaseHash :: Text}
    deriving stock (Eq, Show, Generic)

instance FromJSON Selection
instance ToJSON Selection

-- | Choose five roles even when multiple roles name the same package.
selectedNames :: FilePath -> FilePath -> Text -> IO ()
selectedNames db output corpusName = selectArtifact db corpusName >>= writeJsonFile output

-- | SQL counts include every duplicate row.
fanoutOf :: FilePath -> IO [(Text, Int)]
fanoutOf db = withConnection db $ \conn -> query_ conn "SELECT package_name, COUNT(*) FROM package_vulnerability_ranges GROUP BY package_name ORDER BY COUNT(*), package_name"

-- | Recompute package selection and exact production SQL plans from a retained database.
selectArtifact :: FilePath -> Text -> IO Selection
selectArtifact db corpusName = do
    counts <- fanoutOf db
    let n = length counts
        fanout = Map.fromList counts
        absentName = "ecluse-index-absent-1331"
        named role name = Selected role name (Map.findWithDefault 0 name fanout)
    unless (Map.notMember absentName fanout) (failRunner "the designated absent package is present")
    ranked <- forM [(Median, n `div` 2), (P95, 95 * n `div` 100), (Maximum, n - 1)] $ \(role, index) ->
        maybe (failRunner "the selection requires a nonempty artifact") (pure . uncurry (Selected role)) (counts !!? index)
    let packages = [named Absent absentName, named Corpus corpusName] <> ranked
    source <- readFileText "core/src/Ecluse/Core/Cve/Internal.hs"
    let sqlLines = [(number, line) | (number, line) <- zip [1 :: Int ..] (T.lines source), "SELECT DISTINCT package_name FROM" `T.isInfixOf` line || "SELECT cve_id, introduced_version, fixed_version," `T.isInfixOf` line]
    unless (length sqlLines == 2) (failRunner "production SQL extraction did not find exactly two reader queries")
    plans <- withConnection db $ \conn -> forM sqlLines $ \(number, line) -> do
        let sql = T.takeWhile (/= '"') (T.drop 1 (T.dropWhile (/= '"') line))
            statement = Query ("EXPLAIN QUERY PLAN " <> sql)
        observations <-
            if "?" `T.isInfixOf` sql
                then forM packages $ \package ->
                    PlanObservation (Just (slRole package)) (Just (slName package)) <$> query conn statement (Only (slName package))
                else do
                    plan <- query_ conn statement
                    pure [PlanObservation Nothing Nothing plan]
        when (any (null . poPlan) observations) (failRunner "empty production query plan")
        pure (QueryPlan "core/src/Ecluse/Core/Cve/Internal.hs" number sql observations)
    Selection packages plans <$> digestFile db
