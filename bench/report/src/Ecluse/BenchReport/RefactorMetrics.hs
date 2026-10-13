-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Strict refactor tolerances preserve each repetition and every signed difference.
module Ecluse.BenchReport.RefactorMetrics (auditMetrics, metricControls) where

import Data.Aeson (Object, Value (Object), eitherDecodeStrict', object, withObject, (.:), (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseEither)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.FilePath ((</>))
import UnliftIO.Exception (tryAny)

import Ecluse.BenchReport (BenchRow (..), parseCsv)
import Ecluse.BenchReport.Markdown (hasColumns, records, tableHeading, tables)
import Ecluse.BenchReport.Performance (allocationRequiredRows)
import Ecluse.BenchReport.RefactorInputs (Input (..))
import Ecluse.Test.Contract.Manifest (readJsonFile)
import Ecluse.Test.Contract.Result (auditExactIds)
import Ecluse.Test.Corpus (corpusPackages, cpName, pypiCorpusPackages)

data Rule = WithinKiB | WithinPermille | UnclassifiedCache | NoRise | Exact | Inform
    deriving stock (Eq, Show)

data Metric = Metric Text Integer Rule
    deriving stock (Eq, Show)

data Observation = Observation Text [Metric]
    deriving stock (Eq, Show)

-- | Publish all comparisons before returning audit faults or measured tolerance breaches.
auditMetrics :: FilePath -> IO ([Text], [Text])
auditMetrics root = do
    writeFileText (root </> "scope-limitations.txt") "Mutable cache rows remain unclassified diagnostics. Each row requires review regardless of its delta. Checked criteria do not establish universal parity.\n"
    outcomes <- forM ["benchmark", "allocation", "residency", "versions", "listing"] $ \kind -> do
        result <- tryAny (readKind kind >>= compareKind kind)
        pure $ either (\problem -> ([kind <> ": " <> show problem], [])) id result
    pure (concatMap fst outcomes, concatMap snd outcomes)
  where
    readKind kind = traverse (\passName -> (passName,) <$> readPass root kind passName) (passes kind)
    compareKind kind measured = do
        let pairs = [(left, right) | (i, left) <- zip [0 :: Int ..] measured, (j, right) <- zip [0 ..] measured, i < j]
        rows <- concat <$> traverse (uncurry (comparePasses kind)) pairs
        writeFileText (root </> toString kind <> "-comparison.tsv") $
            T.unlines ("scope\tkind\tbefore-pass\tafter-pass\trow\tmetric\tbefore\tafter\tdelta\trule\tresult" : map fst rows)
        forM_ [("lower", "-"), ("higher", "+")] $ \(direction, sign) ->
            writeFileText (root </> toString (kind <> "-" <> direction <> "-allocation.tsv")) $
                T.unlines [line | (line, _) <- rows, let fields = T.splitOn "\t" line, fields !!? 5 `elem` map Just ["allocated", "preparationAllocated"], maybe False (T.isPrefixOf sign) (fields !!? 8)]
        pure ([], [line | (line, failed) <- rows, failed])

passes :: Text -> [Text]
passes "benchmark" = ["A1", "A2", "A3", "B1", "B2", "B3"]
passes "residency" = ["A1", "B1"]
passes _ = ["A1", "A2", "B1", "B2"]

readPass :: FilePath -> Text -> Text -> IO [Observation]
readPass root kind passName = do
    let directory = root </> toString (kind <> "-" <> passName)
    observations <- case kind of
        "benchmark" -> do
            csv <- readFileText (directory </> "bench-results.csv")
            rows <- either (fail . toString) pure (parseCsv csv)
            traverse benchmark rows
        "allocation" -> readFileText (directory </> "perf-allocation-report.md") >>= either (fail . toString) pure . allocation
        "residency" -> readFileText (directory </> "measure.log") >>= either (fail . toString) pure . residency
        _ -> do
            inputs <- readJsonFile (root </> "inputs" </> if kind == "versions" then "versions.json" else "listings.json")
            traverse (sourceObservation directory kind) inputs
    expected <- expectedIds root kind
    require (auditExactIds "measurement" expected (map observationId observations))
    require ["empty metrics: " <> name | Observation name metrics <- observations, null metrics]
    pure observations

observationId :: Observation -> Text
observationId (Observation name _) = name

expectedIds :: FilePath -> Text -> IO [Text]
expectedIds root = \case
    "benchmark" -> T.lines <$> readFileText "taskfiles/refactor-performance/expected-rows.txt"
    "allocation" -> pure (concatMap (uncurry allocationRequiredRows) catalogues)
    "residency" -> pure [kind <> "/" <> name <> "/" <> shape | (_, names) <- catalogues, name <- names, (kind, shapes) <- residencyShapes, shape <- shapes]
    kind -> map inputId <$> (readJsonFile (root </> "inputs" </> if kind == "versions" then "versions.json" else "listings.json") :: IO [Input])

catalogues :: [(Text, [Text])]
catalogues = [("npm", map cpName corpusPackages), ("pypi", map cpName pypiCorpusPackages)]

residencyShapes :: [(Text, [Text])]
residencyShapes = [("metadata-residency", ["Wire", "Raw", "Typed", "Shared"]), ("metadata-listing", [""]), ("metadata-merge", ["Identical", "Overlapping", "Disjoint", "PublishOrder", "HeavyBase", "HeavyOldBase"]), ("metadata-evaluation", [""])]

benchmark :: BenchRow -> IO Observation
benchmark row = do
    allocated <- maybe (fail "missing benchmark allocation") pure (rowAllocatedBytes row)
    pure (Observation name [Metric "allocated" allocated rule, Metric "mean_ps" (rowMeanPs row) Inform])
  where
    name = rowGroup row <> "." <> rowBench row
    rule
        | "cache maintenance." `T.isPrefixOf` name = UnclassifiedCache
        | any (`T.isInfixOf` name) [".cold production reads (per package).", ".single-version metadata (per package)."] = WithinPermille
        | ".wire+project (per package)." `T.isInfixOf` name && ".full metadata projection" `T.isSuffixOf` name = WithinPermille
        | otherwise = WithinKiB

allocation :: Text -> Either Text [Observation]
allocation raw = traverse row [(fromMaybe "" (tableHeading table), record) | table <- tables raw, hasColumns ["package", "leg", "allocated (bytes)"] table, record <- records table]
  where
    row (ecosystem, record) = do
        package <- field "package" record
        leg <- field "leg" record
        allocated <- number "allocated (bytes)" record
        versions <- number "versions" record
        unless (allocated >= 0 && versions > 0) (Left "invalid allocation measurement")
        time <- field "time (ms)" record >>= maybe (Left "invalid time") Right . (readMaybe . toString :: Text -> Maybe Double)
        unless (isFinite time && time >= 0) (Left "invalid allocation time")
        pure (Observation (ecosystem <> "/" <> package <> "/" <> leg) [Metric "allocated" allocated (if leg == "singleVersion" then WithinKiB else WithinPermille), Metric "versions" versions Exact, Metric "time_ns" (round (time * 1000000)) Inform])
    field key = maybe (Left ("missing allocation " <> key)) Right . Map.lookup key
    number key record = field key record >>= maybe (Left ("invalid allocation " <> key)) Right . readMaybe . toString

residency :: Text -> Either Text [Observation]
residency raw = traverse readLine [(kind, T.drop (T.length kind + 1) line) | line <- T.lines raw, (kind, _) <- residencyShapes, (kind <> " ") `T.isPrefixOf` line]
  where
    readLine (kind, line) = first toText (eitherDecodeStrict' (TE.encodeUtf8 line) >>= parseEither (withObject "residency row" (row kind)))
    row kind record = do
        package <- record .: "package"
        shape <- if kind `elem` ["metadata-residency", "metadata-merge"] then record .: "shape" else pure ""
        metrics <- case kind of
            "metadata-residency" -> record .: "measurement" >>= measurement
            "metadata-evaluation" -> sequence [metric record "weak_head_retained" WithinKiB, metric record "forcing" WithinKiB, metric record "versions" Exact]
            _ -> do
                peaks <- record .: "peaks"
                sequence [metric peaks key rule | (key, rule) <- [("listingSourceBytes", Exact), ("listingBasisBytes", Exact), ("listingServedBytes", Exact), ("listingBaseline", WithinKiB), ("listingReadPeak", WithinKiB), ("listingEntryLive", WithinKiB), ("listingPeak", WithinKiB)]]
        pure (Observation (kind <> "/" <> package <> "/" <> shape) metrics)

measurement :: Object -> Parser [Metric]
measurement record = sequence [metric record key rule | (key, rule) <- [("wireBytes", Exact), ("versions", Exact), ("baselineLive", WithinKiB), ("heldLive", WithinKiB), ("releasedLive", WithinKiB), ("preparationAllocated", Inform), ("preparationMaxLive", Inform)]]

metric :: Object -> Text -> Rule -> Parser Metric
metric record key rule = do
    value <- record .: fromString (toString key)
    when (value < 0 && key `notElem` ["retained", "released", "weak_head_retained", "forcing"]) (fail "negative measurement")
    pure (Metric key value rule)

sourceObservation :: FilePath -> Text -> Input -> IO Observation
sourceObservation directory kind input = do
    status <- T.strip <$> readFileText (directory </> toString (inputId input) <> ".status")
    unless (status == "0") (fail "source probe returned failure")
    value <- readJsonFile (directory </> toString (inputId input) <> ".json")
    observed <- either fail pure (parseEither (sourceMetrics kind input) value)
    rssText <- readFileText (directory </> toString (inputId input) <> ".rss")
    rss <- maybe (fail "missing process peak RSS") pure (readMaybe (toString (T.strip rssText)) :: Maybe Integer)
    when (rss <= 0) (fail "nonpositive process peak RSS")
    pure (Observation (inputId input) (observed <> [Metric "peak_rss_kib" rss Inform]))
sourceMetrics :: Text -> Input -> Value -> Parser [Metric]
sourceMetrics kind input = withObject "maintenance source" parse
  where
    parse record = do
        status <- record .: "status"
        sha <- record .: "sha256"
        unless (status == ("ok" :: Text) && sha == inputDigest input) (fail "failed or mismatched source input")
        if kind == "versions"
            then do
                package <- record .: "package"
                version <- record .: "selected_version"
                ecosystem <- record .: "ecosystem"
                path <- record .: "path"
                mode <- record .: "mode"
                bound <- record .: "body_limit"
                chunk <- record .: "chunk_bytes"
                unless (package == inputPackage input && version == inputVersion input && ecosystem == ("npm" :: Text) && path == inputPath input && mode == ("StreamedVersions" :: Text) && bound == (134217728 :: Int) && chunk == (32768 :: Int)) (fail "version probe workload differs")
                held <- record .: "measurement"
                bytes <- held .: "wireBytes"
                count <- held .: "versions"
                unless (bytes == inputBytes input && count > (0 :: Int)) (fail "incomplete version probe")
                metrics <- measurement held
                elapsed <- metric record "read_project_ns" Inform
                pure (map strictAllocation metrics <> [elapsed])
            else do
                bytes <- record .: "wireBytes"
                count <- record .: "packages"
                architecture <- record .: "architecture"
                capabilities <- record .: "capabilities"
                oracle <- record .: "oracleSha256"
                unless (bytes == inputBytes input && count == (if inputId input == "small" then 20 else 20000 :: Int) && architecture == ("aarch64" :: Text) && capabilities == (1 :: Int) && oracle == inputOracleDigest input) (fail "listing probe workload differs")
                sequence [metric record key rule | (key, rule) <- [("allocated", NoRise), ("retained", Inform), ("released", Inform), ("peak", Inform), ("read_project_ns", Inform), ("packages", Exact)]]
    strictAllocation (Metric "preparationAllocated" value _) = Metric "preparationAllocated" value NoRise
    strictAllocation (Metric key value WithinKiB) = Metric key value Inform
    strictAllocation other = other

comparePasses :: Text -> (Text, [Observation]) -> (Text, [Observation]) -> IO [(Text, Bool)]
comparePasses kind (leftPass, left) (rightPass, right) = do
    require (auditExactIds "paired row" (map observationId left) (map observationId right))
    concat <$> traverse pair left
  where
    rightRows = Map.fromList [(observationId row, row) | row <- right]
    scope = if T.take 1 leftPass == T.take 1 rightPass then T.take 1 leftPass <> "/" <> T.take 1 rightPass else "A/B"
    pair (Observation name leftMetrics) = do
        Observation _ rightMetrics <- maybe (fail "missing paired observation") pure (Map.lookup name rightRows)
        unless (map metricIdentity leftMetrics == map metricIdentity rightMetrics) (fail "paired metric coverage differs")
        pure (zipWith (render name) leftMetrics rightMetrics)
    render name (Metric key before rule) (Metric _ after _) =
        let breached = violates rule before after
            result
                | rule == UnclassifiedCache = "unclassified-cache-review-required"
                | rule == Inform = "informational"
                | breached = if after > before then "higher-breach" else "lower-breach"
                | otherwise = "within"
            line = T.intercalate "\t" [scope, kind, leftPass, rightPass, name, key, show before, show after, signed (after - before), show rule, result]
         in (line, scope == "A/B" && breached)
    metricIdentity (Metric name _ rule) = (name, rule)

violates :: Rule -> Integer -> Integer -> Bool
violates WithinKiB before after = abs (after - before) > 1024
violates WithinPermille before after = abs (after - before) * 1000 > before
violates UnclassifiedCache _ _ = False
violates NoRise before after = after > before
violates Exact before after = after /= before
violates Inform _ _ = False

signed :: Integer -> Text
signed value = (if value > 0 then "+" else "") <> show value

require :: [Text] -> IO ()
require problems = unless (null problems) (fail (toString (T.unlines problems)))

isFinite :: Double -> Bool
isFinite value = not (isNaN value || isInfinite value)

-- | Missing observations and both signs of each fixed tolerance must remain failures.
metricControls :: IO ()
metricControls =
    require $
        ["KiB tolerance control failed" | any (violates WithinKiB 5000) [3976, 6024] || not (all (violates WithinKiB 5000) [3975, 6025])]
            <> ["per-mille tolerance control failed" | any (violates WithinPermille 100000) [99900, 100100] || not (all (violates WithinPermille 100000) [99899, 100101])]
            <> ["no-rise control failed" | violates NoRise 10 9 || not (violates NoRise 10 11)]
            <> ["missing observation accepted" | null (auditExactIds "measurement" ["a", "b"] ["a"])]
            <> ["duplicate observation accepted" | null (auditExactIds "measurement" ["a", "b"] ["a", "a", "b"])]
            <> ["valid listing measurement rejected" | isLeft (parseEither (sourceMetrics "listing" fixtureInput) fixture)]
            <> ["missing source observation accepted" | Object members <- [fixture], key <- KeyMap.keys members, isRight (parseEither (sourceMetrics "listing" fixtureInput) (Object (KeyMap.delete key members)))]
            <> ["mismatched input accepted" | isRight (parseEither (sourceMetrics "listing" fixtureInput{inputDigest = "different"}) fixture)]
            <> ["mismatched oracle accepted" | isRight (parseEither (sourceMetrics "listing" fixtureInput{inputOracleDigest = "different"}) fixture)]
            <> ["valid version measurement rejected" | isLeft (parseEither (sourceMetrics "versions" fixtureInput) versionFixture)]
            <> ["missing version observation accepted" | Object members <- [versionFixture], key <- KeyMap.keys members, isRight (parseEither (sourceMetrics "versions" fixtureInput) (Object (KeyMap.delete key members)))]
            <> ["mismatched version input accepted" | isRight (parseEither (sourceMetrics "versions" fixtureInput{inputDigest = "different"}) versionFixture)]
  where
    fixtureInput = Input "small" "unused" "small" "" "digest" "oracle" 100
    fixture = object ["status" .= ("ok" :: Text), "sha256" .= ("digest" :: Text), "oracleSha256" .= ("oracle" :: Text), "architecture" .= ("aarch64" :: Text), "capabilities" .= (1 :: Int), "wireBytes" .= (100 :: Int), "packages" .= (20 :: Int), "allocated" .= (50 :: Int), "retained" .= (10 :: Int), "released" .= (0 :: Int), "peak" .= (500 :: Int), "read_project_ns" .= (200 :: Int)]
    versionFixture = object ["status" .= ("ok" :: Text), "sha256" .= ("digest" :: Text), "package" .= ("small" :: Text), "selected_version" .= ("" :: Text), "ecosystem" .= ("npm" :: Text), "path" .= ("unused" :: Text), "mode" .= ("StreamedVersions" :: Text), "body_limit" .= (134217728 :: Int), "chunk_bytes" .= (32768 :: Int), "read_project_ns" .= (200 :: Int), "measurement" .= object ["wireBytes" .= (100 :: Int), "versions" .= (2 :: Int), "baselineLive" .= (50 :: Int), "heldLive" .= (60 :: Int), "releasedLive" .= (50 :: Int), "preparationAllocated" .= (500 :: Int), "preparationMaxLive" .= (100 :: Int)]]
