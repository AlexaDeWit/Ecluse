-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Qualify every expected trial and preserve all faults before returning failure.
module Ecluse.OsvIndex.Evidence (auditExperiment, nativeFaults) where

import Data.Aeson (Value, object, (.=))
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO (withBinaryFile)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.OsvIndex.Artifact
import Ecluse.OsvIndex.Audit
import Ecluse.OsvIndex.Contract
import Ecluse.OsvIndex.Fixtures (edgePackages, edgeRowsFaults)
import Ecluse.OsvIndex.Identity (currentBinding, identityFaults)
import Ecluse.OsvIndex.Native (fileIdentity, nativeOutputs)
import Ecluse.OsvIndex.Reader
import Ecluse.Test.Contract.Manifest (digestFile, failRunner, jsonLine, writeJsonFile)

data CaseEvidence = CaseEvidence {ceA :: Snapshot, ceB :: Snapshot, ceSelection :: Selection, ceCountsA :: [(Text, [Int])], ceCountsB :: [(Text, [Int])]}
data Sample = Sample Trial Resource Native (Maybe Snapshot) (Maybe QueryReport)

-- | Missing, malformed and semantically wrong records all produce durable qualification faults.
auditExperiment :: FilePath -> IO ()
auditExperiment root = do
    audit <- newAudit
    manifest <- attempt audit "manifest" (decodeJson (root </> "manifest.json"))
    for_ manifest $ \declared -> do
        faults <- attempt audit "identities" (identityFaults root declared)
        for_ faults (recordFaults audit "identities")
    plan <- attempt audit "trial plan" (decodeJson (root </> "trial-order.json"))
    recordFaults audit "trial plan" ["plan differs from independent obligations" | plan /= Just expectedTrials]
    journal <- attempt audit "execution journal" (decodeLines (root </> "execution.jsonl"))
    for_ journal (recordFaults audit "execution journal" . sequenceFaults expectedTrials)
    caseResults <- forM cases $ \input -> do
        evidence <- attempt audit (toText (caseName input)) (auditCase audit root input)
        pure (input, evidence)
    samples <- forM (zip [1 ..] expectedTrials) $ \(sequenceNumber, trial) ->
        attempt audit (toText (trialDirectory trial)) (auditTrial audit root (fromMaybe [] journal) sequenceNumber (join (Map.lookup (trCase trial) (Map.fromList caseResults))) trial)
    controls <- attempt audit "adversarial controls" (decodeJson (root </> "controls.json") :: IO [(Text, Bool)])
    for_ controls $ \observed -> recordFaults audit "adversarial controls" ["controls missing, duplicated or failed" | not (exactCoverage requiredControls (map fst observed) && all snd observed)]
    faults <- auditFaults audit
    let measured = catMaybes samples
    withBinaryFile (root </> "samples.jsonl") WriteMode $ \handle -> for_ (zip expectedTrials samples) $ \(trial, observed) ->
        BS.hPut handle (jsonLine (object ["trial" .= trial, "rawDirectory" .= trialDirectory trial, "observation" .= fmap sampleValue observed]))
    writeJsonFile (root </> "audit.json") (object ["complete" .= null faults, "faults" .= faults, "observed" .= length measured, "required" .= length expectedTrials, "compilation" .= compileSummaries measured, "queries" .= querySummaries measured])
    writeUtf8File
        (root </> "summary.md")
        ( T.unlines
            (["# Range-index experiment", "", if null faults then "All independent obligations hold." else "The experiment is incomplete. No removal recommendation is supported.", "", "Native wall and CPU measurements exclude identity hashing. Whole-child resource measurements include identity verification and fixture memory.", "Fresh operations include reader acceptance, operation and close. Reused operations share one accepted connection per batch.", "The OS page cache is uncontrolled. No cold-disk claim is supported.", "Review raw failures, adverse samples and spread before deciding removal or retention. A compatible reduction keeps the epoch and requires issue #1234's migration override.", "", "Qualification faults:"] <> ["- " <> faultScope fault <> ": " <> faultDetail fault | fault <- faults])
        )
    unless (null faults) (failRunner "experiment audit failed")

requiredControls :: [Text]
requiredControls = ["valid-query", "empty-query", "missing-query", "duplicate-query", "wrong-query-mode", "wrong-connection-count", "duplicate-package-roles", "valid-reader", "empty-reader", "missing-reader-row", "duplicate-reader-row", "wrong-reader-verdict", "missing-reader-end", "valid-binding", "wrong-identity", "valid-sequence", "missing-event", "duplicate-event", "wrong-order", "actual-mode-lifetimes", "valid-query-counts", "wrong-query-counts", "missing-evidence", "malformed-evidence", "malformed-utf8", "wrong-input-identity", "valid-native", "wrong-command", "wrong-native-sequence"]

auditCase :: Audit -> FilePath -> Case -> IO CaseEvidence
auditCase audit root input = do
    let scope = toText (caseName input)
        directory = root </> "readers" </> caseName input
        db variant = root </> trialDirectory (Trial input variant Warmup) </> "db/artifact.db"
        corpus = case caseEco input of
            Npm -> "lodash"
            _ -> "requests"
    for_ ["select-A", "select-B", "reader", "edges"] $ \name -> checkStatus audit (directory </> name <> ".status")
    a <- snapshotArtifact (db A)
    b <- snapshotArtifact (db B)
    actualA <- selectArtifact (db A) corpus
    actualB <- selectArtifact (db B) corpus
    for_ [(A, actualA), (B, actualB)] $ \(variant, actual) -> do
        recorded <- attempt audit scope (decodeJson (directory </> "selection-" <> variantName variant <> ".json"))
        recordFaults audit scope ["selection roles, fan-out or production query plans differ" | recorded /= Just actual]
    recordFaults audit scope ["variant selection roles differ" | sePackages actualA /= sePackages actualB]
    obligation <- readerObligation (caseEco input) (db A) (sePackages actualA)
    observed <- attempt audit scope (decodeLines (directory </> "reader.jsonl"))
    for_ observed (recordFaults audit scope . readerFaults obligation)
    auditEdges audit scope input directory (a, b) obligation
    countsA <- queryObligation (caseEco input) (db A) (sePackages actualA)
    countsB <- queryObligation (caseEco input) (db B) (sePackages actualA)
    recordFaults audit scope ["variant query result-count cycles differ" | countsA /= countsB]
    pure (CaseEvidence a b actualA countsA countsB)

auditEdges :: Audit -> Text -> Case -> FilePath -> (Snapshot, Snapshot) -> ReaderObligation -> IO ()
auditEdges audit scope input directory (baseA, baseB) baseline = do
    let edgeDir = directory </> "edges"
    expectedHash <- digestFile (edgeDir </> "A.db")
    edgeSelection <- attempt audit scope (decodeJson (edgeDir </> "selection.json"))
    recordFaults audit scope ["edge selection differs" | edgeSelection /= Just (Selection edgePackages [] expectedHash)]
    for_ [A, B] $ \variant -> do
        snapshot <- attempt audit scope (snapshotArtifact (edgeDir </> variantName variant <> ".db"))
        for_ snapshot $ \actual -> do
            let original = if variant == A then baseA else baseB
            recordFaults audit scope ["edge artifact schema, epoch or integrity differs" | snSchema actual /= snSchema original || snEpoch actual /= snEpoch original || snIntegrity actual /= ["ok"]]
        faults <- attempt audit scope (edgeRowsFaults (edgeDir </> variantName variant <> ".db"))
        for_ faults (recordFaults audit scope)
    edgeObligation <- attempt audit scope (readerObligation (caseEco input) (edgeDir </> "A.db") edgePackages)
    events <- attempt audit scope (decodeLines (directory </> "edges.jsonl"))
    for_ edgeObligation $ \obligation -> do
        let retained = filter (\(name, _, _) -> name `notElem` map slName edgePackages) (roRows obligation)
            injected = [(name, count) | (name, count, _) <- roRows obligation, name `elem` map slName edgePackages]
            expected = sort [(slName package, slRows package) | package <- edgePackages, slRows package > 0]
        recordFaults audit scope ["edge artifact changed original rows or lost injected rows" | retained /= roRows baseline || sort injected /= expected]
        for_ events (recordFaults audit scope . readerFaults obligation)

auditTrial :: Audit -> FilePath -> [Event] -> Int -> Maybe CaseEvidence -> Trial -> IO Sample
auditTrial audit root journal sequenceNumber caseEvidence trial = do
    let directory = root </> trialDirectory trial
        scope = toText (trialDirectory trial)
    resource <- decodeJson (directory </> "resource.json")
    native <- decodeJson (directory </> "native.json")
    binding <- currentBinding root trial
    recordFaults audit scope (bindingFaults binding (naBinding native) <> nativeFaults root trial sequenceNumber journal resource native)
    for_ (nativeOutputs trial) $ \relative -> do
        exists <- doesFileExist (root </> relative)
        when exists $ do
            current <- fileIdentity root relative
            recordFaults audit scope ["native output changed after execution" | filter ((== relative) . fiPath) (naOutputs native) /= [current]]
    case trWork trial of
        QueryBatch mode _ -> do
            checkStatus audit (directory </> "query.status")
            report <- decodeJson (directory </> "result.json")
            recordFaults audit scope ["query intervals exceed the native batch" | sum (map (toInteger . opElapsed) (qrOperations report)) > nativeWallNs native]
            for_ caseEvidence $ \evidence -> do
                let counts = if trVariant trial == A then ceCountsA evidence else ceCountsB evidence
                recordFaults audit scope (queryFaults mode (sePackages (ceSelection evidence)) report <> queryCountFaults counts report)
            pure (Sample trial resource native Nothing (Just report))
        _ -> do
            for_ ["compile", "inspect", "compare"] $ \name -> checkStatus audit (directory </> name <> ".status")
            snapshot <- decodeJson (directory </> "snapshot.json")
            comparison <- decodeJson (directory </> "comparison.json")
            for_ caseEvidence $ \evidence -> do
                let expectedComparison = makeComparison (ceA evidence) snapshot
                    candidate = filter (\(_, name, _) -> name == "uq_ranges_segment")
                    expectedIndex = case trVariant trial of
                        A -> candidate (snSchema (ceA evidence))
                        B -> []
                    outputIdentity = [FileIdentity (trialDirectory trial </> "db/artifact.db") (snBytes snapshot) (snFileHash snapshot)]
                recordFaults
                    audit
                    scope
                    ( ["comparison content or semantic outcome differs" | comparison /= expectedComparison || not (null (coFaults comparison))]
                        <> ["wrong candidate index definition" | candidate (snSchema snapshot) /= expectedIndex]
                        <> ["snapshot is not the native compiled artifact" | naOutputs native /= outputIdentity]
                        <> ["warm-up snapshot differs from retained database" | trWork trial == Warmup && snapshot /= (if trVariant trial == A then ceA evidence else ceB evidence)]
                    )
            pure (Sample trial resource native (Just snapshot) Nothing)

-- | Bind commands, outputs and actual event times to the expected trial.
nativeFaults :: FilePath -> Trial -> Int -> [Event] -> Resource -> Native -> [Text]
nativeFaults root trial sequenceNumber journal resource native = [label | (label, ok) <- checks, not ok]
  where
    start = naStart native
    end = naEnd native
    matching phase = filter (\event -> evSequence event == sequenceNumber && evPhase event == phase) journal
    eventShape phase event = evSequence event == sequenceNumber && evTrial event == trial && evPhase event == phase
    checks =
        [ ("wrong measured command", rsCommand resource == trialCommand root trial)
        , ("failed child or native operation", rsExit resource == 0 && isNothing (naFault native))
        , ("wrong native sequence", naSequence native == sequenceNumber && eventShape Started start && eventShape Finished end)
        , ("native events differ from actual journal", matching Started == [start] && matching Finished == [end])
        , ("invalid timing envelope", rsStartedNs resource <= evMonotonicNs start && evMonotonicNs start <= evMonotonicNs end && evMonotonicNs end <= rsEndedNs resource && naCpuPicoseconds native >= 0)
        , ("resource wall time differs from its actual timestamps", abs (rsWall resource - fromIntegral (rsEndedNs resource - rsStartedNs resource) / 1e9) <= 1e-9)
        , ("native CPU exceeds the process envelope", fromInteger (naCpuPicoseconds native) / 1e12 <= rsUser resource + rsSystem resource + 0.01)
        , ("negative process resources", all (\n -> n >= 0 && not (isNaN n || isInfinite n)) [rsWall resource, rsUser resource, rsSystem resource, rsRss resource])
        , ("wrong or duplicate native output inventory", map fiPath (naOutputs native) == nativeOutputs trial)
        ]

checkStatus :: Audit -> FilePath -> IO ()
checkStatus audit path = do
    value <- attempt audit (toText path) (T.strip <$> readUtf8File path)
    recordFaults audit (toText path) ["nonzero or missing process status" | value /= Just "0"]

sampleValue :: Sample -> Value
sampleValue (Sample trial resource native snapshot query) = object ["trial" .= trial, "resource" .= resource, "native" .= native, "snapshot" .= snapshot, "query" .= query]

compileSummaries :: [Sample] -> [Value]
compileSummaries samples =
    [ object ["case" .= input, "variant" .= variant, "expected" .= (6 :: Int), "successfulDecoded" .= length rows, "nativeWallSeconds" .= spread [fromInteger (nativeWallNs native) / 1e9 | (_, native, _) <- rows], "nativeCpuSeconds" .= spread [fromInteger (naCpuPicoseconds native) / 1e12 | (_, native, _) <- rows], "envelopeWallSeconds" .= spread [rsWall resource | (resource, _, _) <- rows], "envelopeUserSeconds" .= spread [rsUser resource | (resource, _, _) <- rows], "envelopeSystemSeconds" .= spread [rsSystem resource | (resource, _, _) <- rows], "envelopePeakRssKiB" .= spread [rsRss resource | (resource, _, _) <- rows], "databaseBytes" .= spread [fromInteger (snBytes snapshot) | (_, _, snapshot) <- rows]]
    | input <- cases
    , variant <- [A, B]
    , let rows = [(resource, native, snapshot) | Sample trial resource native (Just snapshot) _ <- samples, trCase trial == input, trVariant trial == variant, rsExit resource == 0, CompilePair{} <- [trWork trial]]
    ]

querySummaries :: [Sample] -> [Value]
querySummaries samples =
    [ object ["case" .= input, "variant" .= variant, "mode" .= mode, "expected" .= (30 :: Int), "successfulDecoded" .= length rows, "envelopeWallSeconds" .= spread [rsWall resource | (resource, _) <- rows], "nativeNsPerOperation" .= Map.map spread (Map.fromListWith (<>) [(opName op, [fromIntegral (opElapsed op) / fromIntegral (opIterations op)]) | (_, report) <- rows, op <- qrOperations report, opIterations op > 0])]
    | input <- cases
    , variant <- [A, B]
    , mode <- [Reused, Fresh]
    , let rows = [(resource, report) | Sample trial resource _ _ (Just report) <- samples, trCase trial == input, trVariant trial == variant, QueryBatch actualMode _ <- [trWork trial], actualMode == mode, rsExit resource == 0]
    ]

spread :: [Double] -> Value
spread values = case sort values of
    [] -> object ["count" .= (0 :: Int)]
    firstValue : rest ->
        let count = fromIntegral (length values)
            mean = sum values / count
            variance = if count > 1 then sum [(x - mean) ^ (2 :: Int) | x <- values] / (count - 1) else 0
         in object ["count" .= length values, "minimum" .= firstValue, "maximum" .= foldl' max firstValue rest, "mean" .= mean, "sampleStdev" .= sqrt variance]

nativeWallNs :: Native -> Integer
nativeWallNs native = toInteger (evMonotonicNs (naEnd native)) - toInteger (evMonotonicNs (naStart native))
