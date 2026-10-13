-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Adversarial qualification fixtures and a tiny actual connection-lifetime control.
module Ecluse.OsvIndex.Controls (runControls) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import UnliftIO.Exception (tryAny)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.OsvIndex.Artifact (ArtifactComparison (coFaults), Snapshot (snMetadata), makeComparison)
import Ecluse.OsvIndex.Audit (attempt, auditFaults, decodeJson, newAudit, readUtf8File, writeUtf8File)
import Ecluse.OsvIndex.Contract
import Ecluse.OsvIndex.Evidence (nativeFaults)
import Ecluse.OsvIndex.Reader
import Ecluse.Test.Contract.Manifest (failRunner, writeJsonFile)
import Ecluse.Test.Osv (mkValidDbWithRows)

-- | These controls execute remotely before measurements and need no feed download.
runControls :: FilePath -> IO ()
runControls root = do
    runtime <- tryAny $ withSystemTempDirectory "osv-index-controls" $ \directory -> do
        let path = directory </> "fixture.db"
        mkValidDbWithRows path [("epss_status", "available"), ("row_count", "1")] [("fixture", "CVE-fixture", Just "0", Just "1.0.0", Nothing, Just 8, Just 0.1)]
        reused <- measureReader Reused Npm path selected
        fresh <- measureReader Fresh Npm path selected
        audit <- newAudit
        missing <- attempt audit "missing" (decodeJson (directory </> "absent.json") :: IO QueryReport)
        writeUtf8File (directory </> "malformed.json") "{"
        malformed <- attempt audit "malformed" (decodeJson (directory </> "malformed.json") :: IO QueryReport)
        BS.writeFile (directory </> "malformed-utf8.txt") (BS.pack [0xff])
        malformedUtf8 <- attempt audit "malformed UTF-8" (readUtf8File (directory </> "malformed-utf8.txt"))
        faults <- auditFaults audit
        captured <- decodeJson "bench/osv-index/fixtures/captured-npm-snapshot.json"
        pure $ metadataControls captured <> [("actual-mode-lifetimes", null (queryFaults Reused selected reused) && null (queryFaults Fresh selected fresh) && qrConnections reused == 1 && qrConnections fresh > 1), ("missing-evidence", isNothing missing && length faults == 3), ("malformed-evidence", isNothing malformed && length faults == 3), ("malformed-utf8", isNothing malformedUtf8 && length faults == 3)]
    let observations = pureControls <> fromRight ([("actual-mode-lifetimes", False), ("missing-evidence", False), ("malformed-evidence", False), ("malformed-utf8", False)] <> [(name, False) | name <- metadataControlNames]) runtime
    for_ (leftToMaybe runtime) (writeUtf8File (root </> "controls-error.txt") . show)
    writeJsonFile (root </> "controls.json") observations
    unless (all snd observations) (failRunner "adversarial audit control failed")

selected :: [Selected]
selected = Selected Absent "absent" 0 : [Selected role "fixture" 1 | role <- [Corpus, Median, P95, Maximum]]

pureControls :: [(Text, Bool)]
pureControls =
    [ ("valid-query", null (queryFaults Reused selected report))
    , ("valid-query-counts", null (queryCountFaults expectedCounts report))
    , ("wrong-query-counts", not (null (queryCountFaults expectedCounts report{qrOperations = [operation{opTotal = 0} | operation <- operations]})))
    , ("empty-query", rejects report{qrOperations = []})
    , ("missing-query", rejects report{qrOperations = drop 1 operations})
    , ("duplicate-query", rejects report{qrOperations = take 1 operations <> operations})
    , ("wrong-query-mode", rejects report{qrMode = Fresh})
    , ("wrong-connection-count", rejects report{qrConnections = 0})
    , ("duplicate-package-roles", length (expectedOperations selected) == 11 && exactCoverage (expectedOperations selected) (expectedOperations selected))
    , ("valid-reader", null (readerFaults obligation readerEvents))
    , ("empty-reader", rejectsReader [])
    , ("missing-reader-row", rejectsReader [namesEvent, ruleEvent, finishedEvent])
    , ("duplicate-reader-row", rejectsReader [namesEvent, packageEvent, packageEvent, ruleEvent, finishedEvent])
    , ("wrong-reader-verdict", rejectsReader [namesEvent, packageEvent, RuleRead key verdict (Unable "FailDeny" "NoDatabaseLoaded"), finishedEvent])
    , ("missing-reader-end", rejectsReader [namesEvent, packageEvent, ruleEvent])
    , ("valid-binding", null (bindingFaults binding binding))
    , ("wrong-identity", not (null (bindingFaults binding binding{biExecutable = FileIdentity "bin/A" 1 "changed"})))
    , ("wrong-input-identity", not (null (bindingFaults binding binding{biInputs = [FileIdentity "inputs/changed" 1 "changed"]})))
    , ("valid-native", null (nativeFaults "control" trial 1 events resource native))
    , ("wrong-command", not (null (nativeFaults "control" trial 1 events resource{rsCommand = ["wrong"]} native)))
    , ("wrong-native-sequence", not (null (nativeFaults "control" trial 1 events resource native{naSequence = 2})))
    , ("valid-sequence", null (sequenceFaults [trial] events))
    , ("missing-event", not (null (sequenceFaults [trial] (take 1 events))))
    , ("duplicate-event", not (null (sequenceFaults [trial] (events <> events))))
    , ("wrong-order", not (null (sequenceFaults [trial] (reverse events))))
    ]
  where
    operations = [Operation name 20000000 2 2 | name <- expectedOperations selected]
    expectedCounts = [(name, [1]) | name <- expectedOperations selected]
    report = QueryReport Reused selected operations 1
    rejects = not . null . queryFaults Reused selected
    key = ProbeKey "control" "fixture" "1.0.0" "AllowIfRemediatesCve"
    verdict = Granted "control reason"
    obligation = ReaderObligation [("fixture", 1, "row-digest")] [(key, verdict)]
    namesEvent = NamesRead ["fixture"] ["fixture"]
    packageEvent = PackageRead "fixture" 1 1 "row-digest" "row-digest" True
    ruleEvent = RuleRead key verdict verdict
    finishedEvent = ReaderFinished 1 1 1
    readerEvents = [namesEvent, packageEvent, ruleEvent, finishedEvent]
    rejectsReader = not . null . readerFaults obligation
    trial = Trial NpmCorpus A Warmup
    time = UTCTime (fromGregorian 2026 1 1) 0
    events = [Event 1 trial Started 1 time, Event 1 trial Finished 2 time]
    resource = Resource (trialCommand "control" trial) 0 3e-9 0 0 1 0 3
    native = Native binding 1 (Event 1 trial Started 1 time) (Event 1 trial Finished 2 time) 1 [FileIdentity (trialDirectory trial </> "db/artifact.db") 1 "artifact"] Nothing
    binding = Binding trial (Execution "source" "run" "attempt" "ARM64") "manifest" (FileIdentity "bin/A" 1 "binary") [] ["control"] 1 Nothing

metadataControlNames :: [Text]
metadataControlNames = ["metadata-self", "metadata-port-change", "metadata-build-time", "metadata-host-change", "metadata-path-change", "metadata-scheme-change", "metadata-missing-field", "metadata-missing-required-self", "metadata-inconsistent-pair", "metadata-invalid-port", "metadata-authority-field-form", "metadata-url-field-form", "metadata-other-values"]

metadataControls :: Snapshot -> [(Text, Bool)]
metadataControls captured =
    [ ("metadata-self", accepts captured captured)
    , ("metadata-port-change", accepts captured (withPorts "51001" "51002"))
    , ("metadata-build-time", accepts captured (updated [("built_at", "2026-10-14T00:00:00Z")]))
    , ("metadata-host-change", rejects (changed sourceKeys (T.replace "127.0.0.1" "127.0.0.2")))
    , ("metadata-path-change", all (rejects . (\key -> changed [key] (<> "/changed"))) urlKeys)
    , ("metadata-scheme-change", all (rejects . (\key -> changed [key] (T.replace "http://" "https://"))) urlKeys)
    , ("metadata-missing-field", all (rejects . without) (Map.keys original))
    , ("metadata-missing-required-self", all (invalidSelf . without) requiredKeys)
    , ("metadata-inconsistent-pair", all (invalidSelf . (\key -> updated [(key, "127.0.0.1:51001")])) authorityKeys)
    , ("metadata-invalid-port", all invalidSelf ([withPorts port "51002" | port <- invalidPorts] <> [withPorts "51001" port | port <- invalidPorts]))
    , ("metadata-authority-field-form", all (invalidSelf . (\key -> changed [key] ("http://" <>))) authorityKeys)
    , ("metadata-url-field-form", all invalidSelf [updated [(urlKey, prefix <> bare)] | (authorityKey, urlKey) <- sourcePairs, Just bare <- [Map.lookup authorityKey original], prefix <- ["", "http://"]])
    , ("metadata-other-values", not (null otherKeys) && all (rejects . (\key -> changed [key] (<> "changed"))) otherKeys && rejects (updated [("extra-metadata-control", "present")]))
    ]
  where
    original = snMetadata captured
    sourcePairs = [("source_url", "osv_source"), ("epss_source_url", "epss_source")]
    authorityKeys = map fst sourcePairs
    urlKeys = map snd sourcePairs
    sourceKeys = authorityKeys <> urlKeys
    requiredKeys = "built_at" : sourceKeys
    otherKeys = filter (`notElem` requiredKeys) (Map.keys original)
    invalidPorts = ["", "0", "65536", "01000", "not-a-port", "-1", " 51001", "51001 "]
    accepts a b = null (coFaults (makeComparison a b))
    rejects = not . accepts captured
    invalidSelf candidate = not (accepts candidate candidate)
    updated fields = captured{snMetadata = Map.union (Map.fromList fields) original}
    changed keys transform = captured{snMetadata = Map.mapWithKey (\key value -> if key `elem` keys then transform value else value) original}
    without key = captured{snMetadata = Map.delete key original}
    withPorts osvPort epssPort =
        updated
            [ ("source_url", "127.0.0.1:" <> osvPort)
            , ("osv_source", "http://127.0.0.1:" <> osvPort <> "/all.zip")
            , ("epss_source_url", "127.0.0.1:" <> epssPort)
            , ("epss_source", "http://127.0.0.1:" <> epssPort <> "/epss_scores-current.csv.gz")
            ]
