-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Production reader and rule comparisons, with explicit diagnostic-order evidence.
module Ecluse.OsvIndex.Reader (checkReaders, readerBatch) where

import Data.Aeson (FromJSON (parseJSON), Value, object, withObject, (.:), (.=))
import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime (UTCTime), fromGregorian)
import GHC.Clock (getMonotonicTimeNSec)
import System.IO (withBinaryFile)
import UnliftIO.Exception (evaluate)

import Ecluse.Core.Cve
import Ecluse.Core.Cve.Types (DbEtag (DbEtag))
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Osv.Types (UpperBound (..))
import Ecluse.Core.Package (infoVersions, mkPackageName)
import Ecluse.Core.Rules (RuleDeps)
import Ecluse.Core.Rules.Types
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Contract.Manifest (failRunner, jsonLine, readJsonFile, writeJsonFile)
import Ecluse.Test.EcosystemBench (EcosystemBench (ebCorpus, ebEcosystem), ecosystemBenches)
import Ecluse.Test.OsvDb (withServedArtifact)
import Ecluse.Test.Rules (evalRule, servingRuleDeps)

newtype Selection = Selection [Text]
instance FromJSON Selection where
    parseJSON = withObject "query selection" $ \o -> Selection <$> o .: "names"

-- | Compare all names and complete row multisets, then rule results for bounds and captured releases.
checkReaders :: Ecosystem -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
checkReaders eco left right selection output = do
    Selection selected <- readJsonFile selection
    captures <- ecosystemBenches
    withServedArtifact eco left $ \_ a -> withServedArtifact eco right $ \_ b ->
        withBinaryFile output WriteMode $ \handle -> do
            let al = cveDbLookup a
                bl = cveDbLookup b
                deps = servingRuleDeps (DbEtag "osv-index-comparison")
                emit = BS.hPut handle . jsonLine
            an <- sort <$> cveCoveredNames al
            bn <- sort <$> cveCoveredNames bl
            emit (object ["coveredNamesEqual" .= (an == bn), "count" .= length an])
            unless (an == bn) (failRunner "covered names differ")
            for_ an $ \name -> do
                ar <- cveAdvisoriesFor al name
                br <- cveAdvisoriesFor bl name
                let equal = sort (map rowIdentity ar) == sort (map rowIdentity br)
                emit (object ["package" .= name, "rows" .= length ar, "multisetEqual" .= equal, "sequenceEqual" .= (ar == br)])
                unless equal (failRunner ("reader row multiset differs for " <> name))
            for_ selected $ \name -> do
                rows <- cveAdvisoriesFor al name
                let versions = ordNub (["0", "1.0.0", "not-a-version"] <> concatMap boundVersions rows)
                compareEvidence emit (deps al) (deps bl) [identityEvidence (mkPackageName eco Nothing name) (mkVersion eco v) | v <- versions]
            for_ (filter ((== eco) . ebEcosystem) captures) $ \capture ->
                for_ (ebCorpus capture) $ \(_, _, info, _) ->
                    compareEvidence emit (deps al) (deps bl) (map completeEvidence (Map.elems (infoVersions info)))
            emit (object ["complete" .= True, "diagnosticNormalisation" .= ("advisory ID lists sorted with duplicate counts retained, stable comparison ETag" :: Text)])

rowIdentity :: AdvisoryRange -> (Text, Maybe Double, Maybe Text, Text, Maybe Double)
rowIdentity row = (arCveId row, arSeverity row, arIntroduced row, show (arUpperBound row), arEpss row)

boundVersions :: AdvisoryRange -> [Text]
boundVersions row =
    maybeToList (arIntroduced row) <> case arUpperBound row of
        FixedBefore v -> [v]
        LastAffected v -> [v]
        Unbounded -> []

compareEvidence :: (Value -> IO ()) -> RuleDeps -> RuleDeps -> [RuleEvidence] -> IO ()
compareEvidence emit a b evidence = for_ evidence $ \ev -> for_ rules $ \rule -> do
    av <- normaliseVerdict <$> evalRule a context rule ev
    bv <- normaliseVerdict <$> evalRule b context rule ev
    emit (object ["evidence" .= (show ev :: Text), "rule" .= (show rule :: Text), "a" .= (show av :: Text), "b" .= (show bv :: Text), "equal" .= (av == bv)])
    unless (av == bv) (failRunner "rule verdict differs")
  where
    context = EvalContext (UTCTime (fromGregorian 2026 1 1) 0) Nothing
    rules = AllowIfRemediatesCve : [DenyIfCve (DenyIfCveParams n FailDeny) | n <- [0, 8, 10]] <> [DenyIfEpss (DenyIfEpssParams n FailDeny) | n <- [0, 0.5, 1]]

normaliseVerdict :: RuleVerdict -> RuleVerdict
normaliseVerdict = \case
    Allow reason -> Allow (normaliseReason reason)
    Deny etag reason -> Deny etag (normaliseReason reason)
    NoDecision reason -> NoDecision (normaliseReason reason)
    verdict@CannotVet{} -> verdict

normaliseReason :: Reason -> Reason
normaliseReason = \case
    Remediates ids -> Remediates (ordered ids)
    FixesButStillAffected fixed affected -> FixesButStillAffected (ordered fixed) (ordered affected)
    AffectedBy score threshold ids -> AffectedBy score threshold (ordered ids)
    reason -> reason
  where
    ordered = mkAdvisoryIds . NE.sort . unAdvisoryIds

-- | Repeat each production operation until its batch exceeds twenty milliseconds.
readerBatch :: Ecosystem -> FilePath -> FilePath -> FilePath -> IO ()
readerBatch eco db selection output = do
    Selection names <- readJsonFile selection
    withServedArtifact eco db $ \_ opened -> do
        let lookup = cveDbLookup opened
        enumeration <- timeOperation "covered-names" (const (cveCoveredNames lookup >>= evaluate . length))
        ranges <- forM names $ \name -> timeOperation ("ranges:" <> name) (const (cveAdvisoriesFor lookup name >>= evaluate . length))
        fixes <- forM names $ \name -> do
            rows <- cveAdvisoriesFor lookup name
            let prepared = packageAdvisories eco rows
                versions = ordNub ("1.0.0" : concatMap boundVersions (take 10 rows))
            timeOperation ("fixed-at:" <> name) $ \iteration ->
                let version = fromMaybe "1.0.0" (versions !!? (iteration `mod` length versions))
                 in evaluate (length (fixedAt prepared (mkVersion eco version)))
        writeJsonFile output (object ["operations" .= (enumeration : ranges <> fixes), "names" .= names, "cache" .= ("OS cache uncontrolled, opened connection reused within batch" :: Text)])

timeOperation :: Text -> (Int -> IO Int) -> IO Value
timeOperation name action = do
    start <- getMonotonicTimeNSec
    let loop count total = do
            observations <- traverse action [count .. count + 9]
            end <- getMonotonicTimeNSec
            let elapsed = end - start
                nextCount = count + 10
                nextTotal = total + sum observations
            if elapsed < 20000000
                then loop nextCount nextTotal
                else pure (object ["operation" .= name, "elapsedNs" .= elapsed, "iterations" .= nextCount, "observedTotal" .= nextTotal])
    loop (0 :: Int) (0 :: Int)
