-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native reader evidence and independently reconstructed row and rule obligations.
module Ecluse.OsvIndex.Reader (
    ReaderEvent (..),
    ReaderObligation (..),
    VerdictResult (..),
    ProbeKey (..),
    checkReaders,
    readerObligation,
    readerFaults,
    readerBatch,
    measureReader,
    queryObligation,
    queryCountFaults,
) where

import Data.Aeson (FromJSON, ToJSON)
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
import Ecluse.Core.Package (infoVersions, mkPackageName, renderPackageName)
import Ecluse.Core.Rules (VerdictSource (..), readAdvisories, verdictSource)
import Ecluse.Core.Rules.Types
import Ecluse.Core.Version (mkVersion, renderVersion)
import Ecluse.OsvIndex.Artifact (Selection (sePackages), fanoutOf)
import Ecluse.OsvIndex.Contract (Mode (..), Operation (..), QueryReport (..), Selected (..), exactCoverage)
import Ecluse.Test.Contract.Manifest (digest, failRunner, jsonLine, readJsonFile, writeJsonFile)
import Ecluse.Test.EcosystemBench (EcosystemBench (ebCorpus, ebEcosystem), ecosystemBenches)
import Ecluse.Test.OsvDb (withServedArtifact)

-- | The origin keeps bound probes distinct from captured-release probes.
data ProbeKey = ProbeKey Text Text Text Text
    deriving stock (Eq, Ord, Show, Generic)

instance FromJSON ProbeKey
instance ToJSON ProbeKey

-- | Preserve the verdict category, complete reason, refusal alignment and attribution.
data VerdictResult = Granted Text | Refused (Maybe Text) Text | Abstained Text | Unable Text Text
    deriving stock (Eq, Show, Generic)

instance FromJSON VerdictResult
instance ToJSON VerdictResult

-- | Typed events require an explicit completion record after every row and verdict.
data ReaderEvent
    = NamesRead [Text] [Text]
    | PackageRead Text Int Int Text Text Bool
    | RuleRead ProbeKey VerdictResult VerdictResult
    | ReaderFinished Int Int Int
    deriving stock (Eq, Show, Generic)

instance FromJSON ReaderEvent
instance ToJSON ReaderEvent

-- | Expectations come from retained artifacts and captured releases, not report counts.
data ReaderObligation = ReaderObligation
    { roRows :: [(Text, Int, Text)]
    , roVerdicts :: [(ProbeKey, VerdictResult)]
    }
    deriving stock (Eq, Show)

data Probe = Probe {probeKey :: ProbeKey, probeEvidence :: RuleEvidence, probeRule :: Rule}

-- | Rebuild expected packages, row counts and complete rule outcomes from the retained artifact.
readerObligation :: Ecosystem -> FilePath -> [Selected] -> IO ReaderObligation
readerObligation eco db selected = withServedArtifact eco db $ \_ opened -> do
    let lookup = cveDbLookup opened
    fanout <- sort <$> fanoutOf db
    rows <- forM fanout $ \(name, count) -> do
        actual <- cveAdvisoriesFor lookup name
        unless (length actual == count) (failRunner "SQL and native reader row counts differ")
        pure (name, count, rowsDigest actual)
    probes <- probePlan eco lookup selected
    ReaderObligation rows <$> ruleResults lookup probes

-- | Emit complete comparisons with a required terminal count record.
checkReaders :: Ecosystem -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
checkReaders eco left right selection output = do
    selected <- sePackages <$> readJsonFile selection
    withServedArtifact eco left $ \_ a -> withServedArtifact eco right $ \_ b ->
        withBinaryFile output WriteMode $ \handle -> do
            let al = cveDbLookup a
                bl = cveDbLookup b
                emit = BS.hPut handle . jsonLine
            an <- sort <$> cveCoveredNames al
            bn <- sort <$> cveCoveredNames bl
            emit (NamesRead an bn)
            unless (an == bn) (failRunner "covered names differ")
            counts <- forM an $ \name -> do
                ar <- cveAdvisoriesFor al name
                br <- cveAdvisoriesFor bl name
                emit (PackageRead name (length ar) (length br) (rowsDigest ar) (rowsDigest br) (ar == br))
                unless (rowsDigest ar == rowsDigest br) (failRunner ("reader multiset differs for " <> name))
                pure (length ar)
            probes <- probePlan eco al selected
            av <- ruleResults al probes
            bv <- ruleResults bl probes
            unless (map fst av == map fst bv) (failRunner "reader rule probe identities differ")
            for_ (zip av bv) $ \((key, firstResult), (_, secondResult)) -> do
                emit (RuleRead key firstResult secondResult)
                unless (firstResult == secondResult) (failRunner "rule verdict differs")
            emit (ReaderFinished (length an) (sum counts) (length av))

-- | Require exact event coverage and compare outcomes with independently rebuilt expectations.
readerFaults :: ReaderObligation -> [ReaderEvent] -> [Text]
readerFaults obligation events = [label | (label, ok) <- checks, not ok]
  where
    expectedNames = [name | (name, _, _) <- roRows obligation]
    expectedKeys = map fst (roVerdicts obligation)
    names = [(a, b) | NamesRead a b <- events]
    packages = [(name, a, b, ah, bh) | PackageRead name a b ah bh _ <- events]
    rules = [(key, a, b) | RuleRead key a b <- events]
    terminal = [(packagesCount, rowsCount, rulesCount) | ReaderFinished packagesCount rowsCount rulesCount <- events]
    expectedPackages = [(name, count, count, hash, hash) | (name, count, hash) <- roRows obligation]
    expectedRules = [(key, verdict, verdict) | (key, verdict) <- roVerdicts obligation]
    expectedTerminal = [(length expectedNames, sum [count | (_, count, _) <- roRows obligation], length expectedKeys)]
    kinds = map eventKind events
    eventKind = \case
        NamesRead{} -> 0 :: Int
        PackageRead{} -> 1
        RuleRead{} -> 2
        ReaderFinished{} -> 3
    checks =
        [ ("missing or wrong covered-name record", names == [(expectedNames, expectedNames)])
        , ("missing, duplicate or wrong reader package", exactCoverage expectedNames [name | (name, _, _, _, _) <- packages])
        , ("reader row counts or multisets differ", packages == expectedPackages)
        , ("missing, duplicate or unexpected rule probe", exactCoverage expectedKeys [key | (key, _, _) <- rules])
        , ("rule outcome, score, attribution or refusal differs", rules == expectedRules)
        , ("missing or incorrect reader completion counts", terminal == expectedTerminal)
        , ("wrong reader event order", kinds == [0] <> replicate (length expectedNames) 1 <> replicate (length expectedKeys) 2 <> [3])
        ]

rowsDigest :: [AdvisoryRange] -> Text
rowsDigest = digest . jsonLine . sort . map rangeIdentity

rangeIdentity :: AdvisoryRange -> (Text, Maybe Double, Maybe Text, Text, Maybe Double)
rangeIdentity row = (arCveId row, arSeverity row, arIntroduced row, show (arUpperBound row), arEpss row)

boundVersions :: AdvisoryRange -> [Text]
boundVersions row =
    maybeToList (arIntroduced row) <> case arUpperBound row of
        FixedBefore v -> [v]
        LastAffected v -> [v]
        Unbounded -> []

probePlan :: Ecosystem -> CveLookup -> [Selected] -> IO [Probe]
probePlan eco lookup selected = do
    bounds <- fmap concat $ forM selected $ \package -> do
        rows <- cveAdvisoriesFor lookup (slName package)
        let versions = ordNub (["0", "1.0.0", "not-a-version"] <> concatMap boundVersions rows)
        pure [("bound:" <> show (slRole package), identityEvidence (mkPackageName eco Nothing (slName package)) (mkVersion eco v)) | v <- versions]
    captures <- ecosystemBenches
    let releases = [("capture", completeEvidence details) | capture <- captures, ebEcosystem capture == eco, (_, _, info, _) <- ebCorpus capture, details <- Map.elems (infoVersions info)]
        rules = AllowIfRemediatesCve : [DenyIfCve (DenyIfCveParams n FailDeny) | n <- [0, 8, 10]] <> [DenyIfEpss (DenyIfEpssParams n FailDeny) | n <- [0, 0.5, 1]]
    pure [Probe (ProbeKey origin (renderPackageName (evName ev)) (renderVersion (evVersion ev)) (show rule)) ev rule | (origin, ev) <- bounds <> releases, rule <- rules]

ruleResults :: CveLookup -> [Probe] -> IO [(ProbeKey, VerdictResult)]
ruleResults lookup probes = do
    let packages = Map.fromList [(renderPackageName (evName (probeEvidence probe)), evName (probeEvidence probe)) | probe <- probes]
    prepared <- traverse (\package -> readAdvisories package (Just (DbEtag "osv-index-comparison", lookup))) packages
    forM probes $ \probe -> do
        let evidence = probeEvidence probe
        rows <- maybe (failRunner "rule probe package missing") pure (Map.lookup (renderPackageName (evName evidence)) prepared)
        let verdict = case verdictSource (probeRule probe) of
                FromAdvisories _ decide -> decide rows evidence
                FromEvidence decide -> decide (EvalContext (UTCTime (fromGregorian 2026 1 1) 0) Nothing) evidence
        pure (probeKey probe, verdictResult verdict)

verdictResult :: RuleVerdict -> VerdictResult
verdictResult = \case
    Allow reason -> Granted (show (normaliseReason reason))
    Deny etag reason -> Refused (show <$> etag) (show (normaliseReason reason))
    NoDecision reason -> Abstained (show (normaliseReason reason))
    CannotVet alignment inability -> Unable (show alignment) (show inability)

normaliseReason :: Reason -> Reason
normaliseReason = \case
    Remediates ids -> Remediates (ordered ids)
    FixesButStillAffected fixed affected -> FixesButStillAffected (ordered fixed) (ordered affected)
    AffectedBy score threshold ids -> AffectedBy score threshold (ordered ids)
    reason -> reason
  where
    ordered = mkAdvisoryIds . NE.sort . unAdvisoryIds

-- | Each fresh operation opens, accepts, uses and closes its own production reader inside timing.
readerBatch :: Mode -> Ecosystem -> FilePath -> FilePath -> FilePath -> IO ()
readerBatch mode eco db selection output = do
    selected <- sePackages <$> readJsonFile selection
    measureReader mode eco db selected >>= writeJsonFile output

-- | Count actual successful opens while measuring the requested connection lifetime.
measureReader :: Mode -> Ecosystem -> FilePath -> [Selected] -> IO QueryReport
measureReader mode eco db selected = do
    connections <- newIORef (0 :: Int)
    let opened use = withServedArtifact eco db $ \_ reader -> modifyIORef' connections (+ 1) >> use (cveDbLookup reader)
    operations <- case mode of
        Reused -> opened (reusedOperations eco selected)
        Fresh -> freshOperations eco selected opened
    QueryReport mode selected operations <$> readIORef connections

reusedOperations :: Ecosystem -> [Selected] -> CveLookup -> IO [Operation]
reusedOperations eco selected lookup = do
    enumeration <- timeOperation "covered-names" (const (cveCoveredNames lookup >>= evaluate . length))
    ranges <- forM selected $ \package -> timeOperation ("ranges:" <> show (slRole package)) (const (cveAdvisoriesFor lookup (slName package) >>= evaluate . length))
    fixes <- forM selected $ \package -> do
        rows <- cveAdvisoriesFor lookup (slName package)
        let prepared = packageAdvisories eco rows
        timeOperation ("fixed-at:" <> show (slRole package)) (fixedOperation eco prepared rows)
    pure (enumeration : ranges <> fixes)

freshOperations :: Ecosystem -> [Selected] -> ((CveLookup -> IO Int) -> IO Int) -> IO [Operation]
freshOperations eco selected opened = do
    enumeration <- timeOperation "covered-names" (const (opened (cveCoveredNames >=> evaluate . length)))
    ranges <- forM selected $ \package -> timeOperation ("ranges:" <> show (slRole package)) (const (opened (\lookup -> cveAdvisoriesFor lookup (slName package) >>= evaluate . length)))
    fixes <- forM selected $ \package -> timeOperation ("fixed-at:" <> show (slRole package)) $ \iteration -> opened $ \lookup -> do
        rows <- cveAdvisoriesFor lookup (slName package)
        fixedOperation eco (packageAdvisories eco rows) rows iteration
    pure (enumeration : ranges <> fixes)

fixedOperation :: Ecosystem -> PackageAdvisories -> [AdvisoryRange] -> Int -> IO Int
fixedOperation eco prepared rows iteration =
    let versions = fixedVersions rows
        version = fromMaybe "1.0.0" (versions !!? (iteration `mod` length versions))
     in evaluate (length (fixedAt prepared (mkVersion eco version)))

timeOperation :: Text -> (Int -> IO Int) -> IO Operation
timeOperation name action = do
    start <- getMonotonicTimeNSec
    let loop count total = do
            observation <- action count
            end <- getMonotonicTimeNSec
            let !nextCount = count + 1
                !nextTotal = total + observation
            if end - start < 20000000
                then loop nextCount nextTotal
                else pure (Operation name (end - start) nextCount nextTotal)
    loop 0 0

fixedVersions :: [AdvisoryRange] -> [Text]
fixedVersions rows = ordNub ("1.0.0" : concatMap boundVersions (take 10 (sortOn rangeIdentity rows)))

-- | Expected result-count cycles come from retained databases, outside measured batches.
queryObligation :: Ecosystem -> FilePath -> [Selected] -> IO [(Text, [Int])]
queryObligation eco db selected = withServedArtifact eco db $ \_ opened -> do
    let lookup = cveDbLookup opened
    covered <- cveCoveredNames lookup
    perPackage <- forM selected $ \package -> do
        rows <- cveAdvisoriesFor lookup (slName package)
        let prepared = packageAdvisories eco rows
            counts = [length (fixedAt prepared (mkVersion eco version)) | version <- fixedVersions rows]
        pure [("ranges:" <> show (slRole package), [length rows]), ("fixed-at:" <> show (slRole package), counts)]
    pure (("covered-names", [length covered]) : concat perPackage)

-- | Verify observed totals against complete repetitions of each fixed result cycle.
queryCountFaults :: [(Text, [Int])] -> QueryReport -> [Text]
queryCountFaults expected report =
    ["query result count differs: " <> opName operation | operation <- qrOperations report, (Map.lookup (opName operation) cycles >>= (\values -> cycleTotal values (opIterations operation))) /= Just (opTotal operation)]
  where
    cycles = Map.fromList expected
    cycleTotal [] _ = Nothing
    cycleTotal values iterations
        | iterations < 0 = Nothing
        | otherwise =
            let (full, rest) = iterations `divMod` length values
             in Just (full * sum values + sum (take rest values))
