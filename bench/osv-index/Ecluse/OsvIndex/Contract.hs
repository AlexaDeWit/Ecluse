-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Independent obligations for the temporary experiment and its native evidence.
module Ecluse.OsvIndex.Contract (
    Case (..),
    Variant (..),
    Mode (..),
    Work (..),
    Trial (..),
    FileIdentity (..),
    Execution (..),
    VariantIdentity (..),
    Manifest (..),
    Binding (..),
    Phase (..),
    Event (..),
    Native (..),
    Operation (..),
    SelectionRole (..),
    Selected (..),
    QueryReport (..),
    Resource (..),
    cases,
    caseName,
    caseEco,
    variantName,
    trialDirectory,
    expectedTrials,
    trialInputs,
    trialCommand,
    expectedOperations,
    exactCoverage,
    queryFaults,
    sequenceFaults,
    bindingFaults,
    productionBase,
    inputInventory,
    parseTrial,
) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import Data.Time (UTCTime)
import System.FilePath ((</>))

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))

-- | Each fixed input has its own warm-up, paired compiles and query batches.
data Case = NpmCorpus | PyPICorpus | NpmFull | PyPIFull
    deriving stock (Eq, Ord, Show, Generic)

instance FromJSON Case
instance ToJSON Case

-- | A retains the candidate index. B removes only its creation statement.
data Variant = A | B
    deriving stock (Eq, Ord, Show, Generic)

instance FromJSON Variant
instance ToJSON Variant

-- | Connection lifetime is part of the measured workload.
data Mode = Reused | Fresh
    deriving stock (Eq, Ord, Show, Generic)

instance FromJSON Mode
instance ToJSON Mode

-- | Warm-ups remain evidence but do not enter compilation summaries.
data Work = Warmup | CompilePair Int | QueryBatch Mode Int
    deriving stock (Eq, Ord, Show, Generic)

instance FromJSON Work
instance ToJSON Work

-- | A trial must occur exactly once in the independent serial plan.
data Trial = Trial {trCase :: Case, trVariant :: Variant, trWork :: Work}
    deriving stock (Eq, Ord, Show, Generic)

instance FromJSON Trial
instance ToJSON Trial

-- | Input coverage is fixed independently of producer output.
cases :: [Case]
cases = [NpmCorpus, PyPICorpus, NpmFull, PyPIFull]

-- | Stable case labels are also artifact-directory names.
caseName :: Case -> FilePath
caseName = \case
    NpmCorpus -> "npm-corpus"
    PyPICorpus -> "pypi-corpus"
    NpmFull -> "npm-full"
    PyPIFull -> "pypi-full"

-- | The case fixes the production compiler and reader ecosystem.
caseEco :: Case -> Ecosystem
caseEco = \case
    NpmCorpus -> Npm
    PyPICorpus -> PyPI
    NpmFull -> Npm
    PyPIFull -> PyPI

modeName :: Mode -> FilePath
modeName = \case
    Reused -> "reused"
    Fresh -> "fresh"

-- | Variant labels identify separate source trees and copied executables.
variantName :: Variant -> FilePath
variantName = show

sampleName :: Work -> Variant -> FilePath
sampleName work variant = label <> "-" <> variantName variant
  where
    label = case work of
        Warmup -> "warm"
        CompilePair n -> show n
        QueryBatch _ n -> show n

-- | Every trial owns a fresh directory under the artifact root.
trialDirectory :: Trial -> FilePath
trialDirectory (Trial input variant work) = case work of
    QueryBatch mode _ -> "queries" </> caseName input </> modeName mode </> sampleName work variant
    _ -> "samples" </> caseName input </> sampleName work variant

compileTrials :: Case -> [Trial]
compileTrials input = [Trial input variant Warmup | variant <- [A, B]] <> [Trial input variant (CompilePair n) | n <- [1 .. 6], variant <- pairOrder n]

pairOrder :: Int -> [Variant]
pairOrder n = if odd n then [A, B] else [B, A]

-- | These obligations do not depend on Task output or reported observations.
expectedTrials :: [Trial]
expectedTrials = concatMap forCase cases
  where
    forCase input = compileTrials input <> [Trial input variant (QueryBatch mode n) | mode <- [Reused, Fresh], n <- [1 .. 30], variant <- pairOrder n]

-- | Input paths are fixed by the trial, never accepted from a native report.
trialInputs :: Trial -> [FilePath]
trialInputs trial@(Trial input variant work) = case work of
    QueryBatch{} -> [trialDirectory (Trial input variant Warmup) </> "db/artifact.db", "readers" </> caseName input </> "selection-A.json"]
    _ -> ["inputs" </> caseName input <> ".zip", "inputs" </> epss trial]
  where
    epss (Trial NpmCorpus _ _) = "npm-corpus-epss.gz"
    epss (Trial PyPICorpus _ _) = "pypi-corpus-epss.gz"
    epss _ = "epss.csv.gz"

-- | Preserve the exact command so a resource capture cannot describe another workload.
trialCommand :: FilePath -> Trial -> [String]
trialCommand root trial@(Trial input variant work) = (root </> "bin" </> variantName variant) : args
  where
    args = case work of
        QueryBatch mode n -> ["reader-batch", root, caseName input, modeName mode, show n, variantName variant]
        _ -> ["compile", root, caseName input, sampleName (trWork trial) variant]

-- | Paths are relative to the artifact root, or the variant root inside source manifests.
data FileIdentity = FileIdentity {fiPath :: FilePath, fiBytes :: Integer, fiSha256 :: Text}
    deriving stock (Eq, Show, Generic)

instance FromJSON FileIdentity
instance ToJSON FileIdentity

-- | A commit and workflow attempt distinguish copied evidence from this run.
data Execution = Execution {exSource :: Text, exRun :: Text, exAttempt :: Text, exArchitecture :: Text}
    deriving stock (Eq, Show, Generic)

instance FromJSON Execution
instance ToJSON Execution

-- | The complete tracked-file inventory binds a variant to its measured executable.
data VariantIdentity = VariantIdentity {viVariant :: Variant, viFiles :: [FileIdentity], viBinary :: FileIdentity, viFileManifestHash :: Text}
    deriving stock (Eq, Show, Generic)

instance FromJSON VariantIdentity
instance ToJSON VariantIdentity

-- | Immutable preparation identities are verified again after execution.
data Manifest = Manifest
    { mfBase :: Text
    , mfExecution :: Execution
    , mfVariants :: [VariantIdentity]
    , mfInputs :: [FileIdentity]
    , mfFlake :: Text
    , mfPatch :: Text
    , mfConditions :: Text
    , mfTracked :: [FilePath]
    , mfCapabilities :: Int
    , mfProcessors :: Int
    , mfSqlite :: [Text]
    , mfGhcrts :: Maybe String
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON Manifest
instance ToJSON Manifest

-- | A native child records actual identities before its operation timer starts.
data Binding = Binding
    { biTrial :: Trial
    , biExecution :: Execution
    , biManifestHash :: Text
    , biExecutable :: FileIdentity
    , biInputs :: [FileIdentity]
    , biArguments :: [String]
    , biCapabilities :: Int
    , biGhcrts :: Maybe String
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON Binding
instance ToJSON Binding

-- | A missing finish event leaves a trial incomplete.
data Phase = Started | Finished
    deriving stock (Eq, Show, Generic)

instance FromJSON Phase
instance ToJSON Phase

-- | Sequence numbers count observed starts, not requested Task positions.
data Event = Event
    {evSequence :: Int, evTrial :: Trial, evPhase :: Phase, evMonotonicNs :: Word64, evUtc :: UTCTime}
    deriving stock (Eq, Show, Generic)

instance FromJSON Event
instance ToJSON Event

-- | Output hashes follow the timed operation and bind its persisted results.
data Native = Native
    { naBinding :: Binding
    , naSequence :: Int
    , naStart :: Event
    , naEnd :: Event
    , naCpuPicoseconds :: Integer
    , naOutputs :: [FileIdentity]
    , naFault :: Maybe Text
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON Native
instance ToJSON Native

-- | Count totals let the audit reject timed calls that return the wrong work.
data Operation = Operation {opName :: Text, opElapsed :: Word64, opIterations :: Int, opTotal :: Int}
    deriving stock (Eq, Show, Generic)

instance FromJSON Operation
instance ToJSON Operation

-- | Roles stay distinct when their selected package names coincide.
data SelectionRole = Absent | Corpus | Median | P95 | Maximum
    deriving stock (Eq, Ord, Show, Generic)

instance FromJSON SelectionRole
instance ToJSON SelectionRole

-- | Fan-out belongs to the persisted rows, including duplicate copies.
data Selected = Selected {slRole :: SelectionRole, slName :: Text, slRows :: Int}
    deriving stock (Eq, Show, Generic)

instance FromJSON Selected
instance ToJSON Selected

-- | The observed connection count distinguishes the two timing modes.
data QueryReport = QueryReport {qrMode :: Mode, qrNames :: [Selected], qrOperations :: [Operation], qrConnections :: Int}
    deriving stock (Eq, Show, Generic)

instance FromJSON QueryReport
instance ToJSON QueryReport

-- | Every role requires both a range and a fixed-version operation.
expectedOperations :: [Selected] -> [Text]
expectedOperations names = "covered-names" : map (("ranges:" <>) . show . slRole) names <> map (("fixed-at:" <>) . show . slRole) names

-- | Equality alone cannot accept duplicate operation identities.
exactCoverage :: (Ord a) => [a] -> [a] -> Bool
exactCoverage expected actual = sort expected == sort actual && length actual == length (ordNub actual)

-- | Require exact ordered operation coverage, timing floors and connection lifetimes.
queryFaults :: Mode -> [Selected] -> QueryReport -> [Text]
queryFaults mode names report =
    [label | (label, ok) <- checks, not ok]
  where
    operations = qrOperations report
    connections = case mode of
        Reused -> 1
        Fresh -> sum (map opIterations operations)
    checks =
        [ ("wrong query mode", qrMode report == mode)
        , ("wrong query names or order", qrNames report == names)
        , ("missing, duplicate or unexpected operation", exactCoverage (expectedOperations names) (map opName operations))
        , ("wrong operation sequence", map opName operations == expectedOperations names)
        , ("invalid query timing or count", all (\op -> opIterations op > 0 && opElapsed op >= 20000000 && opTotal op >= 0) operations)
        , ("wrong connection lifetime", qrConnections report == connections && connections > 0)
        ]

-- | Actual events must cover the serial plan without gaps or overlaps.
sequenceFaults :: [Trial] -> [Event] -> [Text]
sequenceFaults trials events =
    ["missing, duplicate or reordered native events" | keys /= expected]
        <> ["overlapping or backwards native timing" | not (and (zipWith (<=) times (drop 1 times)))]
        <> ["backwards wall-clock event" | not (and (zipWith (<=) clocks (drop 1 clocks)))]
  where
    keys = [(evSequence e, evTrial e, evPhase e) | e <- events]
    expected = [(n, trial, phase) | (n, trial) <- zip [1 ..] trials, phase <- [Started, Finished]]
    times = map evMonotonicNs events
    clocks = map evUtc events

-- | Compare actual native identities with independently reconstructed identities.
bindingFaults :: Binding -> Binding -> [Text]
bindingFaults expected actual = ["native execution identity, command or input differs" | expected /= actual]

-- | The approved production source is fixed for this experiment.
productionBase :: Text
productionBase = "690a4cd522f3146fc04ae42d6abac684a44a422c"

-- | No additional or missing fixture may silently change the experiment.
inputInventory :: [FilePath]
inputInventory =
    map
        ("inputs" </>)
        ["npm-full.zip", "npm-full.zip.headers", "pypi-full.zip", "pypi-full.zip.headers", "epss.csv.gz", "epss.csv.gz.headers", "npm-corpus.zip", "npm-corpus-epss.gz", "pypi-corpus.zip", "pypi-corpus-epss.gz"]

-- | Only commands belonging to the independent plan may produce measurements.
parseTrial :: FilePath -> [String] -> Maybe Trial
parseTrial root arguments = find ((== arguments) . drop 1 . trialCommand root) expectedTrials

-- | These whole-child measurements include identity checks outside native timing.
data Resource = Resource
    { rsCommand :: [String]
    , rsExit :: Int
    , rsWall :: Double
    , rsUser :: Double
    , rsSystem :: Double
    , rsRss :: Double
    , rsStartedNs :: Word64
    , rsEndedNs :: Word64
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON Resource where
    toJSON resource =
        object
            [ "command" .= rsCommand resource
            , "exitCode" .= rsExit resource
            , "wallSeconds" .= rsWall resource
            , "userSeconds" .= rsUser resource
            , "systemSeconds" .= rsSystem resource
            , "peakRssKiB" .= rsRss resource
            , "startedNs" .= rsStartedNs resource
            , "endedNs" .= rsEndedNs resource
            ]
instance FromJSON Resource where
    parseJSON = withObject "resource capture" $ \o -> Resource <$> o .: "command" <*> o .: "exitCode" <*> o .: "wallSeconds" <*> o .: "userSeconds" <*> o .: "systemSeconds" <*> o .: "peakRssKiB" <*> o .: "startedNs" <*> o .: "endedNs"
