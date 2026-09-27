-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The rule phase of one metadata request with an advisory database, per large capture.
Setup compiles the captured records and a generated worst case through Pilot's compiler, and a
slot serves each artifact the way a synced mount reads it.
-}
module Ecluse.Core.AdvisoryRulesBench (withBenchmarks) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time (nominalDay)
import Network.HTTP.Types (status200)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (localOption)
import Test.Tasty.Bench (Benchmark, RelStDev (RelStDev), bench, bgroup, whnfAppIO)
import Test.Tasty.HUnit (assertFailure, (@?=))
import UnliftIO.Exception (bracket)

import Ecluse.Bench.Corpus (LoadedEntry, benchEvalContext, entryInfo, entryName)
import Ecluse.Core.Cve (CveDb (cveDbClose, cveDbLookup), CveLookup (cveAdvisoriesFor), openCveDb)
import Ecluse.Core.Cve.Slot (newCveSlot, swapIn)
import Ecluse.Core.Cve.Types (DbEtag (DbEtag))
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Osv.Schema (EpssRequirement (EpssRequired))
import Ecluse.Core.Package (infoVersions)
import Ecluse.Core.Package.Filter (FilterPlan (fpDecisions, fpSurvivors))
import Ecluse.Core.Rules (RuleDeps)
import Ecluse.Core.Rules.Types (
    DenyIfCveParams (DenyIfCveParams),
    DenyIfEpssParams (DenyIfEpssParams),
    FailureAlignment (FailDeny),
    PrecededRule,
    Rule (AllowIfOlderThan, AllowIfRemediatesCve, DenyIfCve, DenyIfEpss),
 )
import Ecluse.Test.Corpus (cpName)
import Ecluse.Test.Corpus.Advisories (AdvisoryInputs (..), SyntheticTarget (..), corpusAdvisories, syntheticAdvisories)
import Ecluse.Test.EcosystemBench (EcosystemBench (..))
import Ecluse.Test.OsvDb (compileOsvZipDbWithFeedTo)
import Ecluse.Test.Rules (atDefaultPrecedence, filterPlan, inertRuleDeps, isUndecidable, slotRuleDeps)
import Ecluse.Test.Support (expectRight)

-- | Keep every ecosystem's artifacts open and served while the caller runs the benchmark tree.
withBenchmarks :: [EcosystemBench] -> ([Benchmark] -> IO a) -> IO a
withBenchmarks ecosystems action =
    withSystemTempDirectory "ecluse-advisory-bench" $ \dir ->
        foldr (\ecosystem continue groups -> withEcosystemGroup dir ecosystem (continue . (groups <>))) action ecosystems []

-- The captures whose many releases make the per-version rule cost show.
measuredPackages :: [Text]
measuredPackages = ["typescript", "react", "@types/node", "numpy"]

-- The capture per ecosystem the generated worst case targets, and its advisory count.
heavyPackages :: [Text]
heavyPackages = ["typescript", "numpy"]

heavyAdvisoryCount :: Int
heavyAdvisoryCount = 200

-- The shipped policy: the minimum-age quarantine and the remediation fast lane.
shippedPolicy :: [PrecededRule]
shippedPolicy = map atDefaultPrecedence [AllowIfOlderThan (7 * nominalDay), AllowIfRemediatesCve]

-- The shipped policy with both advisory denies, at the thresholds config/default.yaml suggests.
allAdvisoryRules :: [PrecededRule]
allAdvisoryRules = shippedPolicy <> map atDefaultPrecedence [DenyIfCve (DenyIfCveParams 8 FailDeny), DenyIfEpss (DenyIfEpssParams 0.5 FailDeny)]

withEcosystemGroup :: FilePath -> EcosystemBench -> ([Benchmark] -> IO a) -> IO a
withEcosystemGroup dir ecosystem use = case filter ((`elem` measuredPackages) . packageName) (ebCorpus ecosystem) of
    [] -> use []
    entries -> do
        let heavy = filter ((`elem` heavyPackages) . packageName) entries
        corpus <- corpusAdvisories eco >>= compileInto "corpus"
        synthetic <- syntheticAdvisories eco (map heavyTarget heavy) >>= compileInto "synthetic"
        withServed eco corpus $ \corpusDeps _ -> withServed eco synthetic $ \syntheticDeps syntheticDb -> do
            for_ heavy $ \entry -> do
                ranges <- cveAdvisoriesFor (cveDbLookup syntheticDb) (packageName entry)
                length ranges @?= heavyAdvisoryCount
            use
                [ bgroup
                    ("ecosystem: " <> toString (ecosystemName eco))
                    [ bgroup
                        "rules with an advisory database (per package)"
                        [ rows "shipped policy without a database" inertRuleDeps shippedPolicy entries
                        , rows "shipped policy over corpus advisories" corpusDeps shippedPolicy entries
                        , rows "all advisory rules over corpus advisories" corpusDeps allAdvisoryRules entries
                        , localOption (RelStDev (1 / 0)) (rows "all advisory rules over synthetic advisories" syntheticDeps allAdvisoryRules heavy)
                        ]
                    ]
                ]
  where
    eco = ebEcosystem ecosystem
    compileInto label inputs = compileOsvZipDbWithFeedTo eco EpssRequired (status200, aiEpssFeed inputs) (aiOsvZip inputs) (dir </> toString (ecosystemName eco) </> label)
    heavyTarget entry = SyntheticTarget{stPackage = packageName entry, stVersions = Map.keys (infoVersions (entryInfo entry)), stAdvisories = heavyAdvisoryCount}

packageName :: LoadedEntry -> Text
packageName (package, _, _, _) = cpName package

-- Serve the artifact from a fresh slot, closing it once the caller returns.
withServed :: Ecosystem -> FilePath -> (RuleDeps -> CveDb -> IO a) -> IO a
withServed eco path use =
    bracket (openCveDb eco EpssRequired path >>= expectRight) cveDbClose $ \db -> do
        slot <- newCveSlot
        swapIn slot (DbEtag (toText path)) Nothing db
        use (slotRuleDeps slot) db

rows :: String -> RuleDeps -> [PrecededRule] -> [LoadedEntry] -> Benchmark
rows label deps policy entries = bgroup label [bench (entryName entry) (whnfAppIO (survivors deps policy) entry) | entry <- entries]

-- The admitted count, refused when a read went unanswered, since that row would measure an outage.
survivors :: RuleDeps -> [PrecededRule] -> LoadedEntry -> IO Int
survivors deps policy entry = do
    plan <- filterPlan deps benchEvalContext policy (entryInfo entry)
    when (any isUndecidable (fpDecisions plan)) (assertFailure ("advisory rows: " <> entryName entry <> " left a version undecidable"))
    pure (Set.size (fpSurvivors plan))
