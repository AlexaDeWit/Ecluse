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
import Network.HTTP.Types (status200)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (localOption)
import Test.Tasty.Bench (Benchmark, RelStDev (RelStDev), bench, bgroup, whnfAppIO)
import Test.Tasty.HUnit (assertBool, assertFailure, (@?=))

import Ecluse.Bench.Corpus (LoadedEntry, benchEvalContext, entryInfo, entryName)
import Ecluse.Core.Cve (CveDb (cveDbLookup), CveLookup (cveAdvisoriesFor, cveCoveredNames))
import Ecluse.Core.Ecosystem (ecosystemName)
import Ecluse.Core.Osv.Schema (EpssRequirement (EpssRequired))
import Ecluse.Core.Package (infoVersions)
import Ecluse.Core.Package.Filter (FilterPlan (fpDecisions, fpSurvivors))
import Ecluse.Core.Rules (RuleDeps)
import Ecluse.Core.Rules.Types (PrecededRule)
import Ecluse.Test.Corpus (cpName)
import Ecluse.Test.Corpus.Advisories (
    AdvisoryInputs (..),
    SyntheticTarget (..),
    allAdvisoryRules,
    compileCorpusAdvisories,
    fillerTargets,
    shippedPolicy,
    syntheticAdvisories,
 )
import Ecluse.Test.EcosystemBench (EcosystemBench (..))
import Ecluse.Test.OsvDb (compileOsvZipDbWithFeedTo, withServedArtifact)
import Ecluse.Test.Rules (filterPlan, inertRuleDeps, isUndecidable)

-- | Keep every ecosystem's artifacts open and served while the caller runs the benchmark tree.
withBenchmarks :: [EcosystemBench] -> ([Benchmark] -> IO a) -> IO a
withBenchmarks ecosystems action =
    withSystemTempDirectory "ecluse-advisory-bench" $ \dir ->
        foldr (\ecosystem continue groups -> withEcosystemGroup dir ecosystem (continue . (groups <>))) action ecosystems []

-- The captures whose many releases make the per-version rule cost show.
measuredPackages :: [Text]
measuredPackages = ["typescript", "react", "@types/node", "numpy"]

-- The measured captures the captured records name.
advisedPackages :: [Text]
advisedPackages = ["react", "numpy"]

-- The capture per ecosystem the generated worst case targets, and its advisory count.
heavyPackages :: [Text]
heavyPackages = ["typescript", "numpy"]

heavyAdvisoryCount :: Int
heavyAdvisoryCount = 200

-- Other packages in the generated database, so each lookup searches a table far larger than its own rows.
fillerPackages :: Int
fillerPackages = 20000

withEcosystemGroup :: FilePath -> EcosystemBench -> ([Benchmark] -> IO a) -> IO a
withEcosystemGroup dir ecosystem use = case filter ((`elem` measuredPackages) . packageName) (ebCorpus ecosystem) of
    [] -> use []
    entries -> do
        let heavy = filter ((`elem` heavyPackages) . packageName) entries
            targets = map heavyTarget heavy <> fillerTargets fillerPackages
        corpus <- compileCorpusAdvisories eco (scratch "corpus")
        synthetic <- syntheticAdvisories eco targets >>= compileSynthetic
        withServedArtifact eco corpus $ \corpusDeps corpusDb -> withServedArtifact eco synthetic $ \syntheticDeps syntheticDb -> do
            checkCorpusServed (cveDbLookup corpusDb) entries
            checkSyntheticServed (cveDbLookup syntheticDb) heavy targets
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
    scratch label = dir </> toString (ecosystemName eco) </> label
    compileSynthetic inputs = compileOsvZipDbWithFeedTo eco EpssRequired (status200, aiEpssFeed inputs) (aiOsvZip inputs) (scratch "synthetic")
    heavyTarget entry = SyntheticTarget{stPackage = packageName entry, stVersions = Map.keys (infoVersions (entryInfo entry)), stAdvisories = heavyAdvisoryCount}

{- An empty captured artifact would pass for a speed-up under the shipped policy, since the
remediation rule then abstains, so setup requires ranges for every advised capture it measures. -}
checkCorpusServed :: CveLookup -> [LoadedEntry] -> IO ()
checkCorpusServed corpus entries = case filter ((`elem` advisedPackages) . packageName) entries of
    [] -> assertFailure "advisory rows: no measured capture is one the captured records name"
    advised -> for_ advised $ \entry -> do
        ranges <- cveAdvisoriesFor corpus (packageName entry)
        assertBool ("advisory rows: the captured artifact serves no range for " <> entryName entry) (not (null ranges))

-- The generated artifact serves each worst-case target's advisories and names exactly the targets.
checkSyntheticServed :: CveLookup -> [LoadedEntry] -> [SyntheticTarget] -> IO ()
checkSyntheticServed synthetic heavy targets = do
    for_ heavy $ \entry -> do
        ranges <- cveAdvisoriesFor synthetic (packageName entry)
        length ranges @?= heavyAdvisoryCount
    covered <- cveCoveredNames synthetic
    assertBool "advisory rows: the generated artifact's package names differ from its targets" (sort covered == sort (map stPackage targets))

packageName :: LoadedEntry -> Text
packageName (package, _, _, _) = cpName package

rows :: String -> RuleDeps -> [PrecededRule] -> [LoadedEntry] -> Benchmark
rows label deps policy entries = bgroup label [bench (entryName entry) (whnfAppIO (survivors deps policy) entry) | entry <- entries]

-- The admitted count. An undecidable version fails the row, which catches an unanswered read
-- under the fail-closed deny rules only.
survivors :: RuleDeps -> [PrecededRule] -> LoadedEntry -> IO Int
survivors deps policy entry = do
    plan <- filterPlan deps benchEvalContext policy (entryInfo entry)
    when (any isUndecidable (fpDecisions plan)) (assertFailure ("advisory rows: " <> entryName entry <> " left a version undecidable"))
    pure (Set.size (fpSurvivors plan))
