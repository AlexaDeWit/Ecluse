-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pin where the harness cgroup runs beside a placed proxy, its restore after the run, and the placements refused.
module Ecluse.BenchLoad.PlacementSpec (spec) where

import Data.Text qualified as T
import System.Directory (createDirectory)
import System.FilePath ((</>))
import Test.Hspec
import UnliftIO.Exception (throwIO)
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.BenchLoad.Placement (harnessCpus, harnessLeaf, readCpuList, withHarnessPlacement)
import Ecluse.BenchLoad.Pod (CpuList, parseCpuList, renderCpuList)

-- The failure a run that never finishes stands for.
data Interrupted = Interrupted
    deriving stock (Eq, Show)

instance Exception Interrupted

spec :: Spec
spec = do
    describe "withHarnessPlacement" $ do
        it "keeps the harness cgroup off the proxy's CPUs during the run, and gives it every CPU back after" $
            withSubtree "\n" $ \base -> do
                withHarnessPlacement (Just base) True (cpus "2-3") (leafCpus base) `shouldReturn` "0-1"
                leafCpus base `shouldReturn` "0-3"
        it "gives every CPU back when the run fails" $
            withSubtree "\n" $ \base -> do
                withHarnessPlacement (Just base) True (cpus "2-3") (throwIO Interrupted) `shouldThrow` (== Interrupted)
                leafCpus base `shouldReturn` "0-3"
        it "leaves an unplaced harness cgroup alone, and gives back the CPUs a killed run's placement kept" $ do
            withSubtree "\n" $ \base -> do
                withHarnessPlacement (Just base) True Nothing (leafCpus base) `shouldReturn` ""
                leafCpus base `shouldReturn` ""
            withSubtree "0-1\n" $ \base ->
                withHarnessPlacement (Just base) True Nothing (leafCpus base) `shouldReturn` "0-3"
        it "refuses a placement with no delegated cgroup, or one the harness could not keep" $ do
            withHarnessPlacement Nothing True (cpus "2-3") pass `shouldThrow` anyException
            withSubtree "\n" $ \base -> do
                withHarnessPlacement (Just base) True (cpus "0-1") pass `shouldThrow` anyException
                leafCpus base `shouldReturn` ""
        it "runs as before with neither a delegated cgroup nor a placement" $
            withHarnessPlacement Nothing True Nothing (pure "ran") `shouldReturn` ("ran" :: Text)

    describe "harnessCpus" $ do
        it "keeps the CPUs the proxy does not take" $ do
            rendered (harnessCpus True <$> every <*> cpus "2-3") `shouldBe` Just (Right "0-1")
            rendered (harnessCpus True <$> every <*> cpus "3") `shouldBe` Just (Right "0-2")
            rendered (harnessCpus False <$> every <*> cpus "0-1") `shouldBe` Just (Right "2-3")
        it "refuses CPUs outside the subtree, every CPU, and oha's CPU while oha is pinned" $
            for_ [(True, "3-4"), (False, "0-3"), (True, "0")] $ \(pinned, taken) ->
                (harnessCpus pinned <$> every <*> cpus taken) `shouldSatisfy` maybe False isLeft

    describe "readCpuList" $
        it "reads a CPU list file, and nothing from an absent or empty one" $
            withSystemTempDirectory "ecluse-placement" $ \dir -> do
                writeFileText (dir </> "cpus") "0-1,3\n"
                writeFileText (dir </> "empty") "\n"
                fmap renderCpuList <$> readCpuList (dir </> "cpus") `shouldReturn` Just "0-1,3"
                readCpuList (dir </> "empty") `shouldReturn` Nothing
                readCpuList (dir </> "absent") `shouldReturn` Nothing

-- A delegated cgroup with CPUs 0-3, whose harness cgroup's cpuset.cpus starts with this content.
withSubtree :: Text -> (FilePath -> IO a) -> IO a
withSubtree leaf body =
    withSystemTempDirectory "ecluse-placement" $ \base -> do
        writeFileText (base </> "cpuset.cpus.effective") "0-3\n"
        createDirectory (base </> harnessLeaf)
        writeFileText (base </> harnessLeaf </> "cpuset.cpus") leaf
        body base

leafCpus :: FilePath -> IO Text
leafCpus base = T.strip . decodeUtf8 <$> readFileBS (base </> harnessLeaf </> "cpuset.cpus")

cpus :: Text -> Maybe CpuList
cpus = rightToMaybe . parseCpuList

every :: Maybe CpuList
every = cpus "0-3"

rendered :: Maybe (Either Text CpuList) -> Maybe (Either Text Text)
rendered = fmap (fmap renderCpuList)
