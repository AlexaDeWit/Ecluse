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

import Ecluse.BenchLoad.Error (BenchLoadError (BenchLoadError))
import Ecluse.BenchLoad.Placement (harnessCpus, harnessLeaf, readCpuList, withHarnessPlacement)
import Ecluse.BenchLoad.Pod (CpuList, parseCpuList, renderCpuList)

-- The failure a run that never finishes stands for.
data Interrupted = Interrupted
    deriving stock (Eq, Show)

instance Exception Interrupted

spec :: Spec
spec = do
    describe "withHarnessPlacement" $ do
        it "keeps the harness cgroup off the proxy's CPUs during the run, and empties its list after" $
            withSubtree "\n" $ \base -> do
                withHarnessPlacement (Just base) pinnedOha (cpus "2-3") (leafCpus base) `shouldReturn` "0-1"
                leafFile base `shouldReturn` "\n"
        it "empties the list when the run fails" $
            withSubtree "\n" $ \base -> do
                withHarnessPlacement (Just base) pinnedOha (cpus "2-3") (throwIO Interrupted) `shouldThrow` (== Interrupted)
                leafFile base `shouldReturn` "\n"
        it "leaves an unplaced harness cgroup alone, and empties the list a killed run's placement left" $ do
            withSubtree "\n" $ \base -> do
                withHarnessPlacement (Just base) pinnedOha Nothing (leafCpus base) `shouldReturn` ""
                leafFile base `shouldReturn` "\n"
            withSubtree "0-1\n" $ \base ->
                withHarnessPlacement (Just base) pinnedOha Nothing (leafFile base) `shouldReturn` "\n"
        it "refuses a placement with no delegated cgroup, or one that holds oha's pinned CPU, and leaves the list alone" $ do
            withHarnessPlacement Nothing pinnedOha (cpus "2-3") pass `shouldThrow` failsWith "needs BENCH_LOAD_CGROUP"
            withSubtree "\n" $ \base -> do
                withHarnessPlacement (Just base) pinnedOha (cpus "0-1") pass `shouldThrow` failsWith "holds CPU 0, where BENCH_LOAD_ISOLATE_OHA=1 pins oha"
                leafFile base `shouldReturn` "\n"
        it "runs as before with neither a delegated cgroup nor a placement" $
            withHarnessPlacement Nothing pinnedOha Nothing (pure "ran") `shouldReturn` ("ran" :: Text)

    describe "harnessCpus" $ do
        it "keeps the CPUs the proxy does not take" $ do
            rendered (harnessCpus pinnedOha <$> every <*> cpus "2-3") `shouldBe` Just (Right "0-1")
            rendered (harnessCpus pinnedOha <$> every <*> cpus "3") `shouldBe` Just (Right "0-2")
            rendered (harnessCpus Nothing <$> every <*> cpus "0-1") `shouldBe` Just (Right "2-3")
        it "refuses CPUs outside the subtree, every CPU, and the CPU oha is pinned to" $
            for_ [(pinnedOha, "3-4"), (Nothing, "0-3"), (pinnedOha, "0"), (Just 3, "2-3")] $ \(pinned, taken) ->
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

-- The harness cgroup's cpuset.cpus as written, and as a list.
leafFile, leafCpus :: FilePath -> IO Text
leafFile base = decodeUtf8 <$> readFileBS (base </> harnessLeaf </> "cpuset.cpus")
leafCpus base = T.strip <$> leafFile base

-- oha pinned to CPU 0, as a CI run pins it.
pinnedOha :: Maybe Int
pinnedOha = Just 0

failsWith :: Text -> Selector BenchLoadError
failsWith part (BenchLoadError message) = part `T.isInfixOf` message

cpus :: Text -> Maybe CpuList
cpus = rightToMaybe . parseCpuList

every :: Maybe CpuList
every = cpus "0-3"

rendered :: Maybe (Either Text CpuList) -> Maybe (Either Text Text)
rendered = fmap (fmap renderCpuList)
