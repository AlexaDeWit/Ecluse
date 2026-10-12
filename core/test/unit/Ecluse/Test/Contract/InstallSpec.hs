-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Broken native observations cannot satisfy installation or refusal.
module Ecluse.Test.Contract.InstallSpec (spec) where

import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Ecluse.Test.Contract.Install
import Ecluse.Test.InstalledTree (snapshotTree)

spec :: Spec
spec = describe "installation contract rejects broken observations" $ do
    it "accepts success with the independently expected installation" $
        installsObserved (driver ExitSuccess True True)
    it "rejects a successful client with no installation" $
        installsObserved (driver ExitSuccess False True) `shouldThrow` anyException
    it "rejects a failed client even when files exist" $
        installsObserved (driver (ExitFailure 1) True True) `shouldThrow` anyException
    it "rejects effects left behind by a refused installation" $
        refusesObserved (driver (ExitFailure 1) True False) `shouldThrow` anyException
    it "rejects a successful client with an unexpected installed version" $
        installsObserved (driver ExitSuccess (Just "2.0.0" :: Maybe Text) (Just "1.0.0")) `shouldThrow` anyException
    it "rejects a failed client that leaves an unexpected installed version" $
        refusesObserved (driver (ExitFailure 1) (Just "2.0.0" :: Maybe Text) Nothing) `shouldThrow` anyException
    for_ [("node_modules", "unexpected/package.json", "{\"version\":\"2.0.0\"}"), ("site", "unexpected-2.0.0.dist-info/METADATA", "Version: 2.0.0\n")] $ \(root, relativePath, bytes) ->
        it ("rejects a failed client that leaves unexpected output in " <> root) $
            withSystemTempDirectory "refused-installation" $ \directory -> do
                let path = directory </> root </> relativePath
                createDirectoryIfMissing True (takeDirectory path)
                writeFileText path bytes
                tree <- snapshotTree directory [root]
                tree `shouldSatisfy` (not . null)
                refusesObserved (driver (ExitFailure 1) (Nothing :: Maybe Text, tree) (Nothing, mempty)) `shouldThrow` anyException
    it "accepts refusal without an installation" $
        refusesObserved (driver (ExitFailure 1) False False)
  where
    driver code actual expected =
        InstallDriver
            { installRun = pure (ClientResult "fake client" code "" "", actual)
            , installExpected = expected
            }
