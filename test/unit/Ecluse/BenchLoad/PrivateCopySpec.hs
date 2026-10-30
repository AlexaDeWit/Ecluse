-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Which upstream each ecosystem's private copy serves its cut from, and which copy declares durable state.
module Ecluse.BenchLoad.PrivateCopySpec (spec) where

import Data.ByteString.Lazy qualified as LBS
import Data.Ratio ((%))
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec

import Ecluse.BenchLoad.Cargo (cargoPrivateCopy)
import Ecluse.BenchLoad.Fixture (fetchChecked)
import Ecluse.BenchLoad.Npm (npmPrivateCopy)
import Ecluse.BenchLoad.PrivateCopy (CopyStubs (..), PrivateCopy (pcEcosystem, pcManagedState, pcPackages), shareStubs)
import Ecluse.BenchLoad.PyPI (pypiPrivateCopy)
import Ecluse.BenchLoad.RubyGems (rubyGemsPrivateCopy)
import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems))
import Ecluse.Test.Corpus (cpName)
import Ecluse.Test.Wai (localhost)

spec :: Spec
spec = do
    describe "shareStubs" $ do
        it "serves npm's cut privately and the whole capture publicly" $
            servesCutPrivately npmPrivateCopy "request" "/request"

        it "serves PyPI's cut privately and the whole capture publicly" $
            servesCutPrivately pypiPrivateCopy "requests" "/simple/requests"

    describe "pcManagedState" $
        it "declares durable mirror state for Cargo's private copy alone" $
            [(pcEcosystem copy, isJust (pcManagedState copy)) | copy <- [npmPrivateCopy, pypiPrivateCopy, cargoPrivateCopy, rubyGemsPrivateCopy]]
                `shouldBe` [(Npm, False), (PyPI, False), (Cargo, True), (RubyGems, False)]

-- Another ecosystem's cut refuses the capture, and swapped stubs serve the larger document privately.
servesCutPrivately :: PrivateCopy -> Text -> Text -> Expectation
servesCutPrivately copy name path = do
    stubs <- shareStubs copy{pcPackages = filter ((== name) . cpName) (pcPackages copy)} (5 % 100) 0
    privateBody <- served (csPrivate stubs)
    publicBody <- served (csPublic stubs)
    LBS.length privateBody `shouldSatisfy` (< LBS.length publicBody)
  where
    served app = testWithApplication (pure app) $ \port -> HTTP.responseBody <$> fetchChecked status200 [] (localhost port <> path)
