-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Which upstream each ecosystem's private copy serves its cut from.
module Ecluse.BenchLoad.PrivateCopySpec (spec) where

import Data.ByteString.Lazy qualified as LBS
import Data.Ratio ((%))
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec

import Ecluse.BenchLoad.Fixture (fetchChecked)
import Ecluse.BenchLoad.Npm (npmPrivateCopy)
import Ecluse.BenchLoad.PrivateCopy (CopyStubs (..), PrivateCopy (pcPackages), shareStubs)
import Ecluse.BenchLoad.PyPI (pypiPrivateCopy)
import Ecluse.Test.Corpus (cpName)
import Ecluse.Test.Wai (localhost)

spec :: Spec
spec = describe "shareStubs" $ do
    it "serves npm's cut privately and the whole capture publicly" $
        servesCutPrivately npmPrivateCopy "request" "/request"

    it "serves PyPI's cut privately and the whole capture publicly" $
        servesCutPrivately pypiPrivateCopy "requests" "/simple/requests"

-- Another ecosystem's cut refuses the capture, and swapped stubs serve the larger document privately.
servesCutPrivately :: PrivateCopy -> Text -> Text -> Expectation
servesCutPrivately copy name path = do
    stubs <- shareStubs copy{pcPackages = filter ((== name) . cpName) (pcPackages copy)} (5 % 100) 0
    privateBody <- served (csPrivate stubs)
    publicBody <- served (csPublic stubs)
    LBS.length privateBody `shouldSatisfy` (< LBS.length publicBody)
  where
    served app = testWithApplication (pure app) $ \port -> HTTP.responseBody <$> fetchChecked status200 [] (localhost port <> path)
