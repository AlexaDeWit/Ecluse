-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.SecuritySpec (spec) where

import Ecluse.Core.Security (LimitError (..), TarballHostPolicy (..), defaultLimits)
import Test.Hspec

spec :: Spec
spec = do
    showInstancesSpec

{- | The error\/config types derive 'Show' for diagnostics and test output; assert
each renders so the contract is exercised (and not silently dropped).
-}
showInstancesSpec :: Spec
showInstancesSpec = describe "Show instances" $ do
    it "renders LimitError values" $ do
        show (BodyTooLarge 10) `shouldBe` ("BodyTooLarge 10" :: Text)
        show (TooManyVersions 4 3) `shouldBe` ("TooManyVersions 4 3" :: Text)
        show (TooDeeplyNested 3) `shouldBe` ("TooDeeplyNested 3" :: Text)
    it "renders TarballHostPolicy values" $ do
        show SameHostAsPackument `shouldBe` ("SameHostAsPackument" :: Text)
        show AnyAllowlistedHost `shouldBe` ("AnyAllowlistedHost" :: Text)
    it "renders Limits" $
        show defaultLimits
            `shouldBe` ( "Limits {maxBodyBytes = 12582912, maxVersionCount = 100000, maxNestingDepth = 64}" ::
                            Text
                       )
