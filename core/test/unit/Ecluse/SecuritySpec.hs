-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.SecuritySpec (spec) where

import Ecluse.Core.Security (LimitError (..), defaultLimits)
import Test.Hspec

spec :: Spec
spec = do
    showInstancesSpec

-- | Each case asserts a rendering, so a silently dropped 'Show' instance fails the suite.
showInstancesSpec :: Spec
showInstancesSpec = describe "Show instances" $ do
    it "renders LimitError values" $ do
        show (BodyTooLarge 10) `shouldBe` ("BodyTooLarge 10" :: Text)
        show (TooManyVersions 4 3) `shouldBe` ("TooManyVersions 4 3" :: Text)
        show (TooDeeplyNested 3) `shouldBe` ("TooDeeplyNested 3" :: Text)
    it "renders Limits" $
        show defaultLimits
            `shouldBe` ( "Limits {maxBodyBytes = 12582912, maxVersionCount = 100000, maxNestingDepth = 64}" ::
                            Text
                       )
