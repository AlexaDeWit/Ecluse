-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | How a failed credential discovery leaves the env builder: typed, with no request in its text.
module Ecluse.Runtime.Aws.Env.InternalSpec (spec) where

import Amazonka.S3 qualified as S3
import Test.Hspec

import Ecluse.Runtime.Aws.Env.Internal (newAwsEnvWith)
import Ecluse.Runtime.Test.Aws (refusedAtClosedPort, refusedDiscovery)

spec :: Spec
spec =
    describe "newAwsEnvWith" $
        it "raises a discovery whose signed STS call is refused by its transport cause at the dialled host:port, never the request" $
            void (newAwsEnvWith refusedDiscovery Nothing Nothing S3.defaultService) `shouldThrow` refusedAtClosedPort
