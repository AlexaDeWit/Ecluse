-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | How a failed credential discovery leaves the env builder: typed, with no request in its text.
module Ecluse.Runtime.Aws.EnvSpec (spec) where

import Amazonka.S3 qualified as S3
import Test.Hspec

import Ecluse.Runtime.Aws.Env (newAwsEnvWith)
import Ecluse.Runtime.Test.Aws (refusedAtClosedPort, withRefusedDiscovery)

spec :: Spec
spec = describe "newAwsEnvWith" $
    it "raises a discovery whose STS call is refused by its transport cause at the dialled host:port, never the request" $
        withRefusedDiscovery $ \discovery ->
            void (newAwsEnvWith discovery Nothing Nothing S3.defaultService) `shouldThrow` refusedAtClosedPort
