-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Server.PipelineIntegrationSpec (spec) where

import Test.Hspec

import Ecluse.Core.Server.Pipeline.PackumentIntegrationSpec qualified
import Ecluse.Core.Server.Pipeline.SharedIntegrationSpec qualified
import Ecluse.Core.Server.Pipeline.TarballIntegrationSpec qualified

spec :: Spec
spec = do
    Ecluse.Core.Server.Pipeline.PackumentIntegrationSpec.spec
    Ecluse.Core.Server.Pipeline.TarballIntegrationSpec.spec
    Ecluse.Core.Server.Pipeline.SharedIntegrationSpec.spec
