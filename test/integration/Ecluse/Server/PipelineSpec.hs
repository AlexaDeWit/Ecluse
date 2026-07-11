-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Server.PipelineSpec (spec) where

import Test.Hspec

import Ecluse.Server.Pipeline.PackumentSpec qualified
import Ecluse.Server.Pipeline.SharedSpec qualified
import Ecluse.Server.Pipeline.TarballSpec qualified

spec :: Spec
spec = do
    Ecluse.Server.Pipeline.PackumentSpec.spec
    Ecluse.Server.Pipeline.TarballSpec.spec
    Ecluse.Server.Pipeline.SharedSpec.spec
