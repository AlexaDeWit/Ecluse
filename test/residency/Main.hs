-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The residency gate runs as its own suite (own process, RTS @-T@), so this module
imports the one spec explicitly rather than discovering it. Discovery over the shared
fixture directory would sweep the whole integration tier into this process, and defeat
the isolation the measurement depends on.
-}
module Main (main) where

import Ecluse.Core.Server.Pipeline.TarballResidencySpec qualified as TarballResidencySpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec TarballResidencySpec.spec
