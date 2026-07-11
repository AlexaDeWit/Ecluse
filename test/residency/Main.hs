-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | S54's residency gate runs as its own suite (own process, RTS @-T@), so the
one spec is imported explicitly rather than discovered: discovery over the shared
fixture directory would sweep the whole integration tier into this process and
defeat the isolation the measurement depends on.
-}
module Main (main) where

import Ecluse.Server.TarballResidencySpec qualified as TarballResidencySpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec TarballResidencySpec.spec
