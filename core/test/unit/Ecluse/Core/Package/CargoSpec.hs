-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Cargo lookup and collision keys remain separate.
module Ecluse.Core.Package.CargoSpec (spec) where

import Test.Hspec

import Ecluse.Core.Package.Cargo (cargoCollisionText, cargoNameKey)

-- | Cargo lookup and collision key examples.
spec :: Spec
spec = do
    it "lowers only ASCII case for lookup" $
        cargoNameKey "Acme_Tools-É" `shouldBe` "acme_tools-É"
    it "folds collision separators without collapsing runs or periods" $
        cargoCollisionText "Acme__Tools..X" `shouldBe` "acme--tools..x"
