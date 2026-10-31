-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Refuse to start the smoke suite when its live registry cases miss their measurement obligations.
module SpecHook (hook) where

import Test.Hspec (Spec, runIO)

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems))
import Ecluse.Core.Registry.CargoSmokeSpec qualified as CargoSmoke
import Ecluse.Core.Registry.NpmSmokeSpec qualified as NpmSmoke
import Ecluse.Core.Registry.PyPISmokeSpec qualified as PyPISmoke
import Ecluse.Core.Registry.RubyGemsSmokeSpec qualified as RubyGemsSmoke
import Ecluse.Test.Contract.Measurement (checkLiveCases)

-- | The check reads labels while Hspec builds the tree, so it runs before any live fetch, even under @--dry-run@.
hook :: Spec -> Spec
hook spec = do
    runIO $
        checkLiveCases
            [ (Npm, NpmSmoke.spec)
            , (PyPI, PyPISmoke.spec)
            , (RubyGems, RubyGemsSmoke.spec)
            , (Cargo, CargoSmoke.spec)
            ]
    spec
