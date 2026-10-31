-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The adapter resolver a caller passes, observed through a whole @ecluse proxy@ boot.
module Ecluse.StartupSpec (spec) where

import System.Exit (ExitCode (ExitFailure))
import Test.Hspec

import Ecluse.CLI (AppCommand (RunService))
import Ecluse.Composition.BootError (BootError (MissingAdapter), renderBootError)
import Ecluse.Composition.Support (captureBoot, publicGateEnv)
import Ecluse.Composition.Types (MirrorRole (ServeAndMirror))
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Startup (runWith)
import Ecluse.Test.Env (withEnvVars)

spec :: Spec
spec =
    describe "runWith" $
        it "binds every mount through the caller's adapter resolver" $
            withEnvVars (map fst publicGateEnv) publicGateEnv (captureBoot (runWith (\_ _ _ -> Nothing) (RunService ServeAndMirror)))
                `shouldReturn` (Left (ExitFailure 2), [renderBootError (MissingAdapter Npm)])
