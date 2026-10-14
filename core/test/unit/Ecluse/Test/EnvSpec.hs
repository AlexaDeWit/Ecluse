-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Process environment restoration, including failures during setup.
module Ecluse.Test.EnvSpec (spec) where

import System.Environment (setEnv, unsetEnv)
import Test.Hspec
import UnliftIO.Async (cancel, withAsync)
import UnliftIO.Exception (bracket, throwIO)

import Ecluse.Test.Env (withEnvOverrides, withEnvVars)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))

-- | Environment changes are process-global, so these cases run serially.
spec :: Spec
spec = sequential $ describe "withEnvOverrides" $ do
    it "restores set and absent keys after success" $ isolated $ do
        withEnvOverrides [(setKey, "changed"), (absentKey, "temporary")] $ do
            lookupEnv setKey `shouldReturn` Just "changed"
            lookupEnv absentKey `shouldReturn` Just "temporary"
        restored
    it "restores keys after an action throws" $ isolated $ do
        withEnvOverrides [(setKey, "changed"), (absentKey, "temporary")] (throwIO (TestContractEscape "action"))
            `shouldThrow` (== TestContractEscape "action")
        restored
    it "restores earlier writes when a later environment assignment fails" $ isolated $ do
        withEnvOverrides [(setKey, "changed"), (absentKey, "temporary"), ("ECLTEST_INVALID=KEY", "invalid")] pass
            `shouldThrow` anyIOException
        restored
    it "restores keys when the action is cancelled" $ isolated $ do
        started <- newEmptyMVar
        blocked <- newEmptyMVar
        withAsync (withEnvOverrides [(setKey, "changed"), (absentKey, "temporary")] (putMVar started () >> (takeMVar blocked :: IO ()))) $ \worker -> do
            takeMVar started
            cancel worker
        restored
    it "applies duplicate settings in order and restores the original value" $ isolated $ do
        withEnvOverrides [(setKey, "first"), (setKey, "last")] $
            lookupEnv setKey `shouldReturn` Just "last"
        restored
    it "clears fixture keys and restores the ambient values after success" $ isolated $ do
        withEnvVars [setKey, absentKey] [(absentKey, "temporary")] $ do
            lookupEnv setKey `shouldReturn` Nothing
            lookupEnv absentKey `shouldReturn` Just "temporary"
        restored
    it "restores cleared keys and entries outside the clear list after failure" $ isolated $ do
        withEnvVars [setKey] [(absentKey, "temporary")] (throwIO (TestContractEscape "fixture"))
            `shouldThrow` (== TestContractEscape "fixture")
        restored

setKey, absentKey :: String
setKey = "ECLTEST_ENV_RESTORE_SET"
absentKey = "ECLTEST_ENV_RESTORE_ABSENT"

isolated :: IO a -> IO a
isolated action = bracket save restore $ \_ -> do
    setEnv setKey "original"
    unsetEnv absentKey
    action
  where
    save =
        traverse
            ( \key -> do
                value <- lookupEnv key
                pure (key, value)
            )
            [setKey, absentKey]
    restore = traverse_ (\(key, value) -> maybe (unsetEnv key) (setEnv key) value)

restored :: Expectation
restored = do
    lookupEnv setKey `shouldReturn` Just "original"
    lookupEnv absentKey `shouldReturn` Nothing
