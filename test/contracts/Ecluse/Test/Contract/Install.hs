-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Real-client installation outcomes and native installation observations.
module Ecluse.Test.Contract.Install (
    ClientResult (..),
    InstallDriver (..),
    installsObserved,
    refusesObserved,
    shouldSucceed,
    shouldFail,
    shouldWarn,
    clientReport,
) where

import System.Exit (ExitCode (ExitSuccess))
import Test.Hspec (Expectation, expectationFailure, shouldBe)

-- | The outcome of one client invocation: what ran, its exit code, and its captured output.
data ClientResult = ClientResult
    { crCommand :: Text
    , crExit :: ExitCode
    , crStdout :: Text
    , crStderr :: Text
    }
    deriving stock (Show)

-- | A fresh client invocation with its independent observation of the installed result.
data InstallDriver observation = InstallDriver
    { installRun :: IO (ClientResult, observation)
    , installExpected :: observation
    }

-- | Require client success and the expected native installation result.
installsObserved :: (Eq observation, Show observation) => InstallDriver observation -> Expectation
installsObserved driver = do
    (result, observed) <- installRun driver
    void (shouldSucceed result)
    observed `shouldBe` installExpected driver

-- | Require client refusal and the expected unchanged installation result.
refusesObserved :: (Eq observation, Show observation) => InstallDriver observation -> Expectation
refusesObserved driver = do
    (result, observed) <- installRun driver
    void (shouldFail result)
    observed `shouldBe` installExpected driver

-- | Fail the assertion with the client's output when the command failed.
shouldSucceed :: (MonadIO m) => ClientResult -> m ClientResult
shouldSucceed res = liftIO $ case crExit res of
    ExitSuccess -> pure res
    _ -> expectationFailure (toString (clientReport res "failed")) >> pure res

-- | Fail the assertion with the client's output when the command unexpectedly succeeded.
shouldFail :: (MonadIO m) => ClientResult -> m ClientResult
shouldFail res = liftIO $ case crExit res of
    ExitSuccess -> expectationFailure (toString (clientReport res "incorrectly succeeded")) >> pure res
    _ -> pure res

-- | Fail the assertion with the client's output unless one line of its stderr is exactly this one.
shouldWarn :: (MonadIO m) => Text -> ClientResult -> m ClientResult
shouldWarn line res = liftIO $ do
    unless (line `elem` lines (crStderr res)) $
        expectationFailure (toString (clientReport res ("wrote no stderr line " <> show line)))
    pure res

-- | The command, outcome, and captured output for a failed client assertion.
clientReport :: ClientResult -> Text -> Text
clientReport res outcome =
    crCommand res
        <> " "
        <> outcome
        <> "!\nSTDOUT:\n"
        <> crStdout res
        <> "\nSTDERR:\n"
        <> crStderr res
