-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Test.Contract.PublicationSpec (spec) where

import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import Test.Hspec
import UnliftIO (throwIO)

import Ecluse.Test.Contract.Install (ClientResult (..))
import Ecluse.Test.Contract.Publication

spec :: Spec
spec = describe "publication contract rejects false completion and absence" $ do
    it "rejects a successful client when the exact version is absent" $
        publishesAndInstalls "1.0.0" (driver ExitSuccess ["11.0.0"]) `shouldThrow` anyException
    it "rejects unreadable inventory as proof of refused publication" $
        refusesWithoutPublication
            "1.0.0"
            (driver (ExitFailure 1) []){publicationVersions = throwIO StoreUnreadable}
            `shouldThrow` (== StoreUnreadable)
    it "rejects a refused client that writes to the target" $ do
        versions <- newIORef []
        let broken =
                (driver (ExitFailure 1) [])
                    { publicationVersions = readIORef versions
                    , publicationRun = writeIORef versions ["1.0.0"] >> pure (result (ExitFailure 1))
                    }
        refusesWithoutPublication "1.0.0" broken `shouldThrow` anyException
    it "accepts a refused publication with a readable unchanged target" $
        refusesWithoutPublication "1.0.0" (driver (ExitFailure 1) [])
  where
    result code = ClientResult "fake publisher" code "" ""
    driver code versions =
        PublicationDriver
            { publicationRun = pure (result code)
            , publicationInstall = pure (result ExitSuccess)
            , publicationVersions = pure versions
            , publicationAwait = pure versions
            , publicationSettle = pass
            }

data StoreUnreadable = StoreUnreadable
    deriving stock (Eq, Show)

instance Exception StoreUnreadable
