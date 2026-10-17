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
spec = do
    describe "publication contract rejects false completion and absence" $ do
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
    describe "the non-upload law rejects a write that reached the destination" $ do
        it "rejects a refused write that still changed the destination" $ do
            facts <- newIORef ["version 1.0.0"]
            let changed = (nonUpload (ExitFailure 1) "refused"){nonUploadDestination = readIORef facts, nonUploadWrite = modifyIORef' facts (<> ["deprecated 1.0.0"]) >> pure (refused "refused")}
            refusesNonUpload changed `shouldThrow` anyException
        it "rejects a write the client reports as accepted" $
            refusesNonUpload (nonUpload ExitSuccess "refused") `shouldThrow` anyException
        it "rejects a refusal that omits the expected text" $
            refusesNonUpload (nonUpload (ExitFailure 1) "another refusal") `shouldThrow` anyException
        it "rejects a release the destination never shows" $
            refusesNonUpload (nonUpload (ExitFailure 1) "refused"){nonUploadDestination = pure []} `shouldThrow` anyException
        it "accepts a named refusal that leaves the released destination unchanged" $
            refusesNonUpload (nonUpload (ExitFailure 1) "refused")
  where
    result code = ClientResult "fake publisher" code "" ""
    refused = ClientResult "fake writer" (ExitFailure 1) ""
    driver code versions =
        PublicationDriver
            { publicationRun = pure (result code)
            , publicationInstall = pure (result ExitSuccess)
            , publicationVersions = pure versions
            , publicationAwait = pure versions
            , publicationSettle = pass
            }
    nonUpload code message =
        NonUploadDriver
            { nonUploadRelease = pure (result ExitSuccess)
            , nonUploadWrite = pure (ClientResult "fake writer" code "" message)
            , nonUploadRefusal = ["refused"]
            , nonUploadDestination = pure ["version 1.0.0"]
            , nonUploadSettle = pass
            }

data StoreUnreadable = StoreUnreadable
    deriving stock (Eq, Show)

instance Exception StoreUnreadable
