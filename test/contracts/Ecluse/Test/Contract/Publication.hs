-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Authorised publication and refusal without destination effects.
module Ecluse.Test.Contract.Publication (
    PublicationDriver (..),
    publishesAndInstalls,
    refusesWithoutPublication,
) where

import Test.Hspec (Expectation, shouldBe)

import Ecluse.Test.Contract.Install (ClientResult, shouldFail, shouldSucceed)

-- | Publication, fresh installation, and an independent exact destination inventory.
data PublicationDriver = PublicationDriver
    { publicationRun :: IO ClientResult
    , publicationInstall :: IO ClientResult
    , publicationVersions :: IO [Text]
    , publicationAwait :: IO [Text]
    , publicationSettle :: IO ()
    }

-- | An admitted publication reaches its target and supplies a fresh installation.
publishesAndInstalls :: Text -> PublicationDriver -> Expectation
publishesAndInstalls version driver = do
    void (publicationRun driver >>= shouldSucceed)
    versions <- publicationAwait driver
    (version `elem` versions) `shouldBe` True
    void (publicationInstall driver >>= shouldSucceed)

-- | A refused publication leaves the destination unchanged after the write window.
refusesWithoutPublication :: Text -> PublicationDriver -> Expectation
refusesWithoutPublication version driver = do
    before <- publicationVersions driver
    (version `elem` before) `shouldBe` False
    void (publicationRun driver >>= shouldFail)
    publicationSettle driver
    after <- publicationVersions driver
    after `shouldBe` before
    (version `elem` after) `shouldBe` False
