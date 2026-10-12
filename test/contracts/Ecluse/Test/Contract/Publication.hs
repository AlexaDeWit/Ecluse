-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Authorised publication and refusal without destination effects.
module Ecluse.Test.Contract.Publication (
    PublicationDriver (..),
    PublicationFixture (..),
    publicationContract,
    publishesAndInstalls,
    refusesWithoutPublication,
) where

import Test.Hspec (Expectation, SpecWith, it, shouldBe)

import Ecluse.Test.Contract.Catalogue (caseLabel)

import Ecluse.Test.Contract.Install (ClientResult, shouldFail, shouldSucceed)

-- | Publication, fresh installation, and an independent exact destination inventory.
data PublicationDriver = PublicationDriver
    { publicationRun :: IO ClientResult
    , publicationInstall :: IO ClientResult
    , publicationVersions :: IO [Text]
    , publicationAwait :: IO [Text]
    , publicationSettle :: IO ()
    }

-- | Two independent names exercise accepted and refused publication.
data PublicationFixture = PublicationFixture
    { publicationVersion :: Text
    , publicationAdmitted :: PublicationDriver
    , publicationRefused :: PublicationDriver
    }

-- | Publication must install back, and refusal must leave the target unchanged.
publicationContract :: Text -> SpecWith PublicationFixture
publicationContract prefix = do
    it (caseLabel (prefix <> "/publish-install") "publishes an authorised package and installs it back") $ \fixture ->
        publishesAndInstalls (publicationVersion fixture) (publicationAdmitted fixture)
    it (caseLabel (prefix <> "/scope-refusal") "refuses an unauthorised package without a destination write") $ \fixture ->
        refusesWithoutPublication (publicationVersion fixture) (publicationRefused fixture)

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
