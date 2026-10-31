-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Authorised publication and refusal without destination effects.
module Ecluse.Test.Contract.Publication (
    PublicationDriver (..),
    NonUploadDriver (..),
    PublicationFixture (..),
    publicationContract,
) where

import Data.Text qualified as T
import Test.Hspec (Expectation, SpecWith, expectationFailure, it, shouldBe, shouldNotBe)

import Ecluse.Test.Contract.Catalogue (caseLabel)

import Ecluse.Test.Contract.Install (ClientResult (crStderr, crStdout), RefusalDriver (RefusalDriver), clientReport, refusesWithoutEffects, shouldFail, shouldSucceed)

-- | Publication, fresh installation, and an independent exact destination inventory.
data PublicationDriver = PublicationDriver
    { publicationRun :: IO ClientResult
    , publicationInstall :: IO ClientResult
    , publicationVersions :: IO [Text]
    , publicationAwait :: IO [Text]
    , publicationSettle :: IO ()
    }

-- | A native write that is not a new-release upload, aimed at a release the driver publishes first.
data NonUploadDriver = NonUploadDriver
    { nonUploadRelease :: IO ClientResult
    -- ^ Publish the release and return once the destination shows it.
    , nonUploadWrite :: IO ClientResult
    , nonUploadRefusal :: [Text]
    -- ^ Every text the refused write's output must carry.
    , nonUploadDestination :: IO [Text]
    -- ^ The destination's facts, read independently of the proxy.
    , nonUploadSettle :: IO ()
    }

-- | Independent names exercise accepted and refused publication, and a write that is not a release.
data PublicationFixture = PublicationFixture
    { publicationVersion :: Text
    , publicationAdmitted :: PublicationDriver
    , publicationRefused :: PublicationDriver
    , publicationNonUpload :: NonUploadDriver
    }

-- | Publication must install back, and every refusal must leave the target unchanged.
publicationContract :: Text -> SpecWith PublicationFixture
publicationContract prefix = do
    it (caseLabel (prefix <> "/publish-install") "publishes an authorised package and installs it back") $ \fixture ->
        publishesAndInstalls (publicationVersion fixture) (publicationAdmitted fixture)
    it (caseLabel (prefix <> "/scope-refusal") "refuses an unauthorised package without a destination write") $ \fixture ->
        refusesWithoutPublication (publicationVersion fixture) (publicationRefused fixture)
    it (caseLabel (prefix <> "/non-upload-refusal") "refuses a write that is not a new-release upload without a destination effect") $ \fixture ->
        refusesNonUpload (publicationNonUpload fixture)

-- An admitted publication reaches its target and supplies a fresh installation.
publishesAndInstalls :: Text -> PublicationDriver -> Expectation
publishesAndInstalls version driver = do
    void (publicationRun driver >>= shouldSucceed)
    versions <- publicationAwait driver
    (version `elem` versions) `shouldBe` True
    void (publicationInstall driver >>= shouldSucceed)

-- A refused publication leaves the destination unchanged after the write window.
refusesWithoutPublication :: Text -> PublicationDriver -> Expectation
refusesWithoutPublication version driver = do
    before <- publicationVersions driver
    (version `elem` before) `shouldBe` False
    void (publicationRun driver >>= shouldFail)
    publicationSettle driver
    after <- publicationVersions driver
    after `shouldBe` before
    (version `elem` after) `shouldBe` False

-- The refused write names its refusal, and the destination keeps every fact the release left.
refusesNonUpload :: NonUploadDriver -> Expectation
refusesNonUpload driver = do
    void (nonUploadRelease driver >>= shouldSucceed)
    released <- nonUploadDestination driver
    released `shouldNotBe` []
    refusesWithoutEffects released (RefusalDriver (nonUploadWrite driver >>= namesRefusal) (nonUploadDestination driver) (nonUploadSettle driver))
  where
    namesRefusal result = do
        for_ (nonUploadRefusal driver) $ \text ->
            unless (text `T.isInfixOf` (crStdout result <> crStderr result)) $
                expectationFailure (toString (clientReport result ("omitted the refusal " <> show text)))
        pure result
