-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Maintenance.CodeArtifactSpec (spec) where

import Test.Hspec

import Amazonka qualified as AWS
import Amazonka.Auth (fromKeys)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Registry.Maintenance (
    CompletionNotion (CompletesOnCall),
    DeleteCeiling (AtMost),
    RefillPosture (RefillPermitted),
    StoreFacts (..),
    StoreMaintenance (rehearseDelete, storeFacts),
 )
import Ecluse.Runtime.Maintenance.CodeArtifact (maintenanceForEnv)
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (
    CodeArtifactStore (..),
    codeArtifactFormat,
 )

{- | The facts the CodeArtifact handle supplies, read without a call. The handle is built
over an env made from dummy keys, so nothing here discovers an AWS identity or reaches a
network. Every decision behind the effectful fields is covered in
"Ecluse.Maintenance.CodeArtifact.DecideSpec", against amazonka's own response types.
-}
spec :: Spec
spec = describe "the CodeArtifact handle's standing facts" $ maybe noNpmFormat factCases npmStore

noNpmFormat :: Spec
noNpmFormat = it "has a CodeArtifact format for npm" $ expectationFailure "npm resolved to no CodeArtifact format"

factCases :: CodeArtifactStore -> Spec
factCases store = do
    it "names the backend the Dredger's boot line records" $ do
        facts <- factsFor store
        factBackend facts `shouldBe` "codeartifact"

    it "accepts 100 versions per destructive call" $ do
        facts <- factsFor store
        factDeleteCeiling facts `shouldBe` AtMost 100

    it "records that CodeArtifact re-admits a version published again after a delete" $ do
        facts <- factsFor store
        factRefill facts `shouldBe` RefillPermitted

    it "records that the delete is done by the time the call answers" $ do
        facts <- factsFor store
        factCompletion facts `shouldBe` CompletesOnCall

    it "offers no rehearsal, because CodeArtifact has no call that reports one" $ do
        handle <- handleFor store
        isJust (rehearseDelete handle) `shouldBe` False

factsFor :: CodeArtifactStore -> IO StoreFacts
factsFor store = storeFacts <$> handleFor store

-- Dummy static credentials: the handle is held and read, never sent anywhere.
handleFor :: CodeArtifactStore -> IO StoreMaintenance
handleFor store =
    maintenanceForEnv store
        <$> AWS.newEnv (pure . fromKeys (AWS.AccessKey "AKIDtestkey") (AWS.SecretKey "testsecretkey"))

npmStore :: Maybe CodeArtifactStore
npmStore = coordinates <$> codeArtifactFormat Npm
  where
    coordinates format =
        CodeArtifactStore
            { casDomain = "acme"
            , casDomainOwner = "111122223333"
            , casRegion = "eu-west-1"
            , casRepository = "mirror"
            , casFormat = format
            }
