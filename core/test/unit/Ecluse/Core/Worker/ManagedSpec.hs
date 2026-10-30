-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Worker state reads reject uncertainty and preserve original paired source data.
module Ecluse.Core.Worker.ManagedSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Mirror.State
import Ecluse.Core.Mirror.State.Internal (StateRevision (..))
import Ecluse.Core.Registry.Metadata (VersionDoc (vdDetails))
import Ecluse.Core.Registry.MirrorUpload (inventoryDetails)
import Ecluse.Core.Registry.PyPI.Mirror (pypiStateFormat)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Worker.Managed
import Ecluse.Core.Worker.Types (WorkerMirrorState (..))
import Ecluse.Test.Mirror.State
import Ecluse.Test.Support (expectRight)
import Ecluse.Worker.Support.Managed (ManagedFixture (mfBinding), managedFixture, unmeteredStateBudget)

spec :: Spec
spec = describe "managed worker records" $ do
    it "distinguishes absent records from unexplained unchanged responses" $ do
        fixture <- managedFixture "https://source.test" False
        let binding = mfBinding fixture
            revision = StateRevision (wmsTarget binding) mirrorPackage "one"
            store = (wmsStore binding){msReadProject = \_ _ _ -> pure (Right (StateUnchanged revision))}
        readMirrorRecord unmeteredStateBudget binding mirrorPackage `shouldReturn` Right Nothing
        readMirrorRecord unmeteredStateBudget binding{wmsStore = store} mirrorPackage `shouldReturn` Left StateRevisionMismatch

    it "refuses a record bound to another target" $ do
        fixture <- managedFixture "https://source.test" False
        inventory <- mirrorSample [1] False "https://source.test/" "original"
        record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits (stateTarget "another target") inventory Nothing)
        let binding = mfBinding fixture
            revision = StateRevision (wmsTarget binding) mirrorPackage "one"
            store = (wmsStore binding){msReadProject = \_ _ _ -> pure (Right (StateLoaded revision record))}
        readMirrorRecord unmeteredStateBudget binding{wmsStore = store} mirrorPackage `shouldReturn` Left StateIdentityMismatch

    it "keeps original selected source fields paired through the durable fragment" $ do
        fixture <- managedFixture "https://source.test" False
        inventory <- mirrorSample [1, 2] False "https://source.test/" "original"
        record <- persistInventory unmeteredStateBudget pypiStateFormat defaultLimits (Just (mfBinding fixture)) inventory Nothing >>= expectRight
        case recordVersion mirrorVersion record of
            Nothing -> expectationFailure "record lost its requested version"
            Just version -> do
                let fragment :| _ = versionFragments version
                vdDetails (fragmentDocument fragment) `shouldBe` inventoryDetails inventory
                verifyCompanions unmeteredStateBudget (pure ()) (Just (mfBinding fixture)) version `shouldReturn` Right ()

    it "keeps transient inventories monotonic without durable storage" $ do
        initialInventory <- mirrorSample [1] False "https://source.test/" "first"
        two <- mirrorSample [2] False "https://source.test/" "second"
        record <- persistInventory unmeteredStateBudget pypiStateFormat defaultLimits Nothing initialInventory Nothing >>= expectRight
        updated <- persistInventory unmeteredStateBudget pypiStateFormat defaultLimits Nothing two (Just record) >>= expectRight
        fmap (Map.keys . versionFiles) (recordVersion mirrorVersion updated) `shouldBe` Just (map mirrorFilename [1, 2])

    it "refuses pending companion declarations without an object proof" $ do
        inventory <- mirrorSample [1] True "https://source.test/" "pending"
        record <- persistInventory unmeteredStateBudget pypiStateFormat defaultLimits Nothing inventory Nothing >>= expectRight
        case recordVersion mirrorVersion record of
            Nothing -> expectationFailure "record lost its requested version"
            Just version -> verifyCompanions unmeteredStateBudget (pure ()) Nothing version `shouldReturn` Left StateMissingCompanion
