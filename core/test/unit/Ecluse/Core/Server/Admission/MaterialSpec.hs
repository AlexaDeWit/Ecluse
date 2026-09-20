-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | Material scheduling and release with explicit small weights.
module Ecluse.Core.Server.Admission.MaterialSpec (spec) where

import Test.Hspec
import UnliftIO (cancel, timeout, wait, withAsync)
import UnliftIO.Exception (throwIO, try)

import Ecluse.Core.Server.Admission.Material
import Ecluse.Core.Server.Cache.Store (MaterialReuse (..))
import Ecluse.Test.Support (TestContractEscape (..))

spec :: Spec
spec = do
    describe "material allowance selection" $ do
        it "admits retained work while a second cold request cannot fit" $ do
            admission <- newMaterialAdmissionTuned 5 0 0 allowances
            started <- newEmptyMVar
            release <- newEmptyMVar
            let cold = SelectedMaterial NeedsMaterialisation
            withAsync (withMaterialAdmission admission cold (putMVar started () >> takeMVar release)) $ \holder -> do
                takeMVar started
                withMaterialAdmission admission cold (pure ()) `shouldReturn` Nothing
                withMaterialAdmission admission (SelectedMaterial KnownLocalReuse) (pure ()) `shouldReturn` Just ()
                putMVar release ()
                wait holder `shouldReturn` Just ()

        it "counts only the configured permitted listing legs" $ do
            admission <- newMaterialAdmissionTuned 8 0 0 allowances
            withMaterialAdmission
                admission
                (ListingMaterial 1)
                ( do
                    withMaterialAdmission admission (SelectedMaterial KnownLocalReuse) (pure ()) `shouldReturn` Just ()
                    withMaterialAdmission admission (SelectedMaterial NeedsMaterialisation) (pure ()) `shouldReturn` Nothing
                )
                `shouldReturn` Just ()
            withMaterialAdmission
                admission
                (ListingMaterial 2)
                (withMaterialAdmission admission (SelectedMaterial KnownLocalReuse) (pure ()))
                `shouldReturn` Just Nothing

        it "caps oversized estimates to let a request run alone" $ do
            admission <- newMaterialAdmissionTuned 2 0 0 allowances
            withMaterialAdmission admission (ListingMaterial maxBound) (pure ()) `shouldReturn` Just ()

    describe "release" $ do
        it "returns capacity when metadata or policy throws" $ do
            admission <- newMaterialAdmissionTuned 4 0 0 allowances
            let work = SelectedMaterial NeedsMaterialisation
            outcome <- try (withMaterialAdmission admission work (throwIO (TestContractEscape "policy") :: IO ()))
            outcome `shouldBe` Left (TestContractEscape "policy")
            withMaterialAdmission admission work (pure ()) `shouldReturn` Just ()

        it "returns capacity after an admitted request is cancelled" $ do
            admission <- newMaterialAdmissionTuned 4 0 0 allowances
            started <- newEmptyMVar
            release <- newEmptyMVar
            let work = SelectedMaterial NeedsMaterialisation
            withAsync (withMaterialAdmission admission work (putMVar started () >> takeMVar release)) $ \holder -> do
                takeMVar started
                cancel holder
            timeout 1000000 (withMaterialAdmission admission work (pure ())) `shouldReturn` Just (Just ())

        it "expires a queued request without running its action or leaking its place" $ do
            admission <- newMaterialAdmissionTuned 4 1 1000 allowances
            let work = SelectedMaterial NeedsMaterialisation
            withMaterialAdmission
                admission
                work
                ( do
                    withMaterialAdmission admission work (throwIO (TestContractEscape "ran")) `shouldReturn` (Nothing :: Maybe ())
                    withMaterialAdmission admission work (throwIO (TestContractEscape "ran")) `shouldReturn` (Nothing :: Maybe ())
                )
                `shouldReturn` Just ()
            withMaterialAdmission admission work (pure ()) `shouldReturn` Just ()

allowances :: MaterialAllowances
allowances = MaterialAllowances 4 1 4 2
