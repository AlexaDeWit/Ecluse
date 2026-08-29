-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The serve half of the shared admission projection. The gate turns one 'ArtifactAdmission'
into a serve outcome, and these cases pin the status each outcome reaches the client as.
-}
module Ecluse.Server.Pipeline.TarballSpec (spec) where

import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageDetails, mkPackageName)
import Ecluse.Core.Package.Admission (
    ArtifactAdmission (
        AdmissionBelowFloor,
        AdmissionDenied,
        AdmissionFileAbsent,
        AdmissionIntegrityMissing,
        AdmissionUndecidable
    ),
 )
import Ecluse.Core.Rules.Types (
    Decision (Blocked, Undecidable),
    Transience (WillResolve, WontResolve),
 )
import Ecluse.Core.Server.Pipeline.Tarball (
    PublicArtifactGate (Admitted, Refused),
    artifactOutcomeStatus,
    publicArtifactGate,
 )
import Ecluse.Core.Server.Response (
    ArtifactStatus (Forbidden, NotFound, ServerError, Unavailable'),
 )
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Package (sampleDetails)

-- The version snapshot the gate reasons over. Each case supplies its own verdict, so only the
-- snapshot's validity matters.
details :: PackageDetails
details = sampleDetails (mkPackageName Npm Nothing "thing") (mkVersion Npm "1.0.0")

-- The status a gated verdict renders, or 'Nothing' where the gate admitted it.
statusOf :: ArtifactAdmission -> Maybe ArtifactStatus
statusOf admission = case publicArtifactGate details admission of
    Admitted _ -> Nothing
    Refused decision -> Just (artifactOutcomeStatus decision)

spec :: Spec
spec = describe "publicArtifactGate -- the shared admission verdict on the serve surface" $ do
    it "renders an inability the evaluator expects to clear as a 503" $
        -- The transience comes from the shared projection, the one the worker reads to
        -- redeliver the job.
        statusOf (AdmissionUndecidable (Undecidable (WillResolve Nothing) "advisory source down"))
            `shouldBe` Just (Unavailable' Nothing)

    it "renders an inability no retry can clear as a 500, never as a forwarded 404" $
        -- The 404 belongs to a version the upstream does not have. An internal fault that
        -- reported one would tell a client the version does not exist.
        statusOf (AdmissionUndecidable (Undecidable WontResolve "an internal fault"))
            `shouldBe` Just ServerError

    it "renders a version whose admitted file the upstream no longer carries as a 404" $
        -- The forwarded miss, and the one refusal the 404 override covers.
        statusOf AdmissionFileAbsent `shouldBe` Just NotFound

    it "renders a policy denial as a 403" $
        statusOf (AdmissionDenied (Blocked "test-deny" "denied by current policy")) `shouldBe` Just Forbidden

    it "renders an artifact the integrity floor refuses as a 403" $ do
        statusOf AdmissionBelowFloor `shouldBe` Just Forbidden
        statusOf AdmissionIntegrityMissing `shouldBe` Just Forbidden
