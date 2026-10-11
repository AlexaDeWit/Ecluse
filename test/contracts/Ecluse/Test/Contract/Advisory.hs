-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Advisory publication and last-good retention laws over observable generation evidence.
module Ecluse.Test.Contract.Advisory (
    AdvisoryFixture (..),
    AdvisoryOutcome (..),
    AdvisorySnapshot (..),
    AdvisoryDriver (..),
    advisoryContract,
) where

import Ecluse.Test.Contract.Catalogue (caseLabel)
import Test.Hspec

import Ecluse.Core.Cve (CveDbRejected)
import Ecluse.Core.Cve.Slot (AdvisorySource)
import Ecluse.Core.Cve.Types (DbEtag)

-- | The accepted and refused artifacts are independently authored fixture bytes.
data AdvisoryFixture = AcceptedArtifact | RefusedArtifact
    deriving stock (Eq, Show)

-- | Preserve unexpected outcomes so the contract reports them rather than accepting them.
data AdvisoryOutcome = Accepted DbEtag | Refused DbEtag CveDbRejected | Unexpected Text
    deriving stock (Eq, Show)

-- | Disk and live-generation observations must agree after a failed replacement.
data AdvisorySnapshot = AdvisorySnapshot
    { snapshotBytes :: ByteString
    , snapshotIdentity :: Maybe DbEtag
    , snapshotSource :: Maybe AdvisorySource
    , snapshotInstalledAt :: Maybe Double
    , snapshotRemediation :: Maybe Bool
    , snapshotTemporaryExists :: Bool
    }
    deriving stock (Eq, Show)

-- | Publication returns the producer's identity, independently of the consumer's fetch.
data AdvisoryDriver = AdvisoryDriver
    { publishArtifact :: AdvisoryFixture -> IO DbEtag
    , syncArtifact :: IO AdvisoryOutcome
    , observeArtifact :: IO AdvisorySnapshot
    , acceptedBytes :: ByteString
    }

-- | A rejected replacement preserves bytes, identity, provenance, age, and serving evidence.
advisoryContract :: Text -> CveDbRejected -> SpecWith AdvisoryDriver
advisoryContract contractGroup rejection = describe "advisory generation contract" $
    it (caseLabel (contractGroup <> "/last-good") "preserves the last-good generation after rejecting a replacement") $ \driver -> do
        good <- publishArtifact driver AcceptedArtifact
        syncArtifact driver `shouldReturn` Accepted good
        previousSnapshot <- observeArtifact driver
        snapshotBytes previousSnapshot `shouldBe` acceptedBytes driver
        snapshotIdentity previousSnapshot `shouldBe` Just good
        snapshotSource previousSnapshot `shouldSatisfy` isJust
        snapshotInstalledAt previousSnapshot `shouldSatisfy` isJust
        snapshotRemediation previousSnapshot `shouldBe` Just True
        snapshotTemporaryExists previousSnapshot `shouldBe` False
        bad <- publishArtifact driver RefusedArtifact
        bad `shouldNotBe` good
        syncArtifact driver `shouldReturn` Refused bad rejection
        observeArtifact driver `shouldReturn` previousSnapshot
