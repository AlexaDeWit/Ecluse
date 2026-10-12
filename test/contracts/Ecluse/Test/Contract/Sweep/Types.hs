-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE RankNTypes #-}

-- | Maintenance operations and observations independent of the package protocol and process host.
module Ecluse.Test.Contract.Sweep.Types (
    PackageFixture (..),
    StoreOps (..),
    ClientOps (..),
    SweepOps (..),
    SweepPolicy (..),
    SweepMode (..),
    SweepRun (..),
    RecoveryOps (..),
    RolloutOps (..),
    AdvisoryOps (..),
    IdentityFixture (..),
    RecoveryFixture (..),
    RolloutFixture (..),
    AdvisoryFixture (..),
    servedBytes,
) where

import Data.ByteString.Lazy qualified as LBS
import Ecluse.Test.Contract.Install (ClientResult)
import System.Exit (ExitCode)

-- | Identities come from the provider's independent package corpus.
data PackageFixture = PackageFixture
    { packageName :: Text
    , packageVersion :: Text
    , packageVersions :: [Text]
    }

-- | Direct observations must distinguish missing data from an unreadable store.
data StoreOps store = StoreOps
    { storeSnapshot :: store -> IO (Map Text [Text])
    , storeListing :: store -> IO [Text]
    , storeNamesUnder :: store -> Text -> IO [Text]
    , storeVersions :: store -> Text -> IO [Text]
    , storeHasVersion :: store -> Text -> Text -> IO Bool
    , storeAwaitVersions :: store -> Text -> [Text] -> IO [Text]
    , storeArtifact :: store -> Text -> Text -> IO (Int, Int64)
    , storeArtifactBytes :: store -> Text -> Text -> IO (Int, LBS.ByteString)
    , storeAwaitListed :: store -> Text -> IO (Maybe Text)
    }

-- | Project acquisition isolates each fresh install from earlier client state.
data ClientOps store project = ClientOps
    { clientInstall :: store -> Text -> Maybe Text -> IO ClientResult
    , clientWithProject :: forall a. store -> (project -> IO a) -> IO a
    , clientInstallIn :: project -> Text -> Maybe Text -> IO ClientResult
    , clientInstalledVersion :: project -> Text -> IO (Maybe Text)
    , clientPublish :: store -> Text -> Text -> IO ClientResult
    , clientArtifact :: store -> Text -> Text -> IO (Int, LBS.ByteString)
    , clientMetadataStatus :: store -> Text -> IO Int
    }

-- | Policies name the authority to evaluate, without encoding native configuration.
data SweepPolicy
    = FullIdentity Text
    | AdmissionOnly Text
    | IdentityPolicy [Text]
    | PermissivePolicy
    | AdvisoryPolicy
    deriving stock (Eq, Show)

-- | Consent refusal and preview remain separate from an authorised write.
data SweepMode = Apply | Preview | WithoutConsent
    deriving stock (Eq, Show)

-- | Audit events preserve the role's reported generation without pinning its value.
data SweepRun = SweepRun
    { sweepExit :: ExitCode
    , sweepOutput :: Text
    , sweepAudit :: [Text]
    , sweepEvents :: [Text]
    }
    deriving stock (Eq, Show)

-- | One completed role process and its bounded background-write observation window.
data SweepOps plane = SweepOps
    { runSweep :: plane -> SweepMode -> SweepPolicy -> IO SweepRun
    , awaitWrites :: IO ()
    }

-- | Fault and policy changes preserve the same underlying pair of stores.
data RecoveryOps plane store = RecoveryOps
    { withRelaxedClient :: forall a. plane -> (store -> IO a) -> IO a
    , withSourceWithheld :: forall a. plane -> Text -> Text -> IO a -> IO a
    , withDeletesRefused :: forall a. plane -> IO a -> IO a
    , directPublisher :: store -> IO store
    , refusedDeleteTarget :: Text
    , refusedDeleteDiagnostic :: Text
    }

-- | Independently scheduled roles expose publication evidence through the worker's own log.
data RolloutOps plane store = RolloutOps
    { rolloutQueue :: plane -> IO Text
    , withServingRole :: forall a. plane -> Text -> SweepPolicy -> (store -> IO a) -> IO a
    , withWorkerRole :: forall a. plane -> Text -> (String -> IO a) -> IO a
    , awaitPublished :: String -> Text -> IO Bool
    , wasPublished :: String -> Text -> IO Bool
    }

-- | Generation publication returns process evidence and waits for the proxy's observed swap.
data AdvisoryOps plane store = AdvisoryOps
    { publishNextGeneration :: plane -> IO SweepRun
    , awaitGenerationSwap :: store -> IO Bool
    }

-- | Expected fixture contents and provider-specific diagnostic identifiers.
data IdentityFixture = IdentityFixture
    { identityCasePrefix :: Text
    , identityDenied :: PackageFixture
    , identityPreview :: PackageFixture
    , identitySeeded :: Map Text [Text]
    , identityFirstPartyName :: Text
    , identityFirstPartyVersions :: [Text]
    , identityPublishVersion :: Text
    , identityCacheOnlyVersion :: Text
    , identityPreviewVersion :: Text
    , identityListingPrefix :: Text
    , identityAbsentPrefix :: Text
    , identityConsentDiagnostic :: Text
    , identityConsentEvents :: [Text]
    , identityMirrorDiagnostic :: Text
    , identityCacheDiagnostic :: Text
    }

-- | Recovery uses distinct identities so each case retains its independent initial state.
data RecoveryFixture = RecoveryFixture
    { recoveryCasePrefix :: Text
    , recoverySibling :: PackageFixture
    , recoveryRefused :: PackageFixture
    , recoveryReadmitted :: PackageFixture
    , recoveryMissing :: PackageFixture
    , recoveryLate :: PackageFixture
    , recoveryDeniedVersion :: Text
    }

-- | Role divergence includes an unrelated retained release and a protected first-party release.
data RolloutFixture = RolloutFixture
    { rolloutCasePrefix :: Text
    , rolloutTarget :: PackageFixture
    , rolloutUnaffected :: PackageFixture
    , rolloutRestored :: PackageFixture
    , rolloutLost :: PackageFixture
    , rolloutSentinel :: PackageFixture
    , rolloutFirstPartyName :: Text
    , rolloutFirstPartyVersion :: Text
    }

-- | Advisory identities and expected audit facts come from the independent advisory corpus.
data AdvisoryFixture = AdvisoryFixture
    { advisoryCasePrefix :: Text
    , advisoryRevoked :: PackageFixture
    , advisoryUnaffected :: PackageFixture
    , advisoryVulnerable :: Text
    , advisoryFixed :: Text
    , advisoryIdentifier :: Text
    }

-- | A successful empty response does not establish retained package bytes.
servedBytes :: (Int, Int64) -> Bool
servedBytes (status, size) = status == 200 && size > 0
