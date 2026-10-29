-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | Closed install-fact reads, bound to an immutable version by the native provider.
module Ecluse.Core.Rules.Install (
    InstallEvidenceMode (..),
    InstallEvidenceRead (..),
    InstallEvidenceResult (..),
    InstallEvidenceProvider (..),
    InstallEvidenceContext (..),
    InstallEvidenceRole (..),
    InstallArtifactRead,
    unreadInstallSignal,
    unavailableInstallArtifactRead,
    installEvidenceMode,
    installEvidenceReads,
    InstallEvidenceBudget (..),
    InstallEvidenceBudgetExceeded (..),
    InstallEvidenceFlightBudget (..),
) where

import Ecluse.Core.Package (Artifact, CodeExecSignal, PackageDetails)
import Ecluse.Core.Rules.Effectful (ReadFault (ReadFault))
import Ecluse.Core.Rules.Types (Inability (EvaluationFailed, InstallSignalUnread), Transience (WontResolve))
import Ecluse.Core.Security (Limits)

-- | Required resolution refuses missing evidence at a reached install-code rule.
data InstallEvidenceMode = ProjectedInstallFacts | RequireInstallResolution
    deriving stock (Eq, Show)

-- | The action captures one admitted artifact. Building it must perform no artifact read.
newtype InstallEvidenceRead = InstallEvidenceRead (IO InstallEvidenceResult)

-- | A successful read must determine a signal. Faults retain their retry classification.
data InstallEvidenceResult
    = InstallDetermined CodeExecSignal
    | InstallUndetermined Inability
    | InstallReadFailed ReadFault
    deriving stock (Eq, Show)

-- | The role selects authentication and whether compact anonymous evidence may be shared.
data InstallEvidenceRole = InstallPublicAdmission | InstallWorkerReadmission | InstallStoreRevocation
    deriving stock (Eq, Show)

-- | Charge buffered bytes before retention. The reader checks this artifact's approved authority.
type InstallArtifactRead = Limits -> Artifact -> (Int -> IO ()) -> IO (Either ReadFault ByteString)

-- | The permanent fault for an artifact that no bound reader may reach.
unreadInstallSignal :: ReadFault
unreadInstallSignal = ReadFault WontResolve EvaluationFailed InstallSignalUnread

-- | Refuse required resolution when no reader is bound, without attempting transport.
unavailableInstallArtifactRead :: InstallArtifactRead
unavailableInstallArtifactRead _ _ _ = pure (Left (ReadFault WontResolve EvaluationFailed InstallSignalUnread))

-- | One operation owns its reader, deadline and two separate absolute memory reservations.
data InstallEvidenceContext = InstallEvidenceContext
    { ieLimits :: Limits
    , ieRole :: InstallEvidenceRole
    , ieReadArtifact :: InstallArtifactRead
    , ieBudget :: InstallEvidenceBudget
    , ieFlightBudget :: Maybe InstallEvidenceFlightBudget
    , ieDiagnostic :: ReadFault -> IO ()
    }

-- | Allocate one bounded operation, then bind each immutable admitted version without reading it.
newtype InstallEvidenceProvider = InstallEvidenceProvider
    { newInstallEvidenceReads :: InstallEvidenceContext -> IO (PackageDetails -> IO (Maybe InstallEvidenceRead))
    }

-- | An absent provider preserves projected facts for ecosystems that do not require resolution.
installEvidenceMode :: Maybe InstallEvidenceProvider -> InstallEvidenceMode
installEvidenceMode = maybe ProjectedInstallFacts (const RequireInstallResolution)

-- | Build the operation's context only when a provider exists. Without one, no version has an evidence read.
installEvidenceReads :: Maybe InstallEvidenceProvider -> IO InstallEvidenceContext -> IO (PackageDetails -> IO (Maybe InstallEvidenceRead))
installEvidenceReads provider context = case provider of
    Nothing -> pure (const (pure Nothing))
    Just evidence -> context >>= newInstallEvidenceReads evidence

-- | Keep one bounded reservation alive through an operation's evidence evaluation.
newtype InstallEvidenceBudget = InstallEvidenceBudget
    { withInstallEvidenceBudget :: forall a. ((Int -> IO ()) -> (Int -> IO ()) -> IO a) -> IO (Maybe a)
    }

-- | The callback exceeded its reserved envelope. Native evidence maps this to structured inability.
data InstallEvidenceBudgetExceeded = InstallEvidenceBudgetExceeded
    deriving stock (Show)

instance Exception InstallEvidenceBudgetExceeded

-- | A shared read charges with the priority of every ticket waiting for its exact key.
newtype InstallEvidenceFlightBudget = InstallEvidenceFlightBudget
    { withinInstallEvidenceFlight :: forall a. ByteString -> (InstallEvidenceBudget -> IO a) -> IO a
    }
