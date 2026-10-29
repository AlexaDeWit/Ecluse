-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Bounded artifact transport and independent admission for native install evidence.
module Ecluse.Core.Registry.InstallEvidence (
    readInstallArtifact,
    anonymousInstallArtifactRead,
    boundedArtifactLimits,
    installFetchFault,
    newInstallEvidenceBudget,
) where

import Network.HTTP.Client (Request)
import UnliftIO.Exception (throwIO)
import UnliftIO.MVar qualified as MVar

import Ecluse.Core.Deadline (Deadlines)
import Ecluse.Core.Fault (boundedDetail, tfCause, tfDetail, transportRetryable)
import Ecluse.Core.Fault.Http (isRetryableStatusCode)
import Ecluse.Core.Package (Artifact)
import Ecluse.Core.Registry (BodyOutcome (SuccessBody, UnreadStatus), FetchFault (..), UrlFormationError, renderUrlFormationError)
import Ecluse.Core.Registry.Adapter.Capability (AdapterArtifact (artifactByUrl))
import Ecluse.Core.Registry.Exchange (boundedChargedRead, formThen, withSuccessBodyUsing)
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient)
import Ecluse.Core.Registry.Public (ReadControl)
import Ecluse.Core.Rules.Effectful (ReadFault (..))
import Ecluse.Core.Rules.Install (InstallArtifactRead, InstallEvidenceBudget (..), InstallEvidenceBudgetExceeded (..), unreadInstallSignal)
import Ecluse.Core.Rules.Types (Inability (EvaluationFailed, InstallEvidenceFailed), Transience (WillResolve, WontResolve))
import Ecluse.Core.Security (BodyLimit (MirrorArtifactBodyLimit), Limits (maxMetadataBytes, maxMirrorArtifactBytes, progressFloor))
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Server.Admission.Weighted (AdmissionObservers (..), admissionWaitMicros, newWeightedAdmission, withWeightedAdmission)

-- | The caller supplies the approved request, including its role's credential posture.
readInstallArtifact :: Limits -> ExchangeClient -> ReadControl -> Either UrlFormationError Request -> (Int -> IO ()) -> IO (Either ReadFault ByteString)
readInstallArtifact limits client control request charge = do
    result <- formThen FetchUrlUnformable exchange request
    pure $ case result of
        Left fault -> Left (installFetchFault fault)
        Right (SuccessBody _ bytes) -> Right bytes
        Right (UnreadStatus code) -> Left (faultFor (if isRetryableStatusCode code then WillResolve Nothing else WontResolve) ("archive source answered HTTP " <> show code))
  where
    exchange = withSuccessBodyUsing control client (progressFloor limits) (fmap (fmap snd) . boundedChargedRead charge (MirrorArtifactBodyLimit (maxMirrorArtifactBytes limits)))

-- | Read with no credential at the location the caller approved. An artifact without one stays unread.
anonymousInstallArtifactRead :: Limits -> ExchangeClient -> ReadControl -> AdapterArtifact -> (Artifact -> Maybe RegistryUrl) -> InstallArtifactRead
anonymousInstallArtifactRead limits client control adapter locate bounded artifact charge = case locate artifact of
    Nothing -> pure (Left unreadInstallSignal)
    Just url -> readInstallArtifact (boundedArtifactLimits limits bounded) client control (artifactByUrl adapter Nothing (registryUrlText url)) charge

-- | The role's limits with the artifact bound tightened to the evidence operation's own, never loosened.
boundedArtifactLimits :: Limits -> Limits -> Limits
boundedArtifactLimits limits bounded = limits{maxMirrorArtifactBytes = min (maxMirrorArtifactBytes limits) (maxMirrorArtifactBytes bounded)}

-- | Preserve retry advice without involving the advisory breaker or its source reporter.
installFetchFault :: FetchFault -> ReadFault
installFetchFault = \case
    FetchTransport fault -> faultFor (if transportRetryable (tfCause fault) then WillResolve Nothing else WontResolve) (tfDetail fault)
    FetchCooldown retry -> faultFor (WillResolve (Just retry)) "archive source is cooling down"
    FetchBoundExceeded bound -> faultFor WontResolve (show bound)
    FetchUrlUnformable fault -> faultFor WontResolve (renderUrlFormationError fault)

faultFor :: Transience -> Text -> ReadFault
faultFor transience detail = ReadFault transience EvaluationFailed (InstallEvidenceFailed (boundedDetail detail))

-- | Reserve a metadata-sized envelope when maintenance reaches a cold evidence read.
newInstallEvidenceBudget :: Deadlines -> Limits -> IO InstallEvidenceBudget
newInstallEvidenceBudget deadlines limits = do
    let capacity = max 1 (maxMetadataBytes limits)
    admission <- newWeightedAdmission deadlines capacity 8 admissionWaitMicros
    pure $ InstallEvidenceBudget $ \action -> do
        fmap rightToMaybe . withWeightedAdmission observers admission capacity $ do
            regions <- MVar.newMVar (0, 0)
            let pay change = MVar.modifyMVar_ regions $ \previous -> do
                    let next@(bytes, inspection) = change previous
                    -- The charge callback cannot return a refusal value. The provider catches this typed bound.
                    when (toInteger bytes + toInteger inspection > toInteger capacity) (throwIO InstallEvidenceBudgetExceeded)
                    pure next
            action (\bytes -> pay (\(_, inspection) -> (max 0 bytes, inspection))) (\inspection -> pay (\(bytes, _) -> (bytes, max 0 inspection)))
  where
    observers = AdmissionObservers{onQueued = pass, onShed = pass, onInFlightDelta = const pass}
