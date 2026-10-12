-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Buffered mirror artifact downloads under their own byte ceiling.
The worker outcome classifies exchange faults for retry or drop.
-}
module Ecluse.Core.Worker.Fetch (
    fetchArtifactBytes,
    fetchArtifactBytesCharged,
) where

import Network.HTTP.Client (Request)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Fault (TransportCause (TransportProtocol), transportFault)
import Ecluse.Core.Registry (BodyOutcome (SuccessBody, UnreadStatus), FetchFault (FetchTransport, FetchUrlUnformable), UrlFormationError)
import Ecluse.Core.Registry.Exchange (boundedChargedRead, formThen, withSuccessBody)
import Ecluse.Core.Security (BodyLimit (MirrorArtifactBodyLimit), Limits (progressFloor), maxMirrorArtifactBytes)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Worker.Types (WorkerM, wrManager)

-- | Buffer the whole artifact for verification before publication, under its mirror-artifact cap.
fetchArtifactBytes ::
    Limits ->
    (Maybe ClientCredential -> Text -> Either UrlFormationError Request) ->
    RegistryUrl ->
    WorkerM (Either FetchFault ByteString)
fetchArtifactBytes = fetchArtifactBytesCharged (const (pure ()))

-- | Charge actual buffered source bytes and refuse non-success statuses before integrity verification.
fetchArtifactBytesCharged ::
    (Int -> IO ()) ->
    Limits ->
    (Maybe ClientCredential -> Text -> Either UrlFormationError Request) ->
    RegistryUrl ->
    WorkerM (Either FetchFault ByteString)
fetchArtifactBytesCharged charge limits buildRequest url = do
    manager <- asks wrManager
    result <-
        liftIO $
            formThen
                FetchUrlUnformable
                (withSuccessBody manager (progressFloor limits) (fmap (fmap snd) . boundedChargedRead charge (MirrorArtifactBodyLimit (maxMirrorArtifactBytes limits))))
                (buildRequest Nothing (registryUrlText url))
    pure $
        result >>= \case
            SuccessBody _ bytes -> Right bytes
            UnreadStatus code -> Left (FetchTransport (transportFault TransportProtocol ("artifact source answered HTTP " <> show code)))
