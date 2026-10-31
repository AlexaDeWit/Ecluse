-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Buffered mirror artifact downloads under their own byte ceiling.
The worker outcome classifies exchange faults for retry or drop.
-}
module Ecluse.Core.Worker.Fetch (
    fetchArtifactBytesCharged,
) where

import Network.HTTP.Client (Request)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Fault (TransportCause (TransportProtocol), transportFault)
import Ecluse.Core.Registry (BodyOutcome (SuccessBody, UnreadStatus), FetchFault (FetchTransport, FetchUrlUnformable), UrlFormationError)
import Ecluse.Core.Registry.Exchange (boundedChargedRead, formThen, withSuccessBodyUsing)
import Ecluse.Core.Registry.Public (ReadControl (Controlled))
import Ecluse.Core.Security (BodyLimit (MirrorArtifactBodyLimit), Limits (progressFloor), maxMirrorArtifactBytes)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Telemetry.Upstream (PublicReadKind (WorkerArtifact))
import Ecluse.Core.Worker.Types (WorkerM, wrClient, wrPublicControl)

-- | Charge actual buffered source bytes and refuse non-success statuses before integrity verification.
fetchArtifactBytesCharged ::
    (Int -> IO ()) ->
    Limits ->
    (Maybe ClientCredential -> Text -> Either UrlFormationError Request) ->
    RegistryUrl ->
    WorkerM (Either FetchFault ByteString)
fetchArtifactBytesCharged charge limits buildRequest url = do
    client <- asks wrClient
    control <- asks wrPublicControl
    result <-
        liftIO $
            formThen
                FetchUrlUnformable
                (withSuccessBodyUsing (Controlled control WorkerArtifact) client (progressFloor limits) (fmap (fmap snd) . boundedChargedRead charge (MirrorArtifactBodyLimit (maxMirrorArtifactBytes limits))))
                (buildRequest Nothing (registryUrlText url))
    pure $
        result >>= \case
            SuccessBody _ bytes -> Right bytes
            UnreadStatus code _ -> Left (FetchTransport (transportFault TransportProtocol ("artifact source answered HTTP " <> show code)))
