-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Buffered mirror artifact downloads under their own byte ceiling.
The worker outcome classifies exchange faults for retry or drop.
-}
module Ecluse.Core.Worker.Fetch (
    fetchArtifactBytes,
) where

import Network.HTTP.Client (Request)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Registry (FetchFault (FetchUrlUnformable), RegistryResponse (responseBody), UrlFormationError)
import Ecluse.Core.Registry.Exchange (boundedFetch, formThen)
import Ecluse.Core.Security (BodyLimit (MirrorArtifactBodyLimit), Limits, maxMirrorArtifactBytes)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Worker.Types (WorkerM, wrManager)

-- | Buffer the whole artifact for verification before publication, under its mirror-artifact cap.
fetchArtifactBytes ::
    Limits ->
    (Maybe ClientCredential -> Text -> Either UrlFormationError Request) ->
    RegistryUrl ->
    WorkerM (Either FetchFault ByteString)
fetchArtifactBytes limits buildRequest url = do
    manager <- asks wrManager
    -- The job's URL is absolute and the public artifact fetch is anonymous, so the builder
    -- names no origin and no token.
    liftIO
        ( formThen
            FetchUrlUnformable
            (fmap (fmap responseBody) . boundedFetch manager (MirrorArtifactBodyLimit (maxMirrorArtifactBytes limits)))
            (buildRequest Nothing (registryUrlText url))
        )
