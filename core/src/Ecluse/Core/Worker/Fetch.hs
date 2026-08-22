-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Worker.Fetch (
    ArtifactFetchFault (..),
    fetchArtifactBytes,
) where

import Network.HTTP.Client (HttpException, Manager, Request, brRead, responseBody, withResponse)
import UnliftIO.Exception (try)

import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Fault (TransportFault (tfCause))
import Ecluse.Core.Fault.Http (classifyTransport)
import Ecluse.Core.Registry (UrlFormationError, renderUrlFormationError)
import Ecluse.Core.Registry.Fault (ResponseBoundExceeded (ResponseBoundExceeded))
import Ecluse.Core.Security (Limits, authorityLabel, boundedRead)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Worker.Types (WorkerM, wrManager)

{- | Why a mirror-artifact fetch failed, split by whether a redelivery could ever
help. The job's ack decision ('Ecluse.Core.Worker.Job.mirrorArtifact') then follows
from the type, not from re-parsing a reason string.
-}
data ArtifactFetchFault
    = {- | The artifact exceeded the plan-sized byte cap. Deterministic in the
      artifact's own size, so a redelivery re-fetches the same over-cap bytes and
      fails identically: __terminal__, never worth retrying. Carries the detail.
      -}
      ArtifactOverCap Text
    | {- | A transient fetch fault (an unformable URL, a network failure): a
      redelivery may succeed. Carries the detail.
      -}
      ArtifactUnavailable Text
    deriving stock (Eq, Show)

{- Fetch the artifact bytes into memory under the caller's byte cap. The path is
bounded-buffered, not streamed, because npm publishes by document: the whole tarball must be in
hand to verify it and assemble the @_attachments@ body. -}
fetchArtifactBytes ::
    Limits ->
    (Limits -> Manager -> Text -> Maybe Secret -> Text -> Either UrlFormationError Request) ->
    RegistryUrl ->
    WorkerM (Either ArtifactFetchFault ByteString)
fetchArtifactBytes limits buildRequest url = do
    manager <- asks wrManager
    -- The job's URL is absolute and the public artifact fetch is anonymous, so the builder needs no
    -- base and no token.
    case buildRequest limits manager "" Nothing (registryUrlText url) of
        Left urlErr -> pure (Left (ArtifactUnavailable ("unformable artifact URL: " <> renderUrlFormationError urlErr)))
        Right request ->
            try (liftIO (boundedFetch limits manager request)) <&> \case
                -- The client's rendered exception would print the request path, query, and headers
                -- into the log and the job span, so the reason names only the authority and cause.
                Left (e :: HttpException) ->
                    Left
                        ( ArtifactUnavailable
                            ("artifact fetch from " <> authorityLabel (registryUrlText url) <> " failed: " <> show (tfCause (classifyTransport e)))
                        )
                Right (Left (ResponseBoundExceeded limitErr)) ->
                    Left (ArtifactOverCap ("artifact exceeded the response bound: " <> show limitErr))
                Right (Right bytes) -> Right bytes

{- Read the artifact body under the supplied cap, refusing a body past it, fail-closed. A network
failure throws, and the caller catches it as a transient fault. -}
boundedFetch :: Limits -> Manager -> Request -> IO (Either ResponseBoundExceeded ByteString)
boundedFetch limits manager request =
    withResponse request manager $ \response ->
        boundedRead limits (brRead (responseBody response)) >>= \case
            Right body -> pure (Right body)
            Left limitErr -> pure (Left (ResponseBoundExceeded limitErr))
