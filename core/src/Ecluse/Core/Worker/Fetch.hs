-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Worker.Fetch (
    fetchArtifactBytes,
) where

import Network.HTTP.Client (HttpException, Manager, Request, brRead, responseBody, withResponse)
import UnliftIO.Exception (try)

import Ecluse.Core.Registry.Fault (ResponseBoundExceeded (ResponseBoundExceeded))
import Ecluse.Core.Registry.Npm.Request (artifactRequestByUrl)
import Ecluse.Core.Security (Limits (maxBodyBytes), boundedRead, defaultLimits)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Worker.Types (WorkerM, wrManager)

{- Fetch the artifact bytes from the public upstream at the job's authoritative
URL into memory. The URL arrives as the job's validated 'RegistryUrl' witness, so
the https guarantee is the argument type, not trust in the caller.
Publishing is __publish-by-document__: the npm @PUT \/{pkg}@ carries
the tarball base64-encoded under @_attachments@, so the whole artifact must be in
hand to verify it and assemble the document. This path is therefore
__bounded-buffered__, not streamed -- the bytes are necessarily held -- but the read
is capped (see 'workerArtifactLimits'), so an upstream returning an unbounded body
is refused fail-closed rather than exhausting memory. A network failure is returned
as a transient reason ('Retried' at the call site), not thrown, so a flaky upstream
redelivers rather than killing the iteration. -}
fetchArtifactBytes :: RegistryUrl -> WorkerM (Either Text ByteString)
fetchArtifactBytes url = do
    manager <- asks wrManager
    case artifactRequestByUrl "" Nothing (registryUrlText url) of
        Left urlErr -> pure (Left ("unformable artifact URL: " <> show urlErr))
        Right request ->
            try (liftIO (boundedFetch manager request)) <&> \case
                Left (e :: HttpException) -> Left ("artifact fetch failed: " <> show e)
                Right (Left (ResponseBoundExceeded limitErr)) ->
                    Left ("artifact exceeded the response bound: " <> show limitErr)
                Right (Right bytes) -> Right bytes

{- Open the artifact request and read its body chunk-by-chunk through the bounded
read, returning the whole bytes when within the artifact cap or a typed
'ResponseBoundExceeded' otherwise. A network failure throws (caught by the caller
as a transient reason). The cap bounds the necessarily-buffered tarball so an
unbounded body is refused fail-closed. -}
boundedFetch :: Manager -> Request -> IO (Either ResponseBoundExceeded ByteString)
boundedFetch manager request =
    withResponse request manager $ \response ->
        boundedRead workerArtifactLimits (brRead (responseBody response)) >>= \case
            Right body -> pure (Right body)
            Left limitErr -> pure (Left (ResponseBoundExceeded limitErr))

{- The response-bound budget for an __artifact__ fetch. The metadata-path
'Ecluse.Core.Security.defaultLimits' caps bodies at 12 MiB, which is fine for a packument
but far too small for a real tarball, so the artifact cap is raised to a realistic
ceiling while the other limits (version count, nesting depth) stay at their defaults
(they do not apply to an opaque tarball). A body past this is refused fail-closed
rather than buffered, bounding the worker's memory per in-flight job. -}
workerArtifactLimits :: Limits
workerArtifactLimits = defaultLimits{maxBodyBytes = 512 * 1024 * 1024}
