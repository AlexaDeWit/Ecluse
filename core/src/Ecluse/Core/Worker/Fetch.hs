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

{- Fetch the artifact bytes from the public upstream at the job's authoritative URL
into memory, under the plan-sized artifact byte cap. The URL arrives as the job's
validated 'RegistryUrl' witness, so the https guarantee is the argument type, not
trust in the caller. The request builder is the job ecosystem's own formation, passed
in from the re-evaluation bundle that admitted the job
('Ecluse.Core.Worker.Types.wpBuildArtifactRequest'). Publishing is
__publish-by-document__. The npm @PUT \/{pkg}@ carries the tarball base64-encoded
under @_attachments@, so the whole artifact must be in hand to verify it and assemble
the document. This path is therefore __bounded-buffered__, not streamed, because it
necessarily holds the bytes. The caller's 'Limits' caps the read, and the composition
root sizes that cap from the memory plan's mirror-artifact tenant, in
@Ecluse.Composition.MemoryPlan@. The read refuses a body past the cap, fail-closed,
rather than exhausting the heap the plan partitions. An over-cap body is an
'ArtifactOverCap', terminal at the call site. A network or URL failure is an
'ArtifactUnavailable', transient, so a flaky upstream redelivers rather than killing
the iteration. Neither reason renders the artifact URL. The location comes from
upstream and the reason reaches a log line and a span. The reason carries only the
authority and the bounded transport cause. -}
fetchArtifactBytes ::
    Limits ->
    (Limits -> Manager -> Text -> Maybe Secret -> Text -> Either UrlFormationError Request) ->
    RegistryUrl ->
    WorkerM (Either ArtifactFetchFault ByteString)
fetchArtifactBytes limits buildRequest url = do
    manager <- asks wrManager
    -- The job's URL is authoritative and absolute, with no base to resolve
    -- against, and the public artifact fetch is anonymous. The builder therefore
    -- gets an empty base and no token, which is the by-URL builder's documented
    -- contract.
    case buildRequest limits manager "" Nothing (registryUrlText url) of
        Left urlErr -> pure (Left (ArtifactUnavailable ("unformable artifact URL: " <> renderUrlFormationError urlErr)))
        Right request ->
            try (liftIO (boundedFetch limits manager request)) <&> \case
                -- The client's rendered exception prints the request's path, query, and
                -- headers. The reason therefore names the bounded 'TransportCause' and
                -- the authority the fetch dialled. This text reaches a worker log line
                -- and the mirror-job span's error status.
                Left (e :: HttpException) ->
                    Left
                        ( ArtifactUnavailable
                            ("artifact fetch from " <> authorityLabel (registryUrlText url) <> " failed: " <> show (tfCause (classifyTransport e)))
                        )
                Right (Left (ResponseBoundExceeded limitErr)) ->
                    Left (ArtifactOverCap ("artifact exceeded the response bound: " <> show limitErr))
                Right (Right bytes) -> Right bytes

{- Open the artifact request and read its body chunk-by-chunk through the bounded
read against the supplied cap. It returns the whole bytes when within the cap, and a
typed 'ResponseBoundExceeded' otherwise. A network failure throws (caught by the
caller as a transient reason). The cap bounds the necessarily-buffered tarball, so the
read refuses a body past it, fail-closed. -}
boundedFetch :: Limits -> Manager -> Request -> IO (Either ResponseBoundExceeded ByteString)
boundedFetch limits manager request =
    withResponse request manager $ \response ->
        boundedRead limits (brRead (responseBody response)) >>= \case
            Right body -> pure (Right body)
            Left limitErr -> pure (Left (ResponseBoundExceeded limitErr))
