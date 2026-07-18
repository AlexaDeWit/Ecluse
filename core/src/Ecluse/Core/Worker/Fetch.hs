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
import Ecluse.Core.Registry (UrlFormationError)
import Ecluse.Core.Registry.Fault (ResponseBoundExceeded (ResponseBoundExceeded))
import Ecluse.Core.Security (Limits, boundedRead)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Worker.Types (WorkerM, wrManager)

{- | Why a mirror-artifact fetch failed, split by whether a redelivery could ever
help, so the job's ack decision ('Ecluse.Core.Worker.Job.mirrorArtifact') follows
from the type rather than re-parsing a reason string.
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

{- Fetch the artifact bytes from the public upstream at the job's authoritative
URL into memory, under the plan-sized artifact byte cap. The URL arrives as the
job's validated 'RegistryUrl' witness, so the https guarantee is the argument type,
not trust in the caller. The request builder is the job ecosystem's own formation,
passed in from the re-evaluation bundle that admitted the job
('Ecluse.Core.Worker.Types.wpBuildArtifactRequest'). Publishing is
__publish-by-document__: the npm @PUT \/{pkg}@ carries the tarball base64-encoded
under @_attachments@, so the whole artifact must be in hand to verify it and assemble
the document. This path is therefore __bounded-buffered__, not streamed -- the bytes
are necessarily held -- but the read is capped by the caller's 'Limits' (the
composition root sizes it from the memory plan's mirror-artifact tenant, in
@Ecluse.Composition.MemoryPlan@), so an upstream returning a body past the cap is
refused fail-closed rather than exhausting the heap the plan partitions. An over-cap
body is an 'ArtifactOverCap' (terminal at the call site); a network or URL failure an
'ArtifactUnavailable' (transient), so a flaky upstream redelivers rather than killing
the iteration. -}
fetchArtifactBytes ::
    Limits ->
    (Limits -> Manager -> Text -> Maybe Secret -> Text -> Either UrlFormationError Request) ->
    RegistryUrl ->
    WorkerM (Either ArtifactFetchFault ByteString)
fetchArtifactBytes limits buildRequest url = do
    manager <- asks wrManager
    -- The job's URL is authoritative and absolute (no base to resolve against)
    -- and the public artifact fetch is anonymous, so the builder gets an empty
    -- base and no token (the by-URL builder's documented contract).
    case buildRequest limits manager "" Nothing (registryUrlText url) of
        Left urlErr -> pure (Left (ArtifactUnavailable ("unformable artifact URL: " <> show urlErr)))
        Right request ->
            try (liftIO (boundedFetch limits manager request)) <&> \case
                Left (e :: HttpException) -> Left (ArtifactUnavailable ("artifact fetch failed: " <> show e))
                Right (Left (ResponseBoundExceeded limitErr)) ->
                    Left (ArtifactOverCap ("artifact exceeded the response bound: " <> show limitErr))
                Right (Right bytes) -> Right bytes

{- Open the artifact request and read its body chunk-by-chunk through the bounded
read against the supplied cap, returning the whole bytes when within it or a typed
'ResponseBoundExceeded' otherwise. A network failure throws (caught by the caller as
a transient reason). The cap bounds the necessarily-buffered tarball so a body past
it is refused fail-closed. -}
boundedFetch :: Limits -> Manager -> Request -> IO (Either ResponseBoundExceeded ByteString)
boundedFetch limits manager request =
    withResponse request manager $ \response ->
        boundedRead limits (brRead (responseBody response)) >>= \case
            Right body -> pure (Right body)
            Left limitErr -> pure (Left (ResponseBoundExceeded limitErr))
