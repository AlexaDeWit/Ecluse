-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The mirror-write capability: a shared publish transport, an adapter-provided
protocol codec, and the married 'MirrorPublish' handle the worker's per-ecosystem
bundle carries.

The mirror write splits along what genuinely varies per ecosystem. The
'PublishCodec' is protocol: how a publish document is assembled and shaped into a
request, how a mirror listing is read for the presence probe, and what the
registry's status answer means. The 'MirrorTransport' is everything else: the
trusted-path connection manager, the credential-minting action (with whatever
refresh and breaker apparatus sits behind it), the response bound, and the fault
classification into the typed channels. 'newMirrorPublish' marries the two against
one mirror-target endpoint; the composition root performs that marriage once per
mounted ecosystem, so a new ecosystem contributes a codec and never a transport.

Both effectful operations report failure as a __value__ ('FetchFault' on the
probe, 'PublishFault' on the write), never a throw, so the worker's
fall-through and retry-vs-drop decisions stay total at the call site. The codec
carries no authentication: the bearer is minted by the transport per call and
handed to the codec's request formers, which attach it at the shared single
attach point ('Ecluse.Core.Registry.Npm.Request.withToken' for npm), preserving
the credential-redirect invariant per married client.
-}
module Ecluse.Core.Registry.Publish (
    -- * The adapter's protocol codec
    PublishCodec (..),

    -- * The shared transport
    MirrorTransport (..),

    -- * The married capability
    MirrorPublish (..),
    newMirrorPublish,
) where

import Network.HTTP.Client (
    BodyReader,
    HttpException,
    Manager,
    Request,
    Response (responseStatus),
    brRead,
    httpLbs,
    responseBody,
    withResponse,
 )
import Network.HTTP.Types.Status (statusCode)
import UnliftIO (try)

import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Fault.Http (classifyTransport)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (
    FetchFault (FetchBoundExceeded, FetchTransport, FetchUrlUnformable),
    MirrorArtifact,
    ParseError,
    PublishFault (PublishTransport, PublishUrlUnformable),
    RegistryResponse (RegistryResponse),
    UrlFormationError,
 )
import Ecluse.Core.Security (LimitError, Limits, boundedRead)
import Ecluse.Core.Version (Version)

{- | One ecosystem's mirror-write protocol: the pure request formations and
projections that differ per registry protocol, and nothing effectful. An adapter
registers exactly one of these ('Ecluse.Core.Registry.Adapter.Types.AdapterPublish');
the target endpoint and bearer arrive as arguments from the transport, so the
codec holds no URL, no credential, and no connection state, and the mirror
target's protocol (a packument-fragment @PUT@, a multipart upload, a binary push)
is entirely the codec's to shape through the 'Request' it forms.
-}
data PublishCodec = PublishCodec
    { pcProbeRequest :: Text -> Maybe Secret -> PackageName -> Either UrlFormationError Request
    {- ^ Form the mirror-listing read for the presence probe: the package's
    metadata at the given target endpoint, under the given bearer.
    -}
    , pcParseVersionList :: RegistryResponse -> Either ParseError [Version]
    -- ^ Project a probed metadata response onto the versions the mirror holds.
    , pcPublishRequest :: Text -> Maybe Secret -> PackageName -> Version -> MirrorArtifact -> ByteString -> Either UrlFormationError Request
    {- ^ Form the complete publish request for one verified artifact: document
    assembly and request shaping in one step, from the re-admitted artifact's
    descriptor and the verified bytes.
    -}
    , pcPublishOutcome :: Int -> Either PublishFault ()
    {- ^ What the registry's status answer means for the write: which statuses
    are success (idempotent already-present answers included, where the protocol
    has them) and which are the retryable 'Ecluse.Core.Registry.PublishRejected'.
    Protocol semantics, so it lives with the codec: registries disagree on how an
    immutable re-publish answers.
    -}
    }

{- | The shared half of the mirror write: the trusted-path connection manager, the
credential mint, and the response bound the probe reads under. Supplied by the
environment at construction (the composition root builds one per marriage from
process-wide parts), exactly like the queue handle: nothing here is
ecosystem-shaped.
-}
data MirrorTransport = MirrorTransport
    { ptManager :: Manager
    -- ^ The trusted-path connection manager the mirror target is dialled through.
    , ptMintToken :: IO (Maybe Secret)
    {- ^ Mint the bearer for one request. Minted per call, never cached here: the
    refresh, expiry, and breaker policy live behind the action
    ("Ecluse.Core.Credential.Refresh"), so the marriage always writes under a
    current token.
    -}
    , ptLimits :: Limits
    -- ^ The response bound the probe's metadata read is held to (fail-closed).
    }

{- | The married mirror-write capability one worker bundle carries: the presence
probe's read pair and the verified-bytes publish, all bound to one mirror-target
endpoint under one credential mint. A record of functions (the Handle pattern), so
the worker consumes a plain handle and never sees the codec, the transport, or the
adapter that contributed them.
-}
data MirrorPublish = MirrorPublish
    { mpProbeMetadata :: PackageName -> IO (Either FetchFault RegistryResponse)
    {- ^ Read the package's metadata from the mirror target. Every failure is a
    'FetchFault' value (an unformable URL, a bound breach, a transport fault), so
    the probe's positive-confirmation fall-through is a total match.
    -}
    , mpParseVersionList :: RegistryResponse -> Either ParseError [Version]
    -- ^ Project a probed response onto the versions the mirror holds.
    , mpPublishArtifact :: PackageName -> Version -> MirrorArtifact -> ByteString -> IO (Either PublishFault ())
    {- ^ Publish one verified artifact to the mirror target. Every failure is a
    'PublishFault' value, so the worker's retry-vs-drop decision is total at the
    call site.
    -}
    }

{- | Marry a protocol codec to the shared transport against one mirror-target
endpoint. The transport executes what the codec forms: it mints the bearer per
call, runs the request over the trusted manager, folds a thrown transport failure
into the typed channel ('classifyTransport' on both the probe's read and the
write), reads the probe's body bounded, and hands the write's status answer to the
codec's own outcome classification.
-}
newMirrorPublish :: MirrorTransport -> Text -> PublishCodec -> MirrorPublish
newMirrorPublish transport targetUrl codec =
    MirrorPublish
        { mpProbeMetadata = probeMetadata transport targetUrl codec
        , mpParseVersionList = pcParseVersionList codec
        , mpPublishArtifact = publishArtifact transport targetUrl codec
        }

-- Execute the codec's probe read over the transport: mint, form, dial, and read
-- the body bounded, with every failure folded into the typed 'FetchFault' channel.
probeMetadata :: MirrorTransport -> Text -> PublishCodec -> PackageName -> IO (Either FetchFault RegistryResponse)
probeMetadata transport targetUrl codec name = do
    token <- ptMintToken transport
    case pcProbeRequest codec targetUrl token name of
        Left urlErr -> pure (Left (FetchUrlUnformable urlErr))
        Right request ->
            try (withResponse request (ptManager transport) (readBoundedBody (ptLimits transport) . responseBody))
                <&> \case
                    Left httpErr -> Left (FetchTransport (classifyTransport httpErr))
                    Right (Left limitErr) -> Left (FetchBoundExceeded limitErr)
                    Right (Right response) -> Right response

-- Execute the codec's publish over the transport: mint, form, PUT the whole
-- document, and let the codec classify the status answer; a thrown transport
-- failure is folded through the shared 'classifyTransport' into the retryable
-- 'PublishTransport' value, exactly as the probe folds its own.
publishArtifact :: MirrorTransport -> Text -> PublishCodec -> PackageName -> Version -> MirrorArtifact -> ByteString -> IO (Either PublishFault ())
publishArtifact transport targetUrl codec name version artifact bytes = do
    token <- ptMintToken transport
    case pcPublishRequest codec targetUrl token name version artifact bytes of
        Left urlErr -> pure (Left (PublishUrlUnformable urlErr))
        Right request ->
            try (httpLbs request (ptManager transport)) <&> \case
                Left (err :: HttpException) -> Left (PublishTransport (classifyTransport err))
                Right response -> pcPublishOutcome codec (statusCode (responseStatus response))

-- Read a response body chunk-by-chunk against the budget, returning the whole
-- body within the cap or the breach as a value (never a truncated body).
readBoundedBody :: Limits -> BodyReader -> IO (Either LimitError RegistryResponse)
readBoundedBody limits bodyReader =
    fmap RegistryResponse <$> boundedRead limits (brRead bodyReader)
