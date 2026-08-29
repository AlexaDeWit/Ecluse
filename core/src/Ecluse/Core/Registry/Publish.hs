-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The mirror-write capability: a shared publish transport, an adapter-provided protocol codec,
and the married 'MirrorPublish' handle a worker bundle carries. The split follows what genuinely
varies per ecosystem. The 'PublishCodec' is protocol: it assembles and shapes the request and
says what the registry's status answer means. The 'MirrorTransport' is everything else, so a new
ecosystem contributes a codec and never a transport. Both effectful operations report failure as
a __value__, 'FetchFault' on the probe and 'PublishFault' on the write, so the worker's decisions
stay total at the call site. The codec carries no authentication: the transport mints the bearer
per call, and the codec attaches it at its single attach point.
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

import Network.HTTP.Client (Manager, Request)

import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (
    FetchFault (FetchUrlUnformable),
    MirrorArtifact,
    ParseError,
    PublishFault (PublishFetch),
    RegistryResponse,
    UrlFormationError,
 )
import Ecluse.Core.Registry.Exchange (boundedExchange, boundedFetch, formThen)
import Ecluse.Core.Security (Limits)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Version (Version)

{- | One ecosystem's mirror-write protocol: the pure request formations and projections, nothing
effectful. The endpoint and bearer arrive as arguments, so the codec holds no URL, credential, or
connection state.
-}
data PublishCodec = PublishCodec
    { pcProbeRequest :: Text -> Maybe Secret -> PackageName -> Either UrlFormationError Request
    -- ^ Form the metadata read the presence probe makes against the mirror target.
    , pcParseVersionList :: RegistryResponse -> Either ParseError [Version]
    -- ^ Project a probed metadata response onto the versions the mirror holds.
    , pcPublishRequest :: Text -> Maybe Secret -> PackageName -> Version -> MirrorArtifact -> ByteString -> Either UrlFormationError Request
    -- ^ Form the complete publish request for one verified artifact, document assembly included.
    , pcPublishOutcome :: Int -> Either PublishFault ()
    {- ^ Classify the registry's status answer, counting an idempotent already-present as
    success. Registries disagree on how an immutable re-publish answers, so the codec decides.
    -}
    }

{- | The ecosystem-agnostic half of the mirror write. The composition root builds one per
marriage from process-wide parts.
-}
data MirrorTransport = MirrorTransport
    { ptManager :: Manager
    -- ^ The trusted-path connection manager the worker dials the mirror target through.
    , ptMintToken :: IO (Maybe Secret)
    {- ^ Mint the bearer for one request. Nothing caches it here: refresh, expiry, and breaker
    policy live behind the action ("Ecluse.Core.Credential.Refresh").
    -}
    , ptLimits :: Limits
    -- ^ The response bound every exchange with the mirror target is held to (fail-closed).
    }

{- | The mirror-write capability one worker bundle carries, bound to one mirror-target endpoint
under one credential mint. The worker never sees the codec, the transport, or the adapter.
-}
data MirrorPublish = MirrorPublish
    { mpProbeMetadata :: PackageName -> IO (Either FetchFault RegistryResponse)
    {- ^ Read the package's metadata from the mirror target. Every failure is a 'FetchFault'
    value, so the probe's fall-through match is total.
    -}
    , mpParseVersionList :: RegistryResponse -> Either ParseError [Version]
    -- ^ Project a probed response onto the versions the mirror holds.
    , mpPublishArtifact :: PackageName -> Version -> MirrorArtifact -> ByteString -> IO (Either PublishFault ())
    {- ^ Publish one verified artifact to the mirror target. Every failure is a
    'PublishFault' value, so the worker's retry-vs-drop decision is total at the
    call site.
    -}
    }

{- | Marry a protocol codec to the shared transport against one mirror-target endpoint. The
transport mints a bearer per call and folds every thrown failure into the typed channel.
-}
newMirrorPublish :: MirrorTransport -> RegistryUrl -> PublishCodec -> MirrorPublish
newMirrorPublish transport target codec =
    MirrorPublish
        { mpProbeMetadata = probeMetadata transport targetUrl codec
        , mpParseVersionList = pcParseVersionList codec
        , mpPublishArtifact = publishArtifact transport targetUrl codec
        }
  where
    -- The codec forms URLs from characters, so the egress witness is read once here
    -- rather than at every formation.
    targetUrl = registryUrlText target

-- Execute the codec's probe read over the transport: mint, form, dial, and read
-- the body bounded, with every failure folded into the typed 'FetchFault' channel.
probeMetadata :: MirrorTransport -> Text -> PublishCodec -> PackageName -> IO (Either FetchFault RegistryResponse)
probeMetadata transport targetUrl codec name = do
    token <- ptMintToken transport
    formThen
        FetchUrlUnformable
        (boundedFetch (ptManager transport) (ptLimits transport))
        (pcProbeRequest codec targetUrl token name)

publishArtifact :: MirrorTransport -> Text -> PublishCodec -> PackageName -> Version -> MirrorArtifact -> ByteString -> IO (Either PublishFault ())
publishArtifact transport targetUrl codec name version artifact bytes = do
    token <- ptMintToken transport
    formThen
        (PublishFetch . FetchUrlUnformable)
        (writeArtifact transport codec)
        (pcPublishRequest codec targetUrl token name version artifact bytes)

-- Read the codec's verdict from the answered status. The 'const' projection drops the
-- target's body, which the write has no use for, and the exchange bounds it either way.
writeArtifact :: MirrorTransport -> PublishCodec -> Request -> IO (Either PublishFault ())
writeArtifact transport codec request =
    boundedExchange const (ptManager transport) (ptLimits transport) request
        <&> \case
            Left fault -> Left (PublishFetch fault)
            Right status -> pcPublishOutcome codec status
