-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The mirror-write capability: a shared transport, an adapter-provided protocol codec, and the
married 'MirrorPublish' handle a worker bundle carries. A new ecosystem contributes a codec and
never a transport.

The transport mints the bearer per call and re-seals every request, so no codec can ship a write
that follows a redirect.
-}
module Ecluse.Core.Registry.Publish (
    -- * What one write declares
    PublishPlan (..),

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
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.Exchange (boundedExchange, boundedFetch, formThen)
import Ecluse.Core.Registry.Request (sealRequest)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), Limits, maxMetadataBytes)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Version (Version)

{- | What one mirror write declares: the version, the release tag the store must carry once it
lands, and the version's own metadata. The caller decides the tag, so no codec derives one.
-}
data PublishPlan = PublishPlan
    { ppVersion :: Version
    -- ^ The version these bytes publish.
    , ppLatest :: Version
    -- ^ Always a version the store holds after this write, the published one when it is alone.
    , ppMetadata :: CachedDoc
    -- ^ The version object the public registry served at admission, republished by the codec.
    }
    deriving stock (Eq, Show)

{- | One ecosystem's mirror-write protocol, all pure. The endpoint and bearer arrive as arguments,
so a codec holds no URL, credential, or connection state.
-}
data PublishCodec = PublishCodec
    { pcProbeRequest :: Text -> Maybe Secret -> PackageName -> Either UrlFormationError Request
    -- ^ Form the metadata read the presence probe makes against the mirror target.
    , pcParseVersionList :: RegistryResponse -> Either ParseError [Version]
    -- ^ Project a probed metadata response onto the versions the mirror holds.
    , pcPublishRequest :: Text -> Maybe Secret -> PackageName -> PublishPlan -> MirrorArtifact -> ByteString -> Either PublishFault Request
    {- ^ Form the complete publish request for one verified artifact. A plan whose version object
    the codec cannot read refuses as a value.
    -}
    , pcPublishOutcome :: Int -> Either PublishFault ()
    {- ^ Classify the status answer. Registries disagree on how an immutable re-publish answers,
    so the codec counts an idempotent already-present as success.
    -}
    }

{- | The ecosystem-agnostic half of the mirror write. The composition root builds one per
marriage from process-wide parts.
-}
data MirrorTransport = MirrorTransport
    { ptManager :: Manager
    -- ^ The trusted-path connection manager the worker dials the mirror target through.
    , ptMintToken :: IO (Maybe Secret)
    -- ^ Nothing caches it here: refresh, expiry, and breaker policy live behind the action.
    , ptLimits :: Limits
    -- ^ The response bound every exchange with the mirror target is held to (fail-closed).
    }

{- | What one worker bundle carries, bound to one mirror-target endpoint under one credential
mint. The worker never sees the codec, the transport, or the adapter.
-}
data MirrorPublish = MirrorPublish
    { mpProbeMetadata :: PackageName -> IO (Either FetchFault RegistryResponse)
    -- ^ Every failure is a 'FetchFault' value, so the probe's fall-through match is total.
    , mpParseVersionList :: RegistryResponse -> Either ParseError [Version]
    -- ^ Project a probed response onto the versions the mirror holds.
    , mpPublishArtifact :: PackageName -> PublishPlan -> MirrorArtifact -> ByteString -> IO (Either PublishFault ())
    {- ^ Every failure is a 'PublishFault' value, so the worker's retry-vs-drop decision is
    total at the call site.
    -}
    }

-- | Marry a protocol codec to the shared transport against one mirror-target endpoint.
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

-- Execute the codec's probe read over the transport: mint, form, seal, dial, and read the
-- body bounded, with every failure folded into the typed 'FetchFault' channel.
probeMetadata :: MirrorTransport -> Text -> PublishCodec -> PackageName -> IO (Either FetchFault RegistryResponse)
probeMetadata transport targetUrl codec name = do
    token <- ptMintToken transport
    formThen
        FetchUrlUnformable
        (boundedFetch (ptManager transport) (MetadataBodyLimit (maxMetadataBytes (ptLimits transport))))
        (sealRequest <$> pcProbeRequest codec targetUrl token name)

publishArtifact :: MirrorTransport -> Text -> PublishCodec -> PackageName -> PublishPlan -> MirrorArtifact -> ByteString -> IO (Either PublishFault ())
publishArtifact transport targetUrl codec name plan artifact bytes = do
    token <- ptMintToken transport
    either
        (pure . Left)
        (writeArtifact transport codec . sealRequest)
        (pcPublishRequest codec targetUrl token name plan artifact bytes)

-- Read the codec's verdict from the answered status. The 'const' projection drops the
-- target's body, which the write has no use for, and the exchange bounds it either way.
writeArtifact :: MirrorTransport -> PublishCodec -> Request -> IO (Either PublishFault ())
writeArtifact transport codec request =
    boundedExchange (\status _ _ -> status) (ptManager transport) (MetadataBodyLimit (maxMetadataBytes (ptLimits transport))) request
        <&> \case
            Left fault -> Left (PublishFetch fault)
            Right status -> pcPublishOutcome codec status
