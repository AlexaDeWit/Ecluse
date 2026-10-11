-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Bind a native mirror codec to the shared bounded transport.
Native preparation runs before artifact download, and every request disables redirects.
-}
module Ecluse.Core.Registry.Publish (
    -- * What one write declares
    PublishPlan (..),
    PreparedMirror (..),

    -- * The adapter's protocol codec
    PublishCodec (..),
    fetchVersionList,

    -- * The shared transport
    MirrorTransport (..),

    -- * The married capability
    MirrorPublish (..),
    newMirrorPublish,
) where

import Data.JsonStream.Parser qualified as J

import Network.HTTP.Client (Manager, Request)

import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (
    BodyOutcome,
    FetchFault (FetchUrlUnformable),
    MirrorArtifact,
    ParseError,
    PublishFault (PublishFetch),
    UrlFormationError,
 )
import Ecluse.Core.Registry.Exchange (boundedExchange, boundedJsonFetch, formThen)
import Ecluse.Core.Registry.JsonStream (StreamResult (streamValue))
import Ecluse.Core.Registry.Metadata (VersionDoc)
import Ecluse.Core.Registry.Request (sealRequest)
import Ecluse.Core.Registry.VersionList (VersionListItem, collectVersionList, emptyVersionList, finishVersionList)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), Limits (progressFloor), maxMetadataBytes)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Version (Version)

-- | The release tag the target must carry after publication.
newtype PublishPlan = PublishPlan
    { ppLatest :: Version
    }
    deriving stock (Eq, Show)

-- | A native input checked against the admitted snapshot, bound to its target and credential mint.
newtype PreparedMirror = PreparedMirror
    { publishPrepared :: PublishPlan -> ByteString -> IO (Either PublishFault ())
    }

-- | The codec keeps its checked native input private to its constructor and writer.
data PublishCodec = forall input. PublishCodec
    { pcProbeRequest :: Text -> Maybe Secret -> PackageName -> Either UrlFormationError Request
    , pcVersionListParser :: Limits -> J.Parser VersionListItem
    , pcPrepareInput :: PackageName -> Version -> VersionDoc -> MirrorArtifact -> Either PublishFault input
    , pcPublishRequest :: Text -> Maybe Secret -> PublishPlan -> input -> ByteString -> Either PublishFault Request
    , pcPublishOutcome :: Int -> Either PublishFault ()
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
    { mpProbeMetadata :: PackageName -> IO (Either FetchFault (BodyOutcome (Either ParseError [Version])))
    -- ^ Every failure is a 'FetchFault' value, so the probe's fall-through match is total.
    , mpPrepareArtifact :: PackageName -> Version -> VersionDoc -> MirrorArtifact -> Either PublishFault PreparedMirror
    -- ^ Refuse inconsistent source input before fetching artifact bytes or minting a write credential.
    }

-- | Marry a protocol codec to the shared transport against one mirror-target endpoint.
newMirrorPublish :: MirrorTransport -> RegistryUrl -> PublishCodec -> MirrorPublish
newMirrorPublish transport target PublishCodec{pcProbeRequest, pcVersionListParser, pcPrepareInput, pcPublishRequest, pcPublishOutcome} =
    MirrorPublish
        { mpProbeMetadata = probeMetadata transport targetUrl pcProbeRequest pcVersionListParser
        , mpPrepareArtifact = \name version doc artifact -> do
            input <- pcPrepareInput name version doc artifact
            pure (PreparedMirror (publishArtifact transport targetUrl pcPublishRequest pcPublishOutcome input))
        }
  where
    targetUrl = registryUrlText target

probeMetadata :: MirrorTransport -> Text -> (Text -> Maybe Secret -> PackageName -> Either UrlFormationError Request) -> (Limits -> J.Parser VersionListItem) -> PackageName -> IO (Either FetchFault (BodyOutcome (Either ParseError [Version])))
probeMetadata transport targetUrl request parser name = do
    token <- ptMintToken transport
    formThen
        FetchUrlUnformable
        (fetchVersionList (ptManager transport) (ptLimits transport) (parser (ptLimits transport)))
        (sealRequest <$> request targetUrl token name)

-- | Read a codec's identifiers inside the response lifetime, preserving transport and HTTP outcomes.
fetchVersionList :: Manager -> Limits -> J.Parser VersionListItem -> Request -> IO (Either FetchFault (BodyOutcome (Either ParseError [Version])))
fetchVersionList manager limits parser request =
    fmap (fmap versions) <$> boundedJsonFetch manager (progressFloor limits) (MetadataBodyLimit (maxMetadataBytes limits)) parser (collectVersionList limits) emptyVersionList request
  where
    versions streamed = streamValue streamed >>= finishVersionList

publishArtifact :: MirrorTransport -> Text -> (Text -> Maybe Secret -> PublishPlan -> input -> ByteString -> Either PublishFault Request) -> (Int -> Either PublishFault ()) -> input -> PublishPlan -> ByteString -> IO (Either PublishFault ())
publishArtifact transport targetUrl request outcome input plan bytes = do
    token <- ptMintToken transport
    either
        (pure . Left)
        (writeArtifact transport outcome . sealRequest)
        (request targetUrl token plan input bytes)

writeArtifact :: MirrorTransport -> (Int -> Either PublishFault ()) -> Request -> IO (Either PublishFault ())
writeArtifact transport outcome request =
    boundedExchange (\status _ _ -> status) (ptManager transport) (progressFloor (ptLimits transport)) (MetadataBodyLimit (maxMetadataBytes (ptLimits transport))) request
        <&> \case
            Left fault -> Left (PublishFetch fault)
            Right status -> outcome status
