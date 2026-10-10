-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Held bytes read through the production read driver, with fixed chunks in place of a socket.
module Ecluse.Test.Registry.Metadata.Fetch (
    -- * Bodies
    heldBody,
    sourceBody,
    captureChunks,

    -- * Reads
    heldManifest,
    captureManifest,
    captureVersion,
) where

import Data.ByteString qualified as BS

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (BodyOutcome (SuccessBody), FetchFault (FetchBoundExceeded))
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataRead))
import Ecluse.Core.Registry.Metadata (Manifest, MetadataError, VersionRead)
import Ecluse.Core.Registry.Metadata.Fetch (readManifest, readVersion)
import Ecluse.Core.Registry.Metadata.Fetch.Types (Body (Body), EcosystemRead, ReadTerms (..))
import Ecluse.Core.Security (LimitError, Limits (maxMetadataBytes), defaultLimits)
import Ecluse.Core.Version (Version)
import Ecluse.Test.Corpus (CaptureUpstream (upstreamOrigin))
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Registry.JsonStream (heldChunks)

-- | A 200 response whose body is the chunks, which the read driver takes in place of a socket.
heldBody :: [ByteString] -> Body
heldBody chunks = Body (\consume -> heldChunks chunks >>= fmap answered . consume)

-- | A 200 response whose body is what the source hands out, up to its first empty chunk.
sourceBody :: IO ByteString -> Body
sourceBody next = Body (\consume -> answered <$> consume next)

answered :: Either LimitError a -> Either FetchFault (BodyOutcome a)
answered = bimap FetchBoundExceeded (SuccessBody 200)

{- | A capture cut into the 32 KiB chunks a body arrives in, each a slice of the capture. A harness
cuts them before a measured read, so the read's figure holds no slicing.
-}
captureChunks :: ByteString -> [ByteString]
captureChunks bytes
    | BS.null bytes = []
    | otherwise = case BS.splitAt 32768 bytes of (!piece, rest) -> piece : captureChunks rest

{- | The production full read of held chunks, as a fetch from an origin at the base URL runs it. The
body limit is at least the chunks' size, and the charge has no payer, as outside the memory gate.
-}
heldManifest :: EcosystemRead -> Limits -> Text -> PackageName -> [ByteString] -> IO (Either MetadataError Manifest)
heldManifest eco limits base name chunks = readManifest eco passthroughTracingPort (heldTerms limits base chunks) name (heldBody chunks)

-- | 'heldManifest' of a capture, under the default limits and against the registry it came from.
captureManifest :: AdapterMetadata -> CaptureUpstream -> PackageName -> [ByteString] -> IO (Either MetadataError Manifest)
captureManifest metadata upstream = heldManifest (metadataRead metadata) defaultLimits (upstreamOrigin upstream)

-- | The production selected read of a capture's chunks, on the terms of 'captureManifest'.
captureVersion :: AdapterMetadata -> CaptureUpstream -> PackageName -> Version -> [ByteString] -> IO (Either MetadataError VersionRead)
captureVersion metadata upstream name version chunks =
    readVersion (metadataRead metadata) passthroughTracingPort (heldTerms defaultLimits (upstreamOrigin upstream) chunks) name version (heldBody chunks)

heldTerms :: Limits -> Text -> [ByteString] -> ReadTerms
heldTerms limits base chunks =
    ReadTerms
        { rtLimits = limits{maxMetadataBytes = max (maxMetadataBytes limits) (sum (map BS.length chunks))}
        , rtBaseUrl = base
        , rtChargeFullRead = const pass
        }
