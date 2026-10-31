-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Test bodies and capture reads through the production transport and read driver.
module Ecluse.Test.Registry.Metadata.Fetch (
    -- * Bodies
    fetchTestBody,
    heldBody,
    sourceBody,
    captureChunks,

    -- * Reads
    heldManifest,
    heldWorkspacePeak,
    heldWorkspacePayments,
    captureManifest,
    captureVersion,
) where

import Data.ByteString qualified as BS
import Network.HTTP.Client (Request, Response (responseStatus), brRead, defaultManagerSettings, newManager, responseBody)
import Network.HTTP.Types.Status (statusCode)

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (BodyOutcome (SuccessBody), FetchFault (FetchBoundExceeded), RegistryResponse (RegistryResponse), isAuthorisationFailure)
import Ecluse.Core.Registry.Exchange (runExchange)
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient)
import Ecluse.Core.Registry.Metadata (Manifest, MetadataError, VersionRead)
import Ecluse.Core.Registry.Metadata.Fetch (readManifest, readVersion)
import Ecluse.Core.Registry.Metadata.Fetch.Preparation (PrepareRead, prepareRead)
import Ecluse.Core.Registry.Metadata.Fetch.Types (Body (Body), EcosystemRead, ReadTerms (..))
import Ecluse.Core.Registry.Origin (OriginClient (ocChargeFullReadWorkspace), originClient)
import Ecluse.Core.Security (BodyLimit, LimitError, Limits (maxMetadataBytes), ProgressFloor, boundedRead, defaultLimits)
import Ecluse.Core.Version (Version)
import Ecluse.Test.Corpus (CaptureUpstream (upstreamOrigin))
import Ecluse.Test.Deadline (withExchangeClient)
import Ecluse.Test.Package (unsafeRegistryUrl)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Registry.JsonStream (heldChunks, sliceChunks)

-- | Preserve authorisation refusals without reading their bodies. Bound every other smoke or contract response.
fetchTestBody :: ExchangeClient -> ProgressFloor -> BodyLimit -> Request -> IO (Either FetchFault RegistryResponse)
fetchTestBody client progress limit request = runExchange client progress request $ \response ->
    let code = statusCode (responseStatus response)
     in if isAuthorisationFailure code
            then pure (Right (RegistryResponse code 0 ""))
            else fmap (uncurry (RegistryResponse code)) <$> boundedRead limit (brRead (responseBody response))

-- | A 200 response whose body is the chunks, which the read driver takes in place of a socket.
heldBody :: [ByteString] -> Body
heldBody chunks = Body (\consume -> heldChunks chunks >>= fmap answered . consume)

-- | A 200 response whose body is what the source hands out, up to its first empty chunk.
sourceBody :: IO ByteString -> Body
sourceBody next = Body (\consume -> answered <$> consume next)

answered :: Either LimitError a -> Either FetchFault (BodyOutcome a)
answered = bimap FetchBoundExceeded (SuccessBody 200)

-- | A capture cut into the 32 KiB slices a body arrives in. Cut it before a measured read, so the figure holds no slicing.
captureChunks :: ByteString -> [ByteString]
captureChunks = sliceChunks 32768

-- | The production full read of held chunks from the base URL, under a body limit that admits them and with no memory charge.
heldManifest :: EcosystemRead -> Limits -> Text -> PackageName -> [ByteString] -> IO (Either MetadataError Manifest)
heldManifest eco limits base name chunks = readManifest eco passthroughTracingPort (heldTerms limits base chunks) name (heldBody chunks)

-- | 'heldWorkspacePayments' with only the highest full-read workspace the read paid.
heldWorkspacePeak :: PrepareRead -> Text -> PackageName -> [ByteString] -> IO (Either MetadataError Manifest, Int)
heldWorkspacePeak factory base name chunks = second (foldl' max 0) <$> heldWorkspacePayments factory base name chunks

-- | 'heldManifest' through a reader prepared under an origin at the base URL, with each full-read workspace it paid, in order.
heldWorkspacePayments :: PrepareRead -> Text -> PackageName -> [ByteString] -> IO (Either MetadataError Manifest, [Int])
heldWorkspacePayments factory base name chunks = do
    paid <- newIORef []
    withExchangeClient (newManager defaultManagerSettings) $ \client -> do
        let origin = (originClient defaultLimits client (unsafeRegistryUrl base) Nothing){ocChargeFullReadWorkspace = \amount -> modifyIORef' paid (amount :)}
        nativeRead <- prepareRead factory origin >>= either (fail . show) pure
        result <- heldManifest nativeRead defaultLimits base name chunks
        (result,) . reverse <$> readIORef paid

-- | 'heldManifest' of a capture, under the default limits and against the registry it came from.
captureManifest :: EcosystemRead -> CaptureUpstream -> PackageName -> [ByteString] -> IO (Either MetadataError Manifest)
captureManifest nativeRead upstream = heldManifest nativeRead defaultLimits (upstreamOrigin upstream)

-- | The production selected read of a capture's chunks, on the terms of 'captureManifest'.
captureVersion :: EcosystemRead -> CaptureUpstream -> PackageName -> Version -> [ByteString] -> IO (Either MetadataError VersionRead)
captureVersion nativeRead upstream name version chunks =
    readVersion nativeRead passthroughTracingPort (heldTerms defaultLimits (upstreamOrigin upstream) chunks) name version (heldBody chunks)

heldTerms :: Limits -> Text -> [ByteString] -> ReadTerms
heldTerms limits base chunks =
    ReadTerms
        { rtLimits = limits{maxMetadataBytes = max (maxMetadataBytes limits) (sum (map BS.length chunks))}
        , rtBaseUrl = base
        , rtChargeFullRead = const pass
        }
