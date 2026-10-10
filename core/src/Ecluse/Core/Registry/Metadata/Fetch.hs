-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The one driver for a manifest or version read. It owns the exchange, both spans, the error
mapping, and on a full read the charge for each chunk, the source digest and the 'Manifest'. It
computes the body limit and keys a table afresh for each read, and hands both to the ecosystem's
walk. A read runs the same over a response and over bytes already held: only the
'Ecluse.Core.Registry.Metadata.Fetch.Types.Body' it is given differs.
-}
module Ecluse.Core.Registry.Metadata.Fetch (
    -- * Reading from an origin
    fetchManifest,
    fetchVersion,

    -- * Reading from any body
    readManifest,
    readVersion,
) where

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (FetchFault (FetchUrlUnformable))
import Ecluse.Core.Registry.Exchange (chargedRead, digestingRead, formThen, withSuccessBody)
import Ecluse.Core.Registry.Json.Intern (newInternTable, newTableKey)
import Ecluse.Core.Registry.JsonStream (StreamResult (streamBytes))
import Ecluse.Core.Registry.Metadata (Manifest (..), MetadataError, VersionRead, metadataResponse)
import Ecluse.Core.Registry.Metadata.Fetch.Types (Body (Body), DocumentWalk, EcosystemRead (..), ManifestFetch, ReadTerms (..))
import Ecluse.Core.Registry.Origin (OriginClient (ocChargeFullRead, ocLimits, ocManager, ocToken), originBaseUrl)
import Ecluse.Core.Registry.Request (sealRequest)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError, Limits (maxMetadataBytes, progressFloor))
import Ecluse.Core.Telemetry.Span (TracingPort (spanMetadataDecode, spanMetadataFetch))
import Ecluse.Core.Version (Version)

-- | Fetch a package's whole document from an origin and finish it into a 'Manifest'.
fetchManifest :: EcosystemRead -> ManifestFetch
fetchManifest eco tracing origin name = readManifest eco tracing (originTerms origin) name (originBody eco origin name)

-- | Fetch a package's whole document from an origin and finish only the version asked for.
fetchVersion :: EcosystemRead -> TracingPort -> OriginClient -> PackageName -> Version -> IO (Either MetadataError VersionRead)
fetchVersion eco tracing origin name version = readVersion eco tracing (originTerms origin) name version (originBody eco origin name)

{- | The full read of 'fetchManifest', over any body. Each chunk is paid for, then hashed, before
the walk reads it, and the finish runs after the body closes.
-}
readManifest :: EcosystemRead -> TracingPort -> ReadTerms -> PackageName -> Body -> IO (Either MetadataError Manifest)
readManifest EcosystemRead{erUniqueFields = uniqueFields, erWalkFull = walk, erFinishFull = finish} tracing terms name body = do
    result <- exchange tracing name body (digestingRead decode . chargedRead (rtChargeFullRead terms))
    pure $ do
        (streamed, digest) <- result
        -- The size is read and the document forced first, so the typed view's location check holds none of the walk's result.
        let !bodyBytes = streamBytes streamed
        (info, raw) <- finish limits name base streamed
        pure (raw `seq` Manifest{manifestInfo = info, manifestRaw = raw, manifestBodyBytes = bodyBytes, manifestDigest = digest})
  where
    limits = rtLimits terms
    base = rtBaseUrl terms
    decode = decoding tracing name limits uniqueFields (walk limits name base)

-- | The selected read of 'fetchVersion', over any body. It pays no charge and takes no digest.
readVersion :: EcosystemRead -> TracingPort -> ReadTerms -> PackageName -> Version -> Body -> IO (Either MetadataError VersionRead)
readVersion EcosystemRead{erUniqueFields = uniqueFields, erWalkSelected = walk, erFinishSelected = finish} tracing terms name version body =
    (>>= finish limits name (rtBaseUrl terms) version)
        <$> exchange tracing name body (decoding tracing name limits uniqueFields (walk limits name version))
  where
    limits = rtLimits terms

-- Run a walk over a table keyed afresh for the read, within the metadata body limit.
keyedRead :: Limits -> [Text] -> DocumentWalk s -> IO ByteString -> IO (Either LimitError (StreamResult s))
keyedRead limits uniqueFields walk readChunk = do
    table <- newInternTable <$> newTableKey <*> pure uniqueFields
    walk (MetadataBodyLimit (maxMetadataBytes limits)) table readChunk

decoding :: TracingPort -> PackageName -> Limits -> [Text] -> DocumentWalk s -> IO ByteString -> IO (Either LimitError (StreamResult s))
decoding tracing name limits uniqueFields walk = spanMetadataDecode tracing name . keyedRead limits uniqueFields walk

exchange :: TracingPort -> PackageName -> Body -> (IO ByteString -> IO (Either LimitError a)) -> IO (Either MetadataError a)
exchange tracing name (Body overChunks) consume = metadataResponse <$> spanMetadataFetch tracing name (overChunks consume)

originTerms :: OriginClient -> ReadTerms
originTerms origin = ReadTerms{rtLimits = ocLimits origin, rtBaseUrl = originBaseUrl origin, rtChargeFullRead = ocChargeFullRead origin}

-- The request is sealed here, so no ecosystem's builder can leave a read following redirects.
originBody :: EcosystemRead -> OriginClient -> PackageName -> Body
originBody eco origin name =
    Body
        ( \consume ->
            formThen
                FetchUrlUnformable
                (withSuccessBody (ocManager origin) (progressFloor (ocLimits origin)) consume)
                (sealRequest <$> erRequest eco (originBaseUrl origin) (ocToken origin) name)
        )
