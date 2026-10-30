-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Full and selected metadata reads with bounded decoding, source digests, and public authority control.
module Ecluse.Core.Registry.Metadata.Fetch (
    -- * Reading from an origin
    fetchManifest,
    fetchVersion,
    chargingSelectedReads,

    -- * Reading from any body
    readManifest,
    readVersion,
) where

import GHC.Conc (pseq)

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (FetchFault (FetchUrlUnformable))
import Ecluse.Core.Registry.Exchange (accumulatingCharge, chargedRead, chargedReadWith, digestingRead, formThen, withSuccessBodyUsing)
import Ecluse.Core.Registry.Json.Intern (newInternTable, newTableKey)
import Ecluse.Core.Registry.JsonStream (StreamResult (streamBytes, streamValue))
import Ecluse.Core.Registry.Metadata (Manifest (..), MetadataError, VersionRead, metadataResponse)
import Ecluse.Core.Registry.Metadata.Fetch.Types (Body (Body), DocumentWalk, EcosystemRead (..), ManifestFetch, ReadTerms (..))
import Ecluse.Core.Registry.Origin (OriginClient (ocBaseRequest, ocChargeFullRead, ocClient, ocLimits, ocPublicControl, ocReadKind, ocToken), originBaseUrl)
import Ecluse.Core.Registry.Public (ReadControl (..))
import Ecluse.Core.Registry.Request (sealRequest)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError, Limits (maxMetadataBytes, progressFloor))
import Ecluse.Core.Telemetry.Span (TracingPort (spanMetadataDecode, spanMetadataFetch))
import Ecluse.Core.Telemetry.Upstream (PublicReadKind (Listing, SelectedRead))
import Ecluse.Core.Version (Version)

-- | Charge selected-read chunks before the adapter retains raw publication fields.
chargingSelectedReads :: (ByteString -> Int) -> (Int -> IO ()) -> EcosystemRead -> EcosystemRead
chargingSelectedReads weigh charge EcosystemRead{erRequest, erUniqueFields, erWalkFull, erFinishFull, erRetainedEntries, erWalkSelected, erFinishSelected} =
    EcosystemRead
        { erRequest
        , erUniqueFields
        , erWalkFull
        , erFinishFull
        , erRetainedEntries
        , erWalkSelected = \limits name version bound table readChunk -> do
            pay <- accumulatingCharge charge
            erWalkSelected limits name version bound table (chargedReadWith (pay . weigh) readChunk)
        , erFinishSelected
        }

-- | Fetch a package's whole document from an origin and finish it into a 'Manifest'.
fetchManifest :: EcosystemRead -> ManifestFetch
fetchManifest eco tracing origin name = readManifest eco tracing (originTerms origin) name (originBody Listing eco origin name)

-- | Fetch a package's whole document from an origin and finish only the version asked for.
fetchVersion :: EcosystemRead -> TracingPort -> OriginClient -> PackageName -> Version -> IO (Either MetadataError VersionRead)
fetchVersion eco tracing origin name version = readVersion eco tracing (originTerms origin) name version (originBody SelectedRead eco origin name)

-- | Read a full manifest from any body, charging and hashing chunks before decoding, then finishing after closure.
readManifest :: EcosystemRead -> TracingPort -> ReadTerms -> PackageName -> Body -> IO (Either MetadataError Manifest)
readManifest EcosystemRead{erUniqueFields = uniqueFields, erWalkFull = walk, erFinishFull = finish, erRetainedEntries = retained} tracing terms name body = do
    result <- exchange tracing name body (digestingRead decode . chargedRead (rtChargeFullRead terms))
    pure $ do
        (streamed, digest) <- result
        -- The size, the count and the document are forced first, so the typed view's location check holds none of the walk's result.
        let !bodyBytes = streamBytes streamed
            !entries = either (const mempty) retained (streamValue streamed)
        (info, raw) <- finish limits name base streamed
        pure (raw `pseq` Manifest{manifestInfo = info, manifestRaw = raw, manifestBodyBytes = bodyBytes, manifestRetainedEntries = entries, manifestDigest = digest})
  where
    limits = rtLimits terms
    base = rtBaseUrl terms
    decode = decoding tracing name limits uniqueFields (walk limits name base)

-- | Read one version without a source digest. A wrapped worker walk owns its raw-field charges.
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
originBody :: PublicReadKind -> EcosystemRead -> OriginClient -> PackageName -> Body
originBody kind eco origin name =
    Body
        ( \consume ->
            formThen
                FetchUrlUnformable
                (withSuccessBodyUsing control (ocClient origin) (progressFloor (ocLimits origin)) consume)
                (sealRequest <$> erRequest eco (ocBaseRequest origin) (ocToken origin) name)
        )
  where
    control = maybe Uncontrolled (\handle -> Controlled handle (fromMaybe kind (ocReadKind origin))) (ocPublicControl origin)
