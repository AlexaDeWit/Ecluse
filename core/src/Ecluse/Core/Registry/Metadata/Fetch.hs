-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The one driver for a metadata read. It owns what every ecosystem does alike: the exchange and
its limits, both spans, the error mapping, a table keyed afresh for each read, and on a full read
the charge for each chunk, the source digest and the 'Manifest'. An ecosystem supplies an
'EcosystemRead' and writes none of that, so it cannot leave any of it out. A read runs the same
over a response and over bytes already held: only the 'Body' it is given differs.
-}
module Ecluse.Core.Registry.Metadata.Fetch (
    -- * What an ecosystem supplies
    EcosystemRead (..),
    DocumentWalk,

    -- * Reading from an origin
    ManifestFetch,
    fetchManifest,
    fetchVersion,

    -- * Reading from any body
    Body (..),
    ReadTerms (..),
    readManifest,
    readVersion,
    keyedRead,
) where

import Network.HTTP.Client (Request)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Package (PackageInfo, PackageName)
import Ecluse.Core.Registry (BodyOutcome, FetchFault (FetchUrlUnformable), UrlFormationError)
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.Exchange (chargedRead, digestingRead, formThen, withSuccessBody)
import Ecluse.Core.Registry.Json.Intern (InternTable, newInternTable, newTableKey)
import Ecluse.Core.Registry.JsonStream (StreamResult (streamBytes))
import Ecluse.Core.Registry.Metadata (Manifest (..), MetadataError, VersionRead, metadataResponse)
import Ecluse.Core.Registry.Origin (OriginClient (ocChargeFullRead, ocLimits, ocManager, ocToken), originBaseUrl)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError, Limits (maxMetadataBytes, progressFloor))
import Ecluse.Core.Telemetry.Span (TracingPort (spanMetadataDecode, spanMetadataFetch))
import Ecluse.Core.Version (Version)

{- | What differs between ecosystems in a metadata read. The two type variables are its walks'
results, hidden so that one adapter field holds any ecosystem's reads and only this module runs them.
-}
data EcosystemRead = forall full selected. EcosystemRead
    { erRequest :: Text -> Maybe ClientCredential -> PackageName -> Either UrlFormationError Request
    -- ^ The request for a package's whole document, from an origin's base URL and credential.
    , erUniqueFields :: [Text]
    -- ^ The fields whose values differ in every entry, which the read's table keeps as read.
    , erWalkFull :: Limits -> PackageName -> Text -> DocumentWalk full
    -- ^ The walk that keeps every entry, given the origin's base URL.
    , erFinishFull :: Limits -> PackageName -> Text -> StreamResult full -> Either MetadataError (PackageInfo, CachedDoc)
    -- ^ A full walk's typed view and served document, located against the origin's base URL.
    , erWalkSelected :: Limits -> PackageName -> Version -> DocumentWalk selected
    -- ^ The walk that keeps one version.
    , erFinishSelected :: Limits -> PackageName -> Text -> Version -> StreamResult selected -> Either MetadataError VersionRead
    -- ^ A selected walk's version, located against the origin's base URL.
    }

-- | One walk of a body's chunks, within the body limit and over the table keyed for the read.
type DocumentWalk s = BodyLimit -> InternTable -> IO ByteString -> IO (Either LimitError (StreamResult s))

{- | Fetching and projecting one package's full manifest from an origin. Every failure is a
'MetadataError' value, as it is through the client built over it.
-}
type ManifestFetch = TracingPort -> OriginClient -> PackageName -> IO (Either MetadataError Manifest)

{- | Where a read's bytes come from. It runs a consumer over the chunks while the body is open, and
reports how the exchange went. A response is one such body, and bytes already held are another.
-}
newtype Body = Body (forall a. (IO ByteString -> IO (Either LimitError a)) -> IO (Either FetchFault (BodyOutcome a)))

-- | What a read is held to and what it pays, whatever body it reads.
data ReadTerms = ReadTerms
    { rtLimits :: Limits
    , rtBaseUrl :: Text
    -- ^ The origin's base URL, which a finish resolves artifact locations against.
    , rtChargeFullRead :: Int -> IO ()
    -- ^ Pays for each chunk of a full read before the walk sees it.
    }

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
        (info, raw) <- finish limits name base streamed
        pure Manifest{manifestInfo = info, manifestRaw = raw, manifestBodyBytes = streamBytes streamed, manifestDigest = digest}
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

-- | Run a walk over a table keyed afresh for the read, within the metadata body limit.
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

originBody :: EcosystemRead -> OriginClient -> PackageName -> Body
originBody eco origin name =
    Body
        ( \consume ->
            formThen
                FetchUrlUnformable
                (withSuccessBody (ocManager origin) (progressFloor (ocLimits origin)) consume)
                (erRequest eco (originBaseUrl origin) (ocToken origin) name)
        )
