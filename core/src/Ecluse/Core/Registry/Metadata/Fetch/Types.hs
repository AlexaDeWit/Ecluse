-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The read driver's shared vocabulary: what an ecosystem supplies, where a read's bytes come
from, and what a read is held to. The ecosystem modules and the body sources build these values,
"Ecluse.Core.Registry.Adapter.Capability" holds each ecosystem's, and
"Ecluse.Core.Registry.Metadata.Fetch" runs them.
-}
module Ecluse.Core.Registry.Metadata.Fetch.Types (
    -- * What an ecosystem supplies
    EcosystemRead (..),
    DocumentWalk,

    -- * What a read runs over
    Body (..),
    ReadTerms (..),

    -- * A fetch from an origin
    ManifestFetch,
) where

import Network.HTTP.Client (Request)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Package (PackageInfo, PackageName)
import Ecluse.Core.Registry (BodyOutcome, FetchFault, UrlFormationError)
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.Json.Intern (InternTable)
import Ecluse.Core.Registry.JsonStream (StreamResult)
import Ecluse.Core.Registry.Metadata (Manifest, MetadataError, VersionRead)
import Ecluse.Core.Registry.Origin (OriginClient)
import Ecluse.Core.Security (BodyLimit, LimitError, Limits)
import Ecluse.Core.Telemetry.Span (TracingPort)
import Ecluse.Core.Version (Version)

{- | What differs between ecosystems in a metadata read. The two type variables are its walks'
results, hidden so that one adapter field holds any ecosystem's reads.
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

{- | One walk of a body's chunks. The driver hands it the metadata body limit and a table keyed
afresh for the read, and the walk applies both.
-}
type DocumentWalk s = BodyLimit -> InternTable -> IO ByteString -> IO (Either LimitError (StreamResult s))

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

{- | Fetching and projecting one package's full manifest from an origin. Every failure is a
'MetadataError' value, as it is through the client built over it.
-}
type ManifestFetch = TracingPort -> OriginClient -> PackageName -> IO (Either MetadataError Manifest)
