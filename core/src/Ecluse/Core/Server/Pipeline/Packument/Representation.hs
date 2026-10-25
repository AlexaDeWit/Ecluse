-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Body assembly, validators and output charges for admitted metadata contributions.
module Ecluse.Core.Server.Pipeline.Packument.Representation (
    Contribution (..),
    fingerprintPiece,
    codedETag,
    packumentETag,
    outputBasisBytes,
    assembleServedBody,
) where

import Control.Monad.ST (ST)
import Crypto.Hash (SHA256 (SHA256), hashWith, hashlazy)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Internal (c2w)
import Data.Map.Strict qualified as Map
import Data.Text.Unsafe (lengthWord8)

import Ecluse.Core.Package (Artifact (artEntryKey), PackageDetails (pkgArtifacts), PackageInfo (infoVersions), PackageName, renderPackageName)
import Ecluse.Core.Package.Entry (EntryKey (..))
import Ecluse.Core.Package.Merge (MergePlan (mpSurvivors), Provenance (GatedSource, TrustedSource), SourceId)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataAssemble))
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.Json.Scratch (Scratch, decimalLength, putByte, putDecimal, putRawBytes, putRawText, scratchBytes)
import Ecluse.Core.Registry.Metadata (ContentDigest, digestBytes)
import Ecluse.Core.Server.Conditional (ETag, mkStrongETag, mkWeakETag, renderETag)
import Ecluse.Core.Server.Encoding (ContentCoding (GzipCoding, IdentityCoding))
import Ecluse.Core.Server.Framing (frameBytes, putFrame)
import Ecluse.Core.Snapshot (Snapshot (..))

-- | A parsed contribution with opaque source bytes and a digest for the derived validator.
data Contribution = Contribution
    { srcProvenance :: Provenance
    , srcInfo :: PackageInfo
    , srcValue :: CachedDoc
    , srcDigest :: ContentDigest
    , srcBodyBytes :: Int
    -- ^ The decompressed source size, from which the listing's output charge takes its basis.
    }

-- | Scope surviving versions and exact artifact coordinates to their source digest and provenance.
fingerprintPiece :: Contribution -> (Provenance, ContentDigest, [(Text, [EntryKey])])
fingerprintPiece s =
    ( srcProvenance s
    , srcDigest s
    , [(version, map artEntryKey (toList (pkgArtifacts details))) | (version, details) <- Map.toList (infoVersions (srcInfo s))]
    )

-- | Keep the identity validator, or derive a weak gzip validator because codec changes can alter compressed bytes.
codedETag :: ContentCoding -> ETag -> ETag
codedETag coding identityTag = case coding of
    IdentityCoding -> identityTag
    GzipCoding -> mkWeakETag (hashlazy (toLazyByteString discriminated))
  where
    discriminated = "ecluse:packument-etag:gzip:v1\0" <> frameBytes (encodeUtf8 (renderETag identityTag))

-- | A validator derived from framed inputs so unchanged requests skip assembly. Bump the salt when assembly behaviour changes.
packumentETag :: Text -> [Text] -> PackageName -> [(Provenance, ContentDigest, [(Text, [EntryKey])])] -> ETag
packumentETag mountBaseUrl originBaseUrls name sources =
    mkStrongETag (hashWith SHA256 (scratchBytes 4096 fingerprint))
  where
    fingerprint :: Scratch st -> ST st ()
    fingerprint scratch = do
        putRawBytes scratch "ecluse:packument-etag:v4\0"
        traverse_ (putFramedText scratch) originBaseUrls
        putByte scratch 0
        putRawText scratch mountBaseUrl
        putByte scratch 0
        putRawText scratch (renderPackageName name)
        putByte scratch 0
        traverse_ (putSourcePiece scratch) sources

putSourcePiece :: Scratch st -> (Provenance, ContentDigest, [(Text, [EntryKey])]) -> ST st ()
putSourcePiece scratch (provenance, digest, survivors) = do
    putByte scratch (provenanceTag provenance)
    putByte scratch 0
    putRawBytes scratch (digestBytes digest)
    traverse_ (putVersionPiece scratch) survivors
    putByte scratch 1

putVersionPiece :: Scratch st -> (Text, [EntryKey]) -> ST st ()
putVersionPiece scratch (version, entries) = do
    putFramedText scratch version
    traverse_ (putEntryPiece scratch) entries
    putByte scratch 2

putEntryPiece :: Scratch st -> EntryKey -> ST st ()
putEntryPiece scratch = \case
    ArrayEntry index -> putByte scratch (c2w 'a') >> putFrame scratch (decimalLength index) (putDecimal scratch index)
    RecordEntry index -> putByte scratch (c2w 'r') >> putFrame scratch (decimalLength index) (putDecimal scratch index)
    ObjectEntry key -> putByte scratch (c2w 'o') >> putFramedText scratch key
    SingletonEntry -> putByte scratch (c2w 's')

putFramedText :: Scratch st -> Text -> ST st ()
putFramedText scratch text = putFrame scratch (lengthWord8 text) (putRawText scratch text)

provenanceTag :: Provenance -> Word8
provenanceTag = \case
    TrustedSource -> c2w 't'
    GatedSource -> c2w 'g'

-- | Charge the larger source estimate, adding proportional bytes for versions missing from each anchor.
outputBasisBytes :: MergePlan -> [Contribution] -> Int
outputBasisBytes plan sources = foldl' max 0 (map anchoredOn anchors)
  where
    indexed = zip [0 :: Int ..] sources
    anchors = maybeToList (listToMaybe (sortOn (Down . srcBodyBytes . snd) indexed)) <> maybeToList (baseSource indexed)
    anchoredOn (position, anchor) =
        srcBodyBytes anchor + sum [addedShare (Map.size (mpSurvivors plan) - versionCount anchor) other | (at, other) <- indexed, at /= position]

-- A source's bytes pro rata for the survivors outside the anchor, rounded up.
addedShare :: Int -> Contribution -> Int
addedShare outside source
    | count <= 0 = srcBodyBytes source
    | otherwise = (srcBodyBytes source * min count (max 0 outside) + count - 1) `div` count
  where
    count = versionCount source

versionCount :: Contribution -> Int
versionCount = Map.size . infoVersions . srcInfo

-- | The served document of a merge whose plan names the sources by their position.
assembleServedBody :: AdapterMetadata -> Text -> [Contribution] -> MergePlan -> CachedDoc
assembleServedBody metadata mountBase sources plan =
    metadataAssemble metadata mountBase bySource plan (baseDocument sources)
  where
    bySource :: Map SourceId (Snapshot CachedDoc)
    bySource = Map.fromList (zip [0 ..] [Snapshot (srcDigest source) (srcValue source) | source <- sources])

baseDocument :: [Contribution] -> Maybe CachedDoc
baseDocument = fmap (srcValue . snd) . baseSource . zip [0 :: Int ..]

-- The source whose top-level fields the served document keeps: the trusted one, else the first.
baseSource :: [(Int, Contribution)] -> Maybe (Int, Contribution)
baseSource indexed = find ((== TrustedSource) . srcProvenance . snd) indexed <|> listToMaybe indexed
