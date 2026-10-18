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

import Crypto.Hash (Context, SHA256, hashFinalize, hashInit, hashUpdates)
import Data.ByteString.Builder (Builder, byteString, toLazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map

import Ecluse.Core.Package (Artifact (artEntryKey), PackageDetails (pkgArtifacts), PackageInfo (infoVersions), PackageName, renderPackageName)
import Ecluse.Core.Package.Entry (EntryKey (..))
import Ecluse.Core.Package.Merge (MergePlan (mpSurvivors), Provenance (GatedSource, TrustedSource), SourceId)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataAssemble))
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.Metadata (ContentDigest, digestBytes)
import Ecluse.Core.Server.Conditional (ETag, mkStrongETag, mkWeakETag, renderETag)
import Ecluse.Core.Server.Encoding (ContentCoding (GzipCoding, IdentityCoding))
import Ecluse.Core.Server.Framing (frameBytes)
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
    GzipCoding -> mkWeakETag (hashFinalize (hashUpdates (hashInit :: Context SHA256) (LBS.toChunks (toLazyByteString discriminated))))
  where
    discriminated = "ecluse:packument-etag:gzip:v1\0" <> frameBytes (encodeUtf8 (renderETag identityTag))

-- | A validator derived from framed inputs so unchanged requests skip assembly. Bump the salt when assembly behaviour changes.
packumentETag :: Text -> [Text] -> PackageName -> [(Provenance, ContentDigest, [(Text, [EntryKey])])] -> ETag
packumentETag mountBaseUrl originBaseUrls name sources =
    mkStrongETag (hashFinalize (hashUpdates (hashInit :: Context SHA256) pieces))
  where
    pieces :: [ByteString]
    pieces = LBS.toChunks (toLazyByteString fingerprint)

    fingerprint :: Builder
    fingerprint =
        "ecluse:packument-etag:v4\0"
            <> foldMap (frameBytes . encodeUtf8) originBaseUrls
            <> "\0"
            <> byteString (encodeUtf8 mountBaseUrl)
            <> "\0"
            <> byteString (encodeUtf8 (renderPackageName name))
            <> "\0"
            <> foldMap etagSourcePiece sources

etagSourcePiece :: (Provenance, ContentDigest, [(Text, [EntryKey])]) -> Builder
etagSourcePiece (provenance, digest, survivors) =
    etagProvenanceTag provenance
        <> byteString (digestBytes digest)
        <> foldMap etagVersionPiece survivors
        <> "\1"

etagVersionPiece :: (Text, [EntryKey]) -> Builder
etagVersionPiece (version, entries) =
    frameBytes (encodeUtf8 version) <> foldMap etagEntryPiece entries <> "\2"

etagEntryPiece :: EntryKey -> Builder
etagEntryPiece = \case
    ArrayEntry index -> "a" <> frameBytes (show index)
    ObjectEntry key -> "o" <> frameBytes (encodeUtf8 key)
    SingletonEntry -> "s"

etagProvenanceTag :: Provenance -> Builder
etagProvenanceTag = \case
    TrustedSource -> "t\0"
    GatedSource -> "g\0"

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
