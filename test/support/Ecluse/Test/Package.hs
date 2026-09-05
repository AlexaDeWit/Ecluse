-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Test helpers and fixtures for "Ecluse.Core.Package".

The module name follows this support library's @Ecluse.X → Ecluse.Test.X@ convention.
The @unsafe@ formers lift a known-good fixture string into a domain value and error on a
typo, so a bad fixture fails loudly. Each canonical digest is the empty-input digest of its
algorithm, immaterial beyond being well-formed. The renderers compute npm's hexadecimal
shasums and Subresource-Integrity text apart from the production integrity machinery, so
tests keep a separate oracle.
-}
module Ecluse.Test.Package (
    -- * Constructing hashes from fixtures
    unsafeFilename,
    unsafeHash,
    unsafeRegistryUrl,
    unsafeSriHashes,

    -- * Integrity floor fixtures
    defaultMinIntegrity,
    defaultMinTrustedIntegrity,

    -- * Rendering digest fixtures
    hexSha1Of,
    hexSha256Of,
    hexSha384Of,
    hexSha512Of,
    hexBlake2bOf,
    sriSha256Of,
    sriSha384Of,
    sriSha512Of,
    hexSha1OfLazy,
    sriSha512OfLazy,

    -- * Canonical digest fixtures
    validSha1,
    validSha256,
    validMd5,
    validBlake2b,
    validSha256Sri,
    validSha384Sri,
    validSha512Sri,

    -- * Shared identity fixtures
    unscopedNpm,
    thingName,
    v1_0_0,

    -- * Shared package fixtures
    sampleArtifact,
    artifactWith,
    sampleDetails,
    detailsWith,
    sampleManifest,
) where

import Crypto.Hash (Blake2b_512, Digest, SHA1, SHA256, SHA384, SHA512, hash, hashlazy)
import Data.Aeson (Value (Object))
import Data.ByteArray (ByteArrayAccess)
import Data.ByteArray.Encoding (Base (Base16, Base64), convertToBase)

import Data.Map.Strict qualified as Map

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (
    Artifact (..),
    ArtifactKind (Tarball),
    Availability (Available),
    CodeExecSignal (NoCodeOnInstall),
    Hash,
    HashAlg (SHA256),
    PackageDetails (..),
    PackageInfo (..),
    PackageName,
    Trust (Untrusted),
    mkHash,
    mkPackageName,
    mkSriHashes,
 )
import Ecluse.Core.Package.Integrity (
    MinIntegrity,
    MinTrustedIntegrity,
    mkMinIntegrity,
    mkMinTrustedIntegrity,
 )
import Ecluse.Core.Registry.CachedDocument (npmCached)
import Ecluse.Core.Registry.Metadata (Manifest (Manifest, manifestDigest, manifestInfo, manifestRaw), digestOf)
import Ecluse.Core.Security.Egress (RegistryUrl, mkRegistryUrl)
import Ecluse.Core.Server.Path (Filename, mkFilename)
import Ecluse.Core.Version (Version, mkVersion, renderVersion)

{- HLINT ignore unsafeHash "Avoid restricted function" -}

{- | Build a 'Hash' from a known-valid digest, for fixtures. A malformed digest errors,
so a fixture typo fails loudly instead of silently yielding nothing.
-}
unsafeHash :: HashAlg -> Text -> Hash
unsafeHash alg = either error id . mkHash alg

{- HLINT ignore unsafeSriHashes "Avoid restricted function" -}

{- | Split a known-valid Subresource-Integrity wire string into its per-component
hashes, for fixtures. A malformed component errors, so a fixture typo fails loudly.
-}
unsafeSriHashes :: Text -> NonEmpty Hash
unsafeSriHashes = either error id . mkSriHashes

{- HLINT ignore unsafeRegistryUrl "Avoid restricted function" -}

{- | Build the https egress witness from a known-https fixture URL. A non-https value
errors, so a typo fails loudly.
-}
unsafeRegistryUrl :: Text -> RegistryUrl
unsafeRegistryUrl = either error id . mkRegistryUrl

{- HLINT ignore unsafeFilename "Avoid restricted function" -}

{- | Refine a known-safe fixture string into an artifact 'Filename'. An unsafe path component
errors, so a fixture typo fails loudly.
-}
unsafeFilename :: Text -> Filename
unsafeFilename raw = fromMaybe (error ("unsafe fixture filename: " <> raw)) (mkFilename raw)

{- HLINT ignore defaultMinIntegrity "Avoid restricted function" -}

{- | The SHA-256 public-integrity floor fixture: the hard minimum 'mkMinIntegrity' enforces, so
the construction cannot fail. Use it wherever the floor is not the axis under test.
-}
defaultMinIntegrity :: MinIntegrity
defaultMinIntegrity = either error id (mkMinIntegrity SHA256)

{- HLINT ignore defaultMinTrustedIntegrity "Avoid restricted function" -}

{- | The SHA-256 trusted-integrity floor fixture, the same secure posture as 'defaultMinIntegrity'.
SHA-256 names a concrete algorithm, so 'mkMinTrustedIntegrity' cannot fail here.
-}
defaultMinTrustedIntegrity :: MinTrustedIntegrity
defaultMinTrustedIntegrity = either error id (mkMinTrustedIntegrity SHA256)

-- | Lower-case hexadecimal digests for fixture bytes, named by algorithm.
hexSha1Of, hexSha256Of, hexSha384Of, hexSha512Of, hexBlake2bOf :: ByteString -> Text
hexSha1Of bytes = renderBase Base16 (hash bytes :: Digest SHA1)
hexSha256Of bytes = renderBase Base16 (hash bytes :: Digest SHA256)
hexSha384Of bytes = renderBase Base16 (hash bytes :: Digest SHA384)
hexSha512Of bytes = renderBase Base16 (hash bytes :: Digest SHA512)
hexBlake2bOf bytes = renderBase Base16 (hash bytes :: Digest Blake2b_512)

-- | npm-compatible SRI components for fixture bytes, named by algorithm.
sriSha256Of, sriSha384Of, sriSha512Of :: ByteString -> Text
sriSha256Of bytes = renderSri "sha256-" (hash bytes :: Digest SHA256)
sriSha384Of bytes = renderSri "sha384-" (hash bytes :: Digest SHA384)
sriSha512Of bytes = renderSri "sha512-" (hash bytes :: Digest SHA512)

-- | Chunk-preserving variants for the load harness's payload-sized fixture.
hexSha1OfLazy, sriSha512OfLazy :: LByteString -> Text
hexSha1OfLazy bytes = renderBase Base16 (hashlazy bytes :: Digest SHA1)
sriSha512OfLazy bytes = renderSri "sha512-" (hashlazy bytes :: Digest SHA512)

renderSri :: (ByteArrayAccess digest) => Text -> digest -> Text
renderSri prefix digest = prefix <> renderBase Base64 digest

renderBase :: (ByteArrayAccess digest) => Base -> digest -> Text
renderBase base digest = decodeUtf8 (convertToBase base digest :: ByteString)

{- | Canonical well-formed digests, each the empty-input digest of its algorithm, so every fixture
'Hash' is 'mkHash'-constructible. The values are immaterial beyond being well-formed.
-}
validSha1, validSha256, validMd5, validBlake2b :: Text
validSha1 = "da39a3ee5e6b4b0d3255bfef95601890afd80709"
validSha256 = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
validMd5 = "d41d8cd98f00b204e9800998ecf8427e"
validBlake2b = "786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce"

{- | The canonical well-formed SRI digest fixtures (sha256 \/ sha384 \/ sha512),
each the empty-input digest of its inner algorithm.
-}
validSha256Sri, validSha384Sri, validSha512Sri :: Text
validSha256Sri = "sha256-47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU="
validSha384Sri = "sha384-OLBgp1GsljhM2TJ+sbHjaiH9txEUvgdDTAzHv2P24donTt6/529l+9Ua0vFImLlb"
validSha512Sri = "sha512-z4PhNX7vuL3xVChQ1m2AB9Yg5AULVxXcg/SpIdNs6c5H0NE8XYXysP+DGNKHfuwvY7kxvUdBeoGlODJ6+SfaPg=="

-- | An unscoped npm package name, the identity most fixtures carry.
unscopedNpm :: Text -> PackageName
unscopedNpm = mkPackageName Npm Nothing

-- | The conventional fixture package and version, @thing\@1.0.0@.
thingName :: PackageName
thingName = unscopedNpm "thing"

v1_0_0 :: Version
v1_0_0 = mkVersion Npm "1.0.0"

-- | A single inert artifact. The packument-level tests do not inspect artifacts.
sampleArtifact :: Artifact
sampleArtifact =
    Artifact
        { artFilename = "thing-1.0.0.tgz"
        , artUrl = "https://example.test/thing-1.0.0.tgz"
        , artKind = Tarball
        , artHashes = []
        , artSize = Nothing
        , artInterpreter = Nothing
        , artYanked = False
        , artProvenance = Nothing
        }

{- | A minimal per-version snapshot. Only the name and version carry meaning, and the other fields
are inert defaults.
-}
sampleDetails :: PackageName -> Version -> PackageDetails
sampleDetails name version =
    PackageDetails
        { pkgName = name
        , pkgVersion = version
        , pkgPublishedAt = Nothing
        , pkgInstallCode = NoCodeOnInstall
        , pkgTrust = Untrusted
        , pkgAvailability = Available
        , pkgArtifacts = sampleArtifact :| []
        , pkgLicenses = []
        , pkgPublisher = Nothing
        }

-- | 'sampleArtifact' carrying the given integrity digests, so a test varies only integrity.
artifactWith :: [Hash] -> Artifact
artifactWith hs = sampleArtifact{artHashes = hs}

-- | 'sampleDetails' whose sole artifact carries the given integrity digests.
detailsWith :: PackageName -> Version -> [Hash] -> PackageDetails
detailsWith name version hs = (sampleDetails name version){pkgArtifacts = artifactWith hs :| []}

{- | A full manifest carrying the named versions, for a read boundary that answers one. Only the
typed view carries meaning: the raw document is empty, because no case here re-serialises it.
-}
sampleManifest :: PackageName -> [Version] -> Manifest
sampleManifest name versions =
    Manifest
        { manifestInfo =
            PackageInfo
                { infoName = name
                , infoVersions = Map.fromList [(renderVersion v, sampleDetails name v) | v <- versions]
                , infoDistTags = Map.empty
                , infoInvalidEntries = []
                }
        , manifestRaw = fst npmCached (Object mempty)
        , manifestDigest = digestOf ""
        }
