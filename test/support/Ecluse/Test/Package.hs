{- | Test helpers and fixtures for "Ecluse.Core.Package".

This mirrors the module under test: helpers that support exercising
@Ecluse.Core.Package@ live here, under the @Ecluse.X → Ecluse.Test.X@ convention this
support library follows. It carries the digest plumbing every suite reuses --
'unsafeHash', which lifts a known-good digest into a 'Hash', and the canonical
well-formed digest fixtures (each the empty-input digest of its algorithm) that
appear across the queue, integrity, env, and worker specs.
-}
module Ecluse.Test.Package (
    -- * Constructing hashes from fixtures
    unsafeHash,

    -- * Canonical digest fixtures
    validSha1,
    validSha256,
    validSha384Hex,
    validSha512Hex,
    validMd5,
    validBlake2b,
    validSha256Sri,
    validSha384Sri,
    validSha512Sri,

    -- * Shared fixtures
    sampleArtifact,
    sampleDetails,
) where

import Ecluse.Core.Package (
    Artifact (..),
    ArtifactKind (Tarball),
    Availability (Available),
    CodeExecSignal (NoCodeOnInstall),
    Hash,
    HashAlg,
    PackageDetails (..),
    PackageName,
    Trust (Untrusted),
    mkHash,
 )
import Ecluse.Core.Version (Version)

{- HLINT ignore unsafeHash "Avoid restricted function" -}

{- | Build a 'Hash' from a known-valid digest, for fixtures. Errors on a malformed
digest, so a typo in a fixture fails loudly rather than silently constructing
nothing.
-}
unsafeHash :: HashAlg -> Text -> Hash
unsafeHash alg = either error id . mkHash alg

{- | Canonical well-formed digests -- each the empty-input digest of its algorithm,
so every fixture 'Hash' is 'mkHash'-constructible and survives a validated decode
round-trip. The values are immaterial beyond being well-formed: a suite uses them
wherever a digest must merely parse, not match any particular bytes.
-}
validSha1, validSha256, validSha384Hex, validSha512Hex, validMd5, validBlake2b :: Text
validSha1 = "da39a3ee5e6b4b0d3255bfef95601890afd80709"
validSha256 = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
validSha384Hex = "38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b"
validSha512Hex = "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e"
validMd5 = "d41d8cd98f00b204e9800998ecf8427e"
validBlake2b = "786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce"

{- | The canonical well-formed SRI digest fixtures (sha256 \/ sha384 \/ sha512),
each the empty-input digest of its inner algorithm.
-}
validSha256Sri, validSha384Sri, validSha512Sri :: Text
validSha256Sri = "sha256-47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU="
validSha384Sri = "sha384-OLBgp1GsljhM2TJ+sbHjaiH9txEUvgdDTAzHv2P24donTt6/529l+9Ua0vFImLlb"
validSha512Sri = "sha512-z4PhNX7vuL3xVChQ1m2AB9Yg5AULVxXcg/SpIdNs6c5H0NE8XYXysP+DGNKHfuwvY7kxvUdBeoGlODJ6+SfaPg=="

-- | A single inert artifact; the packument-level tests do not inspect artifacts.
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

{- | A minimal per-version snapshot for a given name and version. Only the fields
a 'PackageInfo' threads through (the name and version) are meaningful here; the
rest are inert defaults.
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
