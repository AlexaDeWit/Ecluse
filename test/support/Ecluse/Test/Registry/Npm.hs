-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared npm fixtures: packument and version-object builders, the public-registry
defaults, the mirror-write subject, the name grammar as a table, and the URL
path-segment generators.

'npmNameVerdicts' is the one list every entry point that splits an npm name asserts
against, so a verdict cannot drift between the read path, the route, the URL rewrite,
the publish allow-list, and the queue decode.
-}
module Ecluse.Test.Registry.Npm (
    -- * Packument fixtures
    VersionSpec (..),
    versionSpec,
    versionValue,
    packumentValue,
    publishedDaysAgo,

    -- * Mirror-write fixtures
    isOdd,
    dummyArtifact,

    -- * The npm name grammar, as a shared table
    npmNameVerdicts,
    nameVerdictLabel,

    -- * Client fixtures
    defaultNpmConfig,
    publicRegistryBaseUrl,

    -- * URL path generators
    genPathSegments,
) where

import Data.Aeson (Value (Object), object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.List.NonEmpty qualified as NE
import Data.Time (UTCTime, addUTCTime, nominalDay)
import Data.Time.Format.ISO8601 (iso8601Show)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Network.HTTP.Client (Manager)

import Ecluse.Core.Package (HashAlg (SHA1), PackageName)
import Ecluse.Core.Registry (MirrorArtifact (MirrorArtifact, maFilename, maHashes, maSize))
import Ecluse.Core.Registry.Npm (NpmClientConfig (..))
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Test.Package (unsafeHash, unscopedNpm, validSha1)

{- | Each npm name a splitter must agree on, paired with whether it names a package. A bare
@\@foo@ is a malformed scoped name, not an unscoped one, so it is refused everywhere.
-}
npmNameVerdicts :: [(Text, Bool)]
npmNameVerdicts =
    [ ("@scope/pkg", True)
    , ("pkg", True)
    , ("@foo", False)
    , ("@foo/", False)
    , ("@/pkg", False)
    , ("@scope/a/b", False)
    , ("@../pkg", False)
    , ("", False)
    , ("@scope/p@g", False)
    , ("a b", False)
    , -- Outside the ASCII boundary: a Hangul filler, a blank braille cell, a variation selector,
      -- and a DEL. None is a format character, so a deny-list on that class admits every one.
      ("pkg\x3164", False)
    , ("pkg\x2800", False)
    , ("pkg\xFE0F", False)
    , ("pkg\x7F", False)
    , ("@sco\x3164\&pe/pkg", False)
    , -- Inside the boundary and outside npm's error tier: a leading separator, a reserved name,
      -- and a character @encodeURIComponent@ would escape.
      (".pkg", False)
    , ("-pkg", False)
    , ("_pkg", False)
    , ("node_modules", False)
    , ("favicon.ico", False)
    , ("@node_modules/pkg", False)
    , ("100%", False)
    , -- npm's warning tier is legacy-valid, so a real name built from it still parses.
      ("JSONStream", True)
    , ("vue~cli!(1)*'", True)
    ]

-- | The example name for one 'npmNameVerdicts' row, so a failure names the input and its verdict.
nameVerdictLabel :: Text -> Bool -> String
nameVerdictLabel raw valid = show raw <> (if valid then " names a package" else " is refused")

-- | The package the registry-client and mirror-write specs address.
isOdd :: PackageName
isOdd = unscopedNpm "is-odd"

{- | The artifact descriptor a mirror write carries for @is-odd\@1.0.0@. Only the filename and
digests reach the publish document, so the size stays absent unless a case sets it.
-}
dummyArtifact :: MirrorArtifact
dummyArtifact =
    MirrorArtifact
        { maFilename = "is-odd-1.0.0.tgz"
        , maHashes = NE.singleton (unsafeHash SHA1 validSha1)
        , maSize = Nothing
        }

{- | The common fields of an npm version object. An extra pair in 'vsExtraPairs' overrides the
common field with the same key.
-}
data VersionSpec = VersionSpec
    { vsName :: Text
    -- ^ The package name self-reported by the version object.
    , vsVersion :: Text
    -- ^ The version string self-reported by the version object.
    , vsTarballUrl :: Text
    -- ^ The upstream artifact URL under @dist.tarball@.
    , vsIntegrity :: Maybe Text
    -- ^ The optional Subresource Integrity value under @dist.integrity@.
    , vsShasum :: Maybe Text
    -- ^ The optional legacy SHA-1 value under @dist.shasum@.
    , vsHasInstallScript :: Bool
    -- ^ Whether to include a representative @scripts.postinstall@ entry.
    , vsExtraPairs :: [Pair]
    -- ^ Site-specific version fields, applied after the common fields.
    }
    deriving stock (Eq, Show)

{- | Start a version fixture with its identity and tarball URL. Optional digests,
install scripts, and site-specific fields stay absent until the caller opts into them.
-}
versionSpec :: Text -> Text -> Text -> VersionSpec
versionSpec name version tarballUrl =
    VersionSpec
        { vsName = name
        , vsVersion = version
        , vsTarballUrl = tarballUrl
        , vsIntegrity = Nothing
        , vsShasum = Nothing
        , vsHasInstallScript = False
        , vsExtraPairs = []
        }

{- | Build the npm version-object shape the test suites share. An unspecified digest
field is absent, matching registry metadata instead of encoding it as @null@.
-}
versionValue :: VersionSpec -> Value
versionValue spec =
    objectWithExtraPairs
        [ "name" .= vsName spec
        , "version" .= vsVersion spec
        , "dist"
            .= object
                ( ["tarball" .= vsTarballUrl spec]
                    <> maybe [] (pure . ("integrity" .=)) (vsIntegrity spec)
                    <> maybe [] (pure . ("shasum" .=)) (vsShasum spec)
                )
        ]
        ( ["scripts" .= object ["postinstall" .= ("node build.js" :: Text)] | vsHasInstallScript spec]
            <> vsExtraPairs spec
        )

{- | Build an npm packument from its decision-bearing fields. The caller supplies the complete
@time@ map because created and modified bookkeeping differs by fixture.
-}
packumentValue ::
    -- | The package name self-reported by the packument.
    Text ->
    -- | The target of @dist-tags.latest@.
    Text ->
    -- | Version keys paired with their version objects.
    [(Text, Value)] ->
    -- | The complete contents of the top-level @time@ object.
    [Pair] ->
    -- | Site-specific top-level fields, applied after the common fields.
    [Pair] ->
    Value
packumentValue name latest versions times =
    objectWithExtraPairs
        [ "name" .= name
        , "dist-tags" .= object ["latest" .= latest]
        , "versions" .= object [(Key.fromText version, value) | (version, value) <- versions]
        , "time" .= object times
        ]

{- | Render an npm @time@ instant the given number of whole days before the caller's
fixture clock.
-}
publishedDaysAgo :: UTCTime -> Integer -> Text
publishedDaysAgo now ageDays =
    toText (iso8601Show (addUTCTime (negate (fromInteger ageDays * nominalDay)) now))

-- Apply site-specific fields last so their exact representation wins.
objectWithExtraPairs :: [Pair] -> [Pair] -> Value
objectWithExtraPairs common extra =
    Object (foldl' insertPair (KeyMap.fromList common) extra)
  where
    insertPair fields (key, value) = KeyMap.insert key value fields

{- | The canonical public npm registry base URL, @https://registry.npmjs.org@.
The default target when no managed backend is configured.
-}
publicRegistryBaseUrl :: Text
publicRegistryBaseUrl = "https://registry.npmjs.org"

{- | An anonymous client config against the public registry at the secure-default response
bounds. Override 'npmBaseUrl', 'npmToken', or 'npmLimits' for a managed backend.
-}
defaultNpmConfig :: Manager -> NpmClientConfig
defaultNpmConfig manager =
    NpmClientConfig
        { npmBaseUrl = publicRegistryBaseUrl
        , npmManager = manager
        , npmToken = Nothing
        , npmLimits = defaultLimits
        }

-- | A URL path of arbitrary segments, at the length a router's property explores.
genPathSegments :: Gen [Text]
genPathSegments = Gen.list (Range.linear 0 4) genPathSegment

{- | One path segment. The weights keep every arm a router property samples: plain names,
scoped names, the literal pool, and free text over the punctuation a path carries.
-}
genPathSegment :: Gen Text
genPathSegment =
    Gen.frequency
        [ (5, genSegmentName)
        , (2, genScopedSegmentName)
        , (4, Gen.element pathSegmentPool)
        , (4, Gen.text (Range.linear 0 8) (Gen.element segmentChars))
        ]

genSegmentName :: Gen Text
genSegmentName = Gen.text (Range.linear 1 8) (Gen.frequency [(8, Gen.alphaNum), (2, Gen.element ['.', '-', '_'])])

genScopedSegmentName :: Gen Text
genScopedSegmentName = do
    scope <- genSegmentName
    Gen.choice
        [ pure ("@" <> scope)
        , (\base -> "@" <> scope <> "/" <> base) <$> genSegmentName
        ]

-- Traversal, separator, control, and extension fragments a segment must never be trusted with,
-- alongside the real names and route words a table has to keep matching.
pathSegmentPool :: [Text]
pathSegmentPool =
    [ ""
    , "."
    , ".."
    , "-"
    , "-foo"
    , "@"
    , "@/x"
    , "@scope"
    , "@scope/"
    , "@scope/pkg"
    , "a/b"
    , "a\\b"
    , "a\tb"
    , "a\0b"
    , "../evil.tgz"
    , "x.tgz"
    , "foo/bar"
    , "lodash"
    , "pkg"
    , "ping"
    , "v1"
    , "search"
    , "lodash-1.0.0.tgz"
    , "code-frame-7.0.0.tgz"
    ]

segmentChars :: String
segmentChars = ['a', 'b', 'c', 'n', 'p', 'm', '@', '-', '/', '.', '%', ' ', '1', '2', '3', '4']
