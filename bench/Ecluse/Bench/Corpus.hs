{- | The realistic input corpus for the work-per-request benchmarks.

Two sources feed the benches:

  * the committed npm fixtures under @core\/test\/unit\/fixtures\/npm\/@ — the same
    real captures the unit suite decodes, including the large untrimmed
    @express.full.json@ packument — located at runtime by the same package-root
    relative path the suite uses (Cabal runs both from the package root); and

  * a __synthetic packument generator__, 'syntheticPackumentValue', which builds an
    npm full-metadata document with an arbitrary number of versions so a bench can
    scale version count up to the order of @100k@ and a complexity assertion can fit
    the curve.

The generator emits a genuine npm-shaped 'Value' — name, @dist-tags@, a @versions@
object, @time@, and @maintainers@ — so it round-trips through the real wire decode
("Ecluse.Core.Registry.Npm.Wire"), the projection ("Ecluse.Core.Registry.Npm.Project"),
and the serve-time URL rewrite ("Ecluse.Core.Registry.Npm.Filter"). Its invariants are
checked by the benchmark's own test cases (see @bench\/Main.hs@), so a malformed
generator fails the run rather than silently benching a degenerate input.
-}
module Ecluse.Bench.Corpus (
    -- * Committed fixtures
    fixtureBytes,
    expressBytes,
    loadExpress,
    expressPackageName,

    -- * Package identity
    benchPackageText,
    benchPackageName,

    -- * Inspecting a packument value
    versionKeysOf,

    -- * Synthetic packument generator
    syntheticPackumentValue,
    syntheticPackumentBytes,
    syntheticProxyBase,

    -- * Projecting into the agnostic core types
    projectInfo,
    syntheticPackageInfo,

    -- * Shared rule-engine inputs
    benchEvalContext,
    benchRules,

    -- * Encoding
    encodeStrict,
) where

import Data.Aeson (Value (Object), object, toJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime (UTCTime), fromGregorian, nominalDay, secondsToDiffTime)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (
    PackageInfo (PackageInfo, infoDistTags, infoName, infoPublishedAt, infoVersions),
    PackageName,
    mkPackageName,
    mkScope,
 )
import Ecluse.Core.Registry.Npm.Project (Projection (Projected), parsePackageInfoFromValue)
import Ecluse.Core.Rules.Types (
    EvalContext (EvalContext),
    PrecededRule,
    Rule (AllowIfOlderThan, AllowScope, DenyInstallTimeExecution),
    atDefaultPrecedence,
 )
import Ecluse.Test.Package (validSha1, validSha512Sri)

{- | Where the committed npm fixtures live, relative to the package root Cabal
runs the benchmark from (the same path the unit suite reads them by).
-}
fixtureRoot :: FilePath
fixtureRoot = "core/test/unit/fixtures/npm/"

{- | Read a committed npm fixture body by file name (under
@core\/test\/unit\/fixtures\/npm\/@), as raw bytes.
-}
fixtureBytes :: FilePath -> IO ByteString
fixtureBytes name = readFileBS (fixtureRoot <> name)

{- | The real, untrimmed @express@ packument — a large legitimate packument
(hundreds of versions) that anchors the realistic end of the corpus.
-}
expressBytes :: IO ByteString
expressBytes = fixtureBytes "express.full.json"

{- | Load the @express@ packument as both its raw bytes and its decoded JSON
'Value', for use as a benchmark @env@. Fails loudly if the committed fixture does
not decode, so a corrupt corpus stops the run rather than benching nothing.
-}
loadExpress :: IO (ByteString, Value)
loadExpress = do
    raw <- expressBytes
    json <- either (fail . ("express fixture did not decode: " <>)) pure (Aeson.eitherDecodeStrict raw)
    pure (raw, json)

{- | The @express@ packument's own name, as an npm 'PackageName' — the requested
name the projection benches match the fixture against.
-}
expressPackageName :: PackageName
expressPackageName = mkPackageName Npm Nothing "express"

{- | The package name the synthetic generator labels its document with. Chosen so
every structural component is safe to interpolate into a rewritten tarball path
(see "Ecluse.Core.Registry.Npm.Filter"), so the serve-time rewrite exercises the real
path rather than bailing out.
-}
benchPackageText :: Text
benchPackageText = "bench-pkg"

-- | 'benchPackageText' as an unscoped npm 'PackageName', for the projection benches.
benchPackageName :: PackageName
benchPackageName = mkPackageName Npm Nothing benchPackageText

{- | The proxy base URL the serve-time rewrite benches rewrite tarball URLs onto —
standing in for a deployment's own public origin.
-}
syntheticProxyBase :: Text
syntheticProxyBase = "https://ecluse.example"

{- | Build a synthetic npm packument 'Value' carrying @versionCount@ versions
(@1.0.0@ .. @1.0.{n-1}@), each with a rewritable @dist.tarball@, a well-formed
integrity digest, a small dependency set, and an install script — the fields the
hot paths actually touch. The document is a faithful npm shape, so it decodes,
projects, filters, and re-serialises exactly as a real packument would.

@versionCount@ is expected to be positive; the benches only ever pass positive
sizes.
-}
syntheticPackumentValue :: Int -> Value
syntheticPackumentValue versionCount =
    object
        [ "name" .= benchPackageText
        , "dist-tags" .= object ["latest" .= versionText (max 0 (versionCount - 1))]
        , "versions" .= Object (KeyMap.fromList [(versionKeyOf i, versionObject i) | i <- indices])
        , "time" .= Object (KeyMap.fromList timeEntries)
        , "maintainers" .= toJSON [object ["name" .= ("ecluse-bench" :: Text)]]
        ]
  where
    indices :: [Int]
    indices = [0 .. versionCount - 1]

    versionKeyOf :: Int -> Key.Key
    versionKeyOf = Key.fromText . versionText

    timeEntries :: [(Key.Key, Value)]
    timeEntries =
        (Key.fromText "created", toJSON publishedAt)
            : (Key.fromText "modified", toJSON publishedAt)
            : [(versionKeyOf i, toJSON publishedAt) | i <- indices]

-- | A synthetic version string: @1.0.{i}@, valid npm semver for every @i >= 0@.
versionText :: Int -> Text
versionText i = "1.0." <> show i

-- | A fixed, well-formed publish timestamp shared by every synthetic version.
publishedAt :: Text
publishedAt = "2020-01-01T00:00:00.000Z"

-- | One synthetic version manifest, with the fields the projection and serve paths read.
versionObject :: Int -> Value
versionObject i =
    object
        [ "name" .= benchPackageText
        , "version" .= versionText i
        , "dist"
            .= object
                [ "tarball" .= tarballUrl i
                , "integrity" .= validSha512Sri
                , "shasum" .= validSha1
                ]
        , "dependencies"
            .= object
                [ "left-pad" .= ("^1.0.0" :: Text)
                , "lodash" .= ("^4.17.0" :: Text)
                ]
        , "scripts" .= object ["postinstall" .= ("node ./build.js" :: Text)]
        ]

-- | The upstream tarball URL a synthetic version reports, before the serve rewrite.
tarballUrl :: Int -> Text
tarballUrl i =
    "https://registry.npmjs.org/"
        <> benchPackageText
        <> "/-/"
        <> benchPackageText
        <> "-"
        <> versionText i
        <> ".tgz"

{- | The version keys of a packument 'Value' — the keys of its @versions@ object,
in 'KeyMap' order. Empty for a value that is not an object with a @versions@ object.
-}
versionKeysOf :: Value -> [Text]
versionKeysOf = \case
    Object o -> case KeyMap.lookup "versions" o of
        Just (Object versions) -> map Key.toText (KeyMap.keys versions)
        _ -> []
    _ -> []

-- | 'syntheticPackumentValue' encoded to the strict JSON bytes a registry would return.
syntheticPackumentBytes :: Int -> ByteString
syntheticPackumentBytes = encodeStrict . syntheticPackumentValue

{- | Project a packument 'Value' into the agnostic 'PackageInfo' for the named
package. A value that does not project (a tested-impossible case for the corpus
here) yields the empty document for that name, so the function stays total without a
partial 'error' — the benchmark's own generator tests guarantee a real projection.
-}
projectInfo :: PackageName -> Value -> PackageInfo
projectInfo name value = case parsePackageInfoFromValue name value of
    Right (Projected info) -> info
    _ ->
        PackageInfo
            { infoName = name
            , infoVersions = Map.empty
            , infoDistTags = Map.empty
            , infoPublishedAt = Map.empty
            }

-- | The synthetic packument of the given version count, projected into 'PackageInfo'.
syntheticPackageInfo :: Int -> PackageInfo
syntheticPackageInfo = projectInfo benchPackageName . syntheticPackumentValue

{- | A fixed evaluation context (a wall-clock @now@) for the rule-engine benches, so
the age-based rule is deterministic across runs.
-}
benchEvalContext :: EvalContext
benchEvalContext = EvalContext (UTCTime (fromGregorian 2026 6 27) (secondsToDiffTime 0))

{- | A representative rule set spanning all three pure rule types — an allow-list, an
install-time-execution deny, and an age quarantine — so the rule sweep exercises
every evaluation arm rather than one.
-}
benchRules :: [PrecededRule]
benchRules =
    [ atDefaultPrecedence (AllowScope (mkScope "trusted-scope"))
    , atDefaultPrecedence DenyInstallTimeExecution
    , atDefaultPrecedence (AllowIfOlderThan (30 * nominalDay))
    ]

-- | Encode a 'Value' to a strict 'ByteString'.
encodeStrict :: Value -> ByteString
encodeStrict = BSL.toStrict . Aeson.encode
