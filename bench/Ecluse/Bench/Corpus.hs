-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The input corpus for the work-per-request benchmarks.

Two sources feed the benches, with distinct jobs:

  * a __curated real-world packument corpus__ ('corpus') -- a pinned set of real npm
    captures of substantial, many-version packages spanning the medium (@lodash@,
    @request@) to heavy (@typescript@, @\@types\/node@, an @aws-sdk@-class package)
    size\/shape spectrum, so the work-per-request figures sample the real distribution
    of large package sizes and shapes rather than one anchor (trivial few-version
    packages stress nothing, so they are deliberately excluded). The captures live under
    @bench\/corpus\/npm\/@ (plus the
    pre-existing untrimmed @express@ anchor reused in place under
    @core\/test\/unit\/fixtures\/npm\/@); they are __frozen data__ -- committed captures
    pinned in @bench\/corpus\/pins.json@ and re-captured deliberately with
    @make gen-bench-corpus@ (not dependency-tracked; see
    @docs\/architecture\/performance.md@). Each retains its real heterogeneous shape --
    varied dependency sets, @peerDependencies@\/@engines@\/@deprecated@, many
    @dist-tags@, large per-version manifests -- trimmed only of pure noise; and

  * a __synthetic packument generator__, 'syntheticPackumentValue', which builds an
    npm full-metadata document with an arbitrary number of versions so a bench can
    scale version count up to the order of @100k@ and a complexity assertion can fit
    the curve. It is retained __only__ for the complexity-scaling (O(n) fit) case -- a
    stress input, not a realistic one: its versions are structurally identical, so it
    is deliberately degenerate where the real corpus is heterogeneous.

The generator emits a genuine npm-shaped 'Value' -- name, @dist-tags@, a @versions@
object, @time@, and @maintainers@ -- so it round-trips through the real wire decode
("Ecluse.Core.Registry.Npm.Wire"), the projection ("Ecluse.Core.Registry.Npm.Project"),
and the serve-time URL rewrite ("Ecluse.Core.Registry.Npm.Filter"). Its invariants are
checked by the benchmark's own test cases (see @bench\/Main.hs@), so a malformed
generator fails the run rather than silently benching a degenerate input.
-}
module Ecluse.Bench.Corpus (
    -- * The curated real-world corpus
    CorpusEntry (..),
    CorpusTier (..),
    corpus,
    loadCorpus,
    LoadedEntry,
    withLoaded,
    entryInfo,
    entryName,

    -- * Reading a committed fixture
    fixtureBytes,

    -- * Inspecting a packument value
    versionKeysOf,

    -- * Synthetic packument generator (complexity-scaling only)
    syntheticPackumentValue,
    syntheticPackumentBytes,
    syntheticProxyBase,
    benchPackageText,
    benchPackageName,

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
    PackageInfo (PackageInfo, infoDistTags, infoInvalidEntries, infoName, infoVersions),
    PackageName,
    mkPackageName,
    mkScope,
 )
import Ecluse.Core.Registry.Npm.Project (Projection (NameMismatch, Projected), parsePackageInfoFromValue)
import Ecluse.Core.Rules.Types (
    EvalContext (EvalContext),
    PrecededRule,
    Rule (AllowIfOlderThan, AllowScope, DenyInstallTimeExecution),
 )
import Ecluse.Test.Package (validSha1, validSha512Sri)
import Ecluse.Test.Rules (atDefaultPrecedence)

{- | A size\/shape tier for a corpus entry, ordering the corpus small-to-heavy and
labelling the rendered benchmark groups so a reader can see where on the distribution
a figure sits.
-}
data CorpusTier = Medium | Large | Heavy
    deriving stock (Eq, Show)

{- | One curated real-world packument capture: the name the projection validates the
capture against, the file it was captured to, and its size tier.
-}
data CorpusEntry = CorpusEntry
    { ceLabel :: Text
    -- ^ The package's display label (e.g. @"express"@ or @"\@types\/node"@).
    , cePackage :: PackageName
    -- ^ The requested name the projection validates the capture's self-reported name against.
    , cePath :: FilePath
    -- ^ The capture's path, relative to the package root Cabal runs the benchmark from.
    , ceTier :: CorpusTier
    -- ^ The entry's size\/shape tier.
    }

{- | Where the curated corpus captures live, relative to the package root Cabal runs
the benchmark from (the same package-root-relative convention the unit suite reads its
fixtures by).
-}
corpusRoot :: FilePath
corpusRoot = "bench/corpus/npm/"

{- | The curated corpus, ordered small-to-heavy. Every entry but @express@ is a pinned
capture under @bench\/corpus\/npm\/@ (refreshed by @make gen-bench-corpus@); @express@
is the pre-existing untrimmed anchor under @core\/test\/unit\/fixtures\/npm\/@, reused
in place and shared with the unit suite.
-}
corpus :: [CorpusEntry]
corpus =
    [ entry Medium "lodash" (unscoped "lodash") (corpusRoot <> "lodash.full.json")
    , entry Medium "request" (unscoped "request") (corpusRoot <> "request.full.json")
    , entry Large "@babel/core" (scoped "babel" "core") (corpusRoot <> "babel-core.full.json")
    , entry Large "express" (unscoped "express") "core/test/unit/fixtures/npm/express.full.json"
    , entry Large "react" (unscoped "react") (corpusRoot <> "react.full.json")
    , entry Large "typescript" (unscoped "typescript") (corpusRoot <> "typescript.full.json")
    , entry Heavy "@aws-sdk/client-s3" (scoped "aws-sdk" "client-s3") (corpusRoot <> "aws-sdk-client-s3.full.json")
    , entry Heavy "webpack" (unscoped "webpack") (corpusRoot <> "webpack.full.json")
    , entry Heavy "@types/node" (scoped "types" "node") (corpusRoot <> "types-node.full.json")
    ]
  where
    entry tier label name path = CorpusEntry{ceLabel = label, cePackage = name, cePath = path, ceTier = tier}
    unscoped = mkPackageName Npm Nothing
    scoped s = mkPackageName Npm (Just (mkScope s))

-- | A corpus entry paired with its loaded raw bytes and decoded JSON 'Value'.
type LoadedEntry = (CorpusEntry, ByteString, Value)

{- | Load every corpus capture as its raw bytes and decoded 'Value', in 'corpus'
order, for use as a benchmark @env@. Fails loudly if a capture is missing, does not
decode, projects to zero versions, or self-reports a name that does not match its
'cePackage' -- so a corrupt or mis-pinned corpus stops the run rather than benching
nothing. Returns just the loaded pairs (the entry metadata is the pure 'corpus', zipped
back on by 'withLoaded') so the @env@ value needs no @NFData@ beyond the bytes and value.
-}
loadCorpus :: IO [(ByteString, Value)]
loadCorpus = traverse loadOne corpus
  where
    loadOne :: CorpusEntry -> IO (ByteString, Value)
    loadOne ce = do
        raw <- fixtureBytes (cePath ce)
        value <- either (failWith ce "did not decode") pure (Aeson.eitherDecodeStrict raw)
        case parsePackageInfoFromValue (cePackage ce) value of
            Right (Projected info)
                | not (Map.null (infoVersions info)) -> pure (raw, value)
                | otherwise -> fail (label ce <> " projected to zero versions")
            Right (NameMismatch reported) -> fail (label ce <> " self-reports name " <> toString reported)
            Left err -> failWith ce "did not project" (show err)

    failWith :: CorpusEntry -> String -> String -> IO a
    failWith ce what detail = fail (label ce <> " " <> what <> ": " <> detail)

    label :: CorpusEntry -> String
    label ce = "corpus capture " <> toString (ceLabel ce)

{- | Pair the pure corpus metadata back onto the loaded bytes\/values, in order -- the
inverse of the split 'loadCorpus' performs so its @env@ value carries no 'PackageName'.
-}
withLoaded :: [(ByteString, Value)] -> [LoadedEntry]
withLoaded = zipWith (\ce (raw, value) -> (ce, raw, value)) corpus

-- | The projected 'PackageInfo' of a loaded corpus entry, against its requested name.
entryInfo :: LoadedEntry -> PackageInfo
entryInfo (ce, _, value) = projectInfo (cePackage ce) value

-- | A loaded entry's benchmark name: its label tagged with its size tier.
entryName :: LoadedEntry -> String
entryName (ce, _, _) = toString (ceLabel ce) <> " (" <> tierName (ceTier ce) <> ")"
  where
    tierName = \case
        Medium -> "medium"
        Large -> "large"
        Heavy -> "heavy"

-- | Read a committed fixture body by its path relative to the package root, as raw bytes.
fixtureBytes :: FilePath -> IO ByteString
fixtureBytes = readFileBS

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

{- | The proxy base URL the serve-time rewrite benches rewrite tarball URLs onto --
standing in for a deployment's own public origin.
-}
syntheticProxyBase :: Text
syntheticProxyBase = "https://ecluse.example"

{- | Build a synthetic npm packument 'Value' carrying @versionCount@ versions
(@1.0.0@ .. @1.0.{n-1}@), each with a rewritable @dist.tarball@, a well-formed
integrity digest, a small dependency set, and an install script -- the fields the
hot paths actually touch. The document is a faithful npm shape, so it decodes,
projects, filters, and re-serialises exactly as a real packument would, but its
versions are structurally identical: it is the complexity-scaling stress input, not a
realistic one (the real distribution is 'corpus').

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

{- | The version keys of a packument 'Value' -- the keys of its @versions@ object,
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
partial 'error' -- the benchmark's own generator tests and 'loadCorpus' guarantee a
real projection.
-}
projectInfo :: PackageName -> Value -> PackageInfo
projectInfo name value = case parsePackageInfoFromValue name value of
    Right (Projected info) -> info
    _ ->
        PackageInfo
            { infoName = name
            , infoVersions = Map.empty
            , infoDistTags = Map.empty
            , infoInvalidEntries = []
            }

-- | The synthetic packument of the given version count, projected into 'PackageInfo'.
syntheticPackageInfo :: Int -> PackageInfo
syntheticPackageInfo = projectInfo benchPackageName . syntheticPackumentValue

{- | A fixed evaluation context (a wall-clock @now@) for the rule-engine benches, so
the age-based rule is deterministic across runs.
-}
benchEvalContext :: EvalContext
benchEvalContext = EvalContext (UTCTime (fromGregorian 2026 6 27) (secondsToDiffTime 0)) Nothing

{- | A representative rule set spanning all three pure rule types -- an allow-list, an
install-time-execution deny, and an age quarantine -- so the rule sweep exercises
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
