-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The benchmark inputs: loading the curated corpus ("Ecluse.Test.Corpus"), and the
synthetic packument generator beside it.

'syntheticPackumentValue' builds an npm document with an arbitrary version count, so a
bench can scale to the order of @100k@ and fit the curve. Its versions are structurally
identical, so it serves __only__ that complexity-scaling case. It stays npm-shaped, so it
round-trips the real decode, projection, and rewrite, and @bench\/Main.hs@ tests that.
-}
module Ecluse.Bench.Corpus (
    -- * The curated real-world corpus
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
import Ecluse.Test.Corpus (CorpusPackage (cpPackage, cpPath, cpTier), CorpusTier (Heavy, Large, Medium), corpusPackages, cpName)
import Ecluse.Test.Package (validSha1, validSha512Sri)
import Ecluse.Test.Registry.Npm (VersionSpec (..), packumentValue, versionSpec, versionValue)
import Ecluse.Test.Rules (atDefaultPrecedence)

-- | The curated corpus small-to-heavy, the order the rendered benchmark groups read in.
corpus :: [CorpusPackage]
corpus = reverse corpusPackages

-- | A corpus package paired with its loaded raw bytes and decoded JSON 'Value'.
type LoadedEntry = (CorpusPackage, ByteString, Value)

{- | Load every corpus capture as raw bytes and a decoded 'Value', in 'corpus' order, for a
benchmark @env@. It fails loudly on a missing, undecodable, empty, or mis-named capture, so a
corrupt corpus stops the run rather than benching nothing.
-}
loadCorpus :: IO [(ByteString, Value)]
loadCorpus = traverse loadOne corpus
  where
    loadOne :: CorpusPackage -> IO (ByteString, Value)
    loadOne cp = do
        raw <- fixtureBytes (cpPath cp)
        value <- either (failWith cp "did not decode") pure (Aeson.eitherDecodeStrict raw)
        case parsePackageInfoFromValue (cpPackage cp) value of
            Right (Projected info)
                | not (Map.null (infoVersions info)) -> pure (raw, value)
                | otherwise -> fail (label cp <> " projected to zero versions")
            Right (NameMismatch reported) -> fail (label cp <> " self-reports name " <> toString reported)
            Left err -> failWith cp "did not project" (show err)

    failWith :: CorpusPackage -> String -> String -> IO a
    failWith cp what detail = fail (label cp <> " " <> what <> ": " <> detail)

    label :: CorpusPackage -> String
    label cp = "corpus capture " <> toString (cpName cp)

{- | Pair the pure corpus metadata back onto the loaded bytes\/values, in order. This
inverts the split 'loadCorpus' performs to keep 'PackageName' out of its @env@ value.
-}
withLoaded :: [(ByteString, Value)] -> [LoadedEntry]
withLoaded = zipWith (\cp (raw, value) -> (cp, raw, value)) corpus

-- | The projected 'PackageInfo' of a loaded corpus entry, against its requested name.
entryInfo :: LoadedEntry -> PackageInfo
entryInfo (cp, _, value) = projectInfo (cpPackage cp) value

-- | A loaded entry's benchmark name: its package name tagged with its size tier.
entryName :: LoadedEntry -> String
entryName (cp, _, _) = toString (cpName cp) <> " (" <> tierName (cpTier cp) <> ")"
  where
    tierName = \case
        Medium -> "medium"
        Large -> "large"
        Heavy -> "heavy"

-- | Read a committed fixture body by its path relative to the package root, as raw bytes.
fixtureBytes :: FilePath -> IO ByteString
fixtureBytes = readFileBS

{- | The name the synthetic generator labels its document with. It is safe to interpolate
into a rewritten tarball path (see "Ecluse.Core.Registry.Npm.Filter"), so the rewrite runs.
-}
benchPackageText :: Text
benchPackageText = "bench-pkg"

-- | 'benchPackageText' as an unscoped npm 'PackageName', for the projection benches.
benchPackageName :: PackageName
benchPackageName = mkPackageName Npm Nothing benchPackageText

{- | Build a synthetic npm packument 'Value' with @versionCount@ versions (@1.0.0@ ..
@1.0.{n-1}@), each carrying the fields the hot paths touch. Its versions are structurally
identical, so it is the complexity-scaling stress input and 'corpus' is the realistic one.
@versionCount@ must be positive.
-}
syntheticPackumentValue :: Int -> Value
syntheticPackumentValue versionCount =
    packumentValue
        benchPackageText
        (versionText (max 0 (versionCount - 1)))
        [(versionText i, versionObject i) | i <- indices]
        timeEntries
        ["maintainers" .= toJSON [object ["name" .= ("ecluse-bench" :: Text)]]]
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
    versionValue
        ( (versionSpec benchPackageText (versionText i) (tarballUrl i))
            { vsIntegrity = Just validSha512Sri
            , vsShasum = Just validSha1
            , vsHasInstallScript = True
            , vsExtraPairs =
                [ "dependencies"
                    .= object
                        [ "left-pad" .= ("^1.0.0" :: Text)
                        , "lodash" .= ("^4.17.0" :: Text)
                        ]
                , "scripts" .= object ["postinstall" .= ("node ./build.js" :: Text)]
                ]
            }
        )

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

{- | The version keys of a packument 'Value': the keys of its @versions@ object, in
'KeyMap' order. Empty for a value that is not an object with a @versions@ object.
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

{- | Project a packument 'Value' into 'PackageInfo' for the named package. A value that does
not project yields the empty document, which keeps the function total without a partial 'error'.
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

{- | A representative rule set spanning all three pure rule types, so the rule sweep
exercises every evaluation arm rather than one.
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
