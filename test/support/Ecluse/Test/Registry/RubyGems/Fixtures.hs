-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Genuine RubyGems files retain independent coordinates, advertised digests and original bytes.
module Ecluse.Test.Registry.RubyGems.Fixtures (
    RubyRelease (..),
    rubyRoot,
    rubyLeaf,
    rubyVersion,
    loadRubyReleases,
    loadRubyReleasesFrom,
    rubyRelease,
    writeRubyRegistry,
    rubyVersions,
    rubyVersionsWith,
    loadFixtureInfo,
) where

import Data.Aeson (Value, eitherDecodeStrict, withObject, (.:))
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import System.Directory (copyFile, createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.RubyGems.Document (GemRow)
import Ecluse.Core.Registry.RubyGems.Project (projectName)
import Ecluse.Core.Registry.RubyGems.Wire (decodeInfoRow)
import Ecluse.Test.Package (hexSha256Of)

-- | The manifest comes from Ruby's native tools rather than the production parsers.
data RubyRelease = RubyRelease
    { releaseName :: Text
    , releaseVersion :: Text
    , releasePlatform :: Text
    , releaseFilename :: Text
    , releaseChecksum :: Text
    , releaseArchive :: ByteString
    , releaseCompactPath :: FilePath
    , releaseCompact :: ByteString
    , releaseManifest :: Value
    , releaseInfoRow :: Text
    }
    deriving stock (Eq, Ord, Show)

-- | The graph root supplies an executable and a requireable module.
rubyRoot :: Text
rubyRoot = "ecluse-fixture-root"

-- | Both root versions resolve the shared transitive leaf under different constraints.
rubyLeaf :: Text
rubyLeaf = "ecluse-fixture-leaf"

-- | The baseline has an old publication timestamp and a stable native identity.
rubyVersion :: Text
rubyVersion = "1.0.0"

-- | A corrupted committed archive or compact specification fails fixture acquisition.
loadRubyReleases :: IO [RubyRelease]
loadRubyReleases = loadRubyReleasesFrom fixtureDirectory

-- | Generated load fixtures use the same native manifest and digest checks as the committed corpus.
loadRubyReleasesFrom :: FilePath -> IO [RubyRelease]
loadRubyReleasesFrom directory = do
    raw <- BS.readFile (directory </> "manifest.json")
    value <- either fail pure (eitherDecodeStrict raw)
    rows <- either fail pure (parseEither (withObject "fixture manifest" (.: "releases")) value)
    traverse load rows
  where
    load row = do
        (name, version, platform, filename, checksum, compactPath, compactChecksum) <- either fail pure (parseEither coordinate row)
        archive <- BS.readFile (directory </> "gems" </> toString filename)
        compact <- BS.readFile (directory </> compactPath)
        unless (hexSha256Of archive == checksum) (fail "Ruby fixture archive differs from its native digest")
        unless (hexSha256Of compact == compactChecksum) (fail "Ruby fixture compact specification differs from its native digest")
        info <- decodeUtf8 <$> BS.readFile (directory </> "info" </> toString name)
        let key = version <> if platform == "ruby" then "" else "-" <> platform
        infoRow <- case filter ((== key) . T.takeWhile (/= ' ')) (T.lines info) of
            [line] -> pure line
            _ -> fail "Ruby fixture info coordinate is absent or ambiguous"
        pure (RubyRelease name version platform filename checksum archive compactPath compact row infoRow)
    coordinate :: Value -> Parser (Text, Text, Text, Text, Text, FilePath, Text)
    coordinate = withObject "fixture release" $ \row ->
        (,,,,,,) <$> row .: "name" <*> row .: "version" <*> row .: "platform" <*> row .: "filename" <*> row .: "sha256" <*> row .: "compact" <*> row .: "compact_sha256"

-- | Ambiguous coordinates cannot provide a passing expected observation.
rubyRelease :: [RubyRelease] -> Text -> Text -> Text -> IO RubyRelease
rubyRelease releases name version platform = case filter matches releases of
    [release] -> pure release
    _ -> fail "Ruby fixture coordinate is absent or ambiguous"
  where
    matches release = releaseName release == name && releaseVersion release == version && releasePlatform release == platform

-- | The fixture directory supports discovery and on-demand native metadata.
writeRubyRegistry :: FilePath -> IO ()
writeRubyRegistry destination = do
    releases <- loadRubyReleases
    createDirectoryIfMissing True destination
    writeFileText (destination </> "versions") (rubyVersions releases)
    forM_ releases $ \release -> do
        let archive = destination </> "gems" </> toString (releaseFilename release)
            compact = destination </> releaseCompactPath release
        traverse_ (createDirectoryIfMissing True . takeDirectory) [archive, compact]
        BS.writeFile archive (releaseArchive release)
        BS.writeFile compact (releaseCompact release)
    createDirectoryIfMissing True (destination </> "info")
    now <- toText . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" <$> getCurrentTime
    forM_ (ordNub (map releaseName releases)) $ \name ->
        if name == "ecluse-fixture-advisory"
            then writeFileText (destination </> "info" </> toString name) ("---\n" <> T.unlines [if releaseVersion release == "1.19.0" then T.replace "2020-01-01T00:00:00Z" now (releaseInfoRow release) else releaseInfoRow release | release <- releases, releaseName release == name])
            else copyFile (fixtureDirectory </> "info" </> toString name) (destination </> "info" </> toString name)

-- | Blank info hints force supported clients to revalidate their cached rows.
rubyVersions :: [RubyRelease] -> Text
rubyVersions = rubyVersionsWith (const "")

-- | Discovery rows ending in each name's info-file hint, which rubygems.org fills with the info body's MD5.
rubyVersionsWith :: (Text -> Text) -> [RubyRelease] -> Text
rubyVersionsWith hint releases =
    "created_at: 2026-01-01T00:00:00Z\n---\n" <> T.unlines (map directoryRow names)
  where
    names = sort (ordNub (map releaseName releases))
    directoryRow name =
        name <> " " <> T.intercalate "," [T.takeWhile (/= ' ') (releaseInfoRow release) | release <- releases, releaseName release == name] <> " " <> hint name

-- | A committed info file's rows, each non-empty line after the header at its position from 1.
loadFixtureInfo :: Text -> IO (PackageName, [GemRow])
loadFixtureInfo raw = do
    name <- either (fail . show) pure (projectName raw)
    bytes <- BS.readFile (fixtureDirectory </> "info" </> toString raw)
    let lines' = filter (not . BS.null) (drop 1 (B8.lines bytes))
    rows <- traverse (\(position, line) -> either (fail . show) pure (decodeInfoRow name (RecordEntry position) line)) (zip [1 ..] lines')
    pure (name, rows)

fixtureDirectory :: FilePath
fixtureDirectory = "test/fixtures/rubygems"
