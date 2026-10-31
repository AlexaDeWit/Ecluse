-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Independent Compact Index and gem archive observations, sharing no production parser.
module Ecluse.Test.Registry.RubyGems.Reference (
    RubyInfoRow (..),
    referenceInfo,
    referenceGemIdentity,
    runRubyReference,
) where

import Codec.Archive.Tar qualified as Tar
import Codec.Compression.Zlib.Internal qualified as Zlib
import Data.Aeson (Value, eitherDecodeStrict)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import System.Directory (findExecutable)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (hClose)
import System.Process (readProcessWithExitCode)
import UnliftIO.Temporary (withSystemTempFile)

import Ecluse.Test.Package (hexSha256Of)

-- | Raw versions and platforms remain distinct from comparison keys.
data RubyInfoRow = RubyInfoRow
    { rowPosition :: Int
    , rowVersion :: Text
    , rowPlatform :: Text
    , rowDependencies :: [(Text, [Text])]
    , rowFields :: [(Text, Text)]
    }
    deriving stock (Eq, Show)

-- | This test reader shares no parser or coordinate helper with the production adapter.
referenceInfo :: ByteString -> Either Text [RubyInfoRow]
referenceInfo bytes = do
    input <- first show (Text.decodeUtf8' bytes)
    let physical = zip [0 ..] (T.splitOn "\n" input)
    case dropWhile ((/= "---") . snd) physical of
        [] -> Left "Compact Index header delimiter is absent"
        _ : rows -> traverse readRow (filter (not . T.null . snd) rows)
  where
    readRow (position, raw) = do
        let (coordinate, rest) = T.breakOn " " raw
            (version, platformSuffix) = T.breakOn "-" coordinate
            platform = if T.null platformSuffix then "ruby" else T.drop 1 platformSuffix
        when (T.null coordinate || T.null rest || T.null platform) (Left "Compact Index coordinate is incomplete")
        let (dependencies, attributes) = T.breakOn "|" (T.drop 1 rest)
        when (T.null attributes) (Left "Compact Index metadata delimiter is absent")
        parsedDependencies <- traverse dependency (nonemptyParts "," dependencies)
        fields <- traverse attribute (nonemptyParts "," (T.drop 1 attributes))
        pure (RubyInfoRow position version platform parsedDependencies fields)
    dependency value = do
        (name, requirements) <- separated ":" value
        pure (name, T.splitOn "&" requirements)
    attribute = separated ":"
    separated delimiter value = case T.breakOn delimiter value of
        (key, remainder)
            | not (T.null key), not (T.null remainder) -> Right (key, T.drop (T.length delimiter) remainder)
        _ -> Left "Compact Index field is incomplete"
    nonemptyParts delimiter = filter (not . T.null) . T.splitOn delimiter

-- | The name, version and platform a gem's metadata declares, once its members match their SHA-256 entries.
referenceGemIdentity :: ByteString -> Either Text (Text, Text, Text)
referenceGemIdentity bytes = do
    members <- Tar.foldEntries member (Right []) (Left . show) (Tar.read (LBS.fromStrict bytes))
    unless (map fst members == ["metadata.gz", "data.tar.gz", "checksums.yaml.gz"]) (Left "gem archive members differ from the native layout")
    let content name = maybeToRight "gem archive lacks a native member" (LBS.toStrict <$> lookup name members)
    metadata <- content "metadata.gz"
    payload <- content "data.tar.gz"
    checksums <- content "checksums.yaml.gz" >>= gunzipText
    let declared = takeWhile ("  " `T.isPrefixOf`) (drop 1 (dropWhile (/= "SHA256:") (T.lines checksums)))
    unless (sort declared == sort ["  metadata.gz: " <> hexSha256Of metadata, "  data.tar.gz: " <> hexSha256Of payload]) (Left "gem archive members differ from their SHA-256 entries")
    specification <- T.lines <$> gunzipText metadata
    let scalar key = maybeToRight ("gem metadata lacks " <> key) (listToMaybe (mapMaybe (T.stripPrefix (key <> ": ")) specification))
    version <- case dropWhile (/= "version: !ruby/object:Gem::Version") specification of
        _ : nested : _ -> maybeToRight "gem metadata lacks its version number" (T.stripPrefix "  version: " nested)
        _ -> Left "gem metadata lacks a version"
    (,version,) <$> scalar "name" <*> scalar "platform"
  where
    member entry rest = case Tar.entryContent entry of
        Tar.NormalFile body _ -> ((Tar.entryPath entry, body) :) <$> rest
        _ -> Left "gem archive has a member that is not a regular file"

gunzipText :: ByteString -> Either Text Text
gunzipText compressed = do
    chunks <- Zlib.foldDecompressStreamWithInput (\chunk rest -> (chunk :) <$> rest) trailing (Left . show) (Zlib.decompressST Zlib.gzipFormat Zlib.defaultDecompressParams) (LBS.fromStrict compressed)
    first show (Text.decodeUtf8' (BS.concat chunks))
  where
    trailing rest = if LBS.null rest then Right [] else Left "gzip member has trailing bytes"

-- | Ruby's native decoder observes exact bytes and fails when the required tool or decoding is unavailable.
runRubyReference :: Text -> ByteString -> IO Value
runRubyReference mode bytes = do
    declared <- fromMaybe "ruby" <$> lookupEnv "ECLTEST_RUBY"
    command <- findExecutable declared >>= maybe (fail "required Ruby reference executable is absent") pure
    withSystemTempFile "ecluse-ruby-reference" $ \path handle -> do
        BS.hPut handle bytes
        hClose handle
        (status, output, diagnostic) <- readProcessWithExitCode command ["test/fixtures/rubygems/reference.rb", toString mode, path] ""
        unless (status == ExitSuccess) (fail ("Ruby reference failed: " <> diagnostic))
        either fail pure (eitherDecodeStrict (encodeUtf8 (toText output)))
