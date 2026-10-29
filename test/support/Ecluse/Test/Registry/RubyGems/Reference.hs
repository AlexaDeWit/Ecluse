-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Independent Compact Index observations retain physical rows and opaque native fields.
module Ecluse.Test.Registry.RubyGems.Reference (
    RubyInfoRow (..),
    referenceInfo,
    runRubyReference,
) where

import Data.Aeson (Value, eitherDecodeStrict)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import System.Directory (findExecutable)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (hClose)
import System.Process (readProcessWithExitCode)
import UnliftIO.Temporary (withSystemTempFile)

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
