-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | External version-ordering tools share process handling. The pinned Cargo oracle is mandatory.
module Ecluse.Test.Version.Oracle (oracleCompare, oracleSort, oracleAvailable) where

import Control.Exception (IOException, try)
import Data.Text qualified as T
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)

import Ecluse.Core.Ecosystem (Ecosystem (..))

-- | Probe a real comparison so an unavailable optional smoke oracle cannot pass vacuously.
oracleAvailable :: Ecosystem -> IO Bool
oracleAvailable ecosystem = (== Just LT) <$> oracleCompare ecosystem "1.0.0" "1.0.1"

-- | Invalid versions yield absence. Missing or failed Cargo tools fail the test.
oracleCompare :: Ecosystem -> Text -> Text -> IO (Maybe Ordering)
oracleCompare ecosystem a b = do
    result <- runOracle ecosystem (compareProgram ecosystem) [a, b]
    case T.strip <$> result of
        Nothing -> pure Nothing
        Just "invalid" | ecosystem == Cargo -> pure Nothing
        Just output -> case parseOrdInt output of
            Just ordering -> pure (Just ordering)
            Nothing | ecosystem == Cargo -> fail ("Cargo version oracle returned an invalid comparison: " <> toString output)
            Nothing -> pure Nothing

-- | Keep native-valid versions in stable precedence order, preserving their original spelling.
oracleSort :: Ecosystem -> [Text] -> IO (Maybe [Text])
oracleSort ecosystem versions =
    fmap (filter (not . T.null) . map T.strip . T.lines) <$> runOracle ecosystem (sortProgram ecosystem) versions

runOracle :: Ecosystem -> (String, [String], Text) -> [Text] -> IO (Maybe Text)
runOracle ecosystem (command, prefix, program) arguments = do
    result <- try (readProcessWithExitCode command (prefix <> map toString arguments) (toString program))
    case result of
        Left (fault :: IOException) -> unavailable (displayException fault)
        Right (ExitFailure code, _, diagnostic) -> unavailable ("exit " <> show code <> ": " <> diagnostic)
        Right (ExitSuccess, output, _) -> pure (Just (toText output))
  where
    unavailable reason
        | ecosystem == Cargo = fail ("Cargo version oracle failed: " <> reason)
        | otherwise = pure Nothing

parseOrdInt :: Text -> Maybe Ordering
parseOrdInt = \case
    "-1" -> Just LT
    "0" -> Just EQ
    "1" -> Just GT
    _ -> Nothing

compareProgram :: Ecosystem -> (String, [String], Text)
compareProgram = \case
    Cargo -> ("cargo-semver-oracle", ["compare"], "")
    Npm ->
        ( "node"
        , ["-"]
        , unlines
            [ "const semver = require('semver');"
            , "const a = process.argv[2], b = process.argv[3];"
            , "if (semver.valid(a) === null || semver.valid(b) === null) process.exit(1);"
            , "console.log(String(semver.compare(a, b)));"
            ]
        )
    PyPI ->
        ( "python3"
        , ["-"]
        , unlines
            [ "import sys"
            , "from packaging.version import Version, InvalidVersion"
            , "try:"
            , "    A, B = Version(sys.argv[1]), Version(sys.argv[2])"
            , "except InvalidVersion:"
            , "    sys.exit(1)"
            , "print((A > B) - (A < B))"
            ]
        )
    RubyGems ->
        ( "ruby"
        , ["-"]
        , unlines
            [ "begin"
            , "  a = Gem::Version.new(ARGV[0])"
            , "  b = Gem::Version.new(ARGV[1])"
            , "rescue ArgumentError"
            , "  exit 1"
            , "end"
            , "puts(a <=> b)"
            ]
        )

sortProgram :: Ecosystem -> (String, [String], Text)
sortProgram = \case
    Cargo -> ("cargo-semver-oracle", ["sort"], "")
    Npm ->
        ( "node"
        , ["-"]
        , unlines
            [ "const semver = require('semver');"
            , "const vs = process.argv.slice(2);"
            , "const valid = vs.filter((v) => semver.valid(v) !== null);"
            , "valid.sort(semver.compare);"
            , "process.stdout.write(valid.join('\\n'));"
            ]
        )
    PyPI ->
        ( "python3"
        , ["-"]
        , unlines
            [ "import sys"
            , "from packaging.version import Version, InvalidVersion"
            , "def ok(v):"
            , "    try:"
            , "        Version(v)"
            , "        return True"
            , "    except InvalidVersion:"
            , "        return False"
            , "vs = [v for v in sys.argv[1:] if ok(v)]"
            , "vs.sort(key=Version)"
            , "sys.stdout.write(\"\\n\".join(vs))"
            ]
        )
    RubyGems ->
        ( "ruby"
        , ["-"]
        , unlines
            [ "def ok(v)"
            , "  Gem::Version.new(v)"
            , "  true"
            , "rescue ArgumentError"
            , "  false"
            , "end"
            , "vs = ARGV.select { |v| ok(v) }"
            , "vs = vs.sort_by { |v| Gem::Version.new(v) }"
            , "STDOUT.write(vs.join(\"\\n\"))"
            ]
        )
