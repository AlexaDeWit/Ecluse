-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The @site-gen@ build-time generator: write the Markdown fragments the site
embeds.

@site-gen threat-register@ renders the OWASP Threat Dragon model at @--model@, and
@site-gen openapi@ renders the document "Ecluse.Manifest" assembles from its fixed
canonical source. Both outputs are derived data, so the build generates them on
demand and the repository carries neither.
-}
module Main (main) where

import Data.List (lookup)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import Ecluse.Manifest (buildOpenApi, canonicalManifestSource)
import Ecluse.Site.OpenApi (renderOpenApiPage)
import Ecluse.Site.ThreatRegister (decodeThreats, renderThreatRegister)

main :: IO ()
main = do
    args <- getArgs
    case parseCommand (map toText args) of
        Left message -> die (toString message)
        Right command -> run command

-- | The page to render, and where each side of it comes from.
data Command
    = ThreatRegister FilePath FilePath
    | OpenApiPage FilePath

run :: Command -> IO ()
run = \case
    ThreatRegister modelPath outPath -> do
        raw <- readFileBS modelPath
        case decodeThreats raw of
            Left err -> die (toString ("site-gen: " <> toText modelPath <> ": " <> err))
            Right threats -> writePage outPath (renderThreatRegister threats)
    OpenApiPage outPath -> writePage outPath (renderOpenApiPage (buildOpenApi canonicalManifestSource))

writePage :: FilePath -> Text -> IO ()
writePage path body = do
    createDirectoryIfMissing True (takeDirectory path)
    writeFileText path body
    putTextLn ("site-gen: wrote " <> toText path)

parseCommand :: [Text] -> Either Text Command
parseCommand = \case
    ("threat-register" : rest) -> do
        flags <- parseFlags ["--model", "--out"] rest
        ThreatRegister <$> flagValue "--model" flags <*> flagValue "--out" flags
    ("openapi" : rest) -> do
        flags <- parseFlags ["--out"] rest
        OpenApiPage <$> flagValue "--out" flags
    _ -> Left usage

-- Only the named flags are accepted, so a flag meant for the other subcommand is
-- a usage error rather than a silently ignored argument.
parseFlags :: [Text] -> [Text] -> Either Text [(Text, Text)]
parseFlags known = go
  where
    go = \case
        [] -> Right []
        (flag : value : rest) | flag `elem` known -> ((flag, value) :) <$> go rest
        (flag : _) -> Left ("site-gen: unexpected argument " <> flag <> "\n" <> usage)

flagValue :: Text -> [(Text, Text)] -> Either Text FilePath
flagValue flag flags =
    maybeToRight ("site-gen: missing " <> flag <> "\n" <> usage) (toString <$> lookup flag flags)

usage :: Text
usage =
    "usage: site-gen threat-register --model <path> --out <file>\n\
    \       site-gen openapi --out <file>"
