-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The @site-gen@ build-time generator: write the OpenAPI reference fragment the
site embeds.

@site-gen openapi@ renders the document "Ecluse.Manifest" assembles from its fixed
canonical source. The output is derived data, so the build generates it on demand and
the repository does not carry it.
-}
module Main (main) where

import Data.List (lookup)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import Ecluse.Manifest (buildOpenApi, canonicalManifestSource)
import Ecluse.Site.OpenApi (renderOpenApiPage)

main :: IO ()
main = do
    args <- getArgs
    case parseCommand (map toText args) of
        Left message -> die (toString message)
        Right command -> run command

-- | The page to render, and where to write it.
newtype Command = OpenApiPage FilePath

run :: Command -> IO ()
run (OpenApiPage outPath) = writePage outPath (renderOpenApiPage (buildOpenApi canonicalManifestSource))

writePage :: FilePath -> Text -> IO ()
writePage path body = do
    createDirectoryIfMissing True (takeDirectory path)
    writeFileText path body
    putTextLn ("site-gen: wrote " <> toText path)

parseCommand :: [Text] -> Either Text Command
parseCommand = \case
    ("openapi" : rest) -> do
        flags <- parseFlags ["--out"] rest
        OpenApiPage <$> flagValue "--out" flags
    _ -> Left usage

-- Only the named flags are accepted, so an unknown flag is a usage error rather
-- than a silently ignored argument.
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
usage = "usage: site-gen openapi --out <file>"
