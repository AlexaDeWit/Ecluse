-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Build-time OpenAPI fragments and search metadata for the assembled site.
module Main (main) where

import Data.List (lookup)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.FilePath (takeBaseName, takeDirectory, takeExtension, takeFileName, (</>))

import Ecluse.Manifest (buildOpenApi, canonicalManifestSource)
import Ecluse.Site.OpenApi (renderOpenApiPage)
import Ecluse.Site.Search (prepareSearchMetadata)

main :: IO ()
main = do
    args <- getArgs
    case parseCommand (map toText args) of
        Left message -> die (toString message)
        Right command -> run command

data Command = OpenApiPage FilePath | SearchMetadata FilePath

run :: Command -> IO ()
run (OpenApiPage outPath) = writePage outPath (renderOpenApiPage (buildOpenApi canonicalManifestSource))
run (SearchMetadata root) = do
    entries <- listDirectory root
    for_ entries $ \entry -> do
        let path = root </> entry
        directory <- doesDirectoryExist path
        if directory
            then run (SearchMetadata path)
            else when (takeExtension path == ".html") $ do
                original <- readFileText path
                let sourceLabel = if takeFileName (takeDirectory path) == "src" then " (source)" else ""
                    prepared = prepareSearchMetadata (toText (takeBaseName path) <> sourceLabel) original
                when (prepared /= original) (writeFileText path prepared)

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
    ("search-metadata" : rest) -> do
        flags <- parseFlags ["--root"] rest
        SearchMetadata <$> flagValue "--root" flags
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
usage = "usage: site-gen openapi --out <file> | search-metadata --root <directory>"
