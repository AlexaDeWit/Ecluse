-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The @openapi-gen@ build-time generator: write Écluse's capability manifest to
disk.

It assembles the OpenAPI 3 document from the __fixed canonical source__
('Ecluse.Manifest.canonicalManifestSource'), never a live or environment-derived
configuration, and renders it deterministically. The generated artifact is
byte-reproducible across machines, so a contract change shows up as a reviewable
line-level diff. It is a non-library component, kept out of the proxy's dependency
closure, as the benchmark components are. The running server has no manifest surface.

The output is __derived data__, a pure function of the source. The build generates it
on demand, and the repository does not carry it. The output path is the first
argument and defaults to @openapi\/openapi.json@, the published capability manifest.
The generator creates the directory if it is absent, so a clean tree regenerates it
cleanly.
-}
module Main (main) where

import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import Ecluse.Manifest (buildOpenApi, canonicalManifestSource, renderManifest)

main :: IO ()
main = do
    args <- getArgs
    let path = case args of
            (p : _) -> p
            [] -> defaultPath
    createDirectoryIfMissing True (takeDirectory path)
    writeFileLBS path (renderManifest (buildOpenApi canonicalManifestSource))
    putStrLn ("openapi-gen: wrote " <> path)

-- | The default output path, relative to the repository root. Git ignores it.
defaultPath :: FilePath
defaultPath = "openapi/openapi.json"
