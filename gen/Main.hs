{- | The @openapi-gen@ build-time generator: write Écluse's capability manifest to
disk.

It assembles the OpenAPI 3 document from the __fixed canonical source__
('Ecluse.Manifest.canonicalManifestSource') -- never a live or environment-derived
configuration -- and renders it deterministically, so the generated artifact is
byte-reproducible across machines and a contract change shows up as a reviewable
line-level diff. It is a non-library component, kept out of the proxy's dependency
closure (like the benchmark components); the running server has no manifest surface.

The output is __derived data__ -- a pure function of the source -- so it is generated
on demand, not committed. The output path is the first argument, defaulting to
@openapi\/openapi.json@ (the published capability manifest); the generator creates the
directory if it is absent, so a clean tree regenerates it cleanly.
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

-- | The default output path (relative to the repository root); git-ignored.
defaultPath :: FilePath
defaultPath = "openapi/openapi.json"
