-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pure codeartifact conformance discovery without service acquisition.
module Main (main) where

import Ecluse.Runtime.Test.PackageStore (codeArtifactCatalogue, validateCodeArtifact)
import Ecluse.Test.Contract.Manifest (failRunner, writePlan)

-- | This fixed entrypoint accepts no selector arguments.
main :: IO ()
main = do
    validateCodeArtifact
    arguments <- getArgs
    unless (null arguments) $ failRunner "usage: ecluse-conformance-codeartifact-manifest"
    writePlan codeArtifactCatalogue
