-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pure codeartifact conformance discovery without service acquisition.
module Main (main) where

import Ecluse.Test.Contract.Manifest (failRunner, writePlan)
import Ecluse.Test.Contract.PackageStore (packageStoreCatalogue, validatePackageStore)

-- | This fixed entrypoint accepts no selector arguments.
main :: IO ()
main = do
    validatePackageStore "codeartifact"
    arguments <- getArgs
    unless (null arguments) $ failRunner "usage: ecluse-conformance-codeartifact-manifest"
    writePlan (packageStoreCatalogue "codeartifact")
