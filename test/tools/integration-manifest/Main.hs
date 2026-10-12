-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Main (main) where

import Ecluse.Test.Contract.Integration.Catalogue (integrationCatalogue)
import Ecluse.Test.Contract.Manifest (failRunner, writePlan)

main :: IO ()
main = do
    arguments <- getArgs
    unless (null arguments) $ failRunner "usage: ecluse-integration-manifest"
    writePlan integrationCatalogue
