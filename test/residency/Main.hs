-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Residency tests and their isolated metadata probe entry point.
Explicit imports keep integration fixtures from registering unrelated examples.
-}
module Main (main) where

import Ecluse.Core.Server.MemoryModelResidencySpec qualified as MemoryModelResidencySpec
import Ecluse.Core.Server.Pipeline.TarballResidencySpec qualified as TarballResidencySpec
import System.Environment (getArgs)
import Test.Hspec (hspec)

main :: IO ()
main =
    getArgs >>= \case
        ["--metadata-probe", shape, path] -> MemoryModelResidencySpec.childMain shape path
        _ -> hspec $ do
            TarballResidencySpec.spec
            MemoryModelResidencySpec.spec
