-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Local archive references remain inside their admitted root.
module Ecluse.Core.Registry.Archive.PathSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.Archive.Path (resolveArchivePath)

spec :: Spec
spec =
    describe "resolveArchivePath" $ do
        it "normalises redundant separators and parent references inside the root" $ do
            resolveArchivePath ["example-1", "src"] ".//../data//file" `shouldBe` Right "example-1/data/file"
            resolveArchivePath [] "scripts//./../build.rs" `shouldBe` Right "build.rs"
            resolveArchivePath [] "scripts//../../build.rs" `shouldBe` Left (ParseError "tar link escapes its archive")
        it "refuses unsafe local references under the same text bound" $ do
            let target = T.replicate 4096 "a"
            resolveArchivePath [] target `shouldBe` Right target
            for_ ["", "/file", "C:/file", "a\\b", "a\NULb", "a\DELb", target <> "a"] $ \invalid ->
                resolveArchivePath [] invalid `shouldBe` Left (ParseError "unsafe tar link target")
