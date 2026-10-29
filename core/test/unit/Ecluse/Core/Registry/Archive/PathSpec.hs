-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Archive member paths and local references remain inside their admitted root.
module Ecluse.Core.Registry.Archive.PathSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.Archive.Path (checkPath, resolveArchivePath)

spec :: Spec
spec = do
    describe "checkPath" $ do
        it "accepts ordinary relative files and directories" $ do
            checkPath "example-1/PKG-INFO" `shouldBe` Right ["example-1", "PKG-INFO"]
            checkPath "example-1/" `shouldBe` Right ["example-1"]
        it "rejects ambiguous and platform-dependent paths" $
            for_ ["../PKG-INFO", "/PKG-INFO", "a/./b", "a//b", "C:/b", "a\\b"] $
                \path -> checkPath path `shouldBe` Left (ParseError "unsafe archive path")
        it "accepts exactly 4096 characters and refuses extra bytes and control characters" $ do
            let path = T.replicate 4096 "a"
            checkPath path `shouldBe` Right [path]
            for_ [path <> "a", "a\NULb", "a\DELb"] $ \invalid ->
                checkPath invalid `shouldBe` Left (ParseError "unsafe archive path")
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
