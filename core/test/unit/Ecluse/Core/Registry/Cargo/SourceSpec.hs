-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Literal Cargo source identities retain native URL components and source kinds.
module Ecluse.Core.Registry.Cargo.SourceSpec (spec) where

import Test.Hspec

import Ecluse.Core.Registry.Cargo.Source
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "native Cargo source identity" $ do
    it "accepts Cargo canonical registry spellings without changing the source kind"
        $ for_
            [ ("https://github.com/rust-lang/crates.io-index", "https://GITHUB.com/RUST-LANG/CRATES.IO-INDEX.git/")
            , ("https://github.com/rust-lang/crates.io-index", "http://github.com/rust-lang/crates.io-index")
            , ("https://github.com/rust-lang/crates.io-index", "sparse+https://index.crates.io/")
            , ("https://registry.test/index", "https://registry.test:443/index.git/")
            , ("sparse+https://registry.test/index", "sparse+https://registry.test/index/")
            , ("https://registry.test/index", "https://registry.test/a/%2e%2e/index/")
            , ("https://github.com/rust-lang/crates.io-index", "https://%67ithub.com/rust-lang/crates.io-index")
            , ("https://[2001:db8::1]/index", "https://[2001:0db8:0:0:0:0:0:1]/index/")
            , ("https://alice:one@registry.test/index?x=1#rev", "https://alice:one@registry.test:443/index/?x=1#rev")
            ]
        $ \(firstSource, secondSource) -> do
            firstIdentity <- expectRight (parseSource firstSource)
            parseSource secondSource `shouldBe` Right firstIdentity
    it "preserves source kind, sparse paths, userinfo, query, fragment, escaping and non-default ports"
        $ for_
            [ ("https://registry.test/index", "sparse+https://registry.test/index/")
            , ("sparse+https://registry.test/index/", "sparse+https://registry.test/index.git/")
            , ("sparse+https://registry.test/index/", "sparse+https://registry.test:443/index/")
            , ("sparse+https://registry.test/index/", "sparse+https://registry.test/Index/")
            , ("sparse+https://registry.test/index/", "sparse+https://REGISTRY.test/index/")
            , ("sparse+https://github.com/rust-lang/crates.io-index", "https://github.com/rust-lang/crates.io-index")
            , ("https://registry.test/index.GIT", "https://registry.test/index.git")
            , ("https://registry.test/index", "https://registry.test/other")
            , ("https://alice:one@registry.test/index", "https://alice:two@registry.test/index")
            , ("https://registry.test/index?x=1", "https://registry.test/index?x=2")
            , ("https://registry.test/index#one", "https://registry.test/index#two")
            , ("https://registry.test/a%2Fb", "https://registry.test/a/b")
            , ("https://registry.test/a%2fb", "https://registry.test/a%2Fb")
            , ("https://registry.test/index", "https://registry.test:444/index")
            ]
        $ \(firstSource, secondSource) -> do
            firstIdentity <- expectRight (parseSource firstSource)
            secondIdentity <- expectRight (parseSource secondSource)
            firstIdentity `shouldNotBe` secondIdentity
    it "refuses malformed and non-registry source declarations" $
        for_ ["relative/index", "https://", "https://registry.test:70000/index", "https://registry.test/%zz", "https://exa%2Fmple/index", "git+https://github.com/rust-lang/crates.io-index"] $ \source ->
            parseSource source `shouldSatisfy` isLeft
