-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Literal Cargo source identities retain native URL components and source kinds.
module Ecluse.Core.Registry.Cargo.SourceSpec (spec) where

import Test.Hspec

import Ecluse.Core.Registry (ParseError (..))
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
            , ("https://registry.test/a%2Fb.git", "https://registry.test/a%252Fb")
            , ("https://registry.test/prefix/a%2Fb.git", "https://registry.test/prefix/a%252Fb")
            , ("https://registry.test/..git", "https://registry.test/")
            , ("https://registry.test/prefix/..git", "https://registry.test/prefix")
            , ("https://198.51.100.1/index", "https://198.51.100.1./index")
            , ("https://198.51.100.1/index", "https://3325256705/index")
            , ("https://198.51.100.1/index", "https://198.51.25601/index")
            , ("https://198.51.100.1/index", "https://198.3367937/index")
            , ("https://198.51.100.1/index", "https://0xc6336401/index")
            , ("https://198.51.100.1/index", "https://0306.063.0144.01/index")
            , ("https://0.0.0.0/index", "https://0x/index")
            , ("sparse+https://registry.test/index", "sparse+https://registry.test/index/")
            , ("https://registry.test/index", "https://registry.test/a/%2e%2e/index/")
            , ("https://registry.test/a/index", "https://registry.test/a/%2e/index")
            , ("https://registry.test/index", "https://registry.test/a/.%2e/index")
            , ("https://registry.test/index", "https://registry.test/a/%2e./index")
            , ("https://registry.test/index", "https://registry.test:/index")
            , ("http://registry.test/index", "http://registry.test:80/index")
            , ("ws://registry.test/index", "ws://registry.test:80/index")
            , ("wss://registry.test/index", "wss://registry.test:443/index")
            , ("ftp://registry.test/index", "ftp://registry.test:21/index")
            , ("https://198.51.100.161/index", "https://0Xc63364A1/index")
            , ("https://198.51.100.1/index", "https://0x0000000000c6336401/index")
            , ("https://198.51.100.1/index", "https://000000000306.063.0144.01/index")
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
            , ("https://registry.test/a%2Fb.git", "https://registry.test/a%2Fb")
            , ("https://registry.test/prefix/a%2Fb.git", "https://registry.test/prefix%252Fa%252Fb")
            , ("https://198.51.100.1/index", "https://198.51.100.2/index")
            , ("sparse+https://198.51.100.1/index", "sparse+https://198.51.100.1./index")
            , ("sparse+https://198.51.100.1/index", "sparse+https://3325256705/index")
            , ("sparse+https://registry.test/a%2Fb.git", "sparse+https://registry.test/a%252Fb")
            , ("https://registry.test/a%2fb", "https://registry.test/a%2Fb")
            , ("https://registry.test/index", "https://registry.test:444/index")
            ]
        $ \(firstSource, secondSource) -> do
            firstIdentity <- expectRight (parseSource firstSource)
            secondIdentity <- expectRight (parseSource secondSource)
            firstIdentity `shouldNotBe` secondIdentity
    it "preserves opaque numeric-looking sparse hosts" $
        for_ ["sparse+https://198.51.100.999/index", "sparse+https://09/index"] $ \source ->
            parseSource source `shouldSatisfy` isRight
    it "refuses native-invalid numeric hosts without falling back to domain identity" $
        for_ ["https://198.51.100.999/index", "https://256.1/index", "https://1.2.3.4.5/index", "https://09/index", "https://0x100000000/index", "https://4294967296/index", "https://1..2/index", "https://10000000000/index", "https://01000000000000/index"] $ \source ->
            parseSource source `shouldBe` Left (ParseError "Cargo registry source URL is malformed")
    it "refuses malformed and non-registry source declarations" $
        for_ ["relative/index", "https://", "https:/index", "https://registry.test:70000/index", "https://registry.test/%zz", "https://exa%2Fmple/index", "git+https://github.com/rust-lang/crates.io-index"] $ \source ->
            parseSource source `shouldBe` Left (ParseError "Cargo registry source URL is malformed")
