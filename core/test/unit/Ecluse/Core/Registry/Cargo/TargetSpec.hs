-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native target parsing keeps structure and uses the existing request depth limit.
module Ecluse.Core.Registry.Cargo.TargetSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Registry.Cargo.Target
import Ecluse.Core.Security.Limits (Limits (maxNestingDepth), defaultLimits)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "native Cargo target declarations" $ do
    it "accepts Cargo formatting, final commas, empty lists and raw identifiers"
        $ for_
            [ ("cfg(any(unix,windows,))", "cfg(any(unix, windows))")
            , ("cfg(all(unix,any(target_os=\"linux\",windows,),))", "cfg(all(unix, any(target_os = \"linux\", windows)))")
            , ("cfg(any())", "cfg( any( ) )")
            , ("cfg(r#unix)", "cfg(unix)")
            ]
        $ \(authored, emitted) -> do
            target <- expectRight (parseTarget defaultLimits authored)
            parseTarget defaultLimits emitted `shouldBe` Right target
    it "keeps quoted values, named targets and expression order distinct" $
        for_ [("cfg(target_os=\"lin ux\")", "cfg(target_os=\"linux\")"), ("x86_64-linux", "X86_64-linux"), ("cfg(any(unix,windows))", "cfg(any(windows,unix))")] $ \(firstTarget, secondTarget) -> do
            firstIdentity <- expectRight (parseTarget defaultLimits firstTarget)
            secondIdentity <- expectRight (parseTarget defaultLimits secondTarget)
            firstIdentity `shouldNotBe` secondIdentity
    it "rejects malformed tokens, extra input and invalid not arity" $
        for_ ["cfg(tar get)", "x 86", "cfg(any(unix,,windows))", "cfg(not(unix,))", "cfg(not())", "cfg(target_os=linux)", "cfg(unix)tail", "cfg(all)", "cfg(target_os=\"linux)"] $ \target ->
            parseTarget defaultLimits target `shouldSatisfy` isLeft
    it "bounds nested strings before descending beyond the configured depth" $ do
        for_ [1, 2, 8] $ \depth -> do
            let limits = defaultLimits{maxNestingDepth = depth}
                nested count = "cfg(" <> T.replicate count "not(" <> "unix" <> T.replicate count ")" <> ")"
            parseTarget limits (nested (depth - 1)) `shouldSatisfy` isRight
            parseTarget limits (nested depth) `shouldSatisfy` isLeft
        parseTarget defaultLimits ("cfg(" <> T.replicate 100000 "not(" <> "unix" <> T.replicate 100000 ")" <> ")") `shouldSatisfy` isLeft
