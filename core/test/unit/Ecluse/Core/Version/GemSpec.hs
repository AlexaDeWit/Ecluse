-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native RubyGems keys preserve canonical comparison values and settle their token graph.
module Ecluse.Core.Version.GemSpec (spec) where

import GHC.Exts.Heap qualified as Heap
import Test.Hspec
import UnliftIO.Exception (evaluate, impureThrow)

import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.Core.Version (parseVersionKey, renderVersionKey)
import Ecluse.Core.Version.Token (VToken (VNum))
import Ecluse.Test.Support (expectRight)

-- | Check canonical spelling and detect unevaluated native token construction without forcing it.
spec :: Spec
spec = do
    describe "renderGem" $
        for_ renderings $ \(raw, expected) ->
            it ("renders " <> toString raw <> " as " <> toString expected) $ do
                renderVersionKey <$> parseVersionKey RubyGems raw `shouldBe` Right expected
                parseVersionKey RubyGems expected `shouldBe` parseVersionKey RubyGems raw
    describe "parseGem token construction" $ do
        it "detects a deliberately deferred token tail without evaluating it" $ do
            let !token = VNum 1
            tokensBox <- Heap.asBox <$> evaluate (token : impureThrow DeferredTokenTail)
            settledPart 64 TokenList tokensBox `shouldReturn` False
        for_ ["12.34.56", "1.2.0.rc.3", "2.0.a.0", "0.0.0", "1.0.0-1", "1.a.0.b", "1.0.a.0.b"] $ \raw ->
            it ("constructs the complete canonical token spine and scalar values for " <> toString raw) $ do
                keyBox <- Heap.asBox <$> (expectRight (parseVersionKey RubyGems raw) >>= evaluate)
                settledPart 64 NativeKey keyBox `shouldReturn` True

{- Raw spellings and their canonical renderings. Gem::Version drops the first run of zeros that starts
a dotted segment and directly precedes a letter. A key that keeps a zero before a letter spells its
first letter after an added zero, which parsing drops again. -}
renderings :: [(Text, Text)]
renderings =
    [("1.19.0", "1.19"), ("2.0.a.0", "2.a"), ("1.0.0-1", "1.pre.1"), ("0.0.0", "0"), ("0.a", "a")]
        <> [("1.a.0.b", "1.a.b"), ("1.a.0b", "1.a.b"), ("1.2.pre.0.rc", "1.2.pre.rc"), ("1.2.0.10.a", "1.2.0.10.a")]
        <> [("1.0.a.0.b", "1.0.a.0.b"), ("1.a0b", "1.0.a.0.b"), ("0.a0a", "0.a.0.a")]

data DeferredTokenTail = DeferredTokenTail
    deriving stock (Eq, Show)

instance Exception DeferredTokenTail

data KeyPart = NativeKey | TokenList | TokenValue

-- Inspect only constructors and indirections, leaving unevaluated token work untouched.
settledPart :: Int -> KeyPart -> Heap.Box -> IO Bool
settledPart remaining part box
    | remaining <= 0 = pure False
    | otherwise =
        Heap.getBoxedClosureData box >>= \case
            Heap.IndClosure{Heap.indirectee = target} -> settledPart (remaining - 1) part target
            Heap.BlackholeClosure{Heap.indirectee = target} -> settledPart (remaining - 1) part target
            Heap.ConstrClosure{Heap.ptrArgs = pointers} -> case (part, pointers) of
                (NativeKey, [tokens]) -> settledPart (remaining - 1) TokenList tokens
                (TokenList, []) -> pure True
                (TokenList, [token, rest]) -> (&&) <$> settledPart (remaining - 1) TokenValue token <*> settledPart (remaining - 1) TokenList rest
                (TokenValue, _) -> pure True
                _ -> pure False
            _ -> pure False
