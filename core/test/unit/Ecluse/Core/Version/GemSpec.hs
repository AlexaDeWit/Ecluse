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
import Ecluse.Core.Version.Gem (GemKey (..), parseGem)
import Ecluse.Core.Version.Token (VToken (VNum))

-- | Check canonical spelling and detect unevaluated native token construction without forcing it.
spec :: Spec
spec = do
    describe "renderGem" $
        for_ [("1.19.0", "1.19"), ("2.0.a.0", "2.a"), ("1.0.0-1", "1.pre.1"), ("0.0.0", "0"), ("0.a", "a")] $ \(raw, expected) ->
            it ("renders " <> toString raw <> " as " <> toString expected) $ do
                renderVersionKey <$> parseVersionKey RubyGems raw `shouldBe` Right expected
                parseVersionKey RubyGems expected `shouldBe` parseVersionKey RubyGems raw
    describe "parseGem token construction" $ do
        it "detects a deliberately deferred token tail without evaluating it" $ do
            let !token = VNum 1
                key = GemKey (token : impureThrow DeferredTokenTail)
            GemKey tokens <- evaluate key
            settledPart 64 TokenList (Heap.asBox tokens) `shouldReturn` False
        for_ ["12.34.56", "1.2.0.rc.3", "2.0.a.0", "0.0.0", "1.0.0-1"] $ \raw ->
            it ("constructs the complete canonical token spine and scalar values for " <> toString raw) $ do
                key <- maybe (fail "valid Gem version was rejected") pure (parseGem raw) >>= evaluate
                let GemKey tokens = key
                settledPart 64 TokenList (Heap.asBox tokens) `shouldReturn` True

data DeferredTokenTail = DeferredTokenTail
    deriving stock (Eq, Show)

instance Exception DeferredTokenTail

data KeyPart = TokenList | TokenValue

-- Inspect only constructors and indirections, leaving unevaluated token work untouched.
settledPart :: Int -> KeyPart -> Heap.Box -> IO Bool
settledPart remaining part box
    | remaining <= 0 = pure False
    | otherwise =
        Heap.getBoxedClosureData box >>= \case
            Heap.IndClosure{Heap.indirectee = target} -> settledPart (remaining - 1) part target
            Heap.BlackholeClosure{Heap.indirectee = target} -> settledPart (remaining - 1) part target
            Heap.ConstrClosure{Heap.ptrArgs = pointers} -> case (part, pointers) of
                (TokenList, []) -> pure True
                (TokenList, [token, rest]) -> (&&) <$> settledPart (remaining - 1) TokenValue token <*> settledPart (remaining - 1) TokenList rest
                (TokenValue, _) -> pure True
                _ -> pure False
            _ -> pure False
