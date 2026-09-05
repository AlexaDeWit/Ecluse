-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.CredentialSpec (spec) where

import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import Test.Hspec

import Ecluse.Core.Credential

-- | A fixed expiry instant for the static-provider test.
anExpiry :: UTCTime
anExpiry = UTCTime (fromGregorian 2026 6 21) 0

spec :: Spec
spec = do
    describe "Secret" $ do
        it "redacts its contents in Show" $
            -- Load-bearing: a token must never reach a log, an error, or any other
            -- 'Show'-derived signal (see observability.md).
            show (mkSecret "super-secret-token") `shouldNotContain` "super-secret-token"

        it "renders a fixed redaction placeholder regardless of contents" $ do
            show (mkSecret "alpha") `shouldBe` ("Secret <REDACTED>" :: String)
            show (mkSecret "beta") `shouldBe` ("Secret <REDACTED>" :: String)

        it "still exposes the real secret via unSecret" $
            -- Redaction is a display concern only. The value must stay usable.
            unSecret (mkSecret "the-token") `shouldBe` "the-token"

        it "compares equal exactly when the underlying token text is equal" $ do
            -- The redacted 'Show' is identical for every secret, so equality must
            -- come from the wrapped text, not the rendered form.
            mkSecret "x" `shouldBe` mkSecret "x"
            mkSecret "x" `shouldNotBe` mkSecret "y"

        it "is unequal for equal-length tokens that differ" $
            -- 'Secret' equality is constant-time, with no content-dependent early out. Same-length,
            -- differing content is the shape that would leak under a short-circuiting compare.
            mkSecret "abcdef" `shouldNotBe` mkSecret "abcxef"

        it "is unequal when one token is a strict prefix of the other" $ do
            -- A prefix match is the case a short-circuiting compare would separate
            -- by time. Equality treats it as plain inequality.
            mkSecret "abc" `shouldNotBe` mkSecret "abcdef"
            mkSecret "abcdef" `shouldNotBe` mkSecret "abc"

        it "treats the empty token like any other (equal to itself, unequal to a non-empty)" $ do
            mkSecret "" `shouldBe` mkSecret ""
            mkSecret "" `shouldNotBe` mkSecret "x"

        it "compares over the full UTF-8 encoding, not a truncated form" $
            -- Equality reflects the whole token text including multi-byte
            -- characters, so it cannot collide two distinct tokens.
            mkSecret "tøken-α" `shouldBe` mkSecret "tøken-α"

        it "never leaks the secret even when embedded in an AuthToken's Show" $ do
            let tok = AuthToken{authSecret = mkSecret "leak-me", authExpiresAt = Just anExpiry}
            T.pack (show tok) `shouldSatisfy` (not . T.isInfixOf "leak-me")

    describe "staticProvider" $ do
        it "currentToken returns the configured token" $ do
            let tok = AuthToken{authSecret = mkSecret "static-token", authExpiresAt = Nothing}
            got <- currentToken (staticProvider tok)
            unSecret (authSecret got) `shouldBe` "static-token"

        it "currentToken returns the same token every call (no expiry, no refresh)" $ do
            let tok = AuthToken{authSecret = mkSecret "static-token", authExpiresAt = Just anExpiry}
                provider = staticProvider tok
            tok1 <- currentToken provider
            tok2 <- currentToken provider
            authExpiresAt tok1 `shouldBe` authExpiresAt tok2
            unSecret (authSecret tok1) `shouldBe` unSecret (authSecret tok2)
