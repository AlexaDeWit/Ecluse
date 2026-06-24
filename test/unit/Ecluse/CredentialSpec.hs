module Ecluse.CredentialSpec (spec) where

import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import Test.Hspec

import Ecluse.Credential

-- | A fixed expiry instant for the static-provider test.
anExpiry :: UTCTime
anExpiry = UTCTime (fromGregorian 2026 6 21) 0

spec :: Spec
spec = do
    describe "Secret" $ do
        it "redacts its contents in Show" $
            -- Load-bearing: a token must never reach a log, error, or any other
            -- 'Show'-derived signal (see observability.md). The literal secret
            -- text must not appear anywhere in the rendered form.
            show (mkSecret "super-secret-token") `shouldNotContain` "super-secret-token"

        it "renders a fixed redaction placeholder regardless of contents" $ do
            show (mkSecret "alpha") `shouldBe` ("Secret <REDACTED>" :: String)
            show (mkSecret "beta") `shouldBe` ("Secret <REDACTED>" :: String)

        it "still exposes the real secret via unSecret" $
            -- Redaction is a display concern only; the value must remain usable.
            unSecret (mkSecret "the-token") `shouldBe` "the-token"

        it "compares equal exactly when the underlying token text is equal" $ do
            -- The redacted 'Show' is identical for every secret, so equality must
            -- come from the wrapped text, not the rendered form: two secrets are
            -- equal iff their tokens are, and differ when the tokens differ.
            mkSecret "x" `shouldBe` mkSecret "x"
            mkSecret "x" `shouldNotBe` mkSecret "y"

        it "is unequal for equal-length tokens that differ" $
            -- A 'Secret' equality is constant-time (no content-dependent early
            -- out), so this same-length, differing-content case must not be
            -- mistaken for equal — the property timing-safe comparison exists to
            -- protect, exercised on the shape that would otherwise leak.
            mkSecret "abcdef" `shouldNotBe` mkSecret "abcxef"

        it "is unequal when one token is a strict prefix of the other" $ do
            -- A prefix match is exactly the case a short-circuiting compare would
            -- separate by time; equality treats it as plain inequality.
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
