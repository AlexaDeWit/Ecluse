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
