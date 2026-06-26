module Ecluse.TextSpec (spec) where

import Test.Hspec

import Ecluse.Core.Text (joinUrlPath, nonBlank, stripTrailingSlash)

{- | Tests for the shared text helpers. They pin the two promises callers depend on:
'nonBlank' treats an empty or all-whitespace value as absent and returns the surviving
text __trimmed__, and the URL-path helpers tolerate exactly one trailing slash on the
base so a join never doubles or drops the separator.
-}
spec :: Spec
spec = do
    nonBlankSpec
    trailingSlashSpec
    joinUrlPathSpec

nonBlankSpec :: Spec
nonBlankSpec = describe "nonBlank" $ do
    it "treats the empty string as absent" $
        nonBlank "" `shouldBe` Nothing

    it "treats an all-whitespace value as absent" $
        nonBlank "   \t\n " `shouldBe` Nothing

    it "trims surrounding whitespace from a present value" $
        nonBlank "  api  " `shouldBe` Just "api"

    it "keeps internal whitespace untouched" $
        nonBlank "  a b  " `shouldBe` Just "a b"

    it "returns a value with no surrounding whitespace unchanged" $
        nonBlank "ecluse" `shouldBe` Just "ecluse"

trailingSlashSpec :: Spec
trailingSlashSpec = describe "stripTrailingSlash" $ do
    it "drops a single trailing slash" $
        stripTrailingSlash "https://host/" `shouldBe` "https://host"

    it "leaves a base without a trailing slash untouched" $
        stripTrailingSlash "https://host" `shouldBe` "https://host"

    it "removes at most one trailing slash" $
        stripTrailingSlash "https://host//" `shouldBe` "https://host/"

    it "leaves an interior slash untouched" $
        stripTrailingSlash "https://host/path" `shouldBe` "https://host/path"

joinUrlPathSpec :: Spec
joinUrlPathSpec = describe "joinUrlPath" $ do
    it "joins a base and a path with exactly one slash" $
        joinUrlPath "https://host" "pkg" `shouldBe` "https://host/pkg"

    it "tolerates one trailing slash on the base without doubling the separator" $
        joinUrlPath "https://host/" "pkg" `shouldBe` "https://host/pkg"

    it "appends the path verbatim" $
        joinUrlPath "https://host" "@scope%2Fname" `shouldBe` "https://host/@scope%2Fname"
