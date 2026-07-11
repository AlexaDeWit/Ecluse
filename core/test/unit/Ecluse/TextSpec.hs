-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.TextSpec (spec) where

import Data.Time (UTCTime (UTCTime), fromGregorian, picosecondsToDiffTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Text (joinUrlPath, nonBlank, renderIso8601Utc, stripTrailingSlash)

{- | Tests for the shared text helpers. They pin the promises callers depend on:
'nonBlank' treats an empty or all-whitespace value as absent and returns the surviving
text __trimmed__, the URL-path helpers tolerate exactly one trailing slash on the
base so a join never doubles or drops the separator, and the hot-path ISO-8601
renderer is byte-for-byte 'iso8601Show' -- pinned by a property over the whole
domain, including the delegating edges.
-}
spec :: Spec
spec = do
    nonBlankSpec
    trailingSlashSpec
    joinUrlPathSpec
    renderIso8601Spec

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

renderIso8601Spec :: Spec
renderIso8601Spec = describe "renderIso8601Utc" $ do
    it "matches iso8601Show byte-for-byte across the domain" $
        hedgehog $ do
            -- The whole fast-path domain plus the delegating edges: expanded-
            -- representation years on either side of 0-9999, and every
            -- picosecond fraction shape (zero, short, full-precision).
            year <- forAll (Gen.integral (Range.linearFrom 2020 (-50) 10500))
            month <- forAll (Gen.int (Range.linear 1 12))
            day <- forAll (Gen.int (Range.linear 1 31))
            picos <-
                forAll $
                    Gen.choice
                        [ (* 1_000_000_000_000) <$> Gen.integral (Range.linear 0 86_399) -- whole seconds
                        , Gen.integral (Range.linear 0 86_399_999_999_999_999) -- arbitrary instant
                        , (+ 114_000_000_000) . (* 1_000_000_000_000) <$> Gen.integral (Range.linear 0 86_399) -- millisecond shape
                        ]
            let t = UTCTime (fromGregorian year month day) (picosecondsToDiffTime picos)
            renderIso8601Utc t === toText (iso8601Show t)

    it "renders the canonical npm shapes" $ do
        let at y m d ps = UTCTime (fromGregorian y m d) (picosecondsToDiffTime ps)
        renderIso8601Utc (at 2015 1 11 ((0 * 3600 + 23 * 60 + 27) * 1_000_000_000_000 + 114_000_000_000))
            `shouldBe` "2015-01-11T00:23:27.114Z"
        renderIso8601Utc (at 2026 6 1 0) `shouldBe` "2026-06-01T00:00:00Z"
        renderIso8601Utc (at 44 12 31 (86_399 * 1_000_000_000_000 + 1))
            `shouldBe` "0044-12-31T23:59:59.000000000001Z"

    it "trims trailing fraction zeros without dropping significant ones" $ do
        let t = UTCTime (fromGregorian 2020 2 29) (picosecondsToDiffTime 100_000_000_000)
        renderIso8601Utc t `shouldBe` "2020-02-29T00:00:00.1Z"

    it "delegates a leap-second reading and stays parity-true" $ do
        let t = UTCTime (fromGregorian 2016 12 31) (picosecondsToDiffTime 86_400_500_000_000_000)
        renderIso8601Utc t `shouldBe` toText (iso8601Show t)
