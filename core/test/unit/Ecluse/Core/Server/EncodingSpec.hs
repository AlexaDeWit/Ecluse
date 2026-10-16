-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Server.EncodingSpec (spec) where

import Network.HTTP.Types.Header (hAcceptEncoding, hContentType, hVary)
import Test.Hspec

import Ecluse.Core.Server.Encoding (
    CodingChoice (NoAcceptableCoding, ServeCoding),
    ContentCoding (GzipCoding, IdentityCoding),
    MetadataCompression (GzipWhenAccepted, IdentityOnly),
    chooseCoding,
    varyAcceptEncoding,
 )

spec :: Spec
spec = do
    describe "chooseCoding" $ do
        it "follows the negotiation table for one Accept-Encoding field" $
            for_ table $ \(field, expected) ->
                (field, offered [field]) `shouldBe` (field, expected)
        it "selects identity when the request sends no Accept-Encoding field" $
            chooseCoding GzipWhenAccepted [] `shouldBe` ServeCoding IdentityCoding
        it "reads every Accept-Encoding field of a request" $ do
            offered ["identity;q=0", "gzip"] `shouldBe` gzip
            offered ["gzip;q=0.5", "identity;q=0.8"] `shouldBe` keepIdentity
        it "lets a duplicate refusal win, and otherwise takes the highest duplicate quality" $ do
            offered ["gzip;q=0.9, gzip;q=0"] `shouldBe` keepIdentity
            offered ["gzip;q=0, gzip;q=0.9"] `shouldBe` keepIdentity
            offered ["gzip;q=0.2, identity;q=0.5, gzip;q=0.9"] `shouldBe` gzip
        it "refuses a coding whose quality is malformed" $
            for_ ["gzip;q=abc", "gzip;q=1.5", "gzip;q=0.1234", "gzip;q=", "gzip;q=-1", "gzip;q=01"] $ \field ->
                (field, offered [field]) `shouldBe` (field, keepIdentity)
        it "reads every legal quality spelling" $ do
            for_ ["gzip;q=1", "gzip;q=1.", "gzip;q=1.000", "gzip;Q=0.001", "gzip; q=0.5"] $ \field ->
                (field, offered [field]) `shouldBe` (field, gzip)
            offered ["gzip;q=0.000, identity"] `shouldBe` keepIdentity
            offered ["gzip;q=0.501, identity;q=0.5"] `shouldBe` gzip
            offered ["gzip;q=0.5, identity;q=0.501"] `shouldBe` keepIdentity
        it "compares coding names case-insensitively and treats x-gzip as gzip" $ do
            offered ["GZIP"] `shouldBe` gzip
            offered ["x-gzip"] `shouldBe` gzip
            offered ["Identity;q=0, GZip;q=0"] `shouldBe` NoAcceptableCoding
        it "serves identity for every request when the operator opts out" $
            for_ (map fst table) $ \field ->
                chooseCoding IdentityOnly [(hAcceptEncoding, field)] `shouldBe` keepIdentity
    describe "varyAcceptEncoding" $ do
        it "adds the field when the response varies on nothing" $
            varyAcceptEncoding [(hContentType, "application/json")] `shouldBe` [(hContentType, "application/json"), (hVary, "Accept-Encoding")]
        it "keeps the field names a response already varies on" $
            varyAcceptEncoding [(hVary, "Authorization")] `shouldBe` [(hVary, "Authorization, Accept-Encoding")]
        it "adds nothing when the response already varies on it, or on everything" $ do
            varyAcceptEncoding [(hVary, "accept-encoding")] `shouldBe` [(hVary, "accept-encoding")]
            varyAcceptEncoding [(hVary, "*")] `shouldBe` [(hVary, "*")]
  where
    offered fields = chooseCoding GzipWhenAccepted [(hAcceptEncoding, field) | field <- fields]
    gzip = ServeCoding GzipCoding
    keepIdentity = ServeCoding IdentityCoding
    table =
        [ ("", keepIdentity)
        , ("gzip", gzip)
        , ("gzip;q=0.5", gzip)
        , ("gzip;q=0.5, identity;q=0.8", keepIdentity)
        , ("gzip;q=0.8, identity;q=0.5", gzip)
        , ("gzip;q=0.7, identity;q=0.7", gzip)
        , ("gzip;q=0, *;q=1", keepIdentity)
        , ("br, zstd", keepIdentity)
        , ("br, identity;q=0", NoAcceptableCoding)
        , ("*", gzip)
        , ("*;q=0", NoAcceptableCoding)
        , ("gzip;q=1, identity;q=0", gzip)
        , ("gzip;q=0, identity;q=0", NoAcceptableCoding)
        , ("*;q=0, identity;q=1", keepIdentity)
        , ("*;q=0, gzip", gzip)
        , ("gzip;q=0.5, *;q=0.8", keepIdentity)
        , ("deflate, gzip, br, zstd", gzip)
        ]
