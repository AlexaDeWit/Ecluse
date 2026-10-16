-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Server.FieldValueSpec (spec) where

import Network.HTTP.Types.Header (hAccept, hAcceptEncoding)
import Test.Hspec

import Ecluse.Core.Server.FieldValue (fieldValues, listMembers, lowerAscii, memberParameters, trimWhitespace)

spec :: Spec
spec = do
    describe "fieldValues" $
        it "returns every value of a repeated field in request order" $
            fieldValues hAcceptEncoding [(hAcceptEncoding, "gzip"), (hAccept, "*/*"), (hAcceptEncoding, "identity")]
                `shouldBe` (["gzip", "identity"] :: [ByteString])
    describe "listMembers" $ do
        it "trims each member and drops empty ones" $
            listMembers " gzip ,\tidentity;q=0.5 ,, " `shouldBe` ["gzip", "identity;q=0.5"]
        it "reads an empty value as no members" $
            listMembers "" `shouldBe` []
    describe "memberParameters" $ do
        it "splits the token from its parameters, lowering parameter names" $
            memberParameters "GZip ; Q = 0.5 ; level" `shouldBe` ("GZip", [("q", "0.5"), ("level", "")])
        it "reads a member without parameters" $
            memberParameters "identity" `shouldBe` ("identity", [])
    describe "trimWhitespace" $
        it "drops spaces and tabs only" $
            trimWhitespace " \t a b \t" `shouldBe` "a b"
    describe "lowerAscii" $
        it "lowers ASCII letters and leaves other bytes alone" $
            lowerAscii "GZip-\201" `shouldBe` "gzip-\201"
