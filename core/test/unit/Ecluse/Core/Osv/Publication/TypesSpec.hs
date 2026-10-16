-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The publication vocabulary's fixed-width forms and its bounded percentage.
module Ecluse.Core.Osv.Publication.TypesSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Osv.Publication.Types
import Ecluse.Test.Snapshot (digestOf)

spec :: Spec
spec = do
    describe "source evidence" $ do
        it "identifies a source by the SHA-256 of its URL, in lowercase hex" $ do
            let npmExport = sourceIdentityOf "https://osv.example.test/npm/all.zip"
            T.length (sourceIdentityHex npmExport) `shouldBe` 64
            parseSourceIdentityHex (sourceIdentityHex npmExport) `shouldBe` Just npmExport
            sourceIdentityOf "https://osv.example.test/PyPI/all.zip" `shouldNotBe` npmExport

        it "reads back only lowercase 64-digit hex" $
            for_ [T.replicate 63 "a", T.replicate 64 "A", T.replicate 64 "g", T.replicate 65 "a"] $ \raw -> do
                parseSourceIdentityHex raw `shouldBe` Nothing
                parseSourceDigestHex raw `shouldBe` Nothing

        it "carries a body digest unchanged through its hex form" $ do
            let digest = sourceDigestOf (digestOf "export bytes")
            parseSourceDigestHex (sourceDigestHex digest) `shouldBe` Just digest

    describe "publication nonces" $ do
        it "draws a fresh 32-digit nonce each time, which reads back" $ do
            first' <- newPublicationNonce
            second' <- newPublicationNonce
            first' `shouldNotBe` second'
            T.length (renderPublicationNonce first') `shouldBe` 32
            parsePublicationNonce (renderPublicationNonce first') `shouldBe` Just first'

        it "refuses any other width" $
            parsePublicationNonce (T.replicate 31 "a") `shouldBe` Nothing

    describe "percentages" $ do
        it "accepts 0 to 100 and nothing outside it" $
            map (isJust . mkPercent) [-1, 0, 2.5, 100, 100.5] `shouldBe` [False, True, True, True, False]
        it "renders whole and fractional shares" $
            map (fmap renderPercent . mkPercent) [2, 2.5] `shouldBe` [Just "2%", Just "2.5%"]
