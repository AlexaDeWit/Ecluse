-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Worker.IntegritySpec (spec) where

import Data.Text qualified as T
import Ecluse.Core.Package (Hash, HashAlg (Blake2b, MD5, SHA1, SHA256, SRI), hashValue, sriBody)
import Ecluse.Core.Package qualified as Pkg
import Ecluse.Core.Worker (IntegrityResult (IntegrityMismatch, IntegrityVerified), verifyIntegrity)
import Ecluse.Test.Package (hexSha512Of, sriSha512Of, unsafeHash)
import Ecluse.Test.Support (expectRight)
import Ecluse.Worker.Support
import Test.Hspec

spec :: Spec
spec = do
    describe "verifyIntegrity" $ do
        it "verifies a sha1-only artifact against its sha1 (no stronger digest present)" $
            verifyIntegrity (unsafeHash SHA1 trueSha1 :| []) tarballBytes `shouldBe` IntegrityVerified

        it "verifies an SRI (sha512)-only artifact against its sha512" $
            verifyIntegrity (unsafeHash SRI trueSri :| []) tarballBytes `shouldBe` IntegrityVerified

        it "accepts a matching strongest SRI alternative in either order" $
            forM_ [trueSri <> " " <> falseSri, falseSri <> " " <> trueSri] $ \wire -> do
                hashes <- expectRight (Pkg.mkSriHashes wire)
                verifyIntegrity hashes tarballBytes `shouldBe` IntegrityVerified

        it "rejects bytes when all strongest SRI alternatives fail" $ do
            hashes <- expectRight (Pkg.mkSriHashes (falseSri <> " " <> sriSha512Of "another representation"))
            verifyIntegrity hashes tarballBytes
                `shouldBe` IntegrityMismatch "the SRI sha512 digest did not match the fetched bytes"

        it "does not let weaker matching digests rescue failing strongest SRI alternatives" $ do
            hashes <- expectRight (Pkg.mkSriHashes (falseSri <> " " <> sriSha512Of "another representation" <> " " <> trueSha256Sri))
            let withShasum = hashes <> (unsafeHash SHA1 trueSha1 :| [])
            verifyIntegrity withShasum tarballBytes
                `shouldBe` IntegrityMismatch "the SRI sha512 digest did not match the fetched bytes"

        it "does not treat raw hashes as SRI alternatives" $
            verifyIntegrity (unsafeHash Pkg.SHA512 trueSha512Hex :| [unsafeHash SRI falseSri]) tarballBytes
                `shouldBe` IntegrityMismatch "the SRI sha512 digest did not match the fetched bytes"

        it "keeps the selected raw assertion when matching SRI alternatives are present" $
            verifyIntegrity (unsafeHash SRI trueSri :| [unsafeHash Pkg.SHA512 (hexSha512Of "other bytes")]) tarballBytes
                `shouldBe` IntegrityMismatch "the SHA512 digest did not match the fetched bytes"

        it "keeps the last equal-ranked raw assertion without treating raw hashes as alternatives" $ do
            let matching = unsafeHash Pkg.SHA512 trueSha512Hex
                different = unsafeHash Pkg.SHA512 (hexSha512Of "other bytes")
            verifyIntegrity (matching :| [different]) tarballBytes
                `shouldBe` IntegrityMismatch "the SHA512 digest did not match the fetched bytes"
            verifyIntegrity (different :| [matching]) tarballBytes `shouldBe` IntegrityVerified

        it "verifies an SRI (sha384)-only artifact against its sha384 (the worker computes sha384)" $
            verifyIntegrity (unsafeHash SRI trueSha384Sri :| []) tarballBytes `shouldBe` IntegrityVerified

        it "verifies a raw SHA384-tagged digest against its hex sha384 (the tag arm, not SRI)" $
            verifyIntegrity (unsafeHash Pkg.SHA384 trueSha384Hex :| []) tarballBytes `shouldBe` IntegrityVerified

        it "REJECTS a sha384 SRI that does not match the fetched bytes (tamper guard)" $
            verifyIntegrity (unsafeHash SRI falseSha384Sri :| []) tarballBytes
                `shouldBe` IntegrityMismatch "the SRI sha384 digest did not match the fetched bytes"

        it "prefers and verifies a co-present sha384 over a matching sha1 (strongest wins, and is computable)" $
            verifyIntegrity (unsafeHash SHA1 trueSha1 :| [unsafeHash SRI trueSha384Sri]) tarballBytes
                `shouldBe` IntegrityVerified

        it "verifies a raw SHA512-tagged digest against its hex sha512 (the tag arm, not SRI)" $
            verifyIntegrity (unsafeHash Pkg.SHA512 trueSha512Hex :| []) tarballBytes `shouldBe` IntegrityVerified

        it "verifies against the strongest digest when both sha512 and sha1 match" $
            verifyIntegrity (unsafeHash SHA1 trueSha1 :| [unsafeHash SRI trueSri]) tarballBytes
                `shouldBe` IntegrityVerified

        it "REJECTS bytes that match the weak sha1 but fail the strong sha512 (tamper guard)" $
            verifyIntegrity (unsafeHash SHA1 trueSha1 :| [unsafeHash SRI falseSri]) tarballBytes
                `shouldSatisfy` isMismatch

        it "reports a mismatch when the sole digest does not match" $
            verifyIntegrity (unsafeHash SHA1 wrongSha1 :| []) tarballBytes
                `shouldSatisfy` isMismatch

        it "verifies a blake2b-only digest (the worker now computes blake2b-512)" $
            verifyIntegrity (unsafeHash Blake2b trueBlake2b :| []) tarballBytes `shouldBe` IntegrityVerified

        it "REJECTS a blake2b digest that does not match the fetched bytes (tamper guard, the new arm)" $
            verifyIntegrity (unsafeHash Blake2b someBlake2b :| []) tarballBytes
                `shouldBe` IntegrityMismatch "the Blake2b digest did not match the fetched bytes"

        it "prefers and verifies a co-present blake2b over a matching sha1 (strongest wins, now computable)" $
            verifyIntegrity (unsafeHash SHA1 trueSha1 :| [unsafeHash Blake2b trueBlake2b]) tarballBytes
                `shouldBe` IntegrityVerified

        it "prefers sha512 over a matching blake2b when both are present" $
            verifyIntegrity (unsafeHash Blake2b trueBlake2b :| [unsafeHash SRI falseSri]) tarballBytes
                `shouldBe` IntegrityMismatch "the SRI sha512 digest did not match the fetched bytes"

        it "verifies a sha256-only digest (the worker now computes sha256, the default floor)" $
            verifyIntegrity (unsafeHash SHA256 trueSha256 :| []) tarballBytes `shouldBe` IntegrityVerified

        it "REJECTS a sha256 digest that does not match the fetched bytes (tamper guard)" $
            verifyIntegrity (unsafeHash SHA256 someSha256 :| []) tarballBytes
                `shouldBe` IntegrityMismatch "the SHA256 digest did not match the fetched bytes"

        it "prefers a computable sha256 over an equal-tier unresolvable SRI, independent of order" $ do
            let realSha256 = unsafeHash SHA256 trueSha256
                unresolvable = unresolvableSri

            verifyIntegrity (unresolvable :| [realSha256]) tarballBytes `shouldBe` IntegrityVerified
            verifyIntegrity (realSha256 :| [unresolvable]) tarballBytes `shouldBe` IntegrityVerified

        it "fails closed on an md5-only digest (the worker will not verify a broken hash)" $
            -- The public floor rejects MD5 before this gate, which also refuses to compute it.
            mismatchDetail (verifyIntegrity (unsafeHash MD5 someMd5 :| []) tarballBytes)
                `shouldBe` Just "the strongest admitted digest (MD5) is in an algorithm the worker cannot verify"

        it "verifies a sha256 SRI (the worker now computes the sha256 inner algorithm)" $
            verifyIntegrity (unsafeHash SRI trueSha256Sri :| []) tarballBytes `shouldBe` IntegrityVerified

        it "REJECTS a sha256 SRI that does not match the fetched bytes (tamper guard)" $
            verifyIntegrity (unsafeHash SRI someSha256Sri :| []) tarballBytes
                `shouldBe` IntegrityMismatch "the SRI sha256 digest did not match the fetched bytes"

        it "does not downgrade to a matching sha1 when a co-present strong sha256 SRI fails" $
            mismatchDetail (verifyIntegrity (unsafeHash SRI someSha256Sri :| [unsafeHash SHA1 trueSha1]) tarballBytes)
                `shouldBe` Just "the SRI sha256 digest did not match the fetched bytes"

        it "names the algorithm in a plain (computable) digest mismatch too" $
            mismatchDetail (verifyIntegrity (unsafeHash SRI falseSri :| []) tarballBytes)
                `shouldBe` Just "the SRI sha512 digest did not match the fetched bytes"

        it "is case-insensitive on the hex shasum" $
            verifyIntegrity (unsafeHash SHA1 (T.toUpper trueSha1) :| []) tarballBytes
                `shouldBe` IntegrityVerified

        it "REJECTS an SRI whose base64 body matches only after case-folding (base64 is case-sensitive)" $
            -- Base64 case-folding would accept a different digest.
            verifyIntegrity (unsafeHash SRI caseVariantSri :| []) tarballBytes
                `shouldBe` IntegrityMismatch "the SRI sha512 digest did not match the fetched bytes"

unresolvableSri :: Hash
unresolvableSri =
    (unsafeHash SRI trueSha256Sri)
        { hashValue = "sha3-" <> sriBody trueSha256Sri
        }
