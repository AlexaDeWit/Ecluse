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
spec = describe "verifyIntegrity" $ do
    soleDigestSpec
    copresentDigestSpec
    sriAlternativeSpec
    selectionSpec

-- Each row is the only digest current metadata carries, so the verdict comes from that
-- algorithm alone: the arms the worker can compute, and the detail it renders when it cannot.
soleDigestSpec :: Spec
soleDigestSpec = describe "a sole admitted digest" $ do
    for_ matchingSoleDigests $ \(label, hash) ->
        it ("verifies " <> label <> " over the fetched bytes") $
            verifyIntegrity (hash :| []) tarballBytes `shouldBe` IntegrityVerified

    for_ failingSoleDigests $ \(label, hash, detail) ->
        it ("REJECTS " <> label <> " that does not match the fetched bytes") $
            verifyIntegrity (hash :| []) tarballBytes `shouldBe` IntegrityMismatch detail

    it "fails closed on an md5-only digest (the worker will not verify a broken hash)" $
        -- The public floor rejects MD5 before this gate, which also refuses to compute it.
        verifyIntegrity (unsafeHash MD5 someMd5 :| []) tarballBytes
            `shouldBe` IntegrityMismatch "the strongest admitted digest (MD5) is in an algorithm the worker cannot verify"

-- | Every digest the worker recomputes, in the spelling current metadata may carry it.
matchingSoleDigests :: [(String, Hash)]
matchingSoleDigests =
    [ ("a hex sha1 shasum", unsafeHash SHA1 trueSha1)
    , ("an upper-cased hex sha1 shasum", unsafeHash SHA1 (T.toUpper trueSha1))
    , ("a sha512 SRI", unsafeHash SRI trueSri)
    , ("a sha384 SRI", unsafeHash SRI trueSha384Sri)
    , ("a sha256 SRI", unsafeHash SRI trueSha256Sri)
    , ("a raw SHA512-tagged hex digest", unsafeHash Pkg.SHA512 trueSha512Hex)
    , ("a raw SHA384-tagged hex digest", unsafeHash Pkg.SHA384 trueSha384Hex)
    , ("a blake2b-512 digest", unsafeHash Blake2b trueBlake2b)
    , ("a sha256 digest", unsafeHash SHA256 trueSha256)
    ]

{- | The tamper direction of the same arms. The detail names the algorithm, because that is
what an operator reads back off the refusal.
-}
failingSoleDigests :: [(String, Hash, Text)]
failingSoleDigests =
    [ ("a hex sha1 shasum", unsafeHash SHA1 wrongSha1, "the SHA1 digest did not match the fetched bytes")
    , ("a sha512 SRI", unsafeHash SRI falseSri, "the SRI sha512 digest did not match the fetched bytes")
    , -- base64 is case-sensitive, so a case-folding comparison would admit a different digest.
      ("a case-folded sha512 SRI body", unsafeHash SRI caseVariantSri, "the SRI sha512 digest did not match the fetched bytes")
    , ("a sha384 SRI", unsafeHash SRI falseSha384Sri, "the SRI sha384 digest did not match the fetched bytes")
    , ("a sha256 SRI", unsafeHash SRI someSha256Sri, "the SRI sha256 digest did not match the fetched bytes")
    , ("a blake2b-512 digest", unsafeHash Blake2b someBlake2b, "the Blake2b digest did not match the fetched bytes")
    , ("a sha256 digest", unsafeHash SHA256 someSha256, "the SHA256 digest did not match the fetched bytes")
    ]

{- | A matching sha1 beside a stronger digest. The strong digest alone decides in either
order, so a weak match can never rescue bytes the strong one refuses.
-}
copresentDigestSpec :: Spec
copresentDigestSpec = describe "a matching sha1 beside a stronger digest" $
    for_ rows $ \(label, strong, expected) ->
        it ("takes the verdict of " <> label <> ", whichever order the two arrive in") $ do
            verifyIntegrity (unsafeHash SHA1 trueSha1 :| [strong]) tarballBytes `shouldBe` expected
            verifyIntegrity (strong :| [unsafeHash SHA1 trueSha1]) tarballBytes `shouldBe` expected
  where
    rows =
        [ ("a matching sha512 SRI", unsafeHash SRI trueSri, IntegrityVerified)
        , ("a matching sha384 SRI", unsafeHash SRI trueSha384Sri, IntegrityVerified)
        , ("a matching blake2b-512 digest", unsafeHash Blake2b trueBlake2b, IntegrityVerified)
        , ("a failing sha512 SRI", unsafeHash SRI falseSri, IntegrityMismatch "the SRI sha512 digest did not match the fetched bytes")
        , ("a failing sha256 SRI", unsafeHash SRI someSha256Sri, IntegrityMismatch "the SRI sha256 digest did not match the fetched bytes")
        ]

-- Several SRI components at the strongest algorithm are alternatives to one another, and
-- nothing else is.
sriAlternativeSpec :: Spec
sriAlternativeSpec = describe "SRI alternatives at the strongest algorithm" $ do
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

-- Which digest of several the gate selects, once they are not alternatives.
selectionSpec :: Spec
selectionSpec = describe "selecting among co-present digests" $ do
    it "keeps the last equal-ranked raw assertion without treating raw hashes as alternatives" $ do
        let matching = unsafeHash Pkg.SHA512 trueSha512Hex
            different = unsafeHash Pkg.SHA512 (hexSha512Of "other bytes")
        verifyIntegrity (matching :| [different]) tarballBytes
            `shouldBe` IntegrityMismatch "the SHA512 digest did not match the fetched bytes"
        verifyIntegrity (different :| [matching]) tarballBytes `shouldBe` IntegrityVerified

    it "prefers sha512 over a matching blake2b when both are present" $
        verifyIntegrity (unsafeHash Blake2b trueBlake2b :| [unsafeHash SRI falseSri]) tarballBytes
            `shouldBe` IntegrityMismatch "the SRI sha512 digest did not match the fetched bytes"

    it "prefers a computable sha256 over an equal-tier unresolvable SRI, independent of order" $ do
        let realSha256 = unsafeHash SHA256 trueSha256
        verifyIntegrity (unresolvableSri :| [realSha256]) tarballBytes `shouldBe` IntegrityVerified
        verifyIntegrity (realSha256 :| [unresolvableSri]) tarballBytes `shouldBe` IntegrityVerified

-- An SRI whose prefix names an algorithm the worker has no digest for.
unresolvableSri :: Hash
unresolvableSri =
    (unsafeHash SRI trueSha256Sri)
        { hashValue = "sha3-" <> sriBody trueSha256Sri
        }
