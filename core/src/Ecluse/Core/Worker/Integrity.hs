-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The integrity gate is the security crux of the worker.

The private upstream later serves a mirrored artifact __without re-running the
rules__, so a corrupt or tampered artifact must never enter it. Verification is
therefore the gate: a hash __mismatch fails the job with no publish__, and the worker
logs it loudly. The gate verifies the __re-admitted__ artifact's digests, the exact
set the worker's ingest re-evaluation floor-checked against current metadata. The
gate therefore always checks the bytes against a digest current policy admitted. The
queue payload contributes no digest to the gate.
-}
module Ecluse.Core.Worker.Integrity (
    IntegrityResult (..),
    verifyIntegrity,
) where

import Data.ByteArray.Encoding (Base (Base16, Base64), convertToBase)
import Data.Text qualified as T

import Ecluse.Core.Package (Hash (hashAlg, hashValue), HashAlg (SRI), computeDigest, sriBody, sriPrefix)
import Ecluse.Core.Package.Integrity (assertedAlg, authoritativeDigest)

{- | The result of verifying fetched bytes against the admitted integrity digests. A mismatch
carries the detail an operator needs to explain why the worker refused a publish.
-}
data IntegrityResult
    = -- | The bytes matched the most authoritative admitted digest.
      IntegrityVerified
    | {- | The bytes failed the integrity gate. Carries a human-readable detail (the
      digest they were checked against, or that the strongest one was uncomputable).
      -}
      IntegrityMismatch Text
    deriving stock (Eq, Show)

{- | Verify fetched artifact bytes against the __most authoritative__ digest the version carries,
never a weaker one while a stronger is present. An npm version carries both an SRI @sha512@ and
a legacy SHA-1 @shasum@, and SHA-1 collision resistance is broken, so passing on either match
would admit a forged artifact. A digest in an algorithm the worker cannot recompute fails
closed.

>>> import Ecluse.Core.Package (mkHash, HashAlg (SHA1)) >>> fmap (\h -> verifyIntegrity (h :| [])
"Hello World") (mkHash SHA1 "0a4d55a8d778e5022fab701977c5d840bbc486d0") Right IntegrityVerified

>>> fmap (\h -> verifyIntegrity (h :| []) "Hello World") (mkHash SHA1
"da39a3ee5e6b4b0d3255bfef95601890afd80709") Right (IntegrityMismatch "the SHA1 digest did not match
the fetched bytes")
-}
verifyIntegrity :: NonEmpty Hash -> ByteString -> IntegrityResult
verifyIntegrity hashes bytes =
    let strongest = authoritativeDigest hashes
     in case matchesDigest (toLazy bytes) strongest of
            Nothing ->
                -- Fail closed: nothing here can prove the bytes, and dropping to a weaker digest
                -- would accept one an attacker could forge.
                IntegrityMismatch
                    ( "the strongest admitted digest ("
                        <> describeDigest strongest
                        <> ") is in an algorithm the worker cannot verify"
                    )
            Just True -> IntegrityVerified
            Just False ->
                IntegrityMismatch ("the " <> describeDigest strongest <> " digest did not match the fetched bytes")

-- Whether the bytes match the digest. 'Nothing' means an unresolvable or unverifiable algorithm,
-- which 'verifyIntegrity' fails closed on. Hex compares case-insensitively. Base64 does not,
-- because folding its case would admit a digest that matches only after a case change.
matchesDigest :: LByteString -> Hash -> Maybe Bool
matchesDigest lazyBytes h = do
    alg <- assertedAlg h
    digestOf <- computeDigest alg
    let digest = digestOf lazyBytes
    pure $ case hashAlg h of
        SRI -> base64 digest == sriBody (hashValue h)
        _ -> hexLower digest == T.toLower (hashValue h)

-- Name a digest for the mismatch detail: the SRI prefix for an SRI, the
-- algorithm otherwise.
describeDigest :: Hash -> Text
describeDigest h = case hashAlg h of
    SRI -> "SRI " <> sriPrefix (hashValue h)
    alg -> show alg

-- The lower-cased hex encoding of raw digest bytes (matching npm's hex shasum form).
hexLower :: ByteString -> Text
hexLower d = T.toLower (decodeUtf8 (convertToBase Base16 d :: ByteString))

-- The standard-base64 encoding of raw digest bytes (matching the SRI @<base64>@ body).
base64 :: ByteString -> Text
base64 d = decodeUtf8 (convertToBase Base64 d :: ByteString)
