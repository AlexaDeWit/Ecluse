-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Verify artifact bytes before publication into the trusted mirror.
The gate uses current metadata from admission, never digests from the queue.
SRI components at the strongest algorithm are alternatives. A weaker digest
cannot rescue a mismatch, because that would permit substitution through a broken hash.
-}
module Ecluse.Core.Worker.Integrity (
    IntegrityResult (..),
    verifyIntegrity,
    verifyIntegrityDigest,
) where

import Data.Text qualified as T

import Ecluse.Core.Package (Hash (hashAlg, hashValue), HashAlg (SRI), computeDigest, sriBody, sriPrefix)
import Ecluse.Core.Package.Hash (base64DigestText, hexDigestText)
import Ecluse.Core.Package.Integrity (assertedAlg, authoritativeDigest)

-- | Whether fetched bytes may enter the mirror, with a refusal detail for the operator.
data IntegrityResult
    = -- | The bytes matched the selected digest or one of its SRI alternatives.
      IntegrityVerified
    | -- | The selected digest mismatched or its algorithm cannot be computed.
      IntegrityMismatch Text
    deriving stock (Eq, Show)

{- | Verify the strongest selected digest, allowing only its same-algorithm SRI alternatives.
A weaker match or an uncomputable selected algorithm never permits publication.
-}
verifyIntegrity :: NonEmpty Hash -> ByteString -> IntegrityResult
verifyIntegrity hashes = either IntegrityMismatch (const IntegrityVerified) . verifyIntegrityDigest hashes

-- | Return the computed algorithm and digest only after the strongest admitted assertion verifies the bytes.
verifyIntegrityDigest :: NonEmpty Hash -> ByteString -> Either Text (HashAlg, ByteString)
verifyIntegrityDigest hashes bytes =
    let strongest = authoritativeDigest hashes
     in case computedDigest strongest bytes of
            Nothing ->
                Left
                    ( "the strongest admitted digest ("
                        <> describeDigest strongest
                        <> ") is in an algorithm the worker cannot verify"
                    )
            Just computed@(alg, digest)
                | matchesDigest alg digest strongest hashes -> Right computed
                | otherwise -> Left ("the " <> describeDigest strongest <> " digest did not match the fetched bytes")

computedDigest :: Hash -> ByteString -> Maybe (HashAlg, ByteString)
computedDigest h bytes = do
    alg <- assertedAlg h
    digestOf <- computeDigest alg
    pure (alg, digestOf (toLazy bytes))

matchesDigest :: HashAlg -> ByteString -> Hash -> NonEmpty Hash -> Bool
matchesDigest alg digest h hashes = case hashAlg h of
    SRI ->
        let encoded = base64DigestText digest
         in any (\candidate -> hashAlg candidate == SRI && assertedAlg candidate == Just alg && sriBody (hashValue candidate) == encoded) hashes
    _ -> hexDigestText digest == T.toLower (hashValue h)

describeDigest :: Hash -> Text
describeDigest h = case hashAlg h of
    SRI -> "SRI " <> sriPrefix (hashValue h)
    alg -> show alg
