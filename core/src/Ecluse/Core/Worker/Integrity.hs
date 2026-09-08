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
) where

import Data.ByteArray.Encoding (Base (Base16, Base64), convertToBase)
import Data.Text qualified as T

import Ecluse.Core.Package (Hash (hashAlg, hashValue), HashAlg (SRI), computeDigest, sriBody, sriPrefix)
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
verifyIntegrity hashes bytes =
    let strongest = authoritativeDigest hashes
     in case matchesDigest (toLazy bytes) strongest hashes of
            Nothing ->
                IntegrityMismatch
                    ( "the strongest admitted digest ("
                        <> describeDigest strongest
                        <> ") is in an algorithm the worker cannot verify"
                    )
            Just True -> IntegrityVerified
            Just False ->
                IntegrityMismatch ("the " <> describeDigest strongest <> " digest did not match the fetched bytes")

matchesDigest :: LByteString -> Hash -> NonEmpty Hash -> Maybe Bool
matchesDigest lazyBytes h hashes = do
    alg <- assertedAlg h
    digestOf <- computeDigest alg
    let digest = digestOf lazyBytes
    pure $ case hashAlg h of
        SRI ->
            let encoded = base64 digest
             in any (\candidate -> hashAlg candidate == SRI && assertedAlg candidate == Just alg && sriBody (hashValue candidate) == encoded) hashes
        _ -> hexLower digest == T.toLower (hashValue h)

describeDigest :: Hash -> Text
describeDigest h = case hashAlg h of
    SRI -> "SRI " <> sriPrefix (hashValue h)
    alg -> show alg

hexLower :: ByteString -> Text
hexLower d = T.toLower (decodeUtf8 (convertToBase Base16 d :: ByteString))

base64 :: ByteString -> Text
base64 d = decodeUtf8 (convertToBase Base64 d :: ByteString)
