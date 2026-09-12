-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The integrity vocabulary shared by admission, merge, and worker verification.
module Ecluse.Core.Package.Hash (
    -- * Hashes
    Hash,
    hashAlg,
    hashValue,
    canonicalHashValue,
    mkHash,
    mkSriHashes,
    HashAlg (..),

    -- * Algorithm vocabulary
    renderHashAlg,
    parseHashAlg,
    sriPrefix,
    sriBody,
    sriAlgorithm,

    -- * Digest computation
    computeDigest,
    isComputable,
) where

import Crypto.Hash (Blake2b_512, Digest, MD5, SHA1, SHA256, SHA384, SHA512, digestFromByteString, hashlazy)
import Data.ByteArray (convert)
import Data.ByteArray.Encoding (Base (Base16, Base64), convertFromBase, convertToBase)
import Data.Text qualified as T
import Data.Universe.Class (Universe (..))
import Data.Universe.Generic (universeGeneric)

{- | A hash algorithm an integrity digest is computed with. The 'Ord' instance is integrity
authority, not constructor order: @SRI < MD5 < SHA1 < SHA256 < SHA384 < Blake2b < SHA512@.
-}
data HashAlg
    = SHA1
    | SHA256
    | SHA384
    | SHA512
    | MD5
    | Blake2b
    | -- | One Subresource-Integrity component. 'mkSriHashes' splits whitespace-separated components.
      SRI
    deriving stock (Eq, Generic, Show)

-- Derived from Generic so a new HashAlg needs no hand-maintained list. The cross-module
-- floor-vs-compute invariant test relies on this enumeration being exhaustive.
instance Universe HashAlg where universe = universeGeneric

instance Ord HashAlg where
    compare a b = compare (hashAlgRank a) (hashAlgRank b)

-- Explicit integrity ordering, weakest to strongest. The gaps are only for
-- readability: order, not arithmetic distance, is the policy.
hashAlgRank :: HashAlg -> Int
hashAlgRank = \case
    SRI -> 0
    MD5 -> 10
    SHA1 -> 20
    SHA256 -> 30
    SHA384 -> 40
    Blake2b -> 50
    SHA512 -> 60

-- | An artifact digest validated by 'mkHash'. Record updates must preserve its encoding and length.
data Hash = Hash
    { hashAlg :: HashAlg
    -- ^ The algorithm the digest was computed with.
    , hashValue :: Text
    {- ^ The digest itself, in the algorithm's wire encoding (e.g. hex, or the
    single @sha512-…@ component for 'SRI').
    -}
    }
    deriving stock (Eq, Show)

-- | Validate encoding and digest length, preserving the wire spelling. Strength is a separate admission decision.
mkHash :: HashAlg -> Text -> Either Text Hash
mkHash alg value
    | isJust (decodeHash alg value) = Right (Hash alg value)
    | otherwise = Left ("malformed " <> renderHashAlg alg <> " digest")

-- | Split SRI components, rejecting the whole string when empty or when any component is malformed.
mkSriHashes :: Text -> Either Text (NonEmpty Hash)
mkSriHashes wire = case nonEmpty (T.words wire) of
    Nothing -> Left "malformed sri digest"
    Just comps -> traverse (mkHash SRI) comps

{- | Lowercase hex for comparison, or 'Nothing' if a record update introduced an invalid digest.
The original 'hashValue' remains unchanged.
-}
canonicalHashValue :: Hash -> Maybe Text
canonicalHashValue h =
    decodeUtf8 . (convertToBase Base16 :: ByteString -> ByteString) <$> decodeHash (hashAlg h) (hashValue h)

decodeHash :: HashAlg -> Text -> Maybe ByteString
decodeHash SRI value = do
    guard (T.words value == [value])
    alg <- sriAlgorithm value
    decodeDigest Base64 alg (sriBody value)
decodeHash alg value = decodeDigest Base16 alg (T.toLower value)

decodeDigest :: Base -> HashAlg -> Text -> Maybe ByteString
decodeDigest base alg value = do
    bytes <- rightToMaybe (convertFromBase base (encodeUtf8 value :: ByteString))
    guard (digestLengthOk alg bytes)
    pure bytes

-- 'digestFromByteString' is the length check: it accepts only an input of exactly the
-- algorithm's digest size.
digestLengthOk :: HashAlg -> ByteString -> Bool
digestLengthOk alg bytes = case alg of
    SHA1 -> isJust (digestFromByteString @SHA1 bytes)
    SHA256 -> isJust (digestFromByteString @SHA256 bytes)
    SHA384 -> isJust (digestFromByteString @SHA384 bytes)
    SHA512 -> isJust (digestFromByteString @SHA512 bytes)
    MD5 -> isJust (digestFromByteString @MD5 bytes)
    Blake2b -> isJust (digestFromByteString @Blake2b_512 bytes)
    SRI -> False

-- | Digest computation for verifiable algorithms. MD5 cannot prove integrity, and SRI must first resolve its algorithm.
computeDigest :: HashAlg -> Maybe (LByteString -> ByteString)
computeDigest = \case
    SHA1 -> Just (digestBytes . hashlazy @SHA1)
    SHA256 -> Just (digestBytes . hashlazy @SHA256)
    SHA384 -> Just (digestBytes . hashlazy @SHA384)
    SHA512 -> Just (digestBytes . hashlazy @SHA512)
    Blake2b -> Just (digestBytes . hashlazy @Blake2b_512)
    MD5 -> Nothing
    SRI -> Nothing
  where
    digestBytes :: Digest a -> ByteString
    digestBytes = convert

-- | Whether the worker can compute and verify the algorithm.
isComputable :: HashAlg -> Bool
isComputable = isJust . computeDigest

-- | The canonical lowercase name, also used in configuration and error text.
renderHashAlg :: HashAlg -> Text
renderHashAlg = \case
    MD5 -> "md5"
    SHA1 -> "sha1"
    SHA256 -> "sha256"
    SHA384 -> "sha384"
    SHA512 -> "sha512"
    Blake2b -> "blake2b"
    SRI -> "sri"

-- | Parse canonical names and single-dash aliases, ignoring case and surrounding whitespace. SRI is not selectable.
parseHashAlg :: Text -> Either Text HashAlg
parseHashAlg raw = case T.toLower (T.strip raw) of
    "md5" -> Right MD5
    "sha1" -> Right SHA1
    "sha-1" -> Right SHA1
    "sha256" -> Right SHA256
    "sha-256" -> Right SHA256
    "sha384" -> Right SHA384
    "sha-384" -> Right SHA384
    "sha512" -> Right SHA512
    "sha-512" -> Right SHA512
    "blake2b" -> Right Blake2b
    _ -> Left ("unknown integrity algorithm: " <> raw)

-- | The token before the first dash. Without a dash, the entire string is the prefix.
sriPrefix :: Text -> Text
sriPrefix = fst . T.breakOn "-"

-- | The body after the first dash, or empty text when there is no dash.
sriBody :: Text -> Text
sriBody = T.drop 1 . snd . T.breakOn "-"

-- | Resolve an SRI prefix. An unsupported prefix asserts no algorithm and clears no integrity floor.
sriAlgorithm :: Text -> Maybe HashAlg
sriAlgorithm sri = case sriPrefix sri of
    "sha256" -> Just SHA256
    "sha384" -> Just SHA384
    "sha512" -> Just SHA512
    _ -> Nothing
