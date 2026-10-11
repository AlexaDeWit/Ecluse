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

    -- * Wire encodings of digest bytes
    hexDigestText,
    base64DigestText,
) where

import Crypto.Hash (Blake2b_512, Digest, MD5, SHA1, SHA256, SHA384, SHA512, digestFromByteString, hashlazy)
import Crypto.Hash qualified as Crypto
import Data.ByteArray (convert)
import Data.ByteArray.Encoding (Base (Base16, Base64), convertFromBase, convertToBase)
import Data.Char (isHexDigit)
import Data.Text qualified as T
import Data.Universe.Class (Universe (..))
import Data.Universe.Generic (universeGeneric)

import Ecluse.Core.Text (isAsciiAlphaNum, toLowerUnlessAsciiUnchanged)

-- | A digest algorithm, ordered by integrity authority: @SRI < MD5 < SHA1 < SHA256 < SHA384 < Blake2b < SHA512@.
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

-- The floor-vs-compute invariant test requires an exhaustive enumeration.
instance Universe HashAlg where universe = universeGeneric

instance Ord HashAlg where
    compare a b = compare (hashAlgRank a) (hashAlgRank b)

-- Admission depends on order, not the distance between ranks.
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
    -- ^ The digest algorithm.
    , hashValue :: Text
    -- ^ Hex, or one @sha512-…@ component for 'SRI'.
    }
    deriving stock (Eq, Show)

-- | Validate encoding and digest length, preserving the wire spelling. Strength is a separate admission decision.
mkHash :: HashAlg -> Text -> Either Text Hash
mkHash alg value
    | isWellFormed alg value = Right (Hash alg value)
    | otherwise = Left ("malformed " <> renderHashAlg alg <> " digest")

-- | Split SRI components, rejecting the whole string when empty or when any component is malformed.
mkSriHashes :: Text -> Either Text (NonEmpty Hash)
mkSriHashes wire = case nonEmpty (T.words wire) of
    Nothing -> Left "malformed sri digest"
    -- A lone component equal to the input is the input itself, so a retained hash adds no text object.
    Just (only :| []) | only == wire -> pure <$> mkHash SRI wire
    Just comps -> traverse (mkHash SRI) comps

isWellFormed :: HashAlg -> Text -> Bool
isWellFormed SRI value = maybe False (isBase64Digest (sriBody value)) (sriAlgorithm value >>= digestSize)
isWellFormed alg value = maybe False (isHexDigest value) (digestSize alg)

-- No character outside ASCII lowercases to a hex digit.
isHexDigest :: Text -> Int -> Bool
isHexDigest value size = T.compareLength value (2 * size) == EQ && T.all isHexDigit value

-- Like the decoder, this leaves the last digit's spare bits unchecked.
isBase64Digest :: Text -> Int -> Bool
isBase64Digest body size =
    T.compareLength digits digitCount == EQ && T.compareLength padding padCount == EQ && T.all (== '=') padding
  where
    (digits, padding) = T.span isBase64Digit body
    digitCount = (4 * size + 2) `div` 3
    padCount = negate digitCount `mod` 4

isBase64Digit :: Char -> Bool
isBase64Digit c = isAsciiAlphaNum c || c == '+' || c == '/'

digestSize :: HashAlg -> Maybe Int
digestSize = \case
    SHA1 -> Just (Crypto.hashDigestSize Crypto.SHA1)
    SHA256 -> Just (Crypto.hashDigestSize Crypto.SHA256)
    SHA384 -> Just (Crypto.hashDigestSize Crypto.SHA384)
    SHA512 -> Just (Crypto.hashDigestSize Crypto.SHA512)
    MD5 -> Just (Crypto.hashDigestSize Crypto.MD5)
    Blake2b -> Just (Crypto.hashDigestSize Crypto.Blake2b_512)
    SRI -> Nothing

-- | The lowercase hex a non-SRI digest is compared and reported in.
hexDigestText :: ByteString -> Text
hexDigestText d = decodeUtf8 (convertToBase Base16 d :: ByteString)

-- | The base64 body an SRI component carries after its algorithm prefix.
base64DigestText :: ByteString -> Text
base64DigestText d = decodeUtf8 (convertToBase Base64 d :: ByteString)

-- | Comparison hex without changing 'hashValue', or 'Nothing' after an invalid record update.
canonicalHashValue :: Hash -> Maybe Text
canonicalHashValue h = hexDigestText <$> decodeHash (hashAlg h) (hashValue h)

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
parseHashAlg raw = hashAlgNamed fallback stripped
  where
    stripped = T.strip raw
    fallback = hashAlgNamed (Left ("unknown integrity algorithm: " <> raw)) (toLowerUnlessAsciiUnchanged stripped)

-- The fallback stays lazy so an exact name does not force normalisation.
hashAlgNamed :: Either Text HashAlg -> Text -> Either Text HashAlg
hashAlgNamed onUnknown = \case
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
    _ -> onUnknown

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
