-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The integrity-digest vocabulary: hash algorithms and their authority order, the
validated 'Hash' value, digest computation, and the Subresource-Integrity wire forms.
This is the single home for that vocabulary, so the worker's tamper gate, the
serve-admission floor, and the queue wire share one notion of what @"sha512"@ means and
what an SRI asserts. It sits in the package layer's lowest module because 'mkHash' needs
it. "Ecluse.Core.Package" re-exports the whole surface, so import this module directly
only where the package vocabulary itself is not needed.
-}
module Ecluse.Core.Package.Hash (
    -- * Hashes
    Hash,
    hashAlg,
    hashValue,
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
import Data.ByteArray.Encoding (Base (Base16, Base64), convertFromBase)
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
    | {- | A single Subresource-Integrity component (npm @dist.integrity@), e.g.
      @"sha512-…"@. Exactly one @\<alg\>-\<base64\>@ component per 'Hash':
      'mkSriHashes' splits a wire string that joins several with whitespace into one
      'Hash' per component, so every reader resolves the same algorithm and digest
      body from 'hashValue'.
      -}
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

{- | An integrity digest of an artifact. The type is __opaque__: 'mkHash' is the only way to
build one, so every value carries the proof that its digest is well-formed for its algorithm.
-}
data Hash = Hash
    { hashAlg :: HashAlg
    -- ^ The algorithm the digest was computed with.
    , hashValue :: Text
    {- ^ The digest itself, in the algorithm's wire encoding (e.g. hex, or the
    single @sha512-…@ component for 'SRI').
    -}
    }
    deriving stock (Eq, Show)

{- | Build a 'Hash', validating the digest is structurally well-formed: correctly encoded and
exactly its algorithm's digest length. This is the only constructor, so a degenerate digest can
never reach an integrity gate (@docs\/architecture\/security.md@ invariant 5). Well-formedness is
not admissibility: the strength floor is "Ecluse.Core.Package.Integrity"'s separate decision.

>>> import Ecluse.Core.Package.Hash (HashAlg (SHA1))
>>> fmap hashAlg (mkHash SHA1 "0a4d55a8d778e5022fab701977c5d840bbc486d0")
Right SHA1

>>> mkHash SHA1 "deadbeef"
Left "malformed sha1 digest"
-}
mkHash :: HashAlg -> Text -> Either Text Hash
mkHash alg value
    | wellFormed alg value = Right (Hash alg value)
    | otherwise = Left ("malformed " <> renderHashAlg alg <> " digest")

{- | Split a Subresource-Integrity wire string (npm's @dist.integrity@) into one 'SRI' 'Hash' per
whitespace-separated component, each built through 'mkHash'. It rejects the whole string when
it holds no component or any component is malformed, so a partial digest set never forms.

>>> fmap length (mkSriHashes
"sha512-z4PhNX7vuL3xVChQ1m2AB9Yg5AULVxXcg/SpIdNs6c5H0NE8XYXysP+DGNKHfuwvY7kxvUdBeoGlODJ6+SfaPg==
sha256-47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=") Right 2

>>> mkSriHashes "  " Left "malformed sri digest"
-}
mkSriHashes :: Text -> Either Text (NonEmpty Hash)
mkSriHashes wire = case nonEmpty (T.words wire) of
    Nothing -> Left "malformed sri digest"
    Just comps -> traverse (mkHash SRI) comps

wellFormed :: HashAlg -> Text -> Bool
wellFormed = \case
    SRI -> wellFormedSri
    alg -> wellFormedHex alg

wellFormedHex :: HashAlg -> Text -> Bool
wellFormedHex alg t =
    case convertFromBase Base16 (encodeUtf8 (T.toLower t) :: ByteString) :: Either String ByteString of
        Left _ -> False
        Right bytes -> digestLengthOk alg bytes

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

{- | The digest function for an algorithm, or 'Nothing' for one Écluse will not verify against.
'MD5' is uncomputable on purpose: a match on a broken hash cannot prove the bytes were not
substituted. A bare 'SRI' names no algorithm, so resolve it with 'sriAlgorithm' first.
Every algorithm the integrity floor ("Ecluse.Core.Package.Integrity") admits is computable here.
-}
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

{- | Whether the worker can compute, and so verify, a digest in the given algorithm.

>>> isComputable SHA256
True

>>> isComputable MD5
False
-}
isComputable :: HashAlg -> Bool
isComputable = isJust . computeDigest

{- An 'SRI' 'Hash' holds exactly one @\<alg\>-\<base64\>@ component with no surrounding
whitespace, because 'sriPrefix' and 'sriBody' read the stored value verbatim.
-}
wellFormedSri :: Text -> Bool
wellFormedSri t = case T.words t of
    [comp] -> comp == t && wellFormedSriComponent comp
    _ -> False

wellFormedSriComponent :: Text -> Bool
wellFormedSriComponent comp
    -- An empty body means no @\<alg\>-\<base64\>@ shape (no separator, or nothing after it).
    | T.null (sriBody comp) = False
    | otherwise = sriBodyOk (sriAlgorithm comp) (sriBody comp)

-- 'sriAlgorithm' resolves the name, so a well-formed component always names an algorithm
-- the strength tier ranks ('assertedAlg').
sriBodyOk :: Maybe HashAlg -> Text -> Bool
sriBodyOk Nothing _ = False
sriBodyOk (Just alg) body =
    case convertFromBase Base64 (encodeUtf8 body :: ByteString) :: Either String ByteString of
        Left _ -> False
        Right bytes -> digestLengthOk alg bytes

{- | The lower-case wire name of an algorithm: the canonical spelling 'parseHashAlg'
reads back. Total and injective, so it doubles as config rendering and error text.

>>> renderHashAlg SHA256
"sha256"
-}
renderHashAlg :: HashAlg -> Text
renderHashAlg = \case
    MD5 -> "md5"
    SHA1 -> "sha1"
    SHA256 -> "sha256"
    SHA384 -> "sha384"
    SHA512 -> "sha512"
    Blake2b -> "blake2b"
    SRI -> "sri"

{- | Parse an algorithm name, tolerating surrounding whitespace, case, and the single
family-separating dash (@"SHA-256"@ and @"sha256"@ both parse). It accepts only these exact
spellings, never an arbitrary internal dash, and it rejects @sri@, which is not config-selectable.

>>> parseHashAlg "SHA-256"
Right SHA256

>>> parseHashAlg "frobnicate"
Left "unknown integrity algorithm: frobnicate"
-}
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

{- | The algorithm-name token of a Subresource-Integrity string: the @\<alg\>@ before
the first @\'-\'@ in @\<alg\>-\<base64\>@. A string with no @\'-\'@ is all prefix.

>>> sriPrefix "sha512-Zm9vYmFy"
"sha512"
-}
sriPrefix :: Text -> Text
sriPrefix = fst . T.breakOn "-"

{- | The base64 digest body of a Subresource-Integrity string: the @\<base64\>@ after
the first @\'-\'@ in @\<alg\>-\<base64\>@. A string with no @\'-\'@ has an empty body.

>>> sriBody "sha512-Zm9vYmFy"
"Zm9vYmFy"
-}
sriBody :: Text -> Text
sriBody = T.drop 1 . snd . T.breakOn "-"

{- | The 'HashAlg' a Subresource-Integrity string names, read from its @\<alg\>@ prefix.
An unrecognised prefix yields 'Nothing', so the string asserts no algorithm and clears no floor.

>>> sriAlgorithm "sha512-Zm9vYmFy"
Just SHA512

>>> sriAlgorithm "sha384-Zm9vYmFy"
Just SHA384
-}
sriAlgorithm :: Text -> Maybe HashAlg
sriAlgorithm sri = case sriPrefix sri of
    "sha256" -> Just SHA256
    "sha384" -> Just SHA384
    "sha512" -> Just SHA512
    _ -> Nothing
