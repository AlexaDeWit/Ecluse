-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The integrity-digest vocabulary: hash algorithms and their authority order,
the validated 'Hash' value, digest computation, and the Subresource-Integrity
wire forms.

This is the single home for the algorithm vocabulary: the wire name an algorithm
renders to and parses from, and how a Subresource-Integrity string is split and
resolved. It lives in the package layer's lowest module because 'mkHash' needs it
and the vocabulary's consumers must never disagree on it: everything that names an
algorithm or reads an SRI (the worker's tamper gate, the serve-admission floor, the
queue wire) defers here, so they share one notion of what @"sha512"@ means and what
an SRI asserts rather than each re-encoding it. "Ecluse.Core.Package" re-exports
this whole surface for its callers; import this module directly only where the
package vocabulary itself is not needed.
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

{- | A hash algorithm an integrity digest is computed with.

The 'Ord' instance is the integrity authority order, not constructor order:
@SRI < MD5 < SHA1 < SHA256 < SHA384 < Blake2b < SHA512@. A bare 'SRI' is a wrapper,
not an algorithm; callers that care about its embedded algorithm should resolve it first.
-}
data HashAlg
    = SHA1
    | SHA256
    | SHA384
    | SHA512
    | MD5
    | Blake2b
    | {- | A single Subresource-Integrity component (npm @dist.integrity@), e.g.
      @"sha512-…"@. Exactly one @\<alg\>-\<base64\>@ component per 'Hash': a wire
      string that joins several with whitespace is split into one 'Hash' per
      component by 'mkSriHashes', so every reader resolves the same algorithm and
      digest body from 'hashValue'.
      -}
      SRI
    deriving stock (Bounded, Enum, Eq, Show)

instance Ord HashAlg where
    compare a b = compare (hashAlgRank a) (hashAlgRank b)

-- Explicit integrity ordering, weakest to strongest. The gaps are only for
-- readability; order, not arithmetic distance, is the policy.
hashAlgRank :: HashAlg -> Int
hashAlgRank = \case
    SRI -> 0
    MD5 -> 10
    SHA1 -> 20
    SHA256 -> 30
    SHA384 -> 40
    Blake2b -> 50
    SHA512 -> 60

{- | An integrity digest of an artifact. __Opaque__: a 'Hash' is built only through
'mkHash', which validates that the digest is well-formed, so every value of this type
carries the proof that its digest could be a real digest of its algorithm. Read it
back through 'hashAlg' and 'hashValue'.
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

{- | Build a 'Hash', validating that the digest is __structurally well-formed__:
cleanly encoded and exactly the byte length its algorithm specifies. This is the only
way to construct a 'Hash', so the type itself is the proof that the digest could be a
real digest of that algorithm -- an empty, truncated, over-long, non-hex, or bad-base64
value is unconstructable and so can never reach an integrity gate as a degenerate
digest (the fail-open this closes is @docs\/architecture\/security.md@ invariant 5).

Well-formedness is __not__ admissibility: a well-formed but weak SHA-1 digest builds
fine; whether it clears the public-integrity floor is the separate decision of
"Ecluse.Core.Package.Integrity". 'mkHash' rejects a malformed digest, never a merely weak one.

A hex-tagged algorithm (everything but 'SRI') takes lower- or upper-case hex of the
algorithm's digest length. An 'SRI' takes __exactly one__ @\<alg\>-\<base64\>@
component, naming a Subresource-Integrity algorithm (@sha256@, @sha384@, @sha512@)
whose base64 body decodes to that algorithm's digest length. A wire string that
joins several components with whitespace is malformed /here/: split it with
'mkSriHashes', which yields one 'Hash' per component, so no reader ever has to
decide which component of a joined string a 'Hash' means.

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

{- | Split a Subresource-Integrity __wire string__ -- one or more
whitespace-separated @\<alg\>-\<base64\>@ components (npm's @dist.integrity@) --
into one 'SRI' 'Hash' per component, each built through the validating 'mkHash'.
The whole string is rejected when it carries no component or /any/ component is
malformed, so a partially-valid value never yields a partial digest set.

This is the one intended path from wire data to 'SRI' hashes. Because each
resulting 'Hash' holds exactly one component, the admission floor, the worker's
tamper gate, and the divergence fingerprint all resolve the same algorithm and
digest body from it -- there is no joined string left for two consumers to read
two different ways.

>>> fmap length (mkSriHashes "sha512-z4PhNX7vuL3xVChQ1m2AB9Yg5AULVxXcg/SpIdNs6c5H0NE8XYXysP+DGNKHfuwvY7kxvUdBeoGlODJ6+SfaPg== sha256-47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=")
Right 2

>>> mkSriHashes "  "
Left "malformed sri digest"
-}
mkSriHashes :: Text -> Either Text (NonEmpty Hash)
mkSriHashes wire = case nonEmpty (T.words wire) of
    Nothing -> Left "malformed sri digest"
    Just comps -> traverse (mkHash SRI) comps

-- Whether a digest string is a well-formed digest of the given algorithm.
wellFormed :: HashAlg -> Text -> Bool
wellFormed = \case
    SRI -> wellFormedSri
    alg -> wellFormedHex alg

-- A hex digest is well-formed when it decodes as hex (case-insensitively) to exactly
-- the algorithm's digest length -- which 'digestFromByteString' decides by accepting
-- only an input of the right size.
wellFormedHex :: HashAlg -> Text -> Bool
wellFormedHex alg t =
    case convertFromBase Base16 (encodeUtf8 (T.toLower t) :: ByteString) :: Either String ByteString of
        Left _ -> False
        Right bytes -> hexDigestOk alg bytes

hexDigestOk :: HashAlg -> ByteString -> Bool
hexDigestOk alg bytes = case alg of
    SHA1 -> isJust (digestFromByteString @SHA1 bytes)
    SHA256 -> isJust (digestFromByteString @SHA256 bytes)
    SHA384 -> isJust (digestFromByteString @SHA384 bytes)
    SHA512 -> isJust (digestFromByteString @SHA512 bytes)
    MD5 -> isJust (digestFromByteString @MD5 bytes)
    Blake2b -> isJust (digestFromByteString @Blake2b_512 bytes)
    SRI -> False

{- | Compute the digest of bytes in a given algorithm, as the raw digest bytes, or
'Nothing' for an algorithm Écluse will not verify against. The computable algorithms are
exactly the collision-resistant ones: 'SHA1', 'SHA256', 'SHA384', 'SHA512', and
Blake2b-512. 'MD5' is deliberately uncomputable here (a match on a broken hash cannot prove
the bytes were not substituted, so the tamper gate never verifies against it), as is the
bare 'SRI' wrapper, which names no algorithm of its own (resolve it with 'sriAlgorithm'
first).

This is the sibling of 'hexDigestOk': both dispatch on the same per-algorithm crypto type,
so they live together and a new 'HashAlg' must be given an arm in each (the 'case' is total,
and the package builds with @-Wincomplete-patterns@ as an error). It is the one place that
defines /which algorithms the worker can verify/; the integrity floor admits by /strength/
("Ecluse.Core.Package.Integrity"), and the invariant that every floor-clearing algorithm is
computable here keeps the worker able to verify whatever the floor admits.
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

{- | Whether the worker can compute (and so verify a digest in) the given algorithm: the
predicate form of 'computeDigest', taken from the same single definition so the computable
set cannot drift from what 'computeDigest' actually computes.

>>> isComputable SHA256
True

>>> isComputable MD5
False
-}
isComputable :: HashAlg -> Bool
isComputable = isJust . computeDigest

{- An 'SRI' 'Hash' carries exactly one canonical @\<alg\>-\<base64\>@ component: no
surrounding whitespace (the first-dash accessors 'sriPrefix'\/'sriBody' read the
stored value verbatim, so a padded value would corrupt both) and never a
whitespace-joined set (that is the wire shape 'mkSriHashes' splits, one 'Hash' per
component). The single-component invariant is what lets every consumer -- the
admission floor, the worker's tamper gate, the divergence fingerprint -- resolve
the same algorithm and digest body from one 'Hash'.
-}
wellFormedSri :: Text -> Bool
wellFormedSri t = case T.words t of
    [comp] -> comp == t && wellFormedSriComponent comp
    _ -> False

wellFormedSriComponent :: Text -> Bool
wellFormedSriComponent comp
    -- An empty body means no @\<alg\>-\<base64\>@ shape (no separator, or nothing after it).
    | T.null (sriBody comp) = False
    | otherwise = sriBodyOk (sriPrefix comp) (sriBody comp)

-- The SRI algorithms recognised are exactly the Subresource-Integrity set
-- (sha256/sha384/sha512); the base64 body must decode to that algorithm's digest
-- length. Each is a modelled 'HashAlg', so a well-formed component both constructs and
-- resolves to an algorithm the strength tier ranks ('assertedAlg').
sriBodyOk :: Text -> Text -> Bool
sriBodyOk algName body =
    case convertFromBase Base64 (encodeUtf8 body :: ByteString) :: Either String ByteString of
        Left _ -> False
        Right bytes -> case algName of
            "sha256" -> isJust (digestFromByteString @SHA256 bytes)
            "sha384" -> isJust (digestFromByteString @SHA384 bytes)
            "sha512" -> isJust (digestFromByteString @SHA512 bytes)
            _ -> False

{- | The lower-case wire name of an algorithm -- the canonical spelling 'parseHashAlg'
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

{- | Parse an algorithm name, tolerating surrounding whitespace and case, and a
single family-separating @\'-\'@ (so @"SHA-256"@ and @"sha256"@ both parse). It
accepts only the canonical names and their documented single-dash aliases: it does
__not__ strip arbitrary internal dashes, so a typo such as @"s-h-a--2-5-6"@ is
rejected rather than silently read as @sha256@. An unrecognised name is reported as
such, distinct from a recognised-but-too-weak floor. The @sri@ wrapper is not a
config-selectable algorithm and is rejected.

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

{- | The algorithm-name token of a Subresource-Integrity string -- the @\<alg\>@ before
the first @\'-\'@ in @\<alg\>-\<base64\>@. A string with no @\'-\'@ is all prefix.

>>> sriPrefix "sha512-Zm9vYmFy"
"sha512"
-}
sriPrefix :: Text -> Text
sriPrefix = fst . T.breakOn "-"

{- | The base64 digest body of a Subresource-Integrity string -- the @\<base64\>@ after
the first @\'-\'@ in @\<alg\>-\<base64\>@. A string with no @\'-\'@ has an empty body.

>>> sriBody "sha512-Zm9vYmFy"
"Zm9vYmFy"
-}
sriBody :: Text -> Text
sriBody = T.drop 1 . snd . T.breakOn "-"

{- | The 'HashAlg' a Subresource-Integrity string names, read from its @\<alg\>@ prefix.
The prefixes resolved are the Subresource-Integrity set @sha256@, @sha384@ and @sha512@
(every long digest the model represents and a registry serves); an unrecognised or
malformed prefix yields 'Nothing', so the string asserts no algorithm and clears no
floor (the fail-closed reading).

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
