{- | The integrity gate is the security crux of the worker.

A mirrored artifact is later served from the private upstream __without re-running
the rules__, so a corrupt or tampered artifact must never enter it. Verification is
therefore the gate: a hash __mismatch fails the job with no publish__ and is logged
loudly. Because the digest is the __serve-time-admitted__ one carried on the job,
the worker mirrors exactly the bytes the rules cleared -- an upstream packument
mutated in the enqueue → process window cannot substitute a different artifact.
-}
module Ecluse.Core.Worker.Integrity (
    IntegrityResult (..),
    verifyIntegrity,
) where

import Data.ByteArray.Encoding (Base (Base16, Base64), convertToBase)
import Data.Foldable (maximumBy)
import Data.Text qualified as T

import Ecluse.Core.Package (Hash (hashAlg, hashValue), HashAlg (SHA256, SRI), computeDigest)
import Ecluse.Core.Package.Integrity (Strength, assertedAlg, integrityStrength, sriBody, sriPrefix)

{- | The result of verifying fetched bytes against the admitted integrity digests.
A sum type, not a 'Bool', so the mismatch carries the detail an operator needs to
explain why a publish was refused.
-}
data IntegrityResult
    = -- | The bytes matched the most authoritative admitted digest.
      IntegrityVerified
    | {- | The bytes failed the integrity gate. Carries a human-readable detail (the
      digest they were checked against, or that the strongest one was uncomputable).
      -}
      IntegrityMismatch Text
    deriving stock (Eq, Show)

{- | Verify fetched artifact bytes against the __most authoritative__ integrity
digest the version carries -- never against a weaker one while a stronger is present.

A real npm version carries both a modern SRI @sha512@ digest and the legacy SHA-1
@shasum@. Passing on /any/ match would let an artifact that matches the weak SHA-1
but fails the strong @sha512@ through -- and SHA-1 collision resistance is broken, so
that is exploitable. So the gate ranks the admitted digests by algorithm authority
(strongest first: @sha512@ \/ @blake2b@ > @sha384@ > @sha256@ > @sha1@ > @md5@), and
checks the bytes against the strongest one present: the bytes pass __iff__ that digest
matches.
A weaker digest can neither override nor rescue a failed strong one.

The bytes are recomputed in the strongest digest's own algorithm through the shared
'Ecluse.Core.Package.computeDigest', the one definition of which algorithms Écluse can
verify. That computable set covers every algorithm the public integrity floor admits, so an
admitted artifact is always verifiable here. If the strongest digest is nonetheless in an
algorithm 'computeDigest' declines (MD5, a forgeable hash) or an SRI whose inner algorithm
does not resolve, the gate __fails closed__ rather than falling back to a weaker digest: a
tampered artifact must never be admitted on the strength of a hash an attacker could forge.

This is the tamper gate before a publish: a mismatch fails the job and never
publishes a corrupt or substituted artifact into the private upstream.

>>> import Ecluse.Core.Package (mkHash, HashAlg (SHA1))
>>> fmap (\h -> verifyIntegrity (h :| []) "Hello World") (mkHash SHA1 "0a4d55a8d778e5022fab701977c5d840bbc486d0")
Right IntegrityVerified

>>> fmap (\h -> verifyIntegrity (h :| []) "Hello World") (mkHash SHA1 "da39a3ee5e6b4b0d3255bfef95601890afd80709")
Right (IntegrityMismatch "the SHA1 digest did not match the fetched bytes")
-}
verifyIntegrity :: NonEmpty Hash -> ByteString -> IntegrityResult
verifyIntegrity hashes bytes =
    let strongest = maximumBy (comparing authority) hashes
     in case matchesDigest (toLazy bytes) strongest of
            Nothing ->
                -- Fail closed: the strongest present digest is in an algorithm we
                -- cannot recompute, so we cannot prove the bytes -- never drop to a
                -- weaker digest an attacker could forge.
                IntegrityMismatch
                    ( "the strongest admitted digest ("
                        <> describeDigest strongest
                        <> ") is in an algorithm the worker cannot verify"
                    )
            Just True -> IntegrityVerified
            Just False ->
                IntegrityMismatch ("the " <> describeDigest strongest <> " digest did not match the fetched bytes")

-- Algorithm authority, strongest first, so 'maximumBy' selects the digest a match
-- must be proven against. It reuses the shared 'integrityStrength' ranking so the
-- tamper gate and the serve-admission floor agree on which algorithms are strong.
-- An SRI is ranked by the algorithm it asserts ('assertedAlg' -- npm's @sha512-…@
-- ranks as 'SHA512'); an SRI whose inner alg is unrecognised asserts nothing and ranks
-- at the SHA-256 floor tier (above the legacy SHA-1/MD5). It therefore WINS the
-- 'maximumBy' and, unresolvable, the gate fails closed in 'matchesDigest' rather than
-- downgrading to a weaker computable digest an attacker who also controls it could
-- forge; it stays below a computable sha512, so a real sha512, when co-present, is
-- still preferred and verified.
authority :: Hash -> Strength
authority = maybe (integrityStrength SHA256) integrityStrength . assertedAlg

-- Whether the fetched bytes match the chosen digest: resolve its algorithm
-- ('assertedAlg', 'Nothing' for an unresolvable SRI), recompute the bytes in that
-- algorithm ('computeDigest', 'Nothing' for one the worker will not verify against),
-- and compare in the digest's own wire encoding. A hex tag compares case-insensitively
-- (hex is); an SRI's base64 body compares case-sensitively (base64 is; folding its case
-- would admit a digest that matches the bytes only after a case change). Either 'Nothing'
-- is the fail-closed case in 'verifyIntegrity'.
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
