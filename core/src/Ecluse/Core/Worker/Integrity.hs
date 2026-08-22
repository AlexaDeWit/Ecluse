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

import Ecluse.Core.Package (Hash (hashAlg, hashValue), HashAlg (SRI), computeDigest)
import Ecluse.Core.Package.Integrity (assertedAlg, authoritativeDigest, sriBody, sriPrefix)

{- | The result of verifying fetched bytes against the admitted integrity digests.
A sum type, not a 'Bool', so the mismatch carries the detail an operator needs to
explain why the worker refused a publish.
-}
data IntegrityResult
    = -- | The bytes matched the most authoritative admitted digest.
      IntegrityVerified
    | {- | The bytes failed the integrity gate. Carries a human-readable detail (the
      digest they were checked against, or that the strongest one was uncomputable).
      -}
      IntegrityMismatch Text
    deriving stock (Eq, Show)

{- | Verify fetched artifact bytes against the __most authoritative__ integrity digest
the version carries, never against a weaker one while a stronger is present.

A real npm version carries both a modern SRI @sha512@ digest and the legacy SHA-1
@shasum@. Passing on /any/ match would let through an artifact that matches the weak
SHA-1 but fails the strong @sha512@. SHA-1 collision resistance is broken, so that is
exploitable. The gate therefore verifies the bytes against the __one__ digest the
shared selection names ('Ecluse.Core.Package.Integrity.authoritativeDigest', the same
authority order the serve-side admission floor ranks by). The bytes pass __iff__ that
digest matches. A weaker digest can neither override nor rescue a failed strong one.
Both sides share the selection, so this gate and the admission floor can never rank
the same digest set two different ways.

The gate recomputes the bytes in that digest's own algorithm through the shared
'Ecluse.Core.Package.computeDigest', the one definition of which algorithms Écluse can
verify. That computable set covers every algorithm the public integrity floor admits,
so an admitted artifact is always verifiable here. Each SRI 'Hash' carries exactly one
@\<alg\>-\<base64\>@ component, because 'Ecluse.Core.Package.mkSriHashes' splits a
joined wire string at construction. The compared digest body is therefore always a
single component's, never a joined string. If the selected digest is in an algorithm
the worker cannot recompute, the gate __fails closed__. A tampered artifact must never
pass on the strength of a hash an attacker could forge.

This is the tamper gate before a publish. A mismatch fails the job and never publishes
a corrupt or substituted artifact into the private upstream.

>>> import Ecluse.Core.Package (mkHash, HashAlg (SHA1))
>>> fmap (\h -> verifyIntegrity (h :| []) "Hello World") (mkHash SHA1 "0a4d55a8d778e5022fab701977c5d840bbc486d0")
Right IntegrityVerified

>>> fmap (\h -> verifyIntegrity (h :| []) "Hello World") (mkHash SHA1 "da39a3ee5e6b4b0d3255bfef95601890afd80709")
Right (IntegrityMismatch "the SHA1 digest did not match the fetched bytes")
-}
verifyIntegrity :: NonEmpty Hash -> ByteString -> IntegrityResult
verifyIntegrity hashes bytes =
    let strongest = authoritativeDigest hashes
     in case matchesDigest (toLazy bytes) strongest of
            Nothing ->
                -- Fail closed: the strongest present digest is in an algorithm the
                -- worker cannot recompute, so nothing here can prove the bytes. Never
                -- drop to a weaker digest an attacker could forge.
                IntegrityMismatch
                    ( "the strongest admitted digest ("
                        <> describeDigest strongest
                        <> ") is in an algorithm the worker cannot verify"
                    )
            Just True -> IntegrityVerified
            Just False ->
                IntegrityMismatch ("the " <> describeDigest strongest <> " digest did not match the fetched bytes")

-- Whether the fetched bytes match the chosen digest. Resolve its algorithm
-- ('assertedAlg', 'Nothing' for an unresolvable SRI). Recompute the bytes in that
-- algorithm ('computeDigest', 'Nothing' for one the worker will not verify against).
-- Compare in the digest's own wire encoding. A hex tag compares
-- case-insensitively, as hex is. An SRI's base64 body compares case-sensitively, as
-- base64 is: folding its case would admit a digest that matches the bytes only after
-- a case change. Either 'Nothing' is the fail-closed case in 'verifyIntegrity'.
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
