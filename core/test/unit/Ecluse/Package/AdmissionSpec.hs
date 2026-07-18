-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The shared admission oracle, and the differential guarantee it exists to hold:
the serve gate and the mirror worker decide one artifact the same way, and what the
admission floor admits the worker's tamper gate can always verify.

The unit cases pin each 'ArtifactAdmission' arm; the golden cases replay the
historical worker\/serve divergences (the floor-admitted-but-unverifiable gap of
issue 409 and the multi-component SRI split-brain of issue 738) so they stay
closed; and the Hedgehog property states the standing contract over arbitrary
digest sets: __floor-admitted implies worker-verifiable__, under one shared
authority order, with tampered bytes always refused.
-}
module Ecluse.Package.AdmissionSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Data.Time (UTCTime (..), fromGregorian)
import Hedgehog (annotateShow, assert, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)
import Text.Show (showsPrec)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (
    Artifact (..),
    Hash,
    HashAlg (Blake2b, SHA1, SHA256, SHA384, SHA512, SRI),
    PackageDetails (..),
    hashValue,
    mkPackageName,
 )
import Ecluse.Core.Package.Admission (
    ArtifactAdmission (
        AdmissionAdmit,
        AdmissionBelowFloor,
        AdmissionDenied,
        AdmissionFileAbsent,
        AdmissionIntegrityMissing,
        AdmissionUndecidable
    ),
    admitArtifact,
 )
import Ecluse.Core.Package.Integrity (
    VersionIntegrity (MeetsFloor),
    assertedAlg,
    authoritativeDigest,
    classifyArtifacts,
 )
import Ecluse.Core.Rules (PreparedRule (PreparedRule, prepEval, prepName, prepPrecedence, prepResilience))
import Ecluse.Core.Rules.Types (
    Decision (Blocked, Undecidable),
    EvalContext (EvalContext),
    FailureAlignment (FailDeny),
    RuleVerdict (Allow, CannotVet, Deny),
 )
import Ecluse.Core.Version (mkVersion)
import Ecluse.Core.Worker.Integrity (IntegrityResult (IntegrityMismatch, IntegrityVerified), verifyIntegrity)
import Ecluse.Test.Package (defaultMinIntegrity, sampleArtifact, sampleDetails, unsafeHash, unsafeSriHashes)
import Ecluse.Test.Package qualified as Package

-- The blessed splitter, as the plain list the artifact fixtures take.
sriHashesOf :: Text -> [Hash]
sriHashesOf = toList . unsafeSriHashes

-- ── fixtures ────────────────────────────────────────────────────────────────────

-- A fixed evaluation context; the injected rules here are not time-sensitive.
ctx :: EvalContext
ctx = EvalContext (UTCTime (fromGregorian 2026 1 1) 0) Nothing

-- A pure prepared rule returning a constant verdict, bypassing config preparation
-- exactly as the worker unit fixtures do.
constRule :: Text -> RuleVerdict -> PreparedRule
constRule ruleName verdict =
    PreparedRule
        { prepName = ruleName
        , prepPrecedence = 0
        , prepResilience = Nothing
        , prepEval = \_ _ -> pure verdict
        }

admitRule, denyRule, cannotVetRule :: PreparedRule
admitRule = constRule "test-admit" (Allow "admitted for test")
denyRule = constRule "test-deny" (Deny "denied by current policy")
cannotVetRule = constRule "test-cannot-vet" (CannotVet FailDeny "no advisory database is loaded")

-- The version snapshot under test: the shared sample snapshot, its one artifact
-- renamed to @thing-1.0.0.tgz@ and carrying the given digests.
detailsWith :: [Hash] -> PackageDetails
detailsWith hs =
    (sampleDetails (mkPackageName Npm Nothing "thing") (mkVersion Npm "1.0.0"))
        { pkgArtifacts = one (artifactWith hs)
        }

artifactWith :: [Hash] -> Artifact
artifactWith hs = sampleArtifact{artFilename = "thing-1.0.0.tgz", artHashes = hs}

-- ── real digests over real bytes ────────────────────────────────────────────────

sampleBytes :: ByteString
sampleBytes = "some tarball bytes the digests below are computed over"

tamperedBytes :: ByteString
tamperedBytes = sampleBytes <> "!"

-- ── the digest-set generator for the differential property ─────────────────────

-- One producible digest kind: how a registry could describe bytes. The 'Show' is
-- hand-written (a builder field has none) so a shrunk counterexample names the kind.
data DigestKind
    = HexOf HashAlg (ByteString -> Text)
    | SriOf Text (ByteString -> Text)

instance Show DigestKind where
    showsPrec _ kind rest = kindLabel kind <> rest
      where
        kindLabel = \case
            HexOf alg _ -> "HexOf " <> show alg
            SriOf label _ -> "SriOf " <> toString label

digestKinds :: [DigestKind]
digestKinds =
    [ HexOf SHA1 Package.hexSha1Of
    , HexOf SHA256 Package.hexSha256Of
    , HexOf SHA384 Package.hexSha384Of
    , HexOf SHA512 Package.hexSha512Of
    , HexOf Blake2b Package.hexBlake2bOf
    , SriOf "sha256" Package.sriSha256Of
    , SriOf "sha384" Package.sriSha384Of
    , SriOf "sha512" Package.sriSha512Of
    ]

hashOfKind :: ByteString -> DigestKind -> Hash
hashOfKind bs = \case
    HexOf alg hexOf -> unsafeHash alg (hexOf bs)
    SriOf _ sriOf -> unsafeHash SRI (sriOf bs)

spec :: Spec
spec = do
    describe "admitArtifact -- the shared serve/worker admission oracle" $ do
        it "admits a rule-admitted, floor-clearing artifact, selecting it by filename" $ do
            let details = detailsWith (sriHashesOf (Package.sriSha512Of sampleBytes))
            admission <- admitArtifact ctx [admitRule] defaultMinIntegrity "thing-1.0.0.tgz" details
            case admission of
                AdmissionAdmit artifact digests -> do
                    artFilename artifact `shouldBe` "thing-1.0.0.tgz"
                    -- The carried digest set is the admitted artifact's own, so both
                    -- consumers (mirror enqueue, worker tamper gate) act on exactly
                    -- what the floor checked.
                    toList digests `shouldBe` artHashes artifact
                other -> expectationFailure ("expected an admit, got " <> show other)

        it "carries a rule denial through as AdmissionDenied (both surfaces render the same decision)" $ do
            let details = detailsWith (sriHashesOf (Package.sriSha512Of sampleBytes))
            admission <- admitArtifact ctx [denyRule] defaultMinIntegrity "thing-1.0.0.tgz" details
            case admission of
                AdmissionDenied Blocked{} -> pass
                other -> expectationFailure ("expected a rule denial, got " <> show other)

        it "carries a fail-closed uncomputable rule through as AdmissionUndecidable" $ do
            let details = detailsWith (sriHashesOf (Package.sriSha512Of sampleBytes))
            admission <- admitArtifact ctx [cannotVetRule] defaultMinIntegrity "thing-1.0.0.tgz" details
            case admission of
                AdmissionUndecidable Undecidable{} -> pass
                other -> expectationFailure ("expected undecidable, got " <> show other)

        it "reports an absent filename as AdmissionFileAbsent, never selecting another artifact" $ do
            let details = detailsWith (sriHashesOf (Package.sriSha512Of sampleBytes))
            admission <- admitArtifact ctx [admitRule] defaultMinIntegrity "renamed-2.0.0.tgz" details
            case admission of
                AdmissionFileAbsent -> pass
                other -> expectationFailure ("expected a file miss, got " <> show other)

        it "refuses a hashless artifact as AdmissionIntegrityMissing" $ do
            admission <- admitArtifact ctx [admitRule] defaultMinIntegrity "thing-1.0.0.tgz" (detailsWith [])
            case admission of
                AdmissionIntegrityMissing -> pass
                other -> expectationFailure ("expected integrity-missing, got " <> show other)

        it "refuses a weak-only digest set as AdmissionBelowFloor" $ do
            let details = detailsWith [unsafeHash SHA1 (Package.hexSha1Of sampleBytes)]
            admission <- admitArtifact ctx [admitRule] defaultMinIntegrity "thing-1.0.0.tgz" details
            case admission of
                AdmissionBelowFloor -> pass
                other -> expectationFailure ("expected below-floor, got " <> show other)

        it "never pays artifact selection or floor classification for a version a rule denies" $ do
            -- A denied version with a hashless artifact must surface the denial, not
            -- the integrity refusal: the rules run first.
            admission <- admitArtifact ctx [denyRule] defaultMinIntegrity "thing-1.0.0.tgz" (detailsWith [])
            case admission of
                AdmissionDenied Blocked{} -> pass
                other -> expectationFailure ("expected the rule denial to win, got " <> show other)

    describe "the closed divergences, replayed (golden corpus)" $ do
        it "#738: a multi-component SRI is admitted at the floor AND verified by the worker" $ do
            -- The historical split-brain: serve admitted on the first component while
            -- the worker compared against the joined tail and could never match, so
            -- the version was served from public forever and never mirrored. Split
            -- into components, both gates read the same digest.
            let joined = Package.sriSha512Of sampleBytes <> " " <> Package.sriSha256Of sampleBytes
                hashes = sriHashesOf joined
                details = detailsWith hashes
            classifyArtifacts defaultMinIntegrity (pkgArtifacts details) `shouldBe` MeetsFloor
            admission <- admitArtifact ctx [admitRule] defaultMinIntegrity "thing-1.0.0.tgz" details
            case admission of
                AdmissionAdmit _ admitted -> do
                    verifyIntegrity admitted sampleBytes `shouldBe` IntegrityVerified
                    case verifyIntegrity admitted tamperedBytes of
                        IntegrityMismatch _ -> pass
                        IntegrityVerified -> expectationFailure "tampered bytes must never verify"
                other -> expectationFailure ("expected an admit, got " <> show other)

        it "#738 (ranking): the strongest component decides, not the first on the wire" $ do
            -- "sha256-… sha512-…" used to rank at the SHA-256 tier because only the
            -- first component was read; per-component hashes rank exactly.
            let joined = Package.sriSha256Of sampleBytes <> " " <> Package.sriSha512Of sampleBytes
                hashes = NE.fromList (sriHashesOf joined)
            assertedAlg (authoritativeDigest hashes) `shouldBe` Just SHA512
            hashValue (authoritativeDigest hashes) `shouldBe` Package.sriSha512Of sampleBytes

        it "#409: a SHA-256-only artifact admitted by the default floor is worker-verifiable" $ do
            -- The predecessor gap: the floor admitted SHA-256 while the worker's
            -- hand-rolled vocabulary could not verify it, stranding the version.
            let hashes = [unsafeHash SHA256 (Package.hexSha256Of sampleBytes)]
                details = detailsWith hashes
            classifyArtifacts defaultMinIntegrity (pkgArtifacts details) `shouldBe` MeetsFloor
            verifyIntegrity (NE.fromList hashes) sampleBytes `shouldBe` IntegrityVerified

    describe "the differential property: floor-admitted implies worker-verifiable" $
        it "holds over arbitrary bytes and any floor-clearing digest set, in any wire order" $
            hedgehog $ do
                bytes <- forAll (Gen.bytes (Range.linear 0 200))
                kinds <- forAll (Gen.shuffle =<< Gen.subsequence digestKinds)
                let hashes = concatMap (expand bytes) kinds
                case nonEmpty hashes of
                    Nothing -> pass -- no digests: NoIntegrity, refused before any verify
                    Just ne -> do
                        annotateShow (map hashValue hashes)
                        let admittedAtFloor =
                                classifyArtifacts defaultMinIntegrity (one (artifactWith hashes)) == MeetsFloor
                        -- The parity direction the mirror depends on: anything the
                        -- serve-side floor admits, the worker's byte gate can prove.
                        when admittedAtFloor $ do
                            verifyIntegrity ne bytes === IntegrityVerified
                            case verifyIntegrity ne (bytes <> "!") of
                                IntegrityMismatch _ -> pass
                                IntegrityVerified -> assert False
  where
    -- Realise one digest kind over the bytes (the joined multi-component wire
    -- shape is pinned by the #738 golden case above).
    expand :: ByteString -> DigestKind -> [Hash]
    expand bytes kind = [hashOfKind bytes kind]
