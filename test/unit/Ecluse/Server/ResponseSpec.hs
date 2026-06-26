module Ecluse.Server.ResponseSpec (spec) where

import Data.Text qualified as T
import Data.Time (UTCTime (..), addUTCTime, fromGregorian, nominalDay)
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (
    Artifact (..),
    ArtifactKind (Tarball),
    Availability (Available),
    CodeExecSignal (NoCodeOnInstall, RunsCodeOnInstall),
    PackageDetails (..),
    Trust (Untrusted),
    mkPackageName,
    mkScope,
 )
import Ecluse.Core.Rules (evalRules)
import Ecluse.Core.Rules.Types (
    Decision (ApprovedEffectful, DeniedEffectful, Undecidable),
    EvalContext (EvalContext),
    Rule (AllowScope, DenyInstallTimeExecution),
    atDefaultPrecedence,
 )
import Ecluse.Core.Version (mkVersion)
import Ecluse.Server.Response (
    ArtifactStatus (..),
    PackumentStatus (..),
    RejectReason (..),
    Rejection (..),
    RetryAfter (..),
    RuleName (..),
    ServeDecision (..),
    Transience (..),
    artifactStatus,
    artifactStatusCode,
    longestRetry,
    mkHelpMessage,
    packumentStatus,
    packumentStatusCode,
    serveDecisionOf,
    unHelpMessage,
 )

-- | A fixed "now" so age-based fixtures are deterministic.
now :: UTCTime
now = UTCTime (fromGregorian 2026 6 20) 0

-- | A single inert artifact; the response model does not inspect artifacts.
sampleArtifact :: Artifact
sampleArtifact =
    Artifact
        { artFilename = "evil-1.0.0.tgz"
        , artUrl = "https://example.test/evil-1.0.0.tgz"
        , artKind = Tarball
        , artHashes = []
        , artSize = Nothing
        , artInterpreter = Nothing
        , artYanked = False
        , artProvenance = Nothing
        }

{- | A scoped package version published @ageDays@ before 'now', whose
install-code signal is supplied so a deny rule can be exercised.
-}
pkg :: Text -> Integer -> CodeExecSignal -> PackageDetails
pkg scope ageDays code =
    PackageDetails
        { pkgName = mkPackageName Npm (Just (mkScope scope)) "pkg"
        , pkgVersion = mkVersion Npm "1.0.0"
        , pkgPublishedAt = Just (addUTCTime (negate (fromInteger ageDays * nominalDay)) now)
        , pkgInstallCode = code
        , pkgTrust = Untrusted
        , pkgAvailability = Available
        , pkgArtifacts = sampleArtifact :| []
        , pkgLicenses = ["MIT"]
        , pkgPublisher = Nothing
        , pkgMaintainers = []
        , pkgDependencies = []
        }

spec :: Spec
spec = do
    describe "artifactStatus — outcome to status (concrete artifact)" $ do
        -- The architecture table (web-layer.md#error-model), as a code-level
        -- truth table. Each serve outcome maps to exactly one artifact status.
        it "Admit streams a 200" $
            artifactStatus Admit `shouldBe` Ok
        it "a policy rejection is a 403" $
            artifactStatus (Reject (Rejection (ByPolicy (RuleName "DenyInstallTimeExecution")) "no"))
                `shouldBe` Forbidden
        it "a will-resolve rejection (no Retry-After) is a 503 with no delay" $
            artifactStatus (Reject (Rejection (Unavailable (WillResolve Nothing)) "down"))
                `shouldBe` Unavailable' Nothing
        it "a will-resolve rejection carries its Retry-After delay" $
            artifactStatus (Reject (Rejection (Unavailable (WillResolve (Just (RetryAfter 30)))) "down"))
                `shouldBe` Unavailable' (Just (RetryAfter 30))
        it "a wont-resolve rejection is a 500, never a 503" $
            artifactStatus (Reject (Rejection (Unavailable WontResolve) "broken"))
                `shouldBe` ServerError
        it "a missing-integrity refusal is a 403 (an admission-policy denial)" $
            artifactStatus (Reject (Rejection MissingIntegrity "no integrity"))
                `shouldBe` Forbidden
        it "a below-floor refusal is a 403 (an admission-policy denial)" $
            artifactStatus (Reject (Rejection BelowIntegrityFloor "too weak"))
                `shouldBe` Forbidden

    describe "artifactStatusCode — numeric HTTP codes" $ do
        it "Ok is 200" $ artifactStatusCode Ok `shouldBe` 200
        it "Forbidden is 403" $ artifactStatusCode Forbidden `shouldBe` 403
        it "an Unavailable' is 503 regardless of the Retry-After delay" $ do
            artifactStatusCode (Unavailable' Nothing) `shouldBe` 503
            artifactStatusCode (Unavailable' (Just (RetryAfter 30))) `shouldBe` 503
        it "ServerError is 500" $ artifactStatusCode ServerError `shouldBe` 500
        it "NotFound is 404" $ artifactStatusCode NotFound `shouldBe` 404

    describe "the 503-only-when-it-will-resolve rule" $
        -- The load-bearing distinction in the error model: a transient
        -- inability to decide invites a retry (503); a permanent/internal one
        -- does not (500). Pin both directions together so the rule is explicit.
        it "503 iff the rejection believes it will resolve, else 500" $ do
            artifactStatusCode (artifactStatus (Reject (Rejection (Unavailable (WillResolve Nothing)) "x")))
                `shouldBe` 503
            artifactStatusCode (artifactStatus (Reject (Rejection (Unavailable WontResolve) "x")))
                `shouldBe` 500

    describe "packumentStatus — status over the merged survivor set" $ do
        let denied = Reject (Rejection (ByPolicy (RuleName "DenyInstallTimeExecution")) "no")
            transient d = Reject (Rejection (Unavailable (WillResolve d)) "down")
            broken = Reject (Rejection (Unavailable WontResolve) "broken")
            invalid = Reject (Rejection UpstreamInvalid "wrong package")
        it "serves (200) when any version survives, whatever else was excluded" $ do
            packumentStatus [Admit] `shouldBe` PackumentOk
            packumentStatus [denied, Admit, broken] `shouldBe` PackumentOk
        it "is 403 when no survivor and every exclusion is by policy" $
            packumentStatus [denied, denied] `shouldBe` PackumentForbidden
        it "is 403 (deny-by-default) for an empty decision set" $
            packumentStatus [] `shouldBe` PackumentForbidden
        it "is 503 when any exclusion may self-heal — a retry may yield survivors" $
            packumentStatus [denied, transient Nothing] `shouldBe` PackumentUnavailable Nothing
        it "prefers 503 over 500: a will-resolve cause outranks a wont-resolve one" $
            packumentStatus [broken, transient Nothing] `shouldBe` PackumentUnavailable Nothing
        it "suggests the longest Retry-After among the transient causes" $
            packumentStatus [transient (Just (RetryAfter 5)), transient (Just (RetryAfter 30))]
                `shouldBe` PackumentUnavailable (Just (RetryAfter 30))
        it "carries a delay even when only some transient causes suggested one" $
            packumentStatus [transient Nothing, transient (Just (RetryAfter 10))]
                `shouldBe` PackumentUnavailable (Just (RetryAfter 10))
        it "is 500 when an exclusion is a permanent inability and none is retryable" $
            packumentStatus [denied, broken] `shouldBe` PackumentServerError
        it "is 403 when no survivor and the only exclusion is a missing-integrity refusal" $
            packumentStatus [Reject (Rejection MissingIntegrity "no integrity")]
                `shouldBe` PackumentForbidden
        it "is 403 when no survivor and the only exclusion is a below-floor refusal" $
            packumentStatus [Reject (Rejection BelowIntegrityFloor "too weak")]
                `shouldBe` PackumentForbidden
        it "is 502 when a responding upstream returned a packument for a different package" $
            packumentStatus [invalid] `shouldBe` PackumentBadGateway
        it "is 502 when both responding origins reported the wrong package" $
            packumentStatus [invalid, invalid] `shouldBe` PackumentBadGateway
        it "prefers 503 over 502: a retryable outage may yet yield a valid document" $
            packumentStatus [invalid, transient Nothing] `shouldBe` PackumentUnavailable Nothing
        it "prefers 502 over 500: a concrete gateway fault outranks a generic permanent inability" $
            packumentStatus [invalid, broken] `shouldBe` PackumentBadGateway
        it "prefers 502 over 403: a misreporting upstream outranks a deny-by-default" $
            packumentStatus [invalid, denied] `shouldBe` PackumentBadGateway

    describe "longestRetry — the longest suggested delay, or none" $ do
        it "is Nothing for an empty list" $
            longestRetry [] `shouldBe` Nothing
        it "is Nothing when no cause suggested a delay" $
            longestRetry [Nothing, Nothing] `shouldBe` Nothing
        it "is the maximum delay among those that suggested one" $
            longestRetry [Just (RetryAfter 5), Nothing, Just (RetryAfter 30), Just (RetryAfter 12)]
                `shouldBe` Just (RetryAfter 30)

    describe "packumentStatusCode — numeric HTTP codes (never 404)" $
        it "maps Ok/Forbidden/Unavailable/ServerError to 200/403/503/500" $ do
            packumentStatusCode PackumentOk `shouldBe` 200
            packumentStatusCode PackumentForbidden `shouldBe` 403
            packumentStatusCode (PackumentUnavailable Nothing) `shouldBe` 503
            packumentStatusCode (PackumentUnavailable (Just (RetryAfter 30))) `shouldBe` 503
            packumentStatusCode PackumentBadGateway `shouldBe` 502
            packumentStatusCode PackumentServerError `shouldBe` 500

    describe "HelpMessage — trimmed at construction" $ do
        it "stores the message trimmed of surrounding whitespace" $
            unHelpMessage (mkHelpMessage "  Contact support.  ")
                `shouldBe` "Contact support."
        it "collapses an all-whitespace message to empty" $
            unHelpMessage (mkHelpMessage " \t ") `shouldBe` ""

    describe "serveDecisionOf — a rules Decision becomes a serve outcome" $ do
        it "a deny-rule decision rejects ByPolicy, naming the rule, with the rendered why" $ do
            let pd = pkg "public" 30 (RunsCodeOnInstall "preinstall hook")
                decision = evalRules (EvalContext now) [atDefaultPrecedence DenyInstallTimeExecution] pd
            case serveDecisionOf pd decision of
                Reject rej -> do
                    rejectionReason rej `shouldBe` ByPolicy (RuleName "DenyInstallTimeExecution")
                    rejectionMessage rej `shouldSatisfy` T.isInfixOf "DenyInstallTimeExecution"
                    rejectionMessage rej `shouldSatisfy` T.isInfixOf "preinstall hook"
                Admit -> expectationFailure "a deny decision must reject, not admit"
        it "a deny-by-default decision rejects ByPolicy (no rule allowed it)" $ do
            let pd = pkg "public" 30 NoCodeOnInstall
                decision = evalRules (EvalContext now) [] pd
            case serveDecisionOf pd decision of
                Reject rej -> do
                    rejectionReason rej `shouldBe` ByPolicy (RuleName "DeniedByDefault")
                    rejectionMessage rej `shouldSatisfy` T.isInfixOf "denied"
                Admit -> expectationFailure "deny-by-default must reject, not admit"
        it "an approved decision admits — only denials reject" $ do
            let pd = pkg "internal" 30 NoCodeOnInstall
                decision = evalRules (EvalContext now) [atDefaultPrecedence (AllowScope (mkScope "internal"))] pd
            serveDecisionOf pd decision `shouldBe` Admit

        it "an effectful approval admits, like a pure approval" $ do
            let pd = pkg "public" 30 NoCodeOnInstall
            serveDecisionOf pd (ApprovedEffectful "AllowAdvisory" "remediates") `shouldBe` Admit

        it "an effectful denial rejects ByPolicy, naming the effectful rule" $ do
            let pd = pkg "public" 30 NoCodeOnInstall
            case serveDecisionOf pd (DeniedEffectful "DenyAdvisory" "affected by an advisory") of
                Reject rej -> do
                    rejectionReason rej `shouldBe` ByPolicy (RuleName "DenyAdvisory")
                    rejectionMessage rej `shouldSatisfy` T.isInfixOf "DenyAdvisory"
                Admit -> expectationFailure "an effectful denial must reject, not admit"

        it "an undecidable decision rejects as Unavailable, carrying its transience" $ do
            -- Fail-closed: a needed effectful rule that could not be consulted
            -- rejects as Unavailable, the transience flowing through to the status.
            let pd = pkg "public" 30 NoCodeOnInstall
            case serveDecisionOf pd (Undecidable (WillResolve (Just (RetryAfter 20))) "advisory source down") of
                Reject rej -> rejectionReason rej `shouldBe` Unavailable (WillResolve (Just (RetryAfter 20)))
                Admit -> expectationFailure "an undecidable decision must reject, not admit"
