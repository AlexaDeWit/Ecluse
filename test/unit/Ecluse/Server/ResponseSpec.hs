module Ecluse.Server.ResponseSpec (spec) where

import Data.Aeson (Value (String), eitherDecode)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as T
import Data.Time (UTCTime (..), addUTCTime, fromGregorian, nominalDay)
import Test.Hspec

import Ecluse.Ecosystem (Ecosystem (Npm))
import Ecluse.Package (
    Artifact (..),
    ArtifactKind (Tarball),
    Availability (Available),
    CodeExecSignal (NoCodeOnInstall, RunsCodeOnInstall),
    PackageDetails (..),
    Trust (Untrusted),
    mkPackageName,
    mkScope,
 )
import Ecluse.Rules (evalRules)
import Ecluse.Rules.Types (
    EvalContext (EvalContext),
    Rule (AllowScope, DenyHasInstallScripts),
    atDefaultPrecedence,
 )
import Ecluse.Server.Response (
    ArtifactStatus (..),
    RejectReason (..),
    Rejection (..),
    RetryAfter (..),
    RuleName (..),
    ServeDecision (..),
    Transience (..),
    artifactStatus,
    artifactStatusCode,
    denialBody,
    mkHelpMessage,
    serveDecisionOf,
    unHelpMessage,
 )
import Ecluse.Version (mkVersion)

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

{- | Decode a denial body and read its @error@ string. 'Right' the string when
the body is a JSON object carrying a string @error@; 'Left' (which fails the
@`shouldBe` Right …@ assertion) for any other shape, so the npm @{"error": …}@
contract is pinned without a partial decode.
-}
errorField :: LByteString -> Either Text Text
errorField raw =
    case eitherDecode raw of
        Right (Aeson.Object o) ->
            case KeyMap.lookup "error" o of
                Just (String msg) -> Right msg
                _ -> Left "denial body has no string \"error\" field"
        _ -> Left "denial body is not a JSON object"

spec :: Spec
spec = do
    describe "artifactStatus — outcome to status (concrete artifact)" $ do
        -- The architecture table (web-layer.md#error-model), as a code-level
        -- truth table. Each serve outcome maps to exactly one artifact status.
        it "Admit streams a 200" $
            artifactStatus Admit `shouldBe` Ok
        it "a policy rejection is a 403" $
            artifactStatus (Reject (Rejection (ByPolicy (RuleName "DenyHasInstallScripts")) "no"))
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

    describe "denialBody — the npm {\"error\": …} shape" $ do
        it "is a JSON object with a string error field carrying the message" $
            errorField (denialBody Nothing "denied because reasons")
                `shouldBe` Right "denied because reasons"
        it "appends a configured help message to the error text" $
            errorField (denialBody (Just (mkHelpMessage "Contact #platform-eng.")) "denied")
                `shouldBe` Right "denied Contact #platform-eng."
        it "appends nothing when no help message is configured" $
            errorField (denialBody Nothing "denied")
                `shouldBe` Right "denied"
        it "does not duplicate spacing when the message already ends in a space" $
            errorField (denialBody (Just (mkHelpMessage "Help.")) "denied ")
                `shouldBe` Right "denied Help."
        it "ignores a blank help message rather than appending empty text" $
            errorField (denialBody (Just (mkHelpMessage "   ")) "denied")
                `shouldBe` Right "denied"

    describe "HelpMessage — trimmed at construction" $ do
        it "stores the message trimmed of surrounding whitespace" $
            unHelpMessage (mkHelpMessage "  Contact support.  ")
                `shouldBe` "Contact support."
        it "collapses an all-whitespace message to empty" $
            unHelpMessage (mkHelpMessage " \t ") `shouldBe` ""

    describe "serveDecisionOf — a rules Decision becomes a serve outcome" $ do
        it "a deny-rule decision rejects ByPolicy, naming the rule, with the rendered why" $ do
            let pd = pkg "public" 30 (RunsCodeOnInstall "preinstall hook")
                decision = evalRules (EvalContext now) [atDefaultPrecedence DenyHasInstallScripts] pd
            case serveDecisionOf pd decision of
                Reject rej -> do
                    rejectionReason rej `shouldBe` ByPolicy (RuleName "DenyHasInstallScripts")
                    rejectionMessage rej `shouldSatisfy` T.isInfixOf "DenyHasInstallScripts"
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
