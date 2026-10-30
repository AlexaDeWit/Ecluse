-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Shared pipeline refusal behaviour under upstream faults and malformed responses.
module Ecluse.Core.Server.Pipeline.SharedIntegrationSpec (spec) where

import Data.Aeson (Value (Object, String), decodeStrict', object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Ecluse.Server.Pipeline.TestSupport
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Wai
import Network.Wai.Test (SResponse (..), simpleBody)
import Test.Hspec
import UnliftIO.Exception (impureThrow, throwIO)

import Ecluse.Core.Breaker (noBreakerReporter)
import Ecluse.Core.Cve (CveQueryFault (CveQueryFault))
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataAssemble))
import Ecluse.Core.Rules (PreparedRule, Resilience (..))
import Ecluse.Core.Rules.Effectful (EffectfulConfig (..), defaultEffectfulConfig, newBreaker)
import Ecluse.Core.Rules.Types (FailureAlignment (..), Reason (FixesNoAdvisory), RuleVerdict (..))
import Ecluse.Core.Security (Limits (..), defaultLimits)
import Ecluse.Core.Server.Context (PackumentDeps (..))
import Ecluse.Runtime.Log (DdContext (DdContext), LogFormat (JsonLog), LogLevel (InfoLevel), newLogEnv)
import Ecluse.Test.Json (textAtPath)
import Ecluse.Test.Log (captureStdout)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Rules (packageRule, remediation, revocation)
import Katip (Environment (Environment), closeScribes)

spec :: Spec
spec = do
    effectfulSpec
    boundsSpec
    boundsLogSpec
    perimeterSpec

-- A resilient advisory rule whose read runs the given effect, then gives every version the verdict.
mkEffectful :: Text -> Int -> EffectfulConfig -> FailureAlignment -> IO () -> RuleVerdict -> IO PreparedRule
mkEffectful name prec cfg align effect verdict = do
    breaker <- newBreaker
    pure (packageRule name prec align (Just (Resilience cfg breaker noBreakerReporter getCurrentTime)) effect verdict)

downEffectfulRule :: IO PreparedRule
downEffectfulRule =
    mkEffectful "DownAdvisory" 400 defaultEffectfulConfig{ecBackoff = []} FailDeny (throwIO (CveQueryFault "advisories-for" "advisory source down")) (NoDecision FixesNoAdvisory)

denyingEffectfulRule :: IO PreparedRule
denyingEffectfulRule =
    mkEffectful "DenyAdvisory" 400 defaultEffectfulConfig FailDeny pass (Deny Nothing revocation)

allowingEffectfulRule :: IO PreparedRule
allowingEffectfulRule =
    mkEffectful "AllowAdvisory" 400 defaultEffectfulConfig FailNoDecision pass (Allow remediation)

effectfulSpec :: Spec
effectfulSpec = describe "effectful rule tier" $ do
    it (caseLabel "integration/pipeline-faults/native-001" "filters an Undecidable version out of a packument and 503s when nothing survives") $ do
        rule <- downEffectfulRule
        privateUp <- servingUpstream (encodePackument (privatePackument [] "0.0.0"))
        publicUp <- servingUpstream (encodePackument (admittingPublic "1.0.0"))
        withProxyEffectful [rule] privateUp publicUp $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 503
            status resp `shouldNotBe` 404

    it (caseLabel "integration/pipeline-faults/native-002" "excludes a version a reachable effectful deny rejects (403 when nothing else survives)") $ do
        rule <- denyingEffectfulRule
        privateUp <- servingUpstream (encodePackument (privatePackument [] "0.0.0"))
        publicUp <- servingUpstream (encodePackument (admittingPublic "1.0.0"))
        withProxyEffectful [rule] privateUp publicUp $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 403

    it (caseLabel "integration/pipeline-faults/native-003" "serves a version a reachable effectful allow admits (a 200 with that version)") $ do
        rule <- allowingEffectfulRule
        privateUp <- servingUpstream (encodePackument (privatePackument [] "0.0.0"))
        publicUp <-
            servingUpstream
                ( encodePackument
                    (packument [("1.0.0", plainVersion "1.0.0")] "1.0.0" [("1.0.0", publishedDaysAgo 1)])
                )
        withProxyEffectful [rule] privateUp publicUp $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["1.0.0"]

    it (caseLabel "integration/pipeline-faults/native-004" "serves a tarball a reachable effectful allow admits on the artifact path") $ do
        rule <- allowingEffectfulRule
        privateUp <- privateArtifactMiss
        let young base = packument [("1.0.0", selfHostedVersion base "1.0.0")] "1.0.0" [("1.0.0", publishedDaysAgo 1)]
        publicUp <- artifactUpstreamServing (encodePackument . young) publicTarballBytes
        withProxyEffectful [rule] privateUp publicUp $ \app -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` publicTarballBytes

    it (caseLabel "integration/pipeline-faults/native-005" "maps a concrete-artifact Undecidable to a 503 on the tarball path") $ do
        rule <- downEffectfulRule
        privateUp <- privateArtifactMiss
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEffectful [rule] privateUp publicUp $ \app -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 503

tightLimits :: Limits
tightLimits =
    defaultLimits
        { maxMetadataBytes = 4096
        , maxVersionCount = 3
        , maxNestingDepth = 8
        }

withLimits :: Limits -> PackumentDeps -> PackumentDeps
withLimits limits d = d{pdLimits = limits}

{- | The typed impure exception the bottoming assembly hook plants: a stand-in
for an invariant break inside the pure render.
-}
newtype AssembleBottom = AssembleBottom Text
    deriving stock (Show)

instance Exception AssembleBottom

-- A deps transform that breaks the metadataAssemble never-throws contract on purpose, for
-- the request-perimeter case: the assembled render bottoms when forced.
withBottomingAssemble :: PackumentDeps -> PackumentDeps
withBottomingAssemble d =
    d{pdMetadata = (pdMetadata d){metadataAssemble = \_ _ _ _ -> impureThrow (AssembleBottom "simulated render invariant break")}}

oversizedPackument :: Text -> LByteString
oversizedPackument v =
    encodePackument $ case admittingPublic v of
        Object o -> Object (KeyMap.insert "_padding" (String (T.replicate 8192 "x")) o)
        other -> other

versionFloodPackument :: LByteString
versionFloodPackument =
    encodePackument
        ( packument
            [(v, plainVersion v) | v <- floodVersions]
            "4.0.0"
            [(v, publishedDaysAgo 30) | v <- floodVersions]
        )
  where
    floodVersions :: [Text]
    floodVersions = ["1.0.0", "2.0.0", "3.0.0", "4.0.0"]

undecodableBody :: LByteString
undecodableBody = "!"

deeplyNestedBody :: LByteString
deeplyNestedBody = encodePackument (packument [("1.0.0", retainedNested)] "1.0.0" [("1.0.0", publishedDaysAgo 30)])
  where
    retainedNested = case plainVersion "1.0.0" of
        Object fields -> Object (KeyMap.insert "exports" nestedValue fields)
        other -> other

nestedValue :: Value
nestedValue = foldr (\_ value -> object ["default" .= value]) (String "./index.js") [1 :: Int .. 32]

boundsSpec :: Spec
boundsSpec = describe "response bounds through the request path (security.md invariant 4)" $ do
    it (caseLabel "integration/pipeline-faults/native-006" "skips deeply nested unknown fields without rejecting the package") $ do
        let body = case admittingPublic "1.0.0" of
                Object fields -> Object (KeyMap.insert "_unknown" nestedValue fields)
                other -> other
        privateUp <- absentUpstream
        publicUp <- servingUpstream (encodePackument body)
        queue <- newTestMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing (withLimits tightLimits) $ \app _env _port -> do
            response <- getThing Nothing app
            status response `shouldBe` 200
            servedVersions response `shouldBe` ["1.0.0"]
            case decodedBody response of
                Object fields -> KeyMap.lookup "_unknown" fields `shouldBe` Nothing
                _ -> expectationFailure "expected a metadata object"

    it (caseLabel "integration/pipeline-faults/native-007" "refuses an oversized private packument with 502, serving no public set") $ do
        privateUp <- servingUpstream (oversizedPackument "9.9.9")
        publicUp <- servingUpstream (encodePackument (admittingPublic "1.0.0"))
        queue <- newTestMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing (withLimits tightLimits) $ \app _env _port -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 502
            servedVersions resp `shouldBe` []

    it (caseLabel "integration/pipeline-faults/native-008" "503s when the only (public) packument is oversized and the private upstream has no such package") $ do
        privateUp <- absentUpstream
        publicUp <- servingUpstream (oversizedPackument "1.0.0")
        queue <- newTestMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing (withLimits tightLimits) $ \app _env _port -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 503
            servedVersions resp `shouldBe` []

    it (caseLabel "integration/pipeline-faults/native-009" "refuses a version-flood public packument fail-closed (nothing served from it)") $ do
        privateUp <- absentUpstream
        publicUp <- servingUpstream versionFloodPackument
        queue <- newTestMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing (withLimits tightLimits) $ \app _env _port -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 503
            servedVersions resp `shouldBe` []

    it (caseLabel "integration/pipeline-faults/native-010" "refuses with 502 when a version-flood document arrives on the private leg") $ do
        privateUp <- servingUpstream versionFloodPackument
        publicUp <- servingUpstream (encodePackument (admittingPublic "1.0.0"))
        queue <- newTestMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing (withLimits tightLimits) $ \app _env _port -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 502
            servedVersions resp `shouldBe` []

    it (caseLabel "integration/pipeline-faults/native-011" "refuses a deeply-nested public document fail-closed (503 with no private package)") $ do
        privateUp <- absentUpstream
        publicUp <- servingUpstream deeplyNestedBody
        queue <- newTestMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing (withLimits tightLimits) $ \app _env _port -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 503
            servedVersions resp `shouldBe` []

    it (caseLabel "integration/pipeline-faults/native-012" "refuses with 502 when a deeply-nested document arrives on the private leg") $ do
        privateUp <- servingUpstream deeplyNestedBody
        publicUp <- servingUpstream (encodePackument (admittingPublic "1.0.0"))
        queue <- newTestMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing (withLimits tightLimits) $ \app _env _port -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 502
            servedVersions resp `shouldBe` []

    it (caseLabel "integration/pipeline-faults/native-013" "serves normally when every document is within the bounds (no false refusal)") $ do
        privateUp <- servingUpstream (encodePackument (privatePackument [("3.0.0", plainVersion "3.0.0")] "3.0.0"))
        publicUp <- servingUpstream (encodePackument (admittingPublic "1.0.0"))
        queue <- newTestMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing (withLimits tightLimits) $ \app _env _port -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["1.0.0", "3.0.0"]

-- The serve path's JSON log lines for one request, over an upstream whose document breaches a
-- bound. The proxy runs on a capturing log environment, so the assertions read real output.
captureBreachLog :: LByteString -> IO Text
captureBreachLog privateBody = do
    privateUp <- servingUpstream privateBody
    publicUp <- failingUpstream
    queue <- newTestMemoryQueue
    logEnv <- newLogEnv JsonLog InfoLevel (DdContext "ecluse" Nothing Nothing Nothing) (Environment "test")
    withProxyOver logEnv queue privateUp publicUp Nothing (withLimits tightLimits) $ \app _env _port ->
        captureStdout $ do
            _ <- getThing Nothing app
            _ <- closeScribes logEnv
            pure ()

boundsLogSpec :: Spec
boundsLogSpec = describe "serve-path warnings are logged before degrading" $ do
    it (caseLabel "integration/pipeline-faults/native-014" "logs a WARNING naming the version-count bound, distinct from a plain parse failure") $ do
        logged <- captureBreachLog versionFloodPackument
        logged `shouldSatisfy` T.isInfixOf "\"status\":\"warn\""
        logged `shouldSatisfy` T.isInfixOf "\"bound\":\"version-count\""
        logged `shouldSatisfy` T.isInfixOf "\"package\":\"thing\""

    it (caseLabel "integration/pipeline-faults/native-015" "logs a WARNING naming the metadata-body-size bound on an oversized body") $ do
        logged <- captureBreachLog (oversizedPackument "9.9.9")
        logged `shouldSatisfy` T.isInfixOf "\"status\":\"warn\""
        logged `shouldSatisfy` T.isInfixOf "\"bound\":\"metadata-body-size\""

    it (caseLabel "integration/pipeline-faults/native-016" "logs a WARNING naming the nesting-depth bound on a deeply-nested body") $ do
        logged <- captureBreachLog deeplyNestedBody
        logged `shouldSatisfy` T.isInfixOf "\"status\":\"warn\""
        logged `shouldSatisfy` T.isInfixOf "\"bound\":\"nesting-depth\""

    it (caseLabel "integration/pipeline-faults/native-017" "logs a WARNING on an undecodable upstream body (the case the bound guards leave silent)") $ do
        logged <- captureBreachLog undecodableBody
        logged `shouldSatisfy` T.isInfixOf "\"status\":\"warn\""
        logged `shouldSatisfy` T.isInfixOf "did not decode"

    it (caseLabel "integration/pipeline-faults/native-018" "tags every serve-path log line with the emitting module") $ do
        logged <- captureBreachLog versionFloodPackument
        let entries = map (decodeStrict' . encodeUtf8) (lines logged) :: [Maybe Value]
            moduleOf entry = textAtPath ["data", "module"] =<< entry
            moduleWhere path value = moduleOf =<< find ((== Just value) . (textAtPath path =<<)) entries
        -- Two log sites fire here: the private version-count breach and the public HTTP 500.
        moduleWhere ["data", "bound"] "version-count" `shouldBe` Just "Ecluse.Server.Pipeline"
        moduleWhere ["message"] "the upstream refused the metadata read" `shouldBe` Just "Ecluse.Server.Pipeline"
        [line | (line, entry) <- zip (lines logged) entries, isNothing (moduleOf entry)] `shouldBe` []

perimeterSpec :: Spec
perimeterSpec = describe "the typed request perimeter (an escaped pre-commit fault)" $
    it (caseLabel "integration/pipeline-faults/native-019" "answers a bottoming assembly with the route's declared neutral 500, never a torn session") $ do
        -- The assembly hook bottoms when the render forces it, an invariant break escaping the
        -- handler pre-commit. The perimeter answers a neutral 500 and the session survives.
        privateUp <- absentUpstream
        publicUp <- servingUpstream (encodePackument (admittingPublic "1.0.0"))
        queue <- newTestMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing withBottomingAssemble $ \app _env _port -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 500
            (decodeUtf8 (simpleBody resp) :: Text) `shouldSatisfy` T.isInfixOf "internal server error"
            (decodeUtf8 (simpleBody resp) :: Text) `shouldSatisfy` (not . T.isInfixOf "simulated render invariant break")
