-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Shared pipeline refusal behaviour under upstream faults and malformed responses.
module Ecluse.Core.Server.Pipeline.SharedIntegrationSpec (spec) where

import Data.Aeson (Key, Value (Object, String), object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Ecluse.Server.Pipeline.TestSupport
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Wai
import Network.HTTP.Types (status403)
import Network.Wai (responseLBS)
import Network.Wai.Test (SResponse (..), simpleBody)
import Test.Hspec
import UnliftIO.Exception (throwIO)

import Ecluse.Core.Breaker (noBreakerReporter)
import Ecluse.Core.Cve (CveQueryFault (CveQueryFault))
import Ecluse.Core.Rules (PackageRead (prVerdict), PreparedRule, Resilience (..))
import Ecluse.Core.Rules.Effectful (EffectfulConfig (..), defaultEffectfulConfig, newBreaker)
import Ecluse.Core.Rules.Types (FailureAlignment (..), Inability (EvaluationFailed), Reason (FixesNoAdvisory), RuleEvidence (evVersion), RuleVerdict (..))
import Ecluse.Core.Security (Limits (..), defaultLimits)
import Ecluse.Core.Server.Context (PackumentDeps (..))
import Ecluse.Core.Version (renderVersion)
import Ecluse.Runtime.Log (DdContext (DdContext), LogFormat (JsonLog), LogLevel (DebugLevel, InfoLevel), newLogEnv)
import Ecluse.Test.Log (captureStdout, lineField, lineWhere)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Rules (mapPackageRead, packageRule, remediation, revocation)
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

-- A reachable advisory rule that cannot vet one version and abstains on the rest.
undecidableFor :: Text -> IO PreparedRule
undecidableFor undecided = mapPackageRead (\packageRead -> packageRead{prVerdict = verdict}) <$> mkEffectful "PartialAdvisory" 400 defaultEffectfulConfig FailDeny pass (NoDecision FixesNoAdvisory)
  where
    verdict _ evidence
        | renderVersion (evVersion evidence) == undecided = CannotVet FailDeny EvaluationFailed
        | otherwise = NoDecision FixesNoAdvisory

siblings :: [Text]
siblings = ["1.0.0", "2.0.0"]

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
        -- One Undecidable version among admitted siblings drops alone, and the siblings serve.
        partial <- undecidableFor "1.0.0"
        siblingsUp <- servingUpstream (encodePackument (packument [(v, plainVersion v) | v <- siblings] "2.0.0" [(v, publishedDaysAgo 30) | v <- siblings]))
        withProxyEffectful [partial] privateUp siblingsUp $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["2.0.0"]

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

-- The serve path's JSON log lines at the default level for one request, over a private document
-- that breaches a bound and a failing public upstream.
captureBreachLog :: LByteString -> IO Text
captureBreachLog = captureBreachLogAt InfoLevel

captureBreachLogAt :: LogLevel -> LByteString -> IO Text
captureBreachLogAt level privateBody = captureServeLog level (servingUpstream privateBody) failingUpstream

-- The serve path's JSON log lines at @level@ for one request over the two upstreams. The proxy
-- runs on a capturing log environment, so the assertions read real output.
captureServeLog :: LogLevel -> IO Upstream -> IO Upstream -> IO Text
captureServeLog level private public = do
    privateUp <- private
    publicUp <- public
    queue <- newTestMemoryQueue
    logEnv <- newLogEnv JsonLog level (DdContext "ecluse" Nothing Nothing Nothing) (Environment "test")
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
        breach <- captureBreachLogAt DebugLevel versionFloodPackument
        -- The private version-count breach, the public HTTP 500, and a debug line of the serve.
        moduleOfLineWhere ["data", "bound"] "version-count" breach `shouldBe` Just "Ecluse.Server.Pipeline"
        moduleOfLineWhere ["message"] "the upstream refused the metadata read" breach `shouldBe` Just "Ecluse.Server.Pipeline"
        moduleOfLineWhere ["message"] "serving packument request for thing" breach `shouldBe` Just "Ecluse.Server.Pipeline"
        untagged breach `shouldBe` []
        -- A private upstream refusing access with 403, beside an admitting public upstream.
        refused <- captureServeLog DebugLevel (upstreamRespondingWith (responseLBS status403 [] "forbidden")) (servingUpstream (encodePackument (admittingPublic "1.0.0")))
        let refusal = lineWhere ["message"] "the upstream refused metadata access" refused
        (lineField ["data", "module"] =<< refusal) `shouldBe` Just "Ecluse.Server.Pipeline"
        (lineField ["data", "package"] =<< refusal) `shouldBe` Just "thing"
        (lineField ["data", "upstream"] =<< refusal) `shouldSatisfy` isJust
        untagged refused `shouldBe` []

-- The @module@ of the first captured line whose key path holds the value.
moduleOfLineWhere :: [Key] -> Text -> Text -> Maybe Text
moduleOfLineWhere path value logged = lineField ["data", "module"] =<< lineWhere path value logged

-- The captured lines that name no module, a line that is not JSON included.
untagged :: Text -> [Text]
untagged = filter (isNothing . lineField ["data", "module"]) . lines

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
