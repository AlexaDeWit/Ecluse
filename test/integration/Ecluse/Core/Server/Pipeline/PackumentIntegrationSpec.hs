-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | HTTP metadata merging, policy filtering, and representation coherence.
module Ecluse.Core.Server.Pipeline.PackumentIntegrationSpec (spec) where

import Data.Aeson (Value (String), object, (.=))
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Ecluse.Core.Package (HashAlg (SHA1, SHA512))
import Ecluse.Core.Package.Integrity (mkMinIntegrity, mkMinTrustedIntegrity)
import Ecluse.Core.Rules (prepare)
import Ecluse.Core.Rules.Types (Rule (AllowIfOlderThan, DenyExternalDependencies, DenyIfDeprecated))
import Ecluse.Core.Server.Context (PackumentDeps (..))
import Ecluse.Server.Pipeline.ServingDriver (npmServingDriver)
import Ecluse.Server.Pipeline.TestSupport
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Serving (servingContract)
import Ecluse.Test.Json (fieldAt, withKeys)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Rules (admissionOnly, atDefaultPrecedence, inertRuleDeps)
import Ecluse.Test.Server.Mount (withPrivateBaseUrl)
import Ecluse.Test.Wai
import Network.Wai.Test (SResponse (..), simpleBody)
import Test.Hspec
import UnliftIO.Exception (throwIO)

spec :: Spec
spec = do
    servingContract "npm" npmServingDriver
    mergeSpec
    availabilitySpec
    externalDependencySpec
    cacheSpec
    noSurvivorsSpec
    edgeAuthSpec
    conditionalSpec
    packumentHeadSpec
    retainedFieldsSpec

mergeSpec :: Spec
mergeSpec = describe "multi-upstream merge (not fallback)" $ do
    it (caseLabel "integration/npm-metadata/native-001" "gates public versions through the rules while trusting private ones") $ do
        privateUp <- servingUpstream (encodePackument (privatePackument [("3.0.0", plainVersion "3.0.0")] "3.0.0"))
        publicUp <-
            servingUpstream
                ( encodePackument
                    ( packument
                        [("1.5.0", plainVersion "1.5.0"), ("2.0.0", plainVersion "2.0.0")]
                        "2.0.0"
                        [("1.5.0", publishedDaysAgo 30), ("2.0.0", publishedDaysAgo 3)]
                    )
                )
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["1.5.0", "3.0.0"]
            servedLatest resp `shouldBe` Just "3.0.0"

    it (caseLabel "integration/npm-metadata/native-002" "denies a public install-script version while admitting a private one of the same key") $ do
        privateUp <- servingUpstream (encodePackument (privatePackument [("1.0.0", plainVersion "1.0.0")] "1.0.0"))
        publicUp <-
            servingUpstream
                ( encodePackument
                    ( packument
                        [("1.0.0", versionObject "1.0.0" (sriFor "public-int") True)]
                        "1.0.0"
                        [("1.0.0", publishedDaysAgo 30)]
                    )
                )
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["1.0.0"]
            servedIntegrity "1.0.0" resp `shouldBe` Just (sriFor "1.0.0")

    it (caseLabel "integration/npm-metadata/native-003" "filters an empty-digest public version out of the served listing (a content-empty digest is no digest)") $ do
        privateUp <- absentUpstream
        publicUp <-
            servingUpstream
                ( encodePackument
                    ( packument
                        [("1.0.0", plainVersion "1.0.0"), ("2.0.0", emptyDigestVersion "2.0.0")]
                        "1.0.0"
                        [("1.0.0", publishedDaysAgo 30), ("2.0.0", publishedDaysAgo 30)]
                    )
                )
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["1.0.0"]

    it (caseLabel "integration/npm-metadata/native-004" "rejects a public version whose only digest is below the floor (SHA-1 shasum)") $ do
        privateUp <- absentUpstream
        publicUp <-
            servingUpstream
                ( encodePackument
                    ( packument
                        [("1.0.0", plainVersion "1.0.0"), ("2.0.0", shasumOnlyVersion "2.0.0")]
                        "1.0.0"
                        [("1.0.0", publishedDaysAgo 30), ("2.0.0", publishedDaysAgo 30)]
                    )
                )
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["1.0.0"]

    it (caseLabel "integration/npm-metadata/native-005" "admits a public version carrying a SHA-256 integrity digest (meets the floor)") $ do
        privateUp <- absentUpstream
        publicUp <-
            servingUpstream
                ( encodePackument
                    ( packument
                        [("1.0.0", versionObject "1.0.0" (sri256For "x") False)]
                        "1.0.0"
                        [("1.0.0", publishedDaysAgo 30)]
                    )
                )
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["1.0.0"]

    it (caseLabel "integration/npm-metadata/native-006" "drops a SHA-1-only trusted-private version from the listing by default (trusted floor SHA-256)") $ do
        privateUp <-
            servingUpstream
                (encodePackument (privatePackument [("1.0.0", shasumOnlyVersion "1.0.0")] "1.0.0"))
        publicUp <- failingUpstream
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 404

    it (caseLabel "integration/npm-metadata/native-007" "lists a SHA-1-only trusted-private version when the trusted floor is loosened to SHA-1") $ do
        sha1Floor <- either (fail . toString) pure (mkMinTrustedIntegrity SHA1)
        privateUp <-
            servingUpstream
                (encodePackument (privatePackument [("1.0.0", shasumOnlyVersion "1.0.0")] "1.0.0"))
        publicUp <- failingUpstream
        queue <- newTestMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing (\d -> d{pdMinTrustedIntegrity = sha1Floor}) $ \app _env _port -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["1.0.0"]

    it (caseLabel "integration/npm-metadata/native-008" "rejects a public SHA-256 version when the floor is raised to SHA-512 (the floor value is wired)") $ do
        sha512Floor <- either (fail . toString) pure (mkMinIntegrity SHA512)
        privateUp <-
            servingUpstream (encodePackument (privatePackument [("3.0.0", plainVersion "3.0.0")] "3.0.0"))
        publicUp <-
            servingUpstream
                ( encodePackument
                    ( packument
                        [("1.0.0", versionObject "1.0.0" (sri256For "x") False)]
                        "1.0.0"
                        [("1.0.0", publishedDaysAgo 30)]
                    )
                )
        queue <- newTestMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing (\d -> d{pdMinIntegrity = sha512Floor}) $ \app _env _port -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["3.0.0"]

    it (caseLabel "integration/npm-metadata/native-009" "drops a public SHA-1-only copy at the same key, serving the private SHA-256 (the weak digest never leaks)") $ do
        privateUp <-
            servingUpstream
                (encodePackument (privatePackument [("1.0.0", versionObject "1.0.0" (sri256For "private") False)] "1.0.0"))
        publicUp <-
            servingUpstream
                ( encodePackument
                    ( packument
                        [("1.0.0", shasumOnlyVersion "1.0.0")]
                        "1.0.0"
                        [("1.0.0", publishedDaysAgo 30)]
                    )
                )
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["1.0.0"]
            servedIntegrity "1.0.0" resp `shouldBe` Just (sri256For "private")

    it (caseLabel "integration/npm-metadata/native-010" "serves the private copy and latest tag on an integrity divergence") $ do
        privateUp <-
            servingUpstream
                (encodePackument (privatePackument [("1.0.0", versionObject "1.0.0" (sriFor "private") False)] "1.0.0"))
        publicUp <-
            servingUpstream
                ( encodePackument
                    ( packument
                        [("1.0.0", versionObject "1.0.0" (sriFor "public") False)]
                        "1.0.0"
                        [("1.0.0", publishedDaysAgo 30)]
                    )
                )
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedIntegrity "1.0.0" resp `shouldBe` Just (sriFor "private")
            servedLatest resp `shouldBe` Just "1.0.0"

    it (caseLabel "integration/npm-metadata/native-011" "keeps the divergent private version beside an agreeing version") $ do
        privateUp <-
            servingUpstream
                ( encodePackument
                    ( privatePackument
                        [ ("1.0.0", versionObject "1.0.0" (sriFor "private") False)
                        , ("2.0.0", versionObject "2.0.0" (sriFor "shared") False)
                        ]
                        "2.0.0"
                    )
                )
        publicUp <-
            servingUpstream
                ( encodePackument
                    ( packument
                        [ ("1.0.0", versionObject "1.0.0" (sriFor "public") False)
                        , ("2.0.0", versionObject "2.0.0" (sriFor "shared") False)
                        ]
                        "2.0.0"
                        [("1.0.0", publishedDaysAgo 30), ("2.0.0", publishedDaysAgo 3)]
                    )
                )
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["1.0.0", "2.0.0"]
            servedLatest resp `shouldBe` Just "2.0.0"

    it (caseLabel "integration/npm-metadata/native-012" "repoints dist-tags.latest to a survivor when the public latest is denied (public-only)") $ do
        privateUp <- absentUpstream
        publicUp <-
            servingUpstream
                ( encodePackument
                    ( packument
                        [("1.0.0", plainVersion "1.0.0"), ("2.0.0", plainVersion "2.0.0")]
                        "2.0.0"
                        [("1.0.0", publishedDaysAgo 30), ("2.0.0", publishedDaysAgo 3)]
                    )
                )
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["1.0.0"]
            servedLatest resp `shouldBe` Just "1.0.0"

cacheSpec :: Spec
cacheSpec = describe "metadata fetch and assembled-response coherence" $ do
    it (caseLabel "integration/npm-metadata/native-013" "fetches changed full metadata within the TTL and replaces the assembled representation") $ do
        privateUp <- absentUpstream
        let v1 =
                encodePackument
                    (packument [("1.0.0", plainVersion "1.0.0")] "1.0.0" [("1.0.0", publishedDaysAgo 30)])
            v2 =
                encodePackument
                    ( packument
                        [("1.0.0", versionObject "1.0.0" (sriFor "updated-1.0.0") False), ("2.0.0", plainVersion "2.0.0")]
                        "2.0.0"
                        [("1.0.0", publishedDaysAgo 30), ("2.0.0", publishedDaysAgo 30)]
                    )
        publicUp <- mutatingUpstream (v1 :| [v2])
        withProxy privateUp publicUp Nothing $ \app -> do
            firstResp <- getThing Nothing app
            status firstResp `shouldBe` 200
            servedVersions firstResp `shouldBe` ["1.0.0"]
            servedLatest firstResp `shouldBe` Just "1.0.0"
            servedIntegrity "1.0.0" firstResp `shouldBe` Just (sriFor "1.0.0")
            firstEtag <- etagOf firstResp
            secondResp <- getThing Nothing app
            status secondResp `shouldBe` 200
            servedVersions secondResp `shouldBe` ["1.0.0", "2.0.0"]
            servedLatest secondResp `shouldBe` Just "2.0.0"
            servedIntegrity "1.0.0" secondResp `shouldBe` Just (sriFor "updated-1.0.0")
            servedIntegrity "2.0.0" secondResp `shouldBe` Just (sriFor "2.0.0")
            secondEtag <- etagOf secondResp
            secondEtag `shouldNotBe` firstEtag
            seenAuth publicUp `shouldReturn` [Nothing, Nothing]

    it (caseLabel "integration/npm-metadata/native-014" "keeps each fetched policy decision and its served fields coherent") $ do
        privateUp <- absentUpstream
        publicUp <-
            servingUpstream
                ( encodePackument
                    ( packument
                        [("1.0.0", plainVersion "1.0.0"), ("2.0.0", plainVersion "2.0.0")]
                        "2.0.0"
                        [("1.0.0", publishedDaysAgo 30), ("2.0.0", publishedDaysAgo 3)]
                    )
                )
        withProxy privateUp publicUp Nothing $ \app -> do
            firstResp <- getThing Nothing app
            servedVersions firstResp `shouldBe` ["1.0.0"]
            secondResp <- getThing Nothing app
            status secondResp `shouldBe` 200
            servedVersions secondResp `shouldBe` ["1.0.0"]
            servedIntegrity "1.0.0" secondResp `shouldBe` Just (sriFor "1.0.0")
            servedVersionKey "1.0.0" "dependencies" secondResp `shouldBe` Just (object ["fixture-dependency" .= ("^1.0.0" :: Text)])
            servedVersionKey "1.0.0" "_unmodeled" secondResp `shouldBe` Nothing
            seenAuth publicUp `shouldReturn` [Nothing, Nothing]

noSurvivorsSpec :: Spec
noSurvivorsSpec = describe "no survivors in the merge" $ do
    it (caseLabel "integration/npm-metadata/native-015" "503s when no version survives and the private upstream is unavailable (transient)") $ do
        privateUp <- failingUpstream
        publicUp <-
            servingUpstream
                ( encodePackument
                    ( packument
                        [("1.0.0", plainVersion "1.0.0"), ("2.0.0", plainVersion "2.0.0")]
                        "2.0.0"
                        [("1.0.0", publishedDaysAgo 1), ("2.0.0", publishedDaysAgo 1)]
                    )
                )
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 503

    it (caseLabel "integration/npm-metadata/native-016" "denies with 404 (never 503) when no version survives and the mount has no private upstream (serve-only pure gate)") $ do
        -- The absent private leg is structural, not an outage. Nothing is unavailable, so the
        -- young-versions denial renders as the denied listing's 404, never the failed-private 503.
        privateUp <- failingUpstream
        publicUp <-
            servingUpstream
                ( encodePackument
                    ( packument
                        [("1.0.0", plainVersion "1.0.0"), ("2.0.0", plainVersion "2.0.0")]
                        "2.0.0"
                        [("1.0.0", publishedDaysAgo 1), ("2.0.0", publishedDaysAgo 1)]
                    )
                )
        queue <- newTestMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing (withPrivateBaseUrl Nothing) $ \app _env _port -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 404
            -- The stubbed private upstream exists only to satisfy the harness. The
            -- absent leg must never fetch from it.
            seenAuth privateUp `shouldReturn` []

    it (caseLabel "integration/npm-metadata/native-017" "serves the filtered public document with no private upstream configured (serve-only pure gate)") $ do
        privateUp <- failingUpstream
        publicUp <-
            servingUpstream
                ( encodePackument
                    ( packument
                        [("1.0.0", plainVersion "1.0.0")]
                        "1.0.0"
                        [("1.0.0", publishedDaysAgo 30)]
                    )
                )
        queue <- newTestMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing (withPrivateBaseUrl Nothing) $ \app _env _port -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["1.0.0"]
            seenAuth privateUp `shouldReturn` []

    it (caseLabel "integration/npm-metadata/native-018" "404s when all public versions are denied and the private upstream genuinely has none") $ do
        privateUp <- servingUpstream (encodePackument (privatePackument [] "0.0.0"))
        publicUp <-
            servingUpstream
                ( encodePackument
                    ( packument
                        [("2.0.0", versionObject "2.0.0" (sriFor "x") True)]
                        "2.0.0"
                        [("2.0.0", publishedDaysAgo 30)]
                    )
                )
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 404

    it (caseLabel "integration/npm-metadata/native-019" "404s when every public version is integrity-inadmissible and the private upstream has none") $ do
        privateUp <- servingUpstream (encodePackument (privatePackument [] "0.0.0"))
        publicUp <-
            servingUpstream
                ( encodePackument
                    ( packument
                        [("1.0.0", shasumOnlyVersion "1.0.0"), ("2.0.0", hashlessVersion "2.0.0")]
                        "1.0.0"
                        [("1.0.0", publishedDaysAgo 30), ("2.0.0", publishedDaysAgo 30)]
                    )
                )
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 404

    it (caseLabel "integration/npm-metadata/native-020" "502s when every responding origin reports a different package (no valid contribution)") $ do
        privateUp <-
            servingUpstream
                (encodePackument (packumentNamed "other" [("1.0.0", plainVersion "1.0.0")] "1.0.0" [("1.0.0", publishedDaysAgo 30)]))
        publicUp <-
            servingUpstream
                (encodePackument (packumentNamed "other" [("2.0.0", plainVersion "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 30)]))
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 502
            reason resp `shouldBe` "Bad Gateway"
            header "Retry-After" resp `shouldBe` Nothing
            status resp `shouldNotBe` 404

    it (caseLabel "integration/npm-metadata/native-021" "502s a public-leg mismatch routed through the metadata cache (private resolves but is empty)") $ do
        privateUp <- servingUpstream (encodePackument (privatePackument [] "0.0.0"))
        publicUp <-
            servingUpstream
                (encodePackument (packumentNamed "other" [("2.0.0", plainVersion "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 30)]))
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 502
            status resp `shouldNotBe` 404

edgeAuthSpec :: Spec
edgeAuthSpec = describe "inbound ECLUSE_SERVER__AUTH_TOKEN validated at the edge" $ do
    it (caseLabel "integration/npm-metadata/native-022" "401s a request with no/incorrect inbound token before proxying") $ do
        privateUp <- servingUpstream (encodePackument (privatePackument [("1.0.0", plainVersion "1.0.0")] "1.0.0"))
        publicUp <- servingUpstream (encodePackument (packument [] "0.0.0" []))
        withProxy privateUp publicUp (Just "edge-secret") $ \app -> do
            resp <- getThing (Just "wrong-token") app
            status resp `shouldBe` 401
            seenAuth privateUp `shouldReturn` []
            seenAuth publicUp `shouldReturn` []

    it (caseLabel "integration/npm-metadata/native-023" "admits a request presenting the correct inbound token") $ do
        privateUp <- servingUpstream (encodePackument (privatePackument [("1.0.0", plainVersion "1.0.0")] "1.0.0"))
        publicUp <- servingUpstream (encodePackument (packument [] "0.0.0" []))
        withProxy privateUp publicUp (Just "edge-secret") $ \app -> do
            resp <- getThing (Just "edge-secret") app
            status resp `shouldBe` 200

conditionalSpec :: Spec
conditionalSpec = describe "own ETag over the served bytes" $ do
    it (caseLabel "integration/npm-metadata/native-024" "serves a 200 with a strong ETag header") $ do
        (privateUp, publicUp) <- twoServingUpstreams
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            header "ETag" resp `shouldSatisfy` isJust

    it (caseLabel "integration/npm-metadata/native-025" "answers 304 when the client echoes our ETag") $ do
        (privateUp, publicUp) <- twoServingUpstreams
        withProxy privateUp publicUp Nothing $ \app -> do
            firstResp <- getThing Nothing app
            etag <- etagOf firstResp
            secondResp <- getThingWith [("If-None-Match", etag)] app
            status secondResp `shouldBe` 304
            simpleBody secondResp `shouldBe` ""

    it (caseLabel "integration/npm-metadata/native-026" "consults the private origin on a 304 -- the conditional never bypasses per-client authorisation") $ do
        (privateUp, publicUp) <- twoServingUpstreams
        withProxy privateUp publicUp Nothing $ \app -> do
            firstResp <- getThing (Just "client-token") app
            etag <- etagOf firstResp
            secondResp <- getThingWith [("If-None-Match", etag), ("Authorization", "Bearer client-token")] app
            status secondResp `shouldBe` 304
            -- The 304 answers about content, never about a skipped authorisation. The pipeline
            -- evaluates the derived validator only after the per-request private fetch resolves.
            seenAuth privateUp `shouldReturn` [Just "Bearer client-token", Just "Bearer client-token"]

    it (caseLabel "integration/npm-metadata/native-027" "re-serves 200 when the private document changes under a matching validator (per-client freshness)") $ do
        -- Only the private leg changes, because the pipeline re-fetches it every request. A changed
        -- merged view must never answer 304 off the warm public entry.
        privateUp <-
            mutatingUpstream
                ( encodePackument (privatePackument [("9.0.0", plainVersion "9.0.0")] "9.0.0")
                    :| [encodePackument (privatePackument [("9.0.1", plainVersion "9.0.1")] "9.0.1")]
                )
        publicUp <-
            servingUpstream
                (encodePackument (packument [("2.0.0", plainVersion "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 30)]))
        withProxy privateUp publicUp Nothing $ \app -> do
            firstResp <- getThing Nothing app
            status firstResp `shouldBe` 200
            etag <- etagOf firstResp
            secondResp <- getThingWith [("If-None-Match", etag)] app
            status secondResp `shouldBe` 200
            header "ETag" secondResp `shouldSatisfy` (/= Just etag)

packumentHeadSpec :: Spec
packumentHeadSpec = describe "HEAD on a packument route (same gating as GET, no body)" $ do
    it (caseLabel "integration/npm-metadata/native-028" "404s a HEAD identically to the GET when every version is withheld by policy") $ do
        privateUp <- servingUpstream (encodePackument (privatePackument [] "0.0.0"))
        publicUp <-
            servingUpstream
                ( encodePackument
                    ( packument
                        [("2.0.0", versionObject "2.0.0" "sha512-x" True)]
                        "2.0.0"
                        [("2.0.0", publishedDaysAgo 30)]
                    )
                )
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- headThing Nothing app
            status resp `shouldBe` 404
            simpleBody resp `shouldBe` ""

    it (caseLabel "integration/npm-metadata/native-029" "503s a HEAD identically to the GET when a needed upstream is unavailable (transient)") $ do
        privateUp <- failingUpstream
        publicUp <-
            servingUpstream
                ( encodePackument
                    ( packument
                        [("1.0.0", plainVersion "1.0.0"), ("2.0.0", plainVersion "2.0.0")]
                        "2.0.0"
                        [("1.0.0", publishedDaysAgo 1), ("2.0.0", publishedDaysAgo 1)]
                    )
                )
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- headThing Nothing app
            status resp `shouldBe` 503
            simpleBody resp `shouldBe` ""

    it (caseLabel "integration/npm-metadata/native-030" "502s a HEAD identically to the GET when every responding origin reports a different package") $ do
        privateUp <-
            servingUpstream
                (encodePackument (packumentNamed "other" [("1.0.0", plainVersion "1.0.0")] "1.0.0" [("1.0.0", publishedDaysAgo 30)]))
        publicUp <-
            servingUpstream
                (encodePackument (packumentNamed "other" [("2.0.0", plainVersion "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 30)]))
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- headThing Nothing app
            status resp `shouldBe` 502
            simpleBody resp `shouldBe` ""

    it (caseLabel "integration/npm-metadata/native-031" "401s a HEAD with a bad edge token before touching any upstream (gating identical to GET)") $ do
        privateUp <- servingUpstream (encodePackument (privatePackument [("1.0.0", plainVersion "1.0.0")] "1.0.0"))
        publicUp <- servingUpstream (encodePackument (packument [] "0.0.0" []))
        withProxy privateUp publicUp (Just "edge-secret") $ \app -> do
            resp <- headThing (Just "wrong-token") app
            status resp `shouldBe` 401
            simpleBody resp `shouldBe` ""
            seenAuth privateUp `shouldReturn` []
            seenAuth publicUp `shouldReturn` []

retainedFieldsSpec :: Spec
retainedFieldsSpec = describe "supported served metadata fields" $ do
    it (caseLabel "integration/npm-metadata/native-032" "retains installation fields and omits unknown metadata") $ do
        privateUp <- servingUpstream (encodePackument (privatePackument [("1.0.0", plainVersion "1.0.0")] "1.0.0"))
        publicUp <- failingUpstream
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            fieldAt "_id" (decodedBody resp) `shouldBe` Nothing
            servedVersionKey "1.0.0" "_unmodeled" resp `shouldBe` Nothing
            servedVersionKey "1.0.0" "dependencies" resp `shouldBe` Just (object ["fixture-dependency" .= ("^1.0.0" :: Text)])
            let pointer = servedVersionKey "1.0.0" "author" resp
            fieldAt "author" (decodedBody resp) `shouldBe` pointer
            pointer `shouldSatisfy` \case
                Just (String text) -> "See http://" `T.isPrefixOf` text && "/thing" `T.isSuffixOf` text
                _ -> False

    it (caseLabel "integration/npm-metadata/native-033" "rewrites dist.tarball under the mount base so artifacts route back through the gate") $ do
        privateUp <- servingUpstream (encodePackument (privatePackument [("1.0.0", plainVersion "1.0.0")] "1.0.0"))
        publicUp <- failingUpstream
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            servedTarball "1.0.0" resp `shouldBe` Just "https://proxy.test/thing/-/thing-1.0.0.tgz"

    it (caseLabel "integration/npm-metadata/native-034" "rewrites a gated public version's dist.tarball under the mount base (rewritten once at assembly)") $ do
        privateUp <- absentUpstream
        publicUp <- servingUpstream (encodePackument (admittingPublic "1.0.0"))
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedTarball "1.0.0" resp `shouldBe` Just "https://proxy.test/thing/-/thing-1.0.0.tgz"

-- | A 200 that carries no validator, which every conditional case here builds on.
data NoETag = NoETag
    deriving stock (Show)

instance Exception NoETag

-- | The validator a conditional case echoes back on its second request.
etagOf :: SResponse -> IO ByteString
etagOf = maybe (throwIO NoETag) pure . header "ETag"

availabilitySpec :: Spec
availabilitySpec = describe "admission-only deprecation policy" $
    for_ [(False, "obsolete release line" :: Text, 404), (False, "", 200), (True, "obsolete release line", 200)] $ \(stored, notice, expected) ->
        it (caseLabel ("integration/npm-metadata/native-035/" <> (if stored then "stored" else if expected == 404 then "denied" else "allowed")) (toText ("serves deprecated stored=" <> show stored <> ", notice=" <> toString notice :: String))) $ do
            let version = withKeys ["deprecated" .= notice] (plainVersion "1.0.0")
                metadata = encodePackument (packument [("1.0.0", version)] "1.0.0" [("1.0.0", publishedDaysAgo 30)])
            publicUp <- servingUpstream metadata
            privateUp <- if stored then servingUpstream metadata else failingUpstream
            queue <- newTestMemoryQueue
            rules <- prepare inertRuleDeps [admissionOnly (atDefaultPrecedence DenyIfDeprecated), atDefaultPrecedence (AllowIfOlderThan 0)]
            let privateLeg = if stored then id else withPrivateBaseUrl Nothing
            withProxyEnvQueueDeps queue privateUp publicUp Nothing (\deps -> privateLeg deps{pdRules = rules}) $ \app _ _ -> do
                response <- getThing Nothing app
                status response `shouldBe` expected
                unless stored $ seenAuth privateUp `shouldReturn` []
                when (expected == 404) $ simpleBody response `shouldSatisfy` (not . T.isInfixOf "obsolete release line" . decodeUtf8 . LBS.toStrict)

externalDependencySpec :: Spec
externalDependencySpec = describe "admission-only dependency deny" $
    it (caseLabel "integration/npm-metadata/native-036" "denies a public version whose dependency names a git source while admitting a private one") $ do
        let gitDependent = withKeys ["dependencies" .= object ["left-pad" .= ("github:acme/left-pad" :: Text)]] . plainVersion
        publicUp <-
            servingUpstream
                ( encodePackument
                    ( packument
                        [("1.0.0", plainVersion "1.0.0"), ("1.1.0", gitDependent "1.1.0")]
                        "1.1.0"
                        [("1.0.0", publishedDaysAgo 30), ("1.1.0", publishedDaysAgo 30)]
                    )
                )
        privateUp <- servingUpstream (encodePackument (privatePackument [("2.0.0", gitDependent "2.0.0")] "2.0.0"))
        queue <- newTestMemoryQueue
        rules <- prepare inertRuleDeps [admissionOnly (atDefaultPrecedence DenyExternalDependencies), atDefaultPrecedence (AllowIfOlderThan 0)]
        withProxyEnvQueueDeps queue privateUp publicUp Nothing (\deps -> deps{pdRules = rules}) $ \app _ _ -> do
            response <- getThing Nothing app
            status response `shouldBe` 200
            servedVersions response `shouldBe` ["1.0.0", "2.0.0"]
