module Ecluse.Server.Pipeline.PackumentSpec (spec) where

import Data.Aeson (Value (String))
import Data.ByteString.Lazy qualified as LBS
import Ecluse.Core.Package (HashAlg (SHA1, SHA512))
import Ecluse.Core.Package.Integrity (mkMinIntegrity, mkMinTrustedIntegrity)
import Ecluse.Core.Queue (newInMemoryQueue)
import Ecluse.Core.Server.Context (PackumentDeps (..))
import Ecluse.Server.Pipeline.TestSupport
import Network.HTTP.Types (status200)
import Network.Wai (requestHeaders, responseLBS)
import Network.Wai.Test (SResponse (..), simpleBody)
import Test.Hspec
import UnliftIO.Exception (throwString)

spec :: Spec
spec = do
    mergeSpec
    credentialSpec
    privateAuthoritySpec
    partialAvailabilitySpec
    cacheSpec
    noSurvivorsSpec
    edgeAuthSpec
    conditionalSpec
    packumentHeadSpec
    losslessSpec

mergeSpec :: Spec
mergeSpec = describe "multi-upstream merge (not fallback)" $ do
    it "serves the union of trusted-private and gated-public versions" $ do
        privateUp <- servingUpstream (encodePackument (privatePackument [("1.0.0", plainVersion "1.0.0")] "1.0.0"))
        publicUp <-
            servingUpstream
                ( encodePackument
                    ( packument
                        [("1.0.0", plainVersion "1.0.0"), ("2.0.0", plainVersion "2.0.0")]
                        "2.0.0"
                        [("1.0.0", publishedDaysAgo 30), ("2.0.0", publishedDaysAgo 30)]
                    )
                )
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["1.0.0", "2.0.0"]

    it "gates public versions through the rules while trusting private ones" $ do
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

    it "denies a public install-script version while admitting a private one of the same key" $ do
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

    it "filters a hashless public version out of the served listing (integrity-presence policy)" $ do
        privateUp <- failingUpstream
        publicUp <-
            servingUpstream
                ( encodePackument
                    ( packument
                        [("1.0.0", plainVersion "1.0.0"), ("2.0.0", hashlessVersion "2.0.0")]
                        "1.0.0"
                        [("1.0.0", publishedDaysAgo 30), ("2.0.0", publishedDaysAgo 30)]
                    )
                )
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["1.0.0"]

    it "filters an empty-digest public version out of the served listing (a content-empty digest is no digest)" $ do
        privateUp <- failingUpstream
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

    it "drops a hashless trusted-private version from the listing by default (uniform floor)" $ do
        privateUp <-
            servingUpstream
                (encodePackument (privatePackumentWith [("1.0.0", hashlessVersion "1.0.0")] "1.0.0"))
        publicUp <- failingUpstream
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 403

    it "rejects a public version whose only digest is below the floor (SHA-1 shasum)" $ do
        privateUp <- failingUpstream
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

    it "admits a public version carrying a SHA-256 integrity digest (meets the floor)" $ do
        privateUp <- failingUpstream
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

    it "drops a SHA-1-only trusted-private version from the listing by default (trusted floor SHA-256)" $ do
        privateUp <-
            servingUpstream
                (encodePackument (privatePackumentWith [("1.0.0", shasumOnlyVersion "1.0.0")] "1.0.0"))
        publicUp <- failingUpstream
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 403

    it "lists a SHA-1-only trusted-private version when the trusted floor is loosened to SHA-1" $ do
        sha1Floor <- either (fail . toString) pure (mkMinTrustedIntegrity SHA1)
        privateUp <-
            servingUpstream
                (encodePackument (privatePackumentWith [("1.0.0", shasumOnlyVersion "1.0.0")] "1.0.0"))
        publicUp <- failingUpstream
        queue <- newInMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing (\d -> d{pdMinTrustedIntegrity = sha1Floor}) $ \app _env _port -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["1.0.0"]

    it "rejects a public SHA-256 version when the floor is raised to SHA-512 (the floor value is wired)" $ do
        sha512Floor <- either (fail . toString) pure (mkMinIntegrity SHA512)
        privateUp <-
            servingUpstream (encodePackument (privatePackumentWith [("3.0.0", plainVersion "3.0.0")] "3.0.0"))
        publicUp <-
            servingUpstream
                ( encodePackument
                    ( packument
                        [("1.0.0", versionObject "1.0.0" (sri256For "x") False)]
                        "1.0.0"
                        [("1.0.0", publishedDaysAgo 30)]
                    )
                )
        queue <- newInMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing (\d -> d{pdMinIntegrity = sha512Floor}) $ \app _env _port -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["3.0.0"]

    it "drops a public SHA-1-only copy at the same key, serving the private SHA-256 (the weak digest never leaks)" $ do
        privateUp <-
            servingUpstream
                (encodePackument (privatePackumentWith [("1.0.0", versionObject "1.0.0" (sri256For "private") False)] "1.0.0"))
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

    it "serves the private copy on an integrity divergence (private wins; flagged in the merge)" $ do
        privateUp <-
            servingUpstream
                (encodePackument (privatePackumentWith [("1.0.0", versionObject "1.0.0" (sriFor "private") False)] "1.0.0"))
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

    it "repoints dist-tags.latest to a survivor when the public latest is denied (public-only)" $ do
        privateUp <- failingUpstream
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

credentialSpec :: Spec
credentialSpec = describe "credential authority (forward-to-private, strip-before-public)" $
    it "forwards the client credential to the private upstream and NEVER to the public upstream" $ do
        privateUp <- servingUpstream (encodePackument (privatePackument [("1.0.0", plainVersion "1.0.0")] "1.0.0"))
        publicUp <-
            servingUpstream
                (encodePackument (packument [("2.0.0", plainVersion "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 30)]))
        withProxy privateUp publicUp Nothing $ \app -> do
            _ <- getThing (Just "client-secret-token") app
            privAuth <- seenAuth privateUp
            pubAuth <- seenAuth publicUp
            privAuth `shouldBe` [Just "Bearer client-secret-token"]
            pubAuth `shouldBe` [Nothing]

privateAuthoritySpec :: Spec
privateAuthoritySpec = describe "private origin is the per-client authority (not cached across clients)" $ do
    it "re-consults the private upstream per client within the TTL -- each client's token reaches it" $ do
        privateUp <- servingUpstream (encodePackument (privatePackument [("1.0.0", plainVersion "1.0.0")] "1.0.0"))
        publicUp <-
            servingUpstream
                (encodePackument (packument [("2.0.0", plainVersion "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 30)]))
        withProxy privateUp publicUp Nothing $ \app -> do
            _ <- getThing (Just "tokenA") app
            _ <- getThing (Just "tokenB") app
            privAuth <- seenAuth privateUp
            pubAuth <- seenAuth publicUp
            privAuth `shouldBe` [Just "Bearer tokenA", Just "Bearer tokenB"]
            pubAuth `shouldBe` [Nothing]

    it "serves byte-identical bodies across identical repeat requests (the assembled representation is reused)" $ do
        (privateUp, publicUp) <- twoServingUpstreams
        withProxy privateUp publicUp Nothing $ \app -> do
            firstResp <- getThing (Just "tokenA") app
            secondResp <- getThing (Just "tokenA") app
            status firstResp `shouldBe` 200
            simpleBody secondResp `shouldBe` simpleBody firstResp
            header "ETag" secondResp `shouldBe` header "ETag" firstResp
            -- The reuse never skips the per-request private authorisation.
            seenAuth privateUp `shouldReturn` [Just "Bearer tokenA", Just "Bearer tokenA"]

    it "never serves one client's assembled document to another with a different private view" $ do
        -- The private upstream answers per credential: client A's token sees 9.0.0,
        -- client B's sees 9.0.1. Interleaved requests must each get their own merged
        -- document -- the assembled store is keyed by content, so B's entry can never
        -- answer A (and the last request proves A's own entry still serves A).
        seen <- newIORef []
        let perToken req respond = do
                modifyIORef' seen (lookupAuth (requestHeaders req) :)
                let body = case lookupAuth (requestHeaders req) of
                        Just "Bearer token-a" -> encodePackument (privatePackument [("9.0.0", plainVersion "9.0.0")] "9.0.0")
                        _ -> encodePackument (privatePackument [("9.0.1", plainVersion "9.0.1")] "9.0.1")
                respond (responseLBS status200 [] body)
        privateUp <- mkUpstream seen perToken
        publicUp <-
            servingUpstream
                (encodePackument (packument [("2.0.0", plainVersion "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 30)]))
        withProxy privateUp publicUp Nothing $ \app -> do
            respA <- getThing (Just "token-a") app
            respB <- getThing (Just "token-b") app
            respA2 <- getThing (Just "token-a") app
            servedVersions respA `shouldBe` ["2.0.0", "9.0.0"]
            servedVersions respB `shouldBe` ["2.0.0", "9.0.1"]
            servedVersions respA2 `shouldBe` ["2.0.0", "9.0.0"]
            simpleBody respA2 `shouldBe` simpleBody respA

partialAvailabilitySpec :: Spec
partialAvailabilitySpec = describe "partial-upstream availability" $ do
    it "serves the public set when the private upstream is unavailable" $ do
        privateUp <- failingUpstream
        publicUp <-
            servingUpstream
                (encodePackument (packument [("2.0.0", plainVersion "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 30)]))
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["2.0.0"]

    it "serves the private set when the public upstream is unavailable" $ do
        privateUp <- servingUpstream (encodePackument (privatePackument [("1.0.0", plainVersion "1.0.0")] "1.0.0"))
        publicUp <- failingUpstream
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["1.0.0"]

    it "degrades a private leg whose body is unparseable, serving the public set" $ do
        privateUp <- servingUpstream "this is not json at all"
        publicUp <-
            servingUpstream
                (encodePackument (packument [("2.0.0", plainVersion "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 30)]))
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["2.0.0"]

    it "degrades a private leg that decodes but does not project to a packument" $ do
        privateUp <- servingUpstream "[1, 2, 3]"
        publicUp <-
            servingUpstream
                (encodePackument (packument [("2.0.0", plainVersion "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 30)]))
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["2.0.0"]

    it "drops a private leg that self-reports a different package, serving the public set (200)" $ do
        privateUp <-
            servingUpstream
                (encodePackument (packumentNamed "other" [("1.0.0", plainVersion "1.0.0")] "1.0.0" [("1.0.0", publishedDaysAgo 30)]))
        publicUp <-
            servingUpstream
                (encodePackument (packument [("2.0.0", plainVersion "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 30)]))
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["2.0.0"]
            topLevel "name" resp `shouldBe` Just (String "thing")

    it "drops a public leg that self-reports a different package, serving the private set (200)" $ do
        privateUp <- servingUpstream (encodePackument (privatePackument [("1.0.0", plainVersion "1.0.0")] "1.0.0"))
        publicUp <-
            servingUpstream
                (encodePackument (packumentNamed "other" [("2.0.0", plainVersion "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 30)]))
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["1.0.0"]
            topLevel "name" resp `shouldBe` Just (String "thing")

cacheSpec :: Spec
cacheSpec = describe "metadata cache (read-through coherence)" $ do
    it "reuses the cached document within the TTL -- a second request does not re-fetch" $ do
        privateUp <- failingUpstream
        let v1 =
                encodePackument
                    (packument [("1.0.0", plainVersion "1.0.0")] "1.0.0" [("1.0.0", publishedDaysAgo 30)])
            v2 =
                encodePackument
                    ( packument
                        [("1.0.0", plainVersion "1.0.0"), ("2.0.0", plainVersion "2.0.0")]
                        "2.0.0"
                        [("1.0.0", publishedDaysAgo 30), ("2.0.0", publishedDaysAgo 30)]
                    )
        publicUp <- mutatingUpstream (v1 :| [v2])
        withProxy privateUp publicUp Nothing $ \app -> do
            firstResp <- getThing Nothing app
            servedVersions firstResp `shouldBe` ["1.0.0"]
            secondResp <- getThing Nothing app
            status secondResp `shouldBe` 200
            servedVersions secondResp `shouldBe` ["1.0.0"]
            seenAuth publicUp `shouldReturn` [Nothing]

    it "serves a coherent pair: the cached typed decision matches the cached bytes" $ do
        privateUp <- failingUpstream
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
            servedVersionKey "1.0.0" "_unmodeled" secondResp `shouldBe` Just (String "kept")

noSurvivorsSpec :: Spec
noSurvivorsSpec = describe "no survivors in the merge" $ do
    it "503s when no version survives and the private upstream is unavailable (transient)" $ do
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

    it "403s when all public versions are denied and the private upstream genuinely has none" $ do
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
            status resp `shouldBe` 403
            status resp `shouldNotBe` 404

    it "403s when every public version is integrity-inadmissible and the private upstream has none" $ do
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
            status resp `shouldBe` 403
            status resp `shouldNotBe` 404

    it "502s when every responding origin reports a different package (no valid contribution)" $ do
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

    it "502s a public-leg mismatch routed through the metadata cache (private resolves but is empty)" $ do
        privateUp <- servingUpstream (encodePackument (privatePackument [] "0.0.0"))
        publicUp <-
            servingUpstream
                (encodePackument (packumentNamed "other" [("2.0.0", plainVersion "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 30)]))
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 502
            status resp `shouldNotBe` 404

edgeAuthSpec :: Spec
edgeAuthSpec = describe "inbound ECLUSE_AUTH_TOKEN validated at the edge" $ do
    it "401s a request with no/incorrect inbound token before proxying" $ do
        privateUp <- servingUpstream (encodePackument (privatePackument [("1.0.0", plainVersion "1.0.0")] "1.0.0"))
        publicUp <- servingUpstream (encodePackument (packument [] "0.0.0" []))
        withProxy privateUp publicUp (Just "edge-secret") $ \app -> do
            resp <- getThing (Just "wrong-token") app
            status resp `shouldBe` 401
            seenAuth privateUp `shouldReturn` []
            seenAuth publicUp `shouldReturn` []

    it "admits a request presenting the correct inbound token" $ do
        privateUp <- servingUpstream (encodePackument (privatePackument [("1.0.0", plainVersion "1.0.0")] "1.0.0"))
        publicUp <- servingUpstream (encodePackument (packument [] "0.0.0" []))
        withProxy privateUp publicUp (Just "edge-secret") $ \app -> do
            resp <- getThing (Just "edge-secret") app
            status resp `shouldBe` 200

conditionalSpec :: Spec
conditionalSpec = describe "own ETag over the served bytes" $ do
    it "serves a 200 with a strong ETag header" $ do
        (privateUp, publicUp) <- twoServingUpstreams
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            header "ETag" resp `shouldSatisfy` isJust

    it "answers 304 when the client echoes our ETag" $ do
        (privateUp, publicUp) <- twoServingUpstreams
        withProxy privateUp publicUp Nothing $ \app -> do
            firstResp <- getThing Nothing app
            etag <- maybe (throwString "no ETag on the 200 response") pure (header "ETag" firstResp)
            secondResp <- getThingWith [("If-None-Match", etag)] app
            status secondResp `shouldBe` 304
            simpleBody secondResp `shouldBe` ""

    it "consults the private origin on a 304 -- the conditional never bypasses per-client authorisation" $ do
        (privateUp, publicUp) <- twoServingUpstreams
        withProxy privateUp publicUp Nothing $ \app -> do
            firstResp <- getThing (Just "client-token") app
            etag <- maybe (throwString "no ETag on the 200 response") pure (header "ETag" firstResp)
            secondResp <- getThingWith [("If-None-Match", etag), ("Authorization", "Bearer client-token")] app
            status secondResp `shouldBe` 304
            -- Both requests reached the private origin carrying the client's own
            -- credential: the 304 is an answer about content, never a skipped
            -- authorisation -- the derived validator is evaluated only after the
            -- per-request private fetch resolved.
            seenAuth privateUp `shouldReturn` [Just "Bearer client-token", Just "Bearer client-token"]

    it "re-serves 200 when the private document changes under a matching validator (per-client freshness)" $ do
        -- The public origin stays cached across both requests; only the private
        -- leg (re-fetched every request) changes. The validator must track it:
        -- a changed merged view is never answered 304 off the warm public entry.
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
            etag <- maybe (throwString "no ETag on the 200 response") pure (header "ETag" firstResp)
            secondResp <- getThingWith [("If-None-Match", etag)] app
            status secondResp `shouldBe` 200
            header "ETag" secondResp `shouldSatisfy` (/= Just etag)

packumentHeadSpec :: Spec
packumentHeadSpec = describe "HEAD on a packument route (same gating as GET, no body)" $ do
    it "answers a 200 HEAD with the GET's status, ETag, and Content-Length but no body" $ do
        (privateUp, publicUp) <- twoServingUpstreams
        withProxy privateUp publicUp Nothing $ \app -> do
            getResp <- getThing Nothing app
            headResp <- headThing Nothing app
            status getResp `shouldBe` 200
            status headResp `shouldBe` 200
            simpleBody headResp `shouldBe` ""
            header "ETag" headResp `shouldSatisfy` isJust
            header "ETag" headResp `shouldBe` header "ETag" getResp
            header "Content-Length" headResp
                `shouldBe` Just (show (LBS.length (simpleBody getResp)))

    it "answers a conditional HEAD that matches our ETag with a bodiless 304" $ do
        (privateUp, publicUp) <- twoServingUpstreams
        withProxy privateUp publicUp Nothing $ \app -> do
            firstResp <- getThing Nothing app
            etag <- maybe (throwString "no ETag on the 200 response") pure (header "ETag" firstResp)
            headResp <- headThingWith [("If-None-Match", etag)] app
            status headResp `shouldBe` 304
            simpleBody headResp `shouldBe` ""

    it "403s a HEAD identically to the GET when every version is withheld by policy" $ do
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
            status resp `shouldBe` 403
            status resp `shouldNotBe` 404
            simpleBody resp `shouldBe` ""

    it "503s a HEAD identically to the GET when a needed upstream is unavailable (transient)" $ do
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

    it "502s a HEAD identically to the GET when every responding origin reports a different package" $ do
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

    it "401s a HEAD with a bad edge token before touching any upstream (gating identical to GET)" $ do
        privateUp <- servingUpstream (encodePackument (privatePackument [("1.0.0", plainVersion "1.0.0")] "1.0.0"))
        publicUp <- servingUpstream (encodePackument (packument [] "0.0.0" []))
        withProxy privateUp publicUp (Just "edge-secret") $ \app -> do
            resp <- headThing (Just "wrong-token") app
            status resp `shouldBe` 401
            simpleBody resp `shouldBe` ""
            seenAuth privateUp `shouldReturn` []
            seenAuth publicUp `shouldReturn` []

losslessSpec :: Spec
losslessSpec = describe "lossless served surface (raw Value edited in place)" $ do
    it "relays unmodeled top-level and per-version keys unchanged" $ do
        privateUp <- servingUpstream (encodePackument (privatePackument [("1.0.0", plainVersion "1.0.0")] "1.0.0"))
        publicUp <- failingUpstream
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            topLevel "_id" resp `shouldBe` Just (String "thing")
            servedVersionKey "1.0.0" "_unmodeled" resp `shouldBe` Just (String "kept")

    it "rewrites dist.tarball under the mount base so artifacts route back through the gate" $ do
        privateUp <- servingUpstream (encodePackument (privatePackument [("1.0.0", plainVersion "1.0.0")] "1.0.0"))
        publicUp <- failingUpstream
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            servedTarball "1.0.0" resp `shouldBe` Just "https://proxy.test/thing/-/thing-1.0.0.tgz"

    it "rewrites a gated public version's dist.tarball under the mount base (rewritten once at assembly)" $ do
        privateUp <- failingUpstream
        publicUp <- servingUpstream (encodePackument (admittingPublic "1.0.0"))
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedTarball "1.0.0" resp `shouldBe` Just "https://proxy.test/thing/-/thing-1.0.0.tgz"
