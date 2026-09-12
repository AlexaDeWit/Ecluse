-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Runtime.ServerMountSpec (spec) where

import Prelude hiding (get)

import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Network.HTTP.Types (Header, Status, hContentType, status200, status404)
import Network.Wai (Application)
import Network.Wai.Test qualified as WaiTest
import Test.Hspec
import Test.Hspec.Wai

import Ecluse.Composition.TelemetrySupport (newAdvisoryHandles)
import Ecluse.Core.Breaker (noBreakerReporter)
import Ecluse.Core.Cve (DbEtag (DbEtag))
import Ecluse.Core.Cve.Slot (swapIn)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Rules (prepare)
import Ecluse.Core.Rules.Types (
    DenyIfCveParams (DenyIfCveParams),
    FailureAlignment (FailDeny),
    PrecededRule,
    Rule (AllowByIdentity, AllowIfOlderThan, DenyIfCve),
 )
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Upstream (MirrorServePlan (NoMirrorWrite))
import Ecluse.Cve.Sync (CveSyncHandle (csEnv, csReady), cveRuleDepsFor, cveSyncReadiness)
import Ecluse.Runtime.Cve.Sync (SyncEnv (syncSlot))
import Ecluse.Runtime.Server (ServerConfig (scCheckReady), application, mkServerConfig)
import Ecluse.Runtime.Test.Support (newTestEnv)
import Ecluse.Service (mountBindingFor)
import Ecluse.Test.Cve (fakeCveDb)
import Ecluse.Test.Package (validSha256, validSha256Sri)
import Ecluse.Test.Registry.Npm (VersionSpec (vsIntegrity), packumentValue, publishedDaysAgo, versionSpec, versionValue)
import Ecluse.Test.Rules (atDefaultPrecedence, noFaultReporter)
import Ecluse.Test.Server.Mount (inertPackumentDeps, npmServeDeps, pypiServeDeps)
import Ecluse.Test.Stub (Captured (capHeaders, capPath), stubLocalhostUrl, withRoutedStub)
import Ecluse.Test.Wai (selfBaseUrlOf, servedVersions, status)

{- | A single npm mount with __inert__ packument-serve dependencies (every upstream a
closed port) and no publish target, resolved as the composition root resolves it.
-}
npmApp :: IO Application
npmApp = application (mkServerConfig (maybeToList (mountBindingFor Npm inertPackumentDeps Nothing))) <$> newTestEnv

spec :: Spec
spec = do
    describe "the composed npm front door (a bare npm mount over mkServerConfig)" $
        with npmApp $ do
            it "mounts npm at /npm (answers /npm/-/ping locally with 200 {})" $
                get "/npm/-/ping" `shouldRespondWith` "{}"{matchStatus = 200}

            it "routes an npm packument under the mount into the data plane (503; upstreams closed)" $
                -- Both upstreams are bound to a closed port, so no version survives and the serve
                -- path answers 503. A 404 would mean the mount's router never claimed the path.
                get "/npm/is-odd" `shouldRespondWith` 503

            it "does NOT mount npm at the root -- /-/ping there is the neutral 404" $
                get "/-/ping" `shouldRespondWith` "Not Found\n"{matchStatus = 404}

            it "renders an unmounted prefix as a neutral text/plain 404" $
                get "/pypi/is-odd" `shouldRespondWith` "Not Found\n"{matchStatus = 404}

    describe "two mounts, one advisory database" $ do
        it "serves npm, refuses the PyPI read, and serves it once the PyPI artifact lands" $
            -- The owner's case: npm synced and the PyPI artifact never arrived. One upstream
            -- serves both documents, so the advisory slot is the only difference between them.
            withRoutedStub upstreamReply $ \stub -> do
                (app, handles) <- partialAdvisoryApp (stubLocalhostUrl stub) advisoryPolicy

                npm <- requestPath app "/npm/leftpad"
                status npm `shouldBe` 200
                servedVersions npm `shouldBe` ["1.0.0"]

                pypi <- requestPath app "/pypi/simple/leftpad/"
                status pypi `shouldBe` 503

                awaiting <- requestPath app "/readyz"
                status awaiting `shouldBe` 200
                bodyOf awaiting `shouldSatisfy` BS.isInfixOf "\"pypi\":\"awaiting startup readiness\""

                -- The recovery is what pins that 503 on the empty slot, because an upstream
                -- failure renders a bare 503 too and this upstream never changed.
                advisoriesLanded handles PyPI
                recovered <- requestPath app "/pypi/simple/leftpad/"
                status recovered `shouldBe` 200

                ready <- requestPath app "/readyz"
                bodyOf ready `shouldSatisfy` BS.isInfixOf "\"npm\":\"ready\""
                bodyOf ready `shouldSatisfy` BS.isInfixOf "\"pypi\":\"ready\""

        it "admits the PyPI read on an identity override while that slot is still empty" $
            -- An intentional allow outranks the advisory deny, so it keeps admitting through
            -- the outage the mount beside it is still waiting out.
            withRoutedStub upstreamReply $ \stub -> do
                (app, _) <- partialAdvisoryApp (stubLocalhostUrl stub) overridePolicy
                overridden <- requestPath app "/pypi/simple/leftpad/"
                status overridden `shouldBe` 200

{- | Both mounts as the composition root resolves them, over one upstream, with npm's advisory
database installed and PyPI's slot empty. The handles come back so a test can land PyPI's.
-}
partialAdvisoryApp :: Text -> [PrecededRule] -> IO (Application, Map.Map Ecosystem CveSyncHandle)
partialAdvisoryApp upstreamBase policy = do
    handles <- Map.fromList <$> newAdvisoryHandles [Npm, PyPI]
    advisoriesLanded handles Npm
    for_ (Map.lookup PyPI handles) $ \handle ->
        atomically (writeTVar (csReady handle) False)
    let depsFor = cveRuleDepsFor handles noBreakerReporter noFaultReporter
    npmRules <- prepare (depsFor Npm) policy
    pypiRules <- prepare (depsFor PyPI) policy
    env <- newTestEnv
    let public = loopbackRegistryUrl upstreamBase
        bindings =
            catMaybes
                [ mountBindingFor Npm (npmServeDeps Nothing public NoMirrorWrite npmRules (pure servedAt)) Nothing
                , mountBindingFor PyPI (pypiServeDeps Nothing public NoMirrorWrite pypiRules (pure servedAt)) Nothing
                ]
        cfg = (mkServerConfig bindings){scCheckReady = cveSyncReadiness handles}
    pure (application cfg env, handles)

-- | One mount's first sync landing: its slot fills, and its one-way readiness flag flips.
advisoriesLanded :: Map.Map Ecosystem CveSyncHandle -> Ecosystem -> IO ()
advisoriesLanded handles eco =
    for_ (Map.lookup eco handles) $ \handle -> do
        swapIn (syncSlot (csEnv handle)) (DbEtag "landed") Nothing (fakeCveDb [])
        atomically (writeTVar (csReady handle) True)

{- | The policy both mounts run. The advisory deny outranks the age allow, so a mount whose slot
is empty refuses what the rule cannot vet, and a mount with its database admits.
-}
advisoryPolicy :: [PrecededRule]
advisoryPolicy =
    [ atDefaultPrecedence (DenyIfCve (DenyIfCveParams 8.0 FailDeny))
    , atDefaultPrecedence (AllowIfOlderThan 0)
    ]

{- | 'advisoryPolicy' under an operator's identity allow, which outranks the advisory deny by
default precedence. PyPI keys a release by its canonical PEP 440 spelling, so @1.0.0@ is @1@.
-}
overridePolicy :: [PrecededRule]
overridePolicy = atDefaultPrecedence (AllowByIdentity "leftpad@1") : advisoryPolicy

-- One upstream for both ecosystems. Each document names the authority that served it, which is
-- the authority a projection accepts artifact locations on.
upstreamReply :: Captured -> (Status, [Header], LByteString)
upstreamReply cap
    | capPath cap == "/leftpad" = served "application/json" (npmPackument authority)
    | "/simple/leftpad" `BS.isPrefixOf` capPath cap =
        served "application/vnd.pypi.simple.v1+json" (pypiIndex authority)
    | otherwise = (status404, [], "")
  where
    authority = selfBaseUrlOf (capHeaders cap)
    served mediaType document = (status200, [(hContentType, mediaType)], encode document)

-- | One admissible npm version, digested to the fixtures' SHA-256 floor.
npmPackument :: Text -> Value
npmPackument authority =
    packumentValue
        "leftpad"
        "1.0.0"
        [("1.0.0", versionValue (versionSpec "leftpad" "1.0.0" (authority <> "/leftpad/-/leftpad-1.0.0.tgz")){vsIntegrity = Just validSha256Sri})]
        ["1.0.0" .= publishedDaysAgo servedAt 30]
        []

-- | One admissible PyPI release, on the index's own authority.
pypiIndex :: Text -> Value
pypiIndex authority =
    object
        [ "name" .= ("leftpad" :: Text)
        , "meta" .= object ["api-version" .= ("1.1" :: Text)]
        , "files"
            .= [ object
                    [ "filename" .= ("leftpad-1.0.0.tar.gz" :: Text)
                    , "url" .= (authority <> "/simple/leftpad/leftpad-1.0.0.tar.gz")
                    , "hashes" .= object ["sha256" .= validSha256]
                    , "requires-python" .= (">=3.10" :: Text)
                    , "upload-time" .= ("2026-01-01T00:00:00Z" :: Text)
                    ]
               ]
        ]

-- | A fixed "now", so the age rule is deterministic.
servedAt :: UTCTime
servedAt = UTCTime (fromGregorian 2026 6 20) 0

-- | One GET against the composed application, as an orchestrator or a client would send it.
requestPath :: Application -> ByteString -> IO WaiTest.SResponse
requestPath app path = WaiTest.runSession (WaiTest.request (WaiTest.setPath WaiTest.defaultRequest path)) app

-- | A response body as strict bytes, for a substring assertion over rendered JSON.
bodyOf :: WaiTest.SResponse -> ByteString
bodyOf = LBS.toStrict . WaiTest.simpleBody
