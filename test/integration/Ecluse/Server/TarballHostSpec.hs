module Ecluse.Server.TarballHostSpec (spec) where

import Prelude hiding (get)

import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString qualified as BS
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian, nominalDay)
import Katip (Environment (Environment), Namespace (Namespace), initLogEnv)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (status200, status302, statusCode)
import Network.HTTP.Types.Header (hHost, hLocation)
import Network.Wai (Application, Request (rawPathInfo), requestHeaders, responseLBS)
import Network.Wai.Handler.Warp (Port, testWithApplication)
import Network.Wai.Test (
    SResponse (simpleBody, simpleStatus),
    defaultRequest,
    request,
    runSession,
    setPath,
 )
import Test.Hspec
import UnliftIO.Exception (try)

import Ecluse.Core.Credential (AuthToken (AuthToken, authExpiresAt, authSecret), CredentialProvider, mkSecret, staticProvider)
import Ecluse.Core.Package.Integrity (defaultMinIntegrity, defaultMinTrustedIntegrity)
import Ecluse.Core.Queue (newInMemoryQueue)
import Ecluse.Core.Registry (ParseError (ParseError), RegistryClient (..))
import Ecluse.Core.Registry.Npm.Route qualified as Npm
import Ecluse.Core.Registry.Npm.Serve (npmRenderer)
import Ecluse.Core.Rules (prepare)
import Ecluse.Core.Rules.Types (PrecededRule, Rule (AllowIfOlderThan), atDefaultPrecedence)
import Ecluse.Core.Security (LoweredHostSet, TarballHostPolicy (AnyAllowlistedHost, SameHostAsPackument), defaultLimits, lowerCaseHosts)
import Ecluse.Core.Server.Cache (defaultCacheConfig, newMetadataCache)
import Ecluse.Core.Server.Context (PackumentDeps (..))
import Ecluse.Env (newEnv, newWorkerHeartbeat)
import Ecluse.Server (MountBinding (..), application, mkServerConfig)
import Ecluse.Telemetry (telemetryDisabled)

{- | The tarball-host policy and the resolved-IP recheck, exercised together through
the __real serve path__ — the coverage S40 (which only unit-tested the pure
'Ecluse.Core.Security.tarballHostAllowed') lacked.

A cross-host @dist.tarball@ is driven through the proxy against an in-process upstream
on loopback. The packument is served on @127.0.0.1@ but names its artifact on a
__different host__ (@localhost@, a loopback alias on a distinct hostname), so the
policy sees a genuine cross-host location while the fetch still lands on the same
in-process server. The public\/artifact manager is the __guarded__ one (the
resolved-IP recheck live), so the three controls are a matched set:

* under the secure-default 'SameHostAsPackument' the cross-host tarball is __refused__
  (@403@) before any artifact fetch;
* under 'AnyAllowlistedHost', with the cross-host on the upstream allowlist and its
  resolved loopback address opted in, it is __served__;
* under 'AnyAllowlistedHost', with the host allowlisted but its resolved internal
  address __not__ opted in, the resolved-IP recheck __blocks__ it at connect time —
  the DNS-rebinding\/resolve-to-internal backstop — so it is never served.
-}
spec :: Spec
spec = describe "tarball-host policy + resolved-IP recheck (real serve path, cross-host)" $ do
    it "refuses a cross-host dist.tarball under the SameHostAsPackument default (403)" $
        withUpstream $ \port -> do
            app <- proxyApp SameHostAsPackument (optIn ["127.0.0.1"]) port
            resp <- getTarball app
            status resp `shouldBe` 403

    it "serves a cross-host dist.tarball under AnyAllowlistedHost when the host is allowlisted and opted in" $
        withUpstream $ \port -> do
            -- localhost is on the allowlist (via the mirror target's host) and its
            -- resolved loopback addresses are opted in — 127.0.0.1 and ::1, in the
            -- canonical form the guard renders (some hosts, incl. CI, resolve
            -- localhost to ::1 only), so the guarded manager reaches it.
            app <- proxyApp AnyAllowlistedHost (optIn ["127.0.0.1", "::1"]) port
            resp <- getTarball app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` tarballBytes

    it "blocks a cross-host dist.tarball that resolves to an internal address when not opted in (resolved-IP recheck)" $
        withUpstream $ \port -> do
            -- The cross-host (localhost) is allowlisted and the policy admits it, but its
            -- resolved address (127.0.0.1) is NOT opted in, so the guarded manager's
            -- connection-time recheck refuses the fetch — never a served body.
            app <- proxyApp AnyAllowlistedHost (lowerCaseHosts Set.empty) port
            outcome <- try (getTarball app) :: IO (Either SomeException SResponse)
            -- The recheck surfaces as a connection failure at the fetch, so the request
            -- fails internally rather than streaming the internal target's bytes.
            case outcome of
                Left _ -> pass
                Right resp -> status resp `shouldNotBe` 200

    it "never follows an upstream tarball redirect on the anonymous plane (the redirect target is not contacted)" $
        withAttacker $ \attackerHits attackerPort ->
            withRedirectingTarballUpstream attackerPort $ \port -> do
                -- localhost (the cross-host the packument names) is allowlisted via the
                -- mirror target and its loopback addresses are opted in, so the artifact
                -- fetch reaches the upstream; the upstream answers the @.tgz@ with a 302 to
                -- a separate loopback server that is ALSO opted in — so the resolved-IP
                -- recheck would not block it. The only thing between the proxy and that
                -- server is the anonymous plane's disabled redirect-following: with it the
                -- hop is never taken, the attacker is never contacted, and no 200 is served.
                app <- proxyApp AnyAllowlistedHost (optIn ["127.0.0.1", "::1"]) port
                resp <- getTarball app
                hits <- readIORef attackerHits
                hits `shouldBe` 0
                status resp `shouldNotBe` 200

    it "renders the mount's 503 (not a bare 500) when the admitted artifact's upstream open fails" $
        withDeadTarballUpstream $ \livePort -> do
            -- The packument resolves and the version is admitted, but its dist.tarball
            -- names a same-host port nothing listens on, so opening the artifact
            -- connection fails. That open-phase failure is recoverable (no response is
            -- committed), so the serve path must render the transient upstream-unavailable
            -- error through the mount's renderer — the 503 its siblings produce — rather
            -- than letting the failure escape uncaught into Warp's generic 500.
            app <- proxyApp SameHostAsPackument (optIn ["127.0.0.1"]) livePort
            resp <- getTarball app
            status resp `shouldBe` 503
            -- The body is the mount renderer's, proving the transient error was rendered
            -- rather than surfaced as Warp's default 500 page.
            simpleBody resp `shouldBe` upstreamUnavailableBody

-- ── the proxy under test ──────────────────────────────────────────────────────

{- The proxy application over the guarded data-plane manager (resolved-IP recheck
live), with the given tarball-host policy and internal opt-in. The private origin is
unreachable, so every request misses to the public origin — the cross-host path under
test. The opt-in is shared between the guarded manager's recheck and the deps' pure
tarball-host gate, so the two halves of the internal-range block stay in step. -}
proxyApp :: TarballHostPolicy -> LoweredHostSet -> Port -> IO Application
proxyApp policy internalOptIn port = do
    -- The validating data-plane manager (it also reaches the in-process http loopback).
    guardedManager <- newManager defaultManagerSettings
    -- A trusted manager for the (unreachable) private origin; the private fetch only
    -- ever misses here, so a plain manager suffices.
    trusted <- newManager defaultManagerSettings
    queue <- newInMemoryQueue
    metadataCache <- newMetadataCache defaultCacheConfig
    logEnv <- initLogEnv (Namespace ["ecluse"]) (Environment "test")
    heartbeat <- newWorkerHeartbeat
    env <- newEnv fakeRegistry queue fakeCredentials guardedManager trusted metadataCache logEnv telemetryDisabled heartbeat
    mountDeps <- deps policy internalOptIn port
    let cfg =
            mkServerConfig
                [ MountBinding
                    { bindingPrefix = "npm" :| []
                    , bindingClassifier = Npm.classify
                    , bindingPackumentDeps = Just mountDeps
                    , bindingPublishDeps = Nothing
                    , bindingRenderer = npmRenderer
                    }
                ]
    pure (application cfg env)

{- The per-mount deps for the cross-host scenario: the public origin is the loopback
upstream on @127.0.0.1@; @localhost@ (the cross-host the packument names) is added to
the allowlist via the mirror target's host, so it is allowlisted yet distinct from the
packument host. The private origin is an unreachable loopback port. -}
deps :: TarballHostPolicy -> LoweredHostSet -> Port -> IO PackumentDeps
deps policy internalOptIn port = do
    prepared <- prepare admitOldEnough
    pure
        PackumentDeps
            { pdPrivateBaseUrl = "http://127.0.0.1:1" -- unreachable: forces a public miss
            , pdPublicBaseUrl = "http://127.0.0.1:" <> show port
            , pdMountBaseUrl = "https://proxy.test"
            , pdMirrorTarget = "http://localhost:9" -- puts localhost on the upstream allowlist
            , pdRules = prepared
            , pdTarballHostPolicy = policy
            , pdAllowedInternalHosts = internalOptIn
            , pdLimits = defaultLimits
            , pdInboundToken = Nothing
            , pdNow = pure fixedNow
            , pdHelp = Nothing
            , pdMinIntegrity = defaultMinIntegrity
            , pdMinTrustedIntegrity = defaultMinTrustedIntegrity
            }

-- ── the upstream double ───────────────────────────────────────────────────────

{- An in-process upstream on loopback: it answers a tarball-slot path with the
artifact bytes, and the packument fetch with a single-version packument whose
@dist.tarball@ names @localhost@ (the cross-host) at this same port — so the honoured
location is genuinely cross-host yet still served here. -}
withUpstream :: (Port -> IO a) -> IO a
withUpstream = testWithApplication (pure app)
  where
    app :: Application
    app req respond =
        respond $
            if ".tgz" `BS.isSuffixOf` rawPathInfo req
                then responseLBS status200 [] tarballBytes
                else responseLBS status200 [] (encode (crossHostPackument (selfPort req)))

-- The port this double was reached on, from the request's @Host@ header, so the
-- cross-host dist.tarball can name @localhost@ at the same port.
selfPort :: Request -> Text
selfPort req =
    case find ((== hHost) . fst) (requestHeaders req) of
        Just (_, hostPort) -> snd (T.breakOnEnd ":" (decodeUtf8 hostPort))
        Nothing -> ""

-- A single-version packument whose dist.tarball is on localhost (the cross-host) at
-- the given port — a loopback alias on a distinct hostname.
crossHostPackument :: Text -> Value
crossHostPackument port =
    object
        [ "name" .= ("thing" :: Text)
        , "dist-tags" .= object ["latest" .= ("1.0.0" :: Text)]
        , "versions"
            .= object
                [ "1.0.0"
                    .= object
                        [ "name" .= ("thing" :: Text)
                        , "version" .= ("1.0.0" :: Text)
                        , "dist"
                            .= object
                                [ "tarball" .= ("http://localhost:" <> port <> "/thing/-/thing-1.0.0.tgz")
                                , "integrity" .= validSri
                                ]
                        ]
                ]
        , "time" .= object ["1.0.0" .= ("2020-01-01T00:00:00.000Z" :: Text)]
        ]

tarballBytes :: LByteString
tarballBytes = "CROSS-HOST-TGZ-BYTES"

{- An in-process server standing in for a redirect target: it records every hit and
would serve a 200 body, so a followed redirect is observable as a non-zero hit count
and a served 200. The anonymous plane must never reach it. -}
withAttacker :: (IORef Int -> Port -> IO a) -> IO a
withAttacker k = do
    hits <- newIORef (0 :: Int)
    testWithApplication (pure (attackerApp hits)) (k hits)
  where
    attackerApp :: IORef Int -> Application
    attackerApp hits _req respond = do
        modifyIORef' hits (+ 1)
        respond (responseLBS status200 [] "ATTACKER-TGZ-BYTES")

{- An in-process upstream that serves the cross-host packument but answers the artifact
@.tgz@ slot with a 302 to the attacker server — the redirect the anonymous plane must
refuse to follow. -}
withRedirectingTarballUpstream :: Port -> (Port -> IO a) -> IO a
withRedirectingTarballUpstream attackerPort = testWithApplication (pure app)
  where
    app :: Application
    app req respond =
        respond $
            if ".tgz" `BS.isSuffixOf` rawPathInfo req
                then responseLBS status302 [(hLocation, attackerLocation)] ""
                else responseLBS status200 [] (encode (crossHostPackument (selfPort req)))
    attackerLocation = encodeUtf8 ("http://127.0.0.1:" <> show attackerPort <> "/thing/-/thing-1.0.0.tgz" :: Text)

{- | A well-formed sha512 SRI (sha512 of the empty string) so the version clears the
integrity gate and reaches the tarball-host policy under test. The bytes are never
hashed against it here — these tests exercise the dist.tarball host gate, not integrity.
-}
validSri :: Text
validSri = "sha512-z4PhNX7vuL3xVChQ1m2AB9Yg5AULVxXcg/SpIdNs6c5H0NE8XYXysP+DGNKHfuwvY7kxvUdBeoGlODJ6+SfaPg=="

{- An in-process upstream whose packument admits a version but names its dist.tarball
on a __same-host port nothing listens on__: the packument fetch succeeds (so the
version reaches the tarball-host gate and is admitted), but opening the artifact
connection fails. The dead port is learned by binding briefly and releasing it before
the live upstream starts, so the artifact open is a clean connection-open failure — the
recoverable phase the serve path must render as the mount's 503. The callback receives
the live packument port. -}
withDeadTarballUpstream :: (Port -> IO a) -> IO a
withDeadTarballUpstream k = do
    deadPort <- testWithApplication (pure noUpstream) pure
    testWithApplication (pure (app deadPort)) k
  where
    -- Always answers the packument (the tarball slot is never reached: its connection
    -- never opens), naming the dead artifact port on the same host as the packument.
    app :: Port -> Application
    app deadPort _req respond =
        respond (responseLBS status200 [] (encode (deadTarballPackument (show deadPort))))

    noUpstream :: Application
    noUpstream _req respond = respond (responseLBS status200 [] "")

-- A single-version packument whose dist.tarball is on the same host (@127.0.0.1@) as
-- the packument — so the secure-default tarball-host policy admits it — at a dead
-- port, so opening the artifact connection fails.
deadTarballPackument :: Text -> Value
deadTarballPackument port =
    object
        [ "name" .= ("thing" :: Text)
        , "dist-tags" .= object ["latest" .= ("1.0.0" :: Text)]
        , "versions"
            .= object
                [ "1.0.0"
                    .= object
                        [ "name" .= ("thing" :: Text)
                        , "version" .= ("1.0.0" :: Text)
                        , "dist"
                            .= object
                                [ "tarball" .= ("http://127.0.0.1:" <> port <> "/thing/-/thing-1.0.0.tgz")
                                , "integrity" .= validSri
                                ]
                        ]
                ]
        , "time" .= object ["1.0.0" .= ("2020-01-01T00:00:00.000Z" :: Text)]
        ]

-- The mount renderer's body for the transient upstream-unavailable 503 the serve path
-- produces when the artifact open fails — the npm @{"error": …}@ shape, distinct from
-- Warp's default 500 page, so the rendered 503 is unambiguous.
upstreamUnavailableBody :: LByteString
upstreamUnavailableBody = "{\"error\":\"the upstream registry was unavailable\"}"

-- ── helpers ───────────────────────────────────────────────────────────────────

-- A @GET /npm/thing/-/thing-1.0.0.tgz@ through the proxy, with no credential.
getTarball :: Application -> IO SResponse
getTarball = runSession (request (setPath defaultRequest "/npm/thing/-/thing-1.0.0.tgz"))

status :: SResponse -> Int
status = statusCode . simpleStatus

optIn :: [Text] -> LoweredHostSet
optIn = lowerCaseHosts . Set.fromList

-- A fixed clock. The fixture version is published in 2020, well before this, so the
-- age quarantine admits it — the gate is satisfied and the focus stays on the
-- tarball-host policy.
fixedNow :: UTCTime
fixedNow = UTCTime (fromGregorian 2026 1 1) 0

-- A permissive rule policy: admit any version older than a week. The fixture version
-- (published 2020) clears it, so every artifact reaches the tarball-host gate.
admitOldEnough :: [PrecededRule]
admitOldEnough = [atDefaultPrecedence (AllowIfOlderThan (7 * nominalDay))]

-- ── handle doubles (the serve path talks to upstreams directly) ───────────────

-- The serve path reaches upstreams through the data-plane managers, never through
-- the 'RegistryClient' handle, so these actions are genuinely unreachable on the path
-- under test — the @error@s are dead branches whose only role is to make a misuse
-- loud. The per-declaration ignore (STYLE.md §10) unblocks them; the bodies stay tiny
-- so its scope is just this double.
{- HLINT ignore fakeRegistry "Avoid restricted function" -}
fakeRegistry :: RegistryClient
fakeRegistry =
    RegistryClient
        { fetchMetadata = const (error "fakeRegistry: fetchMetadata is unused on the serve path")
        , fetchArtifact = \_ _ -> error "fakeRegistry: fetchArtifact is unused on the serve path"
        , publishArtifact = \_ _ _ -> error "fakeRegistry: publishArtifact is unused on the serve path"
        , parsePackageInfo = \_ _ -> Left (ParseError "unused")
        , parseVersionDetails = \_ _ -> Left (ParseError "unused")
        , parseVersionList = const (Left (ParseError "unused"))
        }

fakeCredentials :: CredentialProvider
fakeCredentials = staticProvider AuthToken{authSecret = mkSecret "unused", authExpiresAt = Nothing}
