{-# LANGUAGE RankNTypes #-}

module Ecluse.Server.PipelineSpec (spec) where

import Prelude hiding (get)

import Crypto.Hash (Digest, SHA256, SHA512, hash)
import Data.Aeson (Value (Null, Object, String), eitherDecodeStrict, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteArray.Encoding (Base (Base64), convertToBase)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), addUTCTime, fromGregorian, nominalDay)
import Data.Time.Format.ISO8601 (iso8601Show)
import GHC.IO.Handle (hClose, hDuplicate, hDuplicateTo)
import Katip (Environment (Environment), Namespace (Namespace), closeScribes, initLogEnv)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.HTTP.Types (Header, hAuthorization, methodHead, status200, status304, status404, status500, statusCode, statusMessage)
import Network.HTTP.Types.Header (hETag, hHost, hIfNoneMatch)
import Network.Wai (Application, Request (rawPathInfo, requestHeaders, requestMethod), Response, responseLBS, responseRaw)
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Test (
    SResponse (simpleBody, simpleHeaders, simpleStatus),
    defaultRequest,
    request,
    runSession,
    setPath,
 )
import Test.Hspec
import UnliftIO (bracket)
import UnliftIO.Exception (throwString, try)
import UnliftIO.Temporary (withSystemTempFile)

import Ecluse.Credential (AuthToken (..), CredentialProvider, mkSecret, staticProvider)
import Ecluse.Ecosystem (Ecosystem (Npm))
import Ecluse.Env (Env (envQueue), newEnv, newWorkerHeartbeat)
import Ecluse.Log (LogFormat (JsonLog), newLogEnv)
import Ecluse.Package (HashAlg (SHA512), PackageName, mkPackageName)
import Ecluse.Package.Integrity (defaultMinIntegrity, mkMinIntegrity)
import Ecluse.Queue (
    MirrorJob (jobArtifactUrl, jobMirrorTarget, jobPackage, jobVersion),
    MirrorQueue (enqueue, receive),
    QueueMessage (msgJob),
    newInMemoryQueue,
 )
import Ecluse.Registry (ParseError (..), RegistryClient (..))
import Ecluse.Registry.Npm.Route qualified as Npm
import Ecluse.Registry.Npm.Serve (npmRenderer)
import Ecluse.Rules.Effectful (
    EffectfulConfig (..),
    EffectfulRule (..),
    FailurePolicy (OnUnavailable),
    PrecededEffectfulRule (PrecededEffectfulRule),
    defaultEffectfulConfig,
    newBreaker,
 )
import Ecluse.Rules.Types (
    PrecededRule,
    Rule (AllowIfPublishedBefore, DenyInstallTimeExecution),
    RuleOutcome (Allow, Deny),
    atDefaultPrecedence,
 )
import Ecluse.Security (Limits (maxBodyBytes, maxNestingDepth, maxVersionCount), TarballHostPolicy (AnyAllowlistedHost, SameHostAsPackument), defaultLimits, lowerCaseHosts)
import Ecluse.Server (
    MountBinding (..),
    application,
    mkServerConfig,
 )
import Ecluse.Server.Cache (defaultCacheConfig, newMetadataCache)
import Ecluse.Server.Context (PackumentDeps (..))
import Ecluse.Telemetry (telemetryDisabled)
import Ecluse.Version (Version, mkVersion)

-- ── a fixed clock and the quarantine policy ───────────────────────────────────

-- | A fixed "now" so the age-based admit/deny axis is deterministic under test.
now :: UTCTime
now = UTCTime (fromGregorian 2026 6 20) 0

{- | An ISO-8601 instant @ageDays@ before 'now' (the npm @time@ string), so a
version's survival under the quarantine is controlled purely by its fixture time.
-}
publishedDaysAgo :: Integer -> Text
publishedDaysAgo ageDays =
    toText (iso8601Show (addUTCTime (negate (fromInteger ageDays * nominalDay)) now))

{- | The policy under test: a 7-day publish-age quarantine plus an install-script
deny. A public version is approved iff it is at least 7 days old and declares no
install script; anything else is excluded.
-}
policy :: [PrecededRule]
policy =
    [ atDefaultPrecedence (AllowIfPublishedBefore (7 * nominalDay))
    , atDefaultPrecedence DenyInstallTimeExecution
    ]

-- ── upstream doubles ──────────────────────────────────────────────────────────

{- | An in-process upstream double: it records the @Authorization@ header of every
request it receives (so the credential-authority invariant is assertable), records
the HTTP method of every artifact-slot (tarball) request it receives (so a HEAD that
must not pump the artifact body is assertable), records the @If-None-Match@
conditional validator of every artifact-slot request (so the pass-through
conditional-GET relay is assertable), and serves a fixed response.
-}
data Upstream = Upstream
    { upApp :: Application
    , upSeenAuth :: IORef [Maybe ByteString]
    , upSeenArtifactMethods :: IORef [ByteString]
    , upSeenArtifactValidators :: IORef [Maybe ByteString]
    }

{- | An upstream double serving a fixed packument body with @200@, recording each
request's @Authorization@ header.
-}
servingUpstream :: LByteString -> IO Upstream
servingUpstream body = upstreamRespondingWith (responseLBS status200 [] body)

{- | An upstream double that always answers @500@ — a failed/unavailable upstream, for
the partial-upstream-availability and no-survivors paths.
-}
failingUpstream :: IO Upstream
failingUpstream = upstreamRespondingWith (responseLBS status500 [] "upstream error")

{- | An upstream double that serves a sequence of bodies — its first for the first
request, the next for the next, holding the last once the sequence is exhausted. Lets
a test change what an upstream returns /between/ two requests within the cache TTL, to
assert the served document tracks the latest fetch (the parse is written through, not
read back stale) rather than a cached parse.
-}
mutatingUpstream :: NonEmpty LByteString -> IO Upstream
mutatingUpstream bodies = do
    remaining <- newIORef (toList bodies)
    seen <- newIORef []
    let app req respond = do
            modifyIORef' seen (lookupAuth (requestHeaders req) :)
            body <- atomicModifyIORef' remaining serveNext
            respond (responseLBS status200 [] body)
    mkUpstream seen app
  where
    -- Serve the head and advance, but hold on the last body once exhausted.
    serveNext :: [LByteString] -> ([LByteString], LByteString)
    serveNext (b : rest@(_ : _)) = (rest, b)
    serveNext [b] = ([b], b)
    serveNext [] = ([], "")

-- | Build an upstream double over a fixed response, recording seen auth headers.
upstreamRespondingWith :: Response -> IO Upstream
upstreamRespondingWith response = do
    seen <- newIORef []
    let app :: Application
        app req respond = do
            modifyIORef' seen (lookupAuth (requestHeaders req) :)
            respond response
    mkUpstream seen app

-- The @Authorization@ header value a request carried, if any.
lookupAuth :: [Header] -> Maybe ByteString
lookupAuth headers = snd <$> find ((== hAuthorization) . fst) headers

-- The auth headers an upstream double saw, in arrival order.
seenAuth :: Upstream -> IO [Maybe ByteString]
seenAuth up = reverse <$> readIORef (upSeenAuth up)

-- The HTTP methods an upstream double saw on its artifact-slot (tarball) requests,
-- in arrival order — so a test can assert a HEAD request reached upstream as a HEAD
-- (never the body-pumping GET) and that no full-artifact GET was issued.
seenArtifactMethods :: Upstream -> IO [ByteString]
seenArtifactMethods up = reverse <$> readIORef (upSeenArtifactMethods up)

-- The @If-None-Match@ conditional validators an upstream double saw on its
-- artifact-slot (tarball) requests, in arrival order — so a test can assert the
-- client's validators were relayed onto the upstream artifact request (the
-- pass-through conditional-GET contract). 'Nothing' for a request that carried none.
seenArtifactValidators :: Upstream -> IO [Maybe ByteString]
seenArtifactValidators up = reverse <$> readIORef (upSeenArtifactValidators up)

-- The @If-None-Match@ header value a request carried, if any.
lookupIfNoneMatch :: [Header] -> Maybe ByteString
lookupIfNoneMatch headers = snd <$> find ((== hIfNoneMatch) . fst) headers

{- | Assemble an 'Upstream' over a double that already records each request's
@Authorization@ into the given ref: allocate the artifact-method ref and wrap the
app so a tarball-slot request additionally records its 'requestMethod'. The doubles
keep recording auth themselves (each shapes its own response); this only layers the
artifact-method recording on uniformly.
-}
mkUpstream :: IORef [Maybe ByteString] -> Application -> IO Upstream
mkUpstream seen app = do
    methods <- newIORef []
    validators <- newIORef []
    let recording req respond = do
            when (isTarballPath (rawPathInfo req)) $ do
                modifyIORef' methods (requestMethod req :)
                modifyIORef' validators (lookupIfNoneMatch (requestHeaders req) :)
            app req respond
    pure
        Upstream
            { upApp = recording
            , upSeenAuth = seen
            , upSeenArtifactMethods = methods
            , upSeenArtifactValidators = validators
            }

-- ── tarball upstream doubles (path-aware) ─────────────────────────────────────

{- | Whether a request path is a tarball slot (@\/…\/-\/….tgz@) rather than a
packument. The artifact and packument fetches of a single upstream are distinguished
by this, so one double can answer both.
-}
isTarballPath :: ByteString -> Bool
isTarballPath path = "/-/" `BS.isInfixOf` path && ".tgz" `BS.isSuffixOf` path

{- | The base URL a request reached this in-process double at, recovered from its
@Host@ header (@http:\/\/{host:port}@). The serve path now honours the packument's
@dist.tarball@ rather than reconstructing it, so a double's packument must point its
tarball at __itself__ — and only the @Host@ header names the ephemeral port the test
harness assigned. An absent header (never the case under Warp) falls back to a
loopback host so the helper stays total.
-}
selfBaseUrl :: Request -> Text
selfBaseUrl req =
    case find ((== hHost) . fst) (requestHeaders req) of
        Just (_, hostPort) -> "http://" <> decodeUtf8 hostPort
        Nothing -> "http://127.0.0.1"

{- | A version object whose @dist.tarball@ points at the given base URL's
conventional tarball slot (@{base}\/thing\/-\/thing-{v}.tgz@), with a distinct
integrity and no install script. The serve path fetches this exact URL, so a double
builds it from its own @Host@ ('selfBaseUrl') to address itself.
-}
selfHostedVersion :: Text -> Text -> Value
selfHostedVersion baseUrl version =
    object
        [ "name" .= ("thing" :: Text)
        , "version" .= version
        , "dist"
            .= object
                [ "tarball" .= (baseUrl <> "/thing/-/thing-" <> version <> ".tgz")
                , "integrity" .= sriFor version
                , "shasum" .= validShasum
                ]
        , "_unmodeled" .= ("kept" :: Text)
        ]

{- | An admitting public packument (single old-enough version @v@) whose
@dist.tarball@ points at @baseUrl@ — the self-hosting form the artifact path fetches.
-}
selfHostedAdmitting :: Text -> Text -> Value
selfHostedAdmitting baseUrl v =
    packument [(v, selfHostedVersion baseUrl v)] v [(v, publishedDaysAgo 30)]

{- | A path-aware upstream double: it answers a tarball-slot path with @200@ and the
given artifact bytes, and any other path (the packument fetch) with @200@ and a
packument whose @dist.tarball@ for version @v@ points back at this double (so the
serve path's honour-the-URL fetch returns here). Records each request's
@Authorization@ header. The single double thus serves both fetches the public
artifact path consults — the gating packument and the artifact itself.
-}
artifactUpstream :: Text -> LByteString -> IO Upstream
artifactUpstream version tarballBody = do
    seen <- newIORef []
    let app :: Application
        app req respond = do
            modifyIORef' seen (lookupAuth (requestHeaders req) :)
            respond $
                if isTarballPath (rawPathInfo req)
                    then responseLBS status200 [] tarballBody
                    else responseLBS status200 [] (encodePackument (selfHostedAdmitting (selfBaseUrl req) version))
    mkUpstream seen app

{- | A path-aware upstream double serving a __given__ packument body verbatim (its
@dist.tarball@ already addressing this double via 'selfBaseUrl'), and the given
artifact bytes on a tarball-slot path. For tests that shape the gating packument
themselves (a too-new version, a bad coordinate) while still honouring a
self-referential tarball URL.
-}
artifactUpstreamServing :: (Text -> LByteString) -> LByteString -> IO Upstream
artifactUpstreamServing packumentFor tarballBody = do
    seen <- newIORef []
    let app :: Application
        app req respond = do
            modifyIORef' seen (lookupAuth (requestHeaders req) :)
            respond $
                if isTarballPath (rawPathInfo req)
                    then responseLBS status200 [] tarballBody
                    else responseLBS status200 [] (packumentFor (selfBaseUrl req))
    mkUpstream seen app

{- | A private upstream double that has the artifact: it answers the packument fetch
with a single-version packument whose @dist.tarball@ points back at this double
(@200@), and a tarball-slot path with @200@ and the given bytes (a __private hit__).
The serve path honours the private @dist.tarball@, so the double must serve a
packument naming itself.
-}
privateArtifactHit :: Text -> LByteString -> IO Upstream
privateArtifactHit version = privateArtifactHitWith version []

{- | A private upstream double that has the artifact and tags it with an upstream
header (a @Content-Type@): the packument fetch is a self-referential single-version
packument, a tarball-slot path is answered @200@ with the bytes and that header. Lets
a test assert the relay forwards the artifact's own content headers through.
-}
privateArtifactHitWithHeader :: ByteString -> ByteString -> Text -> LByteString -> IO Upstream
privateArtifactHitWithHeader headerName headerValue version =
    privateArtifactHitWith version [(CI.mk headerName, headerValue)]

{- | The shared private-hit double: the packument fetch returns a self-hosted
single-version packument (@200@), a tarball-slot path returns @200@ with the given
artifact bytes and extra headers. A private hit thus honours the private
@dist.tarball@ — the double names itself as the tarball host.
-}
privateArtifactHitWith :: Text -> [Header] -> LByteString -> IO Upstream
privateArtifactHitWith version extraHeaders tarballBody = do
    seen <- newIORef []
    let app :: Application
        app req respond = do
            modifyIORef' seen (lookupAuth (requestHeaders req) :)
            respond $
                if isTarballPath (rawPathInfo req)
                    then responseLBS status200 extraHeaders tarballBody
                    else responseLBS status200 [] (encodePackument (selfHostedAdmitting (selfBaseUrl req) version))
    mkUpstream seen app

{- | A private upstream double that has a __hashless__ artifact: the packument fetch
returns a single-version packument whose @dist@ carries a @tarball@ pointing back at
this double but neither @integrity@ nor @shasum@ (@200@), and a tarball-slot path
@200@ with the given bytes. A private hit on a version with no integrity digest — the
trusted private path is exempt from the integrity-presence policy, so this still
streams through.
-}
privateArtifactHitHashless :: Text -> LByteString -> IO Upstream
privateArtifactHitHashless version tarballBody = do
    seen <- newIORef []
    let app :: Application
        app req respond = do
            modifyIORef' seen (lookupAuth (requestHeaders req) :)
            respond $
                if isTarballPath (rawPathInfo req)
                    then responseLBS status200 [] tarballBody
                    else
                        responseLBS status200 [] $
                            encodePackument
                                (packument [(version, selfHostedHashless (selfBaseUrl req) version)] version [(version, publishedDaysAgo 1)])
    mkUpstream seen app

{- | A private upstream double that resolves the packument but does __not__ hold the
artifact bytes: the packument fetch is a self-referential single-version packument
(@200@), but a tarball-slot path is a @404@ miss — so the serve path's honour fetch
to the private tarball location misses and falls through to the public origin.
-}
privateArtifactMiss :: IO Upstream
privateArtifactMiss = do
    seen <- newIORef []
    let app :: Application
        app req respond = do
            modifyIORef' seen (lookupAuth (requestHeaders req) :)
            respond $
                if isTarballPath (rawPathInfo req)
                    then responseLBS status404 [] "not found"
                    else responseLBS status200 [] (encodePackument (selfHostedAdmitting (selfBaseUrl req) "1.0.0"))
    mkUpstream seen app

{- | A private upstream double that begins serving the artifact then fails
__mid-stream__: a tarball-slot path is answered (over the raw connection) with a
@200@ that __promises far more than it delivers__ — a large @Content-Length@ but
only a little body — and then returns, so Warp closes the socket and the proxy's
read of the artifact hits EOF short of the promised length and fails at once. Any
other path is a @404@. The handler never throws (so @testWithApplication@ does not
surface a server-side error), and the immediate close keeps the short read from
stalling on a read timeout. It exercises the committed-stream case — once the @200@
is on the wire the serve path must fail internally, not fall through to the public
origin.
-}
privateArtifactMidStreamFailure :: IO Upstream
privateArtifactMidStreamFailure = do
    seen <- newIORef []
    let app :: Application
        app req respond = do
            modifyIORef' seen (lookupAuth (requestHeaders req) :)
            if isTarballPath (rawPathInfo req)
                then respond (responseRaw truncatedArtifact (responseLBS status500 [] "raw unsupported"))
                else respond (responseLBS status200 [] (encodePackument (selfHostedAdmitting (selfBaseUrl req) "1.0.0")))
    mkUpstream seen app
  where
    -- Write a 200 declaring a 1 MiB body, send only a little, then return: Warp
    -- closes the raw socket, so the proxy reads EOF short of the Content-Length and
    -- fails immediately — no exception thrown here, no timeout waited on.
    truncatedArtifact :: IO ByteString -> (ByteString -> IO ()) -> IO ()
    truncatedArtifact _recv send = do
        send "HTTP/1.1 200 OK\r\nContent-Length: 1048576\r\n\r\n"
        send (BS.replicate 1024 0x7a)

-- ── packument fixtures ────────────────────────────────────────────────────────

{- | A minimal npm packument body for @thing@ with the given version objects, a
@dist-tags.latest@, a @time@ map (built from the same versions), and an unmodeled
top-level key — so a test can assert the unmodeled key is relayed unchanged.
-}
packument :: [(Text, Value)] -> Text -> [(Text, Text)] -> Value
packument versions latest times =
    object
        [ "name" .= ("thing" :: Text)
        , "dist-tags" .= object ["latest" .= latest]
        , "versions" .= object [(Key.fromText v, obj) | (v, obj) <- versions]
        , "time" .= object (("created" .= publishedDaysAgo 400) : [(Key.fromText v, String t) | (v, t) <- times])
        , "_id" .= ("thing" :: Text) -- an unmodeled top-level key
        ]

{- | A packument like 'packument' but self-reporting a /different/ top-level @name@,
to exercise the route-name validation. The route under test is always @\/npm\/thing@,
so a packument named anything but @thing@ is validated out (an untrusted, misreporting
origin) and its contribution dropped.
-}
packumentNamed :: Text -> [(Text, Value)] -> Text -> [(Text, Text)] -> Value
packumentNamed nm versions latest times =
    case packument versions latest times of
        Object o -> Object (KeyMap.insert "name" (String nm) o)
        v -> v

{- | A version object with a @dist@ carrying a tarball URL and an @integrity@, plus
an unmodeled per-version key. @scripts@ flags an install script when asked, so the
install-script deny can be exercised.
-}
versionObject :: Text -> Text -> Bool -> Value
versionObject version integrity hasInstall =
    object
        ( [ "name" .= ("thing" :: Text)
          , "version" .= version
          , "dist"
                .= object
                    [ "tarball" .= ("https://upstream.example/thing/-/thing-" <> version <> ".tgz")
                    , "integrity" .= integrity
                    , "shasum" .= validShasum
                    ]
          , "_unmodeled" .= ("kept" :: Text) -- a per-version unmodeled key
          ]
            <> ["scripts" .= object ["postinstall" .= ("node build.js" :: Text)] | hasInstall]
        )

-- A plain (no-install-script) version object with a distinct integrity.
plainVersion :: Text -> Value
plainVersion version = versionObject version (sriFor version) False

{- | A well-formed sha512 (resp. sha256) SRI derived from a label, so the projection's
digest validation keeps it. These serve tests exercise admission, the merge, and
dist-tag reconciliation — not digest realism — so a deterministic well-formed digest
per label stands in for a real one while staying 'mkHash'-constructible.
-}
sriFor, sri256For :: Text -> Text
sriFor label =
    "sha512-" <> decodeUtf8 (convertToBase Base64 (hash (encodeUtf8 label :: ByteString) :: Digest SHA512) :: ByteString)
sri256For label =
    "sha256-" <> decodeUtf8 (convertToBase Base64 (hash (encodeUtf8 label :: ByteString) :: Digest SHA256) :: ByteString)

-- | A well-formed 40-hex SHA-1 shasum (sha1 of the empty string) for the dist fixtures.
validShasum :: Text
validShasum = "da39a3ee5e6b4b0d3255bfef95601890afd80709"

{- | A version object carrying __only a legacy SHA-1 shasum__ (a @dist@ with a tarball
and @shasum@ but no @integrity@ SRI), so it projects to an artifact whose strongest
digest is SHA-1 — below the default SHA-256 floor. The integrity-floor admission policy
refuses such a version from a /public/ upstream; a /private/ one is exempt.
-}
shasumOnlyVersion :: Text -> Value
shasumOnlyVersion version =
    object
        [ "name" .= ("thing" :: Text)
        , "version" .= version
        , "dist"
            .= object
                [ "tarball" .= ("https://upstream.example/thing/-/thing-" <> version <> ".tgz")
                , "shasum" .= validShasum
                ]
        , "_unmodeled" .= ("kept" :: Text)
        ]

{- | A version object carrying __no integrity digest at all__: a @dist@ with a
@tarball@ but neither @integrity@ nor @shasum@ (both optional on the wire), so it
projects to an artifact with empty @artHashes@. The integrity-presence admission
policy refuses such a version from a public upstream.
-}
hashlessVersion :: Text -> Value
hashlessVersion version =
    object
        [ "name" .= ("thing" :: Text)
        , "version" .= version
        , "dist" .= object ["tarball" .= ("https://upstream.example/thing/-/thing-" <> version <> ".tgz")]
        , "_unmodeled" .= ("kept" :: Text)
        ]

{- | A version object whose @dist@ carries __empty-string__ @integrity@ and @shasum@ (a
present-but-content-empty digest pair). The projection normalises an empty digest to
absent, so this projects to an artifact with empty @artHashes@ — identical to
'hashlessVersion' — and the integrity-presence admission policy refuses it from a public
upstream (classified 'NoIntegrity', not 'BelowFloor').
-}
emptyDigestVersion :: Text -> Value
emptyDigestVersion version =
    object
        [ "name" .= ("thing" :: Text)
        , "version" .= version
        , "dist"
            .= object
                [ "tarball" .= ("https://upstream.example/thing/-/thing-" <> version <> ".tgz")
                , "integrity" .= ("" :: Text)
                , "shasum" .= ("" :: Text)
                ]
        , "_unmodeled" .= ("kept" :: Text)
        ]

{- | A hashless version object whose @dist.tarball@ points at @baseUrl@ — the
self-hosting form the artifact path fetches, but with neither @integrity@ nor
@shasum@. The artifact-gate refusal must fire before this URL is ever fetched.
-}
selfHostedHashless :: Text -> Text -> Value
selfHostedHashless baseUrl version =
    object
        [ "name" .= ("thing" :: Text)
        , "version" .= version
        , "dist" .= object ["tarball" .= (baseUrl <> "/thing/-/thing-" <> version <> ".tgz")]
        , "_unmodeled" .= ("kept" :: Text)
        ]

{- | A self-hosting version object carrying __only a legacy SHA-1 shasum__ (no SRI
@integrity@), so its strongest digest is below the default floor. The artifact-gate
refusal (@BelowIntegrityFloor@) must fire before its @dist.tarball@ is ever fetched.
-}
selfHostedShasumOnly :: Text -> Text -> Value
selfHostedShasumOnly baseUrl version =
    object
        [ "name" .= ("thing" :: Text)
        , "version" .= version
        , "dist"
            .= object
                [ "tarball" .= (baseUrl <> "/thing/-/thing-" <> version <> ".tgz")
                , "shasum" .= validShasum
                ]
        , "_unmodeled" .= ("kept" :: Text)
        ]

{- | A self-hosting version object whose @dist@ carries __empty-string__ @integrity@ and
@shasum@, so it projects to no digest at all. The artifact-gate refusal ('MissingIntegrity')
must fire before its @dist.tarball@ is ever fetched.
-}
selfHostedEmptyDigest :: Text -> Text -> Value
selfHostedEmptyDigest baseUrl version =
    object
        [ "name" .= ("thing" :: Text)
        , "version" .= version
        , "dist"
            .= object
                [ "tarball" .= (baseUrl <> "/thing/-/thing-" <> version <> ".tgz")
                , "integrity" .= ("" :: Text)
                , "shasum" .= ("" :: Text)
                ]
        , "_unmodeled" .= ("kept" :: Text)
        ]

-- ── env + proxy assembly ──────────────────────────────────────────────────────

{- | A registry-handle double whose fields are never invoked (the pipeline talks to
upstreams directly via the npm client over the shared 'Manager', not the handle).
-}
fakeRegistry :: RegistryClient
fakeRegistry =
    RegistryClient
        { fetchMetadata = const (refuse "fetchMetadata")
        , fetchArtifact = \_ _ -> refuse "fetchArtifact"
        , publishArtifact = \_ _ _ -> refuse "publishArtifact"
        , parsePackageInfo = \_ _ -> Left (ParseError "unused")
        , parseVersionDetails = \_ _ -> Left (ParseError "unused")
        , parseVersionList = const (Left (ParseError "unused"))
        }
  where
    refuse :: Text -> IO a
    refuse field = throwString (toString ("fakeRegistry: the pipeline must not use the handle field " <> field))

fakeCredentials :: CredentialProvider
fakeCredentials = staticProvider AuthToken{authSecret = mkSecret "unused", authExpiresAt = Nothing}

{- | A fresh 'Env' over handle doubles and a real (no-TLS) manager for the in-process
upstream doubles, carrying the given mirror queue (the in-memory double, or one
rigged to fail for the best-effort-enqueue assertion).
-}
newTestEnvWithQueue :: MirrorQueue -> Manager -> IO Env
newTestEnvWithQueue queue manager = do
    metadataCache <- newMetadataCache defaultCacheConfig
    logEnv <- initLogEnv (Namespace ["ecluse"]) (Environment "test")
    heartbeat <- newWorkerHeartbeat
    newEnv fakeRegistry queue fakeCredentials manager manager metadataCache logEnv telemetryDisabled heartbeat

{- | The packument-serve dependencies pointing at two in-process upstream ports,
with the given inbound edge token (usually 'Nothing').

The in-process upstream doubles bind loopback, so @127.0.0.1@ is opted in to the
internal-range block for the __public__ leg's honoured-tarball gate — the unit-test
analogue of an operator deliberately opting an internal /public/ host in. The
__private__ leg needs no opt-in: it gates as the trusted origin, exempt from the
internal-range block (the serve-path mirror of
'Ecluse.Security.Egress.newTrustedTlsManager'); a test empties this set to assert that
exemption directly. The default tarball-host policy is the secure 'SameHostAsPackument';
a test overrides it where it exercises the cross-host relaxation.
-}
deps :: Int -> Int -> Maybe Text -> PackumentDeps
deps privatePort publicPort inbound =
    PackumentDeps
        { pdPrivateBaseUrl = localhost privatePort
        , pdPublicBaseUrl = localhost publicPort
        , pdMountBaseUrl = "https://proxy.test"
        , pdMirrorTarget = "https://mirror.test"
        , pdRules = policy
        , pdEffectfulRules = []
        , pdTarballHostPolicy = SameHostAsPackument
        , pdAllowedInternalHosts = lowerCaseHosts (Set.singleton "127.0.0.1")
        , pdLimits = defaultLimits
        , pdInboundToken = mkSecret <$> inbound
        , pdNow = pure now
        , pdHelp = Nothing
        , pdMinIntegrity = defaultMinIntegrity
        }

{- | The packument-serve dependencies as 'deps', but with the given effectful rules
wired into the policy, so a test can drive the effectful tier end to end through the
pipeline.
-}
depsWith :: [PrecededEffectfulRule] -> Int -> Int -> PackumentDeps
depsWith effectful privatePort publicPort =
    (deps privatePort publicPort Nothing){pdEffectfulRules = effectful}

localhost :: Int -> Text
localhost port = "http://127.0.0.1:" <> show port

{- | Run an assertion against a proxy whose two upstream origins are the given
in-process doubles, with access to the proxy's own 'Env' (so a test can drain the
mirror queue) and over the given mirror queue. The upstream apps are hosted on
ephemeral ports via Warp; the proxy is driven in-process through a WAI session (no
proxy socket).
-}
withProxyEnvQueue ::
    MirrorQueue ->
    Upstream ->
    Upstream ->
    Maybe Text ->
    -- The continuation sees the proxy application, its 'Env' (to drain the queue),
    -- and the public upstream's ephemeral port (to assert an enqueued artifact URL).
    (forall a. (Application -> Env -> Int -> IO a) -> IO a)
withProxyEnvQueue queue privateUp publicUp inbound =
    withProxyEnvQueueDeps queue privateUp publicUp inbound id

{- | Like 'withProxyEnvQueue', but with the mount's 'PackumentDeps' passed through
the given transform first — so a test can break one origin's base URL (an unformable
upstream URL) without a new harness.
-}
withProxyEnvQueueDeps ::
    MirrorQueue ->
    Upstream ->
    Upstream ->
    Maybe Text ->
    (PackumentDeps -> PackumentDeps) ->
    (forall a. (Application -> Env -> Int -> IO a) -> IO a)
withProxyEnvQueueDeps queue privateUp publicUp inbound tweakDeps k =
    testWithApplication (pure (upApp privateUp)) $ \privatePort ->
        testWithApplication (pure (upApp publicUp)) $ \publicPort -> do
            manager <- newManager defaultManagerSettings
            env <- newTestEnvWithQueue queue manager
            let cfg =
                    mkServerConfig
                        [ MountBinding
                            { bindingPrefix = "npm" :| []
                            , bindingClassifier = Npm.classify
                            , bindingPackumentDeps = Just (tweakDeps (deps privatePort publicPort inbound))
                            , bindingRenderer = npmRenderer
                            }
                        ]
            k (application cfg env) env publicPort

{- | Run an assertion against a proxy over the two upstream doubles and the proxy's
own 'Env' (over a vanilla in-memory queue), so a test can drain the enqueued mirror
jobs.
-}
withProxyEnv ::
    Upstream ->
    Upstream ->
    Maybe Text ->
    (forall a. (Application -> Env -> IO a) -> IO a)
withProxyEnv privateUp publicUp inbound k = do
    queue <- newInMemoryQueue
    withProxyEnvQueue queue privateUp publicUp inbound (\app env _port -> k app env)

{- | Run an assertion against a proxy whose two upstream origins are the given
in-process doubles. The upstream apps are hosted on ephemeral ports via Warp; the
proxy is driven in-process through a WAI session (no proxy socket).
-}
withProxy ::
    Upstream ->
    Upstream ->
    Maybe Text ->
    (forall a. (Application -> IO a) -> IO a)
withProxy privateUp publicUp inbound k =
    withProxyEnv privateUp publicUp inbound (\app _env -> k app)

{- | Run an assertion against a proxy whose npm mount carries the given effectful
rules, so a request flows through both rule tiers. The two upstream doubles are
hosted on ephemeral ports as elsewhere; the effectful rules see the public version.
-}
withProxyEffectful ::
    [PrecededEffectfulRule] ->
    Upstream ->
    Upstream ->
    (forall a. (Application -> IO a) -> IO a)
withProxyEffectful effectful privateUp publicUp k = do
    queue <- newInMemoryQueue
    testWithApplication (pure (upApp privateUp)) $ \privatePort ->
        testWithApplication (pure (upApp publicUp)) $ \publicPort -> do
            manager <- newManager defaultManagerSettings
            env <- newTestEnvWithQueue queue manager
            let cfg =
                    mkServerConfig
                        [ MountBinding
                            { bindingPrefix = "npm" :| []
                            , bindingClassifier = Npm.classify
                            , bindingPackumentDeps = Just (depsWith effectful privatePort publicPort)
                            , bindingRenderer = npmRenderer
                            }
                        ]
            k (application cfg env)

-- ── driving the proxy ─────────────────────────────────────────────────────────

-- | A @GET /npm/thing@ request carrying the given (optional) bearer credential.
getThing :: Maybe Text -> Application -> IO SResponse
getThing bearer = runSession (request (setPath baseRequest "/npm/thing"))
  where
    baseRequest =
        defaultRequest{requestHeaders = maybe [] (\t -> [(hAuthorization, "Bearer " <> encodeUtf8 t)]) bearer}

{- | A @GET /npm/thing@ with no credential and the given extra request headers
(e.g. a conditional @If-None-Match@).
-}
getThingWith :: [Header] -> Application -> IO SResponse
getThingWith extra =
    runSession (request (setPath defaultRequest{requestHeaders = extra} "/npm/thing"))

{- | A @HEAD /npm/thing@ request carrying the given (optional) bearer credential — the
same packument coordinate as 'getThing', issued as a HEAD so the serve path must answer
with the GET's status and headers but no body.
-}
headThing :: Maybe Text -> Application -> IO SResponse
headThing bearer =
    runSession (request (setPath baseRequest "/npm/thing"){requestMethod = methodHead})
  where
    baseRequest =
        defaultRequest{requestHeaders = maybe [] (\t -> [(hAuthorization, "Bearer " <> encodeUtf8 t)]) bearer}

{- | A @HEAD /npm/thing@ with no credential and the given extra request headers (e.g. a
conditional @If-None-Match@), to drive the own-ETag conditional on the HEAD path.
-}
headThingWith :: [Header] -> Application -> IO SResponse
headThingWith extra =
    runSession (request (setPath defaultRequest{requestHeaders = extra} "/npm/thing"){requestMethod = methodHead})

{- | A @GET /npm/thing/-/thing-{version}.tgz@ artifact request carrying the given
(optional) bearer credential — the tarball path for @thing@ at one version.
-}
getTarball :: Text -> Maybe Text -> Application -> IO SResponse
getTarball version bearer =
    runSession (request (setPath baseRequest path))
  where
    path = "/npm/thing/-/thing-" <> encodeUtf8 version <> ".tgz"
    baseRequest =
        defaultRequest{requestHeaders = maybe [] (\t -> [(hAuthorization, "Bearer " <> encodeUtf8 t)]) bearer}

{- | A @GET /npm/thing/-/thing-{version}.tgz@ artifact request with no credential
and the given extra request headers (e.g. a conditional @If-None-Match@), to drive
the pass-through conditional-GET relay.
-}
getTarballWith :: Text -> [Header] -> Application -> IO SResponse
getTarballWith version extra =
    runSession (request (setPath defaultRequest{requestHeaders = extra} path))
  where
    path = "/npm/thing/-/thing-" <> encodeUtf8 version <> ".tgz"

{- | A @HEAD /npm/thing/-/thing-{version}.tgz@ artifact request carrying the given
(optional) bearer credential — the same tarball coordinate as 'getTarball', issued
as a HEAD so the serve path must answer without pumping the full artifact body.
-}
headTarball :: Text -> Maybe Text -> Application -> IO SResponse
headTarball version bearer =
    runSession (request (setPath baseRequest path){requestMethod = methodHead})
  where
    path = "/npm/thing/-/thing-" <> encodeUtf8 version <> ".tgz"
    baseRequest =
        defaultRequest{requestHeaders = maybe [] (\t -> [(hAuthorization, "Bearer " <> encodeUtf8 t)]) bearer}

-- | Drain every mirror job currently enqueued on the proxy's queue, in FIFO order.
drainJobs :: Env -> IO [MirrorJob]
drainJobs env = map msgJob <$> receive (envQueue env)

-- The decoded JSON body of a proxy response, or 'Null' if it did not decode (a
-- non-JSON body then surfaces as a plain assertion mismatch, not a crash).
decodedBody :: SResponse -> Value
decodedBody resp = fromRight Null (eitherDecodeStrict (LBS.toStrict (simpleBody resp)))

-- The version keys present in a served packument body.
servedVersions :: SResponse -> [Text]
servedVersions resp = case decodedBody resp of
    Object o -> case KeyMap.lookup "versions" o of
        Just (Object vs) -> sort (map Key.toText (KeyMap.keys vs))
        _ -> []
    _ -> []

-- The value at a top-level key in the served body (for relayed unmodeled keys).
topLevel :: Text -> SResponse -> Maybe Value
topLevel key resp = case decodedBody resp of
    Object o -> KeyMap.lookup (Key.fromText key) o
    _ -> Nothing

-- A version object's @dist.tarball@ in the served body.
servedTarball :: Text -> SResponse -> Maybe Text
servedTarball version resp = do
    Object o <- Just (decodedBody resp)
    Object vs <- KeyMap.lookup "versions" o
    Object vo <- KeyMap.lookup (Key.fromText version) vs
    Object dist <- KeyMap.lookup "dist" vo
    String tarball <- KeyMap.lookup "tarball" dist
    pure tarball

-- The served @dist-tags.latest@ target.
servedLatest :: SResponse -> Maybe Text
servedLatest resp = do
    Object o <- Just (decodedBody resp)
    Object tags <- KeyMap.lookup "dist-tags" o
    String latest <- KeyMap.lookup "latest" tags
    pure latest

status :: SResponse -> Int
status = statusCode . simpleStatus

{- The HTTP reason phrase of a response (e.g. @"Forbidden"@). Reading it forces the
status' message, which the @mkStatus@-built serve statuses carry as a lazy field —
so an assertion over it exercises the per-status reason mapping the serve path
threads through, not just the numeric code. -}
reason :: SResponse -> ByteString
reason = statusMessage . simpleStatus

header :: ByteString -> SResponse -> Maybe ByteString
header name resp = snd <$> find ((== CI.mk name) . fst) (simpleHeaders resp)

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
    tarballSpec
    effectfulSpec
    boundsSpec
    boundsLogSpec

-- ── multi-upstream merge ──────────────────────────────────────────────────────

mergeSpec :: Spec
mergeSpec = describe "multi-upstream merge (not fallback)" $ do
    it "serves the union of trusted-private and gated-public versions" $ do
        -- Private holds 1.0.0 (a fresh version it trusts unfiltered); public holds
        -- an old 1.0.0 (collision) and an old 2.0.0. All public versions are old
        -- enough to clear the quarantine, so the union is {1.0.0, 2.0.0}.
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
        -- Public 2.0.0 is too new (3 days < 7-day quarantine) → excluded; public
        -- 1.5.0 is old enough → kept; private 3.0.0 is trusted regardless of age.
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
            -- 2.0.0 (too new) is filtered out; 1.5.0 (public, old) and 3.0.0
            -- (private, trusted) survive.
            servedVersions resp `shouldBe` ["1.5.0", "3.0.0"]
            -- the denied, newer public latest (2.0.0) must not become the merged
            -- latest: it resolves to the trusted private 3.0.0.
            servedLatest resp `shouldBe` Just "3.0.0"

    it "denies a public install-script version while admitting a private one of the same key" $ do
        -- Both upstreams carry 1.0.0; the public copy declares an install script
        -- (denied), but the private copy is trusted unfiltered — so 1.0.0 survives
        -- (private wins the collision).
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
            -- The served 1.0.0 is the private (trusted) copy: its integrity wins.
            servedIntegrity "1.0.0" resp `shouldBe` Just (sriFor "1.0.0")

    it "filters a hashless public version out of the served listing (integrity-presence policy)" $ do
        -- Public 2.0.0 carries no integrity digest (neither integrity nor shasum)
        -- → inadmissible, filtered from the served packument so a client never sees
        -- a version it could not fetch. Public 1.0.0 has a digest → kept.
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
        -- Public 2.0.0 carries `integrity:""`/`shasum:""` — present-but-content-empty
        -- digests the projection normalises to none, so the integrity-presence policy
        -- drops it from the served packument exactly as a truly hashless one. 1.0.0 has a
        -- real digest → kept. Proves the projection fix composes with the existing gate
        -- (no parallel check); without it the empty strings would project to degenerate
        -- empty Hashes and 2.0.0 would leak into the listing.
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

    it "lists a hashless version from the trusted private upstream (the private path is exempt)" $ do
        -- The private upstream is trusted: its versions enter unfiltered, so a
        -- hashless private 1.0.0 is still served in the listing. The
        -- integrity-presence policy applies to public versions only.
        privateUp <-
            servingUpstream
                (encodePackument (privatePackumentWith [("1.0.0", hashlessVersion "1.0.0")] "1.0.0"))
        publicUp <- failingUpstream
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["1.0.0"]

    it "rejects a public version whose only digest is below the floor (SHA-1 shasum)" $ do
        -- Public 2.0.0 carries only a legacy SHA-1 shasum (below the default SHA-256
        -- floor) → inadmissible, filtered from the served packument. (This is also the
        -- matrix's forbidden case: a public SHA-1-only version is rejected regardless of
        -- what a private upstream carries.) Public 1.0.0 has a sha512 digest → kept.
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
        -- A public version whose integrity SRI is sha256 clears the default SHA-256
        -- floor and is served (alongside its legacy shasum, which is irrelevant once a
        -- floor-clearing digest is present).
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

    it "lists a SHA-1-only version from the trusted private upstream (the floor is public-only)" $ do
        -- The integrity floor applies to public versions only: a trusted private version
        -- carrying just a legacy SHA-1 shasum still enters the merge and is served.
        privateUp <-
            servingUpstream
                (encodePackument (privatePackumentWith [("1.0.0", shasumOnlyVersion "1.0.0")] "1.0.0"))
        publicUp <- failingUpstream
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["1.0.0"]

    it "rejects a public SHA-256 version when the floor is raised to SHA-512 (the floor value is wired)" $ do
        -- The floor VALUE flows from config to the gate, not just its presence: under
        -- the default SHA-256 floor this sha256 version is admitted, but with the floor
        -- raised to SHA-512 it no longer clears it and is filtered, leaving only the
        -- trusted private 3.0.0.
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
        -- The architect's named combined case: a public copy of 1.0.0 carries only a
        -- legacy SHA-1 shasum (below the floor) and a private copy of the SAME version
        -- carries a strong SHA-256. The public copy is refused by the integrity floor
        -- and never reaches the merge, so the served 1.0.0 is the trusted private copy
        -- and its SHA-256 integrity — the weak public digest never enters the served
        -- packument.
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
        -- Same version key, differing integrity across upstreams: the private copy
        -- wins the served document. The divergence is recorded in the MergePlan
        -- (asserted directly in Ecluse.Package.MergeSpec); here we pin that the
        -- served bytes are the trusted copy's, never the public one's.
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
        -- Public latest points at 2.0.0 (3 days old → denied by the quarantine);
        -- 1.0.0 (30 days) survives. With the private origin down, the served document
        -- is public-only — its latest must repoint to the surviving 1.0.0, never
        -- remain the withheld 2.0.0. This pins that the merge reconciles latest over
        -- the FILTERED public set, not the raw one.
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

-- ── credential authority (the non-negotiable invariant) ───────────────────────

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
            -- The private origin received the client's bearer credential verbatim.
            privAuth `shouldBe` [Just "Bearer client-secret-token"]
            -- The public origin was queried ANONYMOUSLY — the internal token never left
            -- for the public registry. This is the load-bearing security assertion.
            pubAuth `shouldBe` [Nothing]

-- ── private origin is the per-client authority (uncached across clients) ───────

privateAuthoritySpec :: Spec
privateAuthoritySpec = describe "private origin is the per-client authority (not cached across clients)" $
    it "re-consults the private upstream per client within the TTL — each client's token reaches it" $ do
        -- Two requests to the SAME proxy with DIFFERENT client bearer tokens, well
        -- within the 60s metadata-cache TTL. The private upstream is the authority for
        -- who may read what, so its metadata must not be shared across clients: each
        -- request must re-consult it with that client's OWN forwarded token. Were the
        -- private origin cached (keyed by base URL, with no credential dimension), the
        -- second request would be a hit and the upstream would see only tokenA — client
        -- B would be served A's private document, its token never validated upstream.
        privateUp <- servingUpstream (encodePackument (privatePackument [("1.0.0", plainVersion "1.0.0")] "1.0.0"))
        publicUp <-
            servingUpstream
                (encodePackument (packument [("2.0.0", plainVersion "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 30)]))
        withProxy privateUp publicUp Nothing $ \app -> do
            _ <- getThing (Just "tokenA") app
            _ <- getThing (Just "tokenB") app
            privAuth <- seenAuth privateUp
            pubAuth <- seenAuth publicUp
            -- The private upstream saw BOTH client tokens, in order — it was
            -- re-authorized per client and never served a shared cached entry.
            privAuth `shouldBe` [Just "Bearer tokenA", Just "Bearer tokenB"]
            -- The anonymous public origin IS cached: it was hit once, anonymously, and the
            -- second request collapsed onto the cached entry (public caching retained).
            pubAuth `shouldBe` [Nothing]

-- ── partial-upstream availability ─────────────────────────────────────────────

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
        -- The private upstream returns a 200 with a non-JSON body, so it does not
        -- decode into a packument: the contribution degrades to nothing (the
        -- parse-failure path), exactly as a bound breach does, and the public set is
        -- served. Distinct from a 5xx outage — here the body itself is malformed.
        privateUp <- servingUpstream "this is not json at all"
        publicUp <-
            servingUpstream
                (encodePackument (packument [("2.0.0", plainVersion "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 30)]))
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["2.0.0"]

    it "degrades a private leg that decodes but does not project to a packument" $ do
        -- The private body is valid JSON but not a packument shape (no name/versions),
        -- so it decodes to a Value yet fails projection — the inner decodeFailure path,
        -- distinct from an outright JSON parse error. It degrades and the public set
        -- is served.
        privateUp <- servingUpstream "[1, 2, 3]"
        publicUp <-
            servingUpstream
                (encodePackument (packument [("2.0.0", plainVersion "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 30)]))
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["2.0.0"]

    it "drops a private leg that self-reports a different package, serving the public set (200)" $ do
        -- The private upstream answers, but its packument's top-level `name` is `other`,
        -- not the requested `thing`: it is validated out (untrusted for this request) and
        -- dropped, and the public set still serves 200. A single misreporting upstream
        -- must never deny a package another upstream serves.
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
            -- The served name is the genuine `thing` the surviving (public) origin reported.
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

-- ── cache read-through coherence ──────────────────────────────────────────────

cacheSpec :: Spec
cacheSpec = describe "metadata cache (read-through coherence)" $ do
    it "reuses the cached document within the TTL — a second request does not re-fetch" $ do
        -- The public upstream would gain 2.0.0 on a second fetch, but within the 60s
        -- TTL the packument path reads through: the second request is served from the
        -- cached entry, so the upstream is hit exactly once and the served set stays
        -- {1.0.0}. The mutating double is the witness — 2.0.0 appears only if a second
        -- fetch occurred, so its absence proves the single-flight reuse. The private
        -- origin is absent, so the served set is exactly the cached public versions.
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
            -- Read-through reuse: the cached v1 is served, not the upstream's later v2.
            servedVersions secondResp `shouldBe` ["1.0.0"]
            -- Only the first request reached the public upstream; the second collapsed
            -- onto the cached entry (the regained single-flight benefit).
            seenAuth publicUp `shouldReturn` [Nothing]

    it "serves a coherent pair: the cached typed decision matches the cached bytes" $ do
        -- A hit returns the typed view and the exact bytes it was parsed from, so the
        -- served document is internally coherent — never a stale typed decision paired
        -- with fresh bytes. The public upstream serves 1.0.0 (kept) and 2.0.0 (too new
        -- → denied); the second request, served from cache, must keep that same
        -- decision (only 1.0.0) over the same bytes (1.0.0's integrity and its
        -- unmodeled key relayed unchanged).
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
            -- The cached decision (2.0.0 denied) holds on the hit.
            servedVersions secondResp `shouldBe` ["1.0.0"]
            -- The served bytes are 1.0.0's own: its integrity and unmodeled key, the
            -- exact bytes the cached typed view was decided over.
            servedIntegrity "1.0.0" secondResp `shouldBe` Just (sriFor "1.0.0")
            servedVersionKey "1.0.0" "_unmodeled" secondResp `shouldBe` Just (String "kept")

-- ── no survivors ──────────────────────────────────────────────────────────────

noSurvivorsSpec :: Spec
noSurvivorsSpec = describe "no survivors in the merge" $ do
    it "503s when no version survives and the private upstream is unavailable (transient)" $ do
        privateUp <- failingUpstream
        publicUp <-
            servingUpstream
                ( encodePackument
                    -- Both public versions are too new to clear the quarantine →
                    -- all denied → no survivors. With a failed private origin this
                    -- would be 503; here the private origin failing makes it transient.
                    ( packument
                        [("1.0.0", plainVersion "1.0.0"), ("2.0.0", plainVersion "2.0.0")]
                        "2.0.0"
                        [("1.0.0", publishedDaysAgo 1), ("2.0.0", publishedDaysAgo 1)]
                    )
                )
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            -- A failed private origin is a needed-upstream-unavailable signal (transient)
            -- → 503, inviting a retry, even though every public version was by policy.
            status resp `shouldBe` 503

    it "403s when all public versions are denied and the private upstream genuinely has none" $ do
        -- The private upstream resolves but holds no versions (an empty packument):
        -- no transient cause, every exclusion is by policy → 403.
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
            -- Never a 404: the package exists; its versions were withheld.
            status resp `shouldNotBe` 404

    it "403s when every public version is integrity-inadmissible and the private upstream has none" $ do
        -- The private upstream resolves but holds no versions; the public versions are
        -- all inadmissible by the integrity floor — one below it (a SHA-1-only shasum)
        -- and one with no digest at all. With no survivor and every exclusion a
        -- deny-by-default admission refusal (BelowIntegrityFloor / MissingIntegrity), the
        -- packument request is a 403 (it forces both refusal projections in the gate).
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
        -- Both upstreams answer, but each self-reports `other` for the requested
        -- `thing`: neither is a valid contribution. With no valid origin, the request
        -- is a 502 (a responding upstream returned an invalid response) — distinct from
        -- a genuine absence (which is not refused this way).
        privateUp <-
            servingUpstream
                (encodePackument (packumentNamed "other" [("1.0.0", plainVersion "1.0.0")] "1.0.0" [("1.0.0", publishedDaysAgo 30)]))
        publicUp <-
            servingUpstream
                (encodePackument (packumentNamed "other" [("2.0.0", plainVersion "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 30)]))
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 502
            -- The 502 reason phrase is rendered, and a gateway fault is not retryable —
            -- no Retry-After (unlike a 503).
            reason resp `shouldBe` "Bad Gateway"
            header "Retry-After" resp `shouldBe` Nothing
            -- A mismatch is "upstream returned an invalid response", not "not found".
            status resp `shouldNotBe` 404

    it "502s a public-leg mismatch routed through the metadata cache (private resolves but is empty)" $ do
        -- Isolates the public-cache path: the private leg resolves a valid but empty
        -- `thing` packument (so it adds no transient signal), while the public leg —
        -- fetched through `resolveMetadata` — answers with a packument for `other`. The
        -- typed mismatch is re-thrown through the cache leader and recovered as the
        -- public origin's bad-gateway signal, so with no valid contribution it is a 502.
        privateUp <- servingUpstream (encodePackument (privatePackument [] "0.0.0"))
        publicUp <-
            servingUpstream
                (encodePackument (packumentNamed "other" [("2.0.0", plainVersion "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 30)]))
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 502
            status resp `shouldNotBe` 404

-- ── edge authentication ───────────────────────────────────────────────────────

edgeAuthSpec :: Spec
edgeAuthSpec = describe "inbound PROXY_AUTH_TOKEN validated at the edge" $ do
    it "401s a request with no/incorrect inbound token before proxying" $ do
        privateUp <- servingUpstream (encodePackument (privatePackument [("1.0.0", plainVersion "1.0.0")] "1.0.0"))
        publicUp <- servingUpstream (encodePackument (packument [] "0.0.0" []))
        withProxy privateUp publicUp (Just "edge-secret") $ \app -> do
            resp <- getThing (Just "wrong-token") app
            status resp `shouldBe` 401
            -- No upstream was touched — the edge rejected before fetching.
            seenAuth privateUp `shouldReturn` []
            seenAuth publicUp `shouldReturn` []

    it "admits a request presenting the correct inbound token" $ do
        privateUp <- servingUpstream (encodePackument (privatePackument [("1.0.0", plainVersion "1.0.0")] "1.0.0"))
        publicUp <- servingUpstream (encodePackument (packument [] "0.0.0" []))
        withProxy privateUp publicUp (Just "edge-secret") $ \app -> do
            resp <- getThing (Just "edge-secret") app
            status resp `shouldBe` 200

-- ── own-ETag conditional ──────────────────────────────────────────────────────

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
            -- A 304 carries no body.
            simpleBody secondResp `shouldBe` ""

-- ── HEAD on a packument route ─────────────────────────────────────────────────

{- | HEAD on a packument route runs the __identical pipeline and gating__ as the GET
but answers bodiless (HTTP semantics: a HEAD reply carries no message body). Unlike the
tarball HEAD, a packument body is assembled locally, so this is an HTTP-correctness fix,
not the artifact-egress amplification the tarball HEAD guards against. Each case mirrors
a GET case to prove the gating is unchanged — asserting the same status and an empty
body, and (on the @200@) that the GET's status, ETag, and would-be @Content-Length@ are
all reproduced.
-}
packumentHeadSpec :: Spec
packumentHeadSpec = describe "HEAD on a packument route (same gating as GET, no body)" $ do
    it "answers a 200 HEAD with the GET's status, ETag, and Content-Length but no body" $ do
        (privateUp, publicUp) <- twoServingUpstreams
        withProxy privateUp publicUp Nothing $ \app -> do
            getResp <- getThing Nothing app
            headResp <- headThing Nothing app
            -- Same status as the GET would render.
            status getResp `shouldBe` 200
            status headResp `shouldBe` 200
            -- A HEAD reply carries no body.
            simpleBody headResp `shouldBe` ""
            -- The own ETag is present and identical to the GET's.
            header "ETag" headResp `shouldSatisfy` isJust
            header "ETag" headResp `shouldBe` header "ETag" getResp
            -- The would-be body's Content-Length is advertised: the length the GET
            -- actually served.
            header "Content-Length" headResp
                `shouldBe` Just (show (LBS.length (simpleBody getResp)))

    it "answers a conditional HEAD that matches our ETag with a bodiless 304" $ do
        (privateUp, publicUp) <- twoServingUpstreams
        withProxy privateUp publicUp Nothing $ \app -> do
            firstResp <- getThing Nothing app
            etag <- maybe (throwString "no ETag on the 200 response") pure (header "ETag" firstResp)
            headResp <- headThingWith [("If-None-Match", etag)] app
            -- Consistent with the GET conditional path: a bodiless 304.
            status headResp `shouldBe` 304
            simpleBody headResp `shouldBe` ""

    it "403s a HEAD identically to the GET when every version is withheld by policy" $ do
        -- Mirrors the GET no-survivors 403: the private upstream resolves but holds no
        -- versions, and the only public version declares an install script (denied).
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
            -- Never a 404, and bodiless.
            status resp `shouldNotBe` 404
            simpleBody resp `shouldBe` ""

    it "503s a HEAD identically to the GET when a needed upstream is unavailable (transient)" $ do
        -- Mirrors the GET no-survivors 503: the private upstream is down and every
        -- public version is too new to clear the quarantine.
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
        -- Mirrors the GET 502: both upstreams answer but self-report `other`, so no
        -- valid contribution remains.
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
            -- The edge rejected before any upstream fetch, exactly as the GET path does.
            seenAuth privateUp `shouldReturn` []
            seenAuth publicUp `shouldReturn` []

-- ── losslessness (decision surface vs served surface) ─────────────────────────

losslessSpec :: Spec
losslessSpec = describe "lossless served surface (raw Value edited in place)" $ do
    it "relays unmodeled top-level and per-version keys unchanged" $ do
        privateUp <- servingUpstream (encodePackument (privatePackument [("1.0.0", plainVersion "1.0.0")] "1.0.0"))
        publicUp <- failingUpstream
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            -- The unmodeled top-level @_id@ survives the merge/replay.
            topLevel "_id" resp `shouldBe` Just (String "thing")
            -- The unmodeled per-version key survives in the served version object.
            servedVersionKey "1.0.0" "_unmodeled" resp `shouldBe` Just (String "kept")

    it "rewrites dist.tarball under the mount base so artifacts route back through the gate" $ do
        privateUp <- servingUpstream (encodePackument (privatePackument [("1.0.0", plainVersion "1.0.0")] "1.0.0"))
        publicUp <- failingUpstream
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            servedTarball "1.0.0" resp `shouldBe` Just "https://proxy.test/thing/-/thing-1.0.0.tgz"

-- ── the artifact (tarball) path ───────────────────────────────────────────────

-- The opaque bytes a tarball double serves, distinct per origin so a test can pin
-- which upstream the served artifact came from.
privateTarballBytes :: LByteString
privateTarballBytes = "PRIVATE-TGZ-BYTES"

publicTarballBytes :: LByteString
publicTarballBytes = "PUBLIC-TGZ-BYTES"

-- A public packument whose single version @v@ clears the quarantine (admitted on
-- the artifact path). Used on the PACKUMENT path, where its dist.tarball is only
-- relayed (rewritten under the mount base), never fetched.
admittingPublic :: Text -> Value
admittingPublic v = packument [(v, plainVersion v)] v [(v, publishedDaysAgo 30)]

{- | A path-aware public double whose packument names its @dist.tarball@ on a
__different host__ (@crossHost@) than the one the packument was served on, while
still serving the tarball bytes itself. @crossHost@ that resolves to this server (a
loopback alias like @localhost@) lets a cross-host admit actually fetch through; the
host text drives the tarball-host policy decision regardless.
-}
crossHostPublicUpstream :: Text -> Text -> LByteString -> IO Upstream
crossHostPublicUpstream crossHost version tarballBody = do
    seen <- newIORef []
    let app :: Application
        app req respond = do
            modifyIORef' seen (lookupAuth (requestHeaders req) :)
            respond $
                if isTarballPath (rawPathInfo req)
                    then responseLBS status200 [] tarballBody
                    else responseLBS status200 [] (encodePackument (crossHostPackument req))
        -- The packument is served on this host (its own Host), but its dist.tarball
        -- names @crossHost@ at this same port — so the policy sees a cross-host URL.
        crossHostPackument req =
            let port = snd (T.breakOnEnd ":" (decodeUtf8 (maybe "" snd (find ((== hHost) . fst) (requestHeaders req)))))
                tarballBase = "http://" <> crossHost <> ":" <> port
             in selfHostedAdmitting tarballBase version
    mkUpstream seen app

{- | A double whose version's @dist.tarball@ sits at a __non-conventional path__
(@\/files\/{filename}@, not the npm @\/-\/@ slot) on its own host, with the given
@filename@ as the artifact name. The serve path honours that exact URL rather than
reconstructing @{base}\/{pkg}\/-\/{file}@, so the double serves the bytes at the
honoured path (matched by @.tgz@ suffix) — proving the location is honoured, not
rebuilt. The artifact is selected by @filename@, so a request whose filename differs
finds no match.
-}
honouredPathUpstream :: Text -> Text -> LByteString -> IO Upstream
honouredPathUpstream version filename tarballBody = do
    seen <- newIORef []
    let app :: Application
        app req respond = do
            modifyIORef' seen (lookupAuth (requestHeaders req) :)
            respond $
                if ".tgz" `BS.isSuffixOf` rawPathInfo req
                    then responseLBS status200 [] tarballBody
                    else responseLBS status200 [] (encodePackument (altPackument (selfBaseUrl req)))
        altPackument base =
            let dist =
                    object
                        [ "tarball" .= (base <> "/files/" <> filename)
                        , "integrity" .= sriFor version
                        ]
                vo = object ["name" .= ("thing" :: Text), "version" .= version, "dist" .= dist]
             in packument [(version, vo)] version [(version, publishedDaysAgo 30)]
    mkUpstream seen app

{- | A path-aware upstream double that honours a conditional artifact request: its
packument fetch is a self-referential single-version admitting packument (@200@), and
a tarball-slot path answers a bodiless @304 Not Modified@ (carrying an @ETag@) when
the request carries an @If-None-Match@, else @200@ with the artifact bytes. Lets a
test drive the pass-through conditional-GET relay end to end: a client validator
relayed upstream that matches must come straight back as a relayed @304@, the artifact
never re-downloaded. Used as either the private or the public origin's double.
-}
conditionalArtifactUpstream :: Text -> LByteString -> IO Upstream
conditionalArtifactUpstream version tarballBody = do
    seen <- newIORef []
    let app :: Application
        app req respond = do
            modifyIORef' seen (lookupAuth (requestHeaders req) :)
            respond $
                if isTarballPath (rawPathInfo req)
                    then conditionalTarball (requestHeaders req)
                    else responseLBS status200 [] (encodePackument (selfHostedAdmitting (selfBaseUrl req) version))
    mkUpstream seen app
  where
    -- A relayed client validator turns the upstream artifact fetch into a 304; an
    -- unconditional fetch still serves the bytes.
    conditionalTarball :: [Header] -> Response
    conditionalTarball headers
        | any ((== hIfNoneMatch) . fst) headers = responseLBS status304 [(hETag, "\"v1\"")] ""
        | otherwise = responseLBS status200 [] tarballBody

-- A flat projection of a mirror job, for an order-stable equality assertion over
-- the four coordinates the job carries (package, version, public artifact URL,
-- mirror target).
jobShape :: MirrorJob -> (PackageName, Version, Text, Text)
jobShape job = (jobPackage job, jobVersion job, jobArtifactUrl job, jobMirrorTarget job)

tarballSpec :: Spec
tarballSpec = describe "artifact (tarball) path" $ do
    it "streams the private artifact unfiltered on a private hit (public never consulted)" $ do
        -- The private upstream has the artifact: it is streamed straight through,
        -- the public origin never queried and no mirror job enqueued (the bytes are
        -- already vetted; mirroring is for public-sourced artifacts).
        privateUp <- privateArtifactHit "1.0.0" privateTarballBytes
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- getTarball "1.0.0" (Just "client-token") app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` privateTarballBytes
            -- The public upstream was never touched on a private hit.
            seenAuth publicUp `shouldReturn` []
            -- A private hit enqueues nothing — only a public-sourced admit does.
            drainJobs env `shouldReturn` []

    it "relays the upstream status and content headers through on a private hit" $ do
        -- The relay forwards the artifact's own status and content headers verbatim
        -- (the client verifies dist.integrity over exactly these bytes). Asserting a
        -- relayed Content-Type forces the header relay the streaming path applies.
        privateUp <- privateArtifactHitWithHeader "Content-Type" "application/octet-stream" "1.0.0" privateTarballBytes
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app _env -> do
            resp <- getTarball "1.0.0" (Just "client-token") app
            status resp `shouldBe` 200
            reason resp `shouldBe` "OK"
            header "Content-Type" resp `shouldBe` Just "application/octet-stream"
            simpleBody resp `shouldBe` privateTarballBytes

    it "falls through to the public origin when the private upstream URL is unformable" $ do
        -- An empty private base URL cannot form an artifact request, so the private
        -- origin yields a clean miss (never an error) and the serve path falls through
        -- to the public origin — the artifact is still served from public.
        privateUp <- privateArtifactHit "1.0.0" privateTarballBytes
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        queue <- newInMemoryQueue
        let breakPrivate d = d{pdPrivateBaseUrl = ""}
        withProxyEnvQueueDeps queue privateUp publicUp Nothing breakPrivate $ \app _env _port -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 200
            -- Served from public (the unformable private origin contributed nothing).
            simpleBody resp `shouldBe` publicTarballBytes

    it "401s a tarball request that fails edge authentication, before any upstream fetch" $ do
        -- The inbound edge token gates the tarball path exactly as the packument
        -- path: a missing/incorrect token is a 401 rendered before either upstream
        -- is touched.
        privateUp <- privateArtifactHit "1.0.0" privateTarballBytes
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp (Just "edge-secret") $ \app _env -> do
            resp <- getTarball "1.0.0" (Just "wrong-token") app
            status resp `shouldBe` 401
            -- No upstream was consulted — the edge rejected first.
            seenAuth privateUp `shouldReturn` []
            seenAuth publicUp `shouldReturn` []

    it "forwards the client credential to the private origin, never to the public" $ do
        -- On a private MISS the artifact still comes from public, but the gating
        -- packument and artifact fetches must be anonymous; the private origin saw the
        -- client's bearer on BOTH its requests — the packument (to find the artifact's
        -- authoritative URL) and the honoured tarball fetch (which misses, falling
        -- through to public).
        privateUp <- privateArtifactMiss
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app _env -> do
            _ <- getTarball "1.0.0" (Just "client-secret-token") app
            privAuth <- seenAuth privateUp
            pubAuth <- seenAuth publicUp
            -- Both private-origin requests (packument + honoured tarball) carried the
            -- client's bearer credential.
            privAuth `shouldBe` [Just "Bearer client-secret-token", Just "Bearer client-secret-token"]
            -- Both public-origin requests (packument gate + artifact fetch) were anonymous.
            pubAuth `shouldBe` [Nothing, Nothing]

    it "on a private miss: gates the version, streams from public, and enqueues a mirror job" $ do
        privateUp <- privateArtifactMiss
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        queue <- newInMemoryQueue
        withProxyEnvQueue queue privateUp publicUp Nothing $ \app env publicPort -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 200
            -- The served bytes are the PUBLIC artifact's (private missed).
            simpleBody resp `shouldBe` publicTarballBytes
            -- Exactly one demand-driven mirror job was enqueued, naming the public
            -- artifact URL and the mount's mirror target.
            jobs <- drainJobs env
            map jobShape jobs
                `shouldBe` [
                               ( mkPackageName Npm Nothing "thing"
                               , mkVersion Npm "1.0.0"
                               , localhost publicPort <> "/thing/-/thing-1.0.0.tgz"
                               , "https://mirror.test"
                               )
                           ]

    it "rejects a too-new version with 403 and enqueues nothing (policy denial)" $ do
        -- 2.0.0 is 3 days old (< the 7-day quarantine) → denied by policy → 403,
        -- and a rejected artifact is never mirrored.
        privateUp <- privateArtifactMiss
        let tooNew base = packument [("2.0.0", selfHostedVersion base "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 3)]
        publicUp <- artifactUpstreamServing (encodePackument . tooNew) publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- getTarball "2.0.0" Nothing app
            status resp `shouldBe` 403
            -- The reason phrase the serve error model maps a policy denial to.
            reason resp `shouldBe` "Forbidden"
            drainJobs env `shouldReturn` []

    it "refuses a hashless public version with 403 before fetching the artifact (integrity-presence policy)" $ do
        -- 1.0.0 is old enough to clear the quarantine and declares no install
        -- script, so the rules admit it — but its public dist carries neither
        -- integrity nor shasum. A public version with no integrity digest is
        -- inadmissible: refused 403 (a policy denial, not a 500), the artifact
        -- never fetched and nothing enqueued.
        privateUp <- privateArtifactMiss
        let hashless base = packument [("1.0.0", selfHostedHashless base "1.0.0")] "1.0.0" [("1.0.0", publishedDaysAgo 30)]
        publicUp <- artifactUpstreamServing (encodePackument . hashless) publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 403
            reason resp `shouldBe` "Forbidden"
            -- The artifact itself was never fetched: only the gating packument was requested.
            seenAuth publicUp `shouldReturn` [Nothing]
            drainJobs env `shouldReturn` []

    it "refuses an empty-digest public version with 403 MissingIntegrity before fetching the artifact" $ do
        -- 1.0.0 clears the rules but its public dist carries `integrity:""`/`shasum:""`.
        -- The projection normalises those to no digest, so the artifact gate classifies
        -- the version NoIntegrity and refuses it with the MissingIntegrity 403 — the same
        -- path a truly hashless version takes, the existing gate firing on the fixed
        -- projection. The MissingIntegrity message (not BelowIntegrityFloor) pins that the
        -- empty strings became NO digest, not a present-but-weak one; the tarball is never
        -- fetched and nothing is enqueued.
        privateUp <- privateArtifactMiss
        let emptyDigest base = packument [("1.0.0", selfHostedEmptyDigest base "1.0.0")] "1.0.0" [("1.0.0", publishedDaysAgo 30)]
        publicUp <- artifactUpstreamServing (encodePackument . emptyDigest) publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 403
            reason resp `shouldBe` "Forbidden"
            decodedBody resp
                `shouldBe` object ["error" .= ("this version carries no integrity digest and cannot be served from a public upstream" :: Text)]
            seenAuth publicUp `shouldReturn` [Nothing]
            drainJobs env `shouldReturn` []

    it "refuses a below-floor public version with 403 before fetching the artifact (integrity floor)" $ do
        -- 1.0.0 clears the rules but its public dist carries only a legacy SHA-1
        -- shasum (no SRI integrity) — below the default SHA-256 floor. The artifact
        -- gate refuses it 403 (a BelowIntegrityFloor policy denial), the tarball never
        -- fetched and nothing enqueued — the BelowFloor arm of the concrete-artifact gate.
        privateUp <- privateArtifactMiss
        let belowFloor base = packument [("1.0.0", selfHostedShasumOnly base "1.0.0")] "1.0.0" [("1.0.0", publishedDaysAgo 30)]
        publicUp <- artifactUpstreamServing (encodePackument . belowFloor) publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 403
            reason resp `shouldBe` "Forbidden"
            seenAuth publicUp `shouldReturn` [Nothing]
            drainJobs env `shouldReturn` []

    it "admits a digest-bearing public version on the artifact path (no regression)" $ do
        -- The same version WITH an integrity digest is admitted and streamed as
        -- today — the integrity-presence policy refuses only the hashless case.
        privateUp <- privateArtifactMiss
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app _env -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` publicTarballBytes

    it "serves a hashless version from the trusted private upstream (the private path is exempt)" $ do
        -- The private upstream is trusted, so the integrity-presence policy does
        -- not apply to it: a private hit with no integrity digest still streams
        -- through, the public origin never consulted.
        privateUp <- privateArtifactHitHashless "1.0.0" privateTarballBytes
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- getTarball "1.0.0" (Just "client-token") app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` privateTarballBytes
            seenAuth publicUp `shouldReturn` []
            drainJobs env `shouldReturn` []

    it "503s when the public upstream is unavailable (transient), enqueuing nothing" $ do
        privateUp <- privateArtifactMiss
        publicUp <- failingUpstream
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 503
            reason resp `shouldBe` "Service Unavailable"
            -- The rendered body carries the transient-outage reason the serve path
            -- attaches to an unavailable public upstream.
            decodedBody resp `shouldBe` object ["error" .= ("the upstream registry was unavailable" :: Text)]
            drainJobs env `shouldReturn` []

    it "404s a version absent from the public metadata (forwarded miss), enqueuing nothing" $ do
        -- The package resolves but the requested 9.9.9 is not among its versions: a
        -- forwarded upstream miss → 404, never a fabricated admit.
        privateUp <- privateArtifactMiss
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- getTarball "9.9.9" Nothing app
            status resp `shouldBe` 404
            -- The version-absent miss is carried as a WontResolve rejection but
            -- rendered as a forwarded 404 — its reason phrase, not the 500 a
            -- WontResolve would otherwise map to.
            reason resp `shouldBe` "Not Found"
            drainJobs env `shouldReturn` []

    it "serves the artifact even when the enqueue fails (best-effort, non-blocking)" $ do
        -- The queue's enqueue throws; the client must still receive the streamed
        -- artifact with a 200. The enqueue failure is swallowed, never surfaced.
        privateUp <- privateArtifactMiss
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        failingQueue <- newFailingQueue
        withProxyEnvQueue failingQueue privateUp publicUp Nothing $ \app _env _port -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` publicTarballBytes

    it "500s when an admitted artifact's upstream URL cannot be formed (internal fault)" $ do
        -- The version run carries a character that is a safe path component (so the
        -- route accepts it and the gate admits the version) yet makes the upstream
        -- artifact URL unparseable: an internal/config fault, distinct from the
        -- rule/upstream serve outcomes — a 500 with the internal-error body, and no
        -- mirror job. (The gate's metadata URL — @{base}/{pkg}@ — still forms, so the
        -- gate admits; only the artifact URL — @{base}/{pkg}/-/{file}@ — fails.)
        privateUp <- privateArtifactMiss
        let badVersion = "1.0.0[" -- '[' is a safe component but unparseable in a URL path
        -- The honoured dist.tarball URL carries the bad version in its filename,
        -- so its host is the (allowlisted, opted-in) upstream — the host gate
        -- passes — but the URL itself is unparseable, the internal-error path.
            admitting base = packument [(badVersion, selfHostedVersion base badVersion)] badVersion [(badVersion, publishedDaysAgo 30)]
        publicUp <- artifactUpstreamServing (encodePackument . admitting) publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- getTarball badVersion Nothing app
            status resp `shouldBe` 500
            reason resp `shouldBe` "Internal Server Error"
            decodedBody resp `shouldBe` object ["error" .= ("could not form the upstream artifact URL" :: Text)]
            drainJobs env `shouldReturn` []

    it "gates a lockfile install hitting the tarball URL with no preceding packument request" $ do
        -- An `npm ci` install fetches the tarball directly; the gate decides on this
        -- path alone (the version metadata is fetched here), with no prior packument
        -- request. A denied version is still refused.
        privateUp <- privateArtifactMiss
        let tooNew base = packument [("2.0.0", selfHostedVersion base "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 3)]
        publicUp <- artifactUpstreamServing (encodePackument . tooNew) publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app _env ->
            getTarball "2.0.0" Nothing app >>= \resp -> status resp `shouldBe` 403

    it "fails internally on a mid-stream private failure, never falling through to public" $ do
        -- The private origin commits a 200 then drops the connection mid-body. Because
        -- the response is already on the wire, the serve path must fail (the broken
        -- stream surfaces as an error here) rather than swallow it and respond a
        -- second time over the half-sent artifact — so the public origin is never
        -- consulted and nothing is enqueued.
        privateUp <- privateArtifactMidStreamFailure
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            (_ :: Either SomeException SResponse) <- try (getTarball "1.0.0" (Just "client-token") app)
            seenAuth publicUp `shouldReturn` []
            drainJobs env `shouldReturn` []

    it "serves a same-host public artifact at its honoured dist.tarball location" $ do
        -- The public packument's dist.tarball is on the SAME host that served it
        -- (the secure-default SameHostAsPackument is satisfied); the honoured URL is
        -- fetched and its bytes streamed — no regression on the shipped same-host path.
        privateUp <- privateArtifactMiss
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app _env -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` publicTarballBytes

    it "refuses a cross-host public dist.tarball under the SameHostAsPackument default (403, no fetch)" $ do
        -- The packument is served on 127.0.0.1 but its dist.tarball names a different
        -- host (localhost). Under the secure default the cross-host location is refused
        -- before any artifact fetch — the tarball-host policy on the real serve path.
        privateUp <- privateArtifactMiss
        publicUp <- crossHostPublicUpstream "localhost" "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 403
            reason resp `shouldBe` "Forbidden"
            -- The artifact was never fetched: only the gating packument was requested.
            seenAuth publicUp `shouldReturn` [Nothing]
            drainJobs env `shouldReturn` []

    it "serves a cross-host public dist.tarball under AnyAllowlistedHost when the host is allowlisted" $ do
        -- With the opt-in policy and the cross-host (localhost) on the upstream
        -- allowlist (carried via the mirror target's host), the cross-host location is
        -- honoured and its bytes streamed. localhost resolves to this same loopback
        -- server, so the fetch reaches the artifact.
        privateUp <- privateArtifactMiss
        publicUp <- crossHostPublicUpstream "localhost" "1.0.0" publicTarballBytes
        queue <- newInMemoryQueue
        let relax d = d{pdTarballHostPolicy = AnyAllowlistedHost, pdMirrorTarget = "http://localhost:9"}
        withProxyEnvQueueDeps queue privateUp publicUp Nothing relax $ \app _env _port -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` publicTarballBytes

    it "refuses a cross-host public dist.tarball under AnyAllowlistedHost when the host is off the allowlist" $ do
        -- The opt-in relaxes SameHostAsPackument but never escapes the allowlist: a
        -- cross-host (localhost) NOT among the configured upstream hosts is still
        -- refused (the allowlist is the load-bearing control, security.md invariant 2).
        privateUp <- privateArtifactMiss
        publicUp <- crossHostPublicUpstream "localhost" "1.0.0" publicTarballBytes
        queue <- newInMemoryQueue
        let relax d = d{pdTarballHostPolicy = AnyAllowlistedHost}
        withProxyEnvQueueDeps queue privateUp publicUp Nothing relax $ \app _env _port -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 403

    it "404s a requested filename absent from the version's artifacts (selection by filename)" $ do
        -- The version's only artifact is named thing-1.0.0-alt.tgz; a request for the
        -- conventional thing-1.0.0.tgz matches no artifact, so it is a forwarded miss
        -- (404), never a fabricated fetch of the unrequested file.
        privateUp <- privateArtifactMiss
        publicUp <- honouredPathUpstream "1.0.0" "thing-1.0.0-alt.tgz" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 404
            reason resp `shouldBe` "Not Found"
            drainJobs env `shouldReturn` []

    it "honours a non-conventional public dist.tarball path (not a reconstructed /-/ URL)" $ do
        -- The public packument's dist.tarball sits at /files/… on the same host — a
        -- path the npm /-/ convention would never reconstruct. The serve path honours
        -- that exact URL and streams its bytes, proving the location is honoured rather
        -- than rebuilt from (package, file).
        privateUp <- privateArtifactMiss
        publicUp <- honouredPathUpstream "1.0.0" "thing-1.0.0.tgz" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app _env -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` publicTarballBytes

    it "honours the private dist.tarball location on a private hit (not a reconstructed URL)" $ do
        -- The private packument's dist.tarball sits at a non-conventional /files/ path;
        -- the serve path honours that exact URL (the private upstream is trusted), so
        -- the private bytes stream through and the public origin is never consulted.
        privateUp <- honouredPathUpstream "1.0.0" "thing-1.0.0.tgz" privateTarballBytes
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` privateTarballBytes
            seenAuth publicUp `shouldReturn` []
            drainJobs env `shouldReturn` []

    it "refuses a cross-host private dist.tarball under the default and falls through to public" $ do
        -- The private packument (on 127.0.0.1) names its dist.tarball on a different
        -- host (localhost). The tarball-host policy gates the private leg too: under the
        -- secure default the cross-host private location is refused, so the private leg
        -- is a clean miss and the artifact is served from the public origin instead.
        privateUp <- crossHostPublicUpstream "localhost" "1.0.0" privateTarballBytes
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app _env -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 200
            -- Served from public: the private cross-host location was refused, never fetched.
            simpleBody resp `shouldBe` publicTarballBytes

    it "serves a same-host private dist.tarball on an internal-IP private origin with no opt-in (trusted-origin exempt)" $ do
        -- The trusted private origin lives on an internal IP literal (127.0.0.1, the
        -- in-process double's loopback bind) and serves a same-host dist.tarball. With
        -- the internal-range opt-in empty, the private leg's tarball-host gate must STILL
        -- admit it — the trusted origin is exempt from the internal-range block exactly as
        -- the connection layer's newTrustedTlsManager carries no resolved-IP recheck
        -- (security.md invariant 3). Were the private path subject to the block, the gate
        -- would refuse the same-host private tarball and the request would fall through to
        -- public — an asymmetric, install-breaking failure (the same-host private metadata
        -- already resolved). The allowlist + same-host checks stay intact for the private
        -- leg; only the internal-range conjunct is exempted by trust.
        privateUp <- privateArtifactHit "1.0.0" privateTarballBytes
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        let noOptIn d = d{pdAllowedInternalHosts = lowerCaseHosts Set.empty}
        queue <- newInMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing noOptIn $ \app _env _port -> do
            resp <- getTarball "1.0.0" (Just "client-token") app
            status resp `shouldBe` 200
            -- Streamed from the trusted private origin, not fallen through to public.
            simpleBody resp `shouldBe` privateTarballBytes
            seenAuth publicUp `shouldReturn` []

    conditionalArtifactSpec

    headTarballSpec

{- | Pass-through conditional-GET on the artifact path: the client's validators are
relayed onto the upstream artifact request (on both the private and public legs), and
an upstream @304 Not Modified@ is relayed straight back to the client as a bodiless
@304@ rather than treated as a miss/fall-through — so a conditional artifact GET whose
validator matches upstream never re-downloads the tarball. (The merged-packument path
answers conditional requests against our own ETag instead; see 'conditionalSpec'.)
-}
conditionalArtifactSpec :: Spec
conditionalArtifactSpec = describe "pass-through conditional GET on the artifact path" $ do
    it "relays a private-upstream 304 back as a bodiless 304 (never re-downloaded, never fallen through)" $ do
        -- The private upstream answers the conditional artifact fetch with a 304: it
        -- must be relayed straight back as a bodiless 304, the public origin never
        -- consulted (a 304 is a relay, not a private miss) and no mirror job enqueued
        -- (no bytes were served).
        privateUp <- conditionalArtifactUpstream "1.0.0" privateTarballBytes
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- getTarballWith "1.0.0" [(hIfNoneMatch, "\"v1\"")] app
            status resp `shouldBe` 304
            -- A 304 carries no body — the tarball was not re-downloaded or pumped.
            simpleBody resp `shouldBe` ""
            -- The upstream's validator (ETag) is relayed back to the client.
            header "ETag" resp `shouldBe` Just "\"v1\""
            -- The 304 was relayed, not treated as a private miss falling through.
            seenAuth publicUp `shouldReturn` []
            drainJobs env `shouldReturn` []

    it "relays a public-upstream 304 back as a bodiless 304 on a private miss" $ do
        -- A private miss falls through to the public origin, which answers the
        -- conditional artifact fetch with a 304 — relayed straight back as a bodiless
        -- 304, the public bytes never re-downloaded.
        privateUp <- privateArtifactMiss
        publicUp <- conditionalArtifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app _env -> do
            resp <- getTarballWith "1.0.0" [(hIfNoneMatch, "\"v1\"")] app
            status resp `shouldBe` 304
            simpleBody resp `shouldBe` ""
            header "ETag" resp `shouldBe` Just "\"v1\""

    it "forwards the client's If-None-Match onto BOTH the private and public artifact upstream requests" $ do
        -- The private leg misses (its tarball slot 404s), so the artifact comes from
        -- public — and the client's conditional validator must be relayed onto BOTH the
        -- private artifact fetch (which misses) and the public artifact fetch.
        privateUp <- privateArtifactMiss
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app _env -> do
            _ <- getTarballWith "1.0.0" [(hIfNoneMatch, "\"client-etag\"")] app
            seenArtifactValidators privateUp `shouldReturn` [Just "\"client-etag\""]
            seenArtifactValidators publicUp `shouldReturn` [Just "\"client-etag\""]

    it "streams a non-conditional artifact GET normally (no regression)" $ do
        -- With no client validator, the upstream artifact fetch is unconditional and
        -- the full body streams through as before — the conditional wiring is inert on
        -- the unconditional path. The upstream saw no If-None-Match.
        privateUp <- conditionalArtifactUpstream "1.0.0" privateTarballBytes
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app _env -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` privateTarballBytes
            seenArtifactValidators privateUp `shouldReturn` [Nothing]

{- | HEAD on a tarball route must never run the full-GET streaming pump: a bodiless
HEAD must not be able to force the proxy to open the upstream artifact connection and
pump a whole artifact body to nowhere — the DoS-amplification lever this guards
against. A HEAD goes through the identical gating and upstream-request construction as
the GET path, but issues the upstream request as a HEAD and relays its status (and
safe headers) with no body.
-}
headTarballSpec :: Spec
headTarballSpec = describe "HEAD on a tarball route (no full-artifact body pump)" $ do
    it "answers a private-hit HEAD by probing upstream as a HEAD, never a body GET" $ do
        -- The private upstream has the artifact. A HEAD must reach its artifact slot
        -- as a HEAD — never the body-pumping GET — and the client gets a 200 with an
        -- empty body. (A GET reaching upstream would be the amplification this prevents.)
        privateUp <- privateArtifactHit "1.0.0" privateTarballBytes
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- headTarball "1.0.0" (Just "client-token") app
            status resp `shouldBe` 200
            -- No body on a HEAD reply.
            simpleBody resp `shouldBe` ""
            -- The upstream artifact slot was contacted as a HEAD, never a GET.
            seenArtifactMethods privateUp `shouldReturn` [methodHead]
            -- The public origin was never consulted on a private hit, and no mirror
            -- job is enqueued (a HEAD serves no bytes — there is nothing to back-fill).
            seenAuth publicUp `shouldReturn` []
            drainJobs env `shouldReturn` []

    it "answers a public-admit HEAD by probing upstream as a HEAD, enqueuing nothing" $ do
        -- A private miss falls through to the public origin, the version is gated and
        -- admitted, and the artifact slot is probed as a HEAD (never a GET). The reply
        -- is a 200 with an empty body; a HEAD enqueues no mirror job.
        privateUp <- privateArtifactMiss
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- headTarball "1.0.0" Nothing app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` ""
            -- The public artifact slot was contacted as a HEAD, never a body GET.
            seenArtifactMethods publicUp `shouldReturn` [methodHead]
            drainJobs env `shouldReturn` []

    it "falls a private-artifact-miss HEAD through to a public HEAD (recoverable miss, both probed as HEAD)" $ do
        -- The private upstream resolves the metadata (so the version is selected) but
        -- 404s the artifact: probeUpstreamWhen's `not (accept upstreamStatus)` makes that
        -- a RECOVERABLE miss, exactly as a GET, so the request must fall through to the
        -- public origin — which has the artifact. The fall-through is probed as a HEAD on
        -- BOTH legs (never a body GET on either): the private artifact slot is contacted
        -- as a HEAD and misses, then the public artifact slot is contacted as a HEAD. The
        -- reply is bodiless with the public status, and a HEAD enqueues nothing.
        privateUp <- privateArtifactMiss
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- headTarball "1.0.0" (Just "client-token") app
            status resp `shouldBe` 200
            -- Bodiless reply carrying the public origin's status.
            simpleBody resp `shouldBe` ""
            -- The private artifact slot was probed as a HEAD (and 404'd — a recoverable miss).
            seenArtifactMethods privateUp `shouldReturn` [methodHead]
            -- It fell through: the public artifact slot was then probed as a HEAD too,
            -- never a body GET on either leg.
            seenArtifactMethods publicUp `shouldReturn` [methodHead]
            -- A HEAD serves no bytes, so nothing is enqueued for back-fill.
            drainJobs env `shouldReturn` []

    it "denies a too-new version with 403 and an empty body, never touching the artifact" $ do
        -- The gating is identical to GET: a version inside the quarantine is a policy
        -- denial. A denied HEAD is a 403 with no body, and the artifact slot is never
        -- contacted by any method.
        privateUp <- privateArtifactMiss
        let tooNew base = packument [("2.0.0", selfHostedVersion base "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 3)]
        publicUp <- artifactUpstreamServing (encodePackument . tooNew) publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- headTarball "2.0.0" Nothing app
            status resp `shouldBe` 403
            simpleBody resp `shouldBe` ""
            -- The artifact was never fetched, by any method — the gate refused first.
            seenArtifactMethods publicUp `shouldReturn` []
            drainJobs env `shouldReturn` []

-- ── the effectful rule tier through the pipeline ───────────────────────────────

{- | An effectful rule, fresh breaker and a no-retry fast config, that always fails
its IO (its source is down). Wired at a precedence above the pure quarantine, it is
consulted on an otherwise-admitted version and, exhausted, fail-closes it.
-}
downEffectfulRule :: IO PrecededEffectfulRule
downEffectfulRule = do
    breaker <- newBreaker
    let rule =
            EffectfulRule
                { erName = "DownAdvisory"
                , erEval = \_ -> throwString "advisory source down"
                , erConfig = defaultEffectfulConfig{ecBackoff = []}
                , erOnError = OnUnavailable
                , erBreaker = breaker
                }
    pure (PrecededEffectfulRule 400 rule)

{- | An effectful rule that denies the version outright (its source is reachable and
returns a verdict), wired above the pure tier so its deny stands.
-}
denyingEffectfulRule :: IO PrecededEffectfulRule
denyingEffectfulRule = do
    breaker <- newBreaker
    let rule =
            EffectfulRule
                { erName = "DenyAdvisory"
                , erEval = \_ -> pure (Deny "affected by a known advisory")
                , erConfig = defaultEffectfulConfig
                , erOnError = OnUnavailable
                , erBreaker = breaker
                }
    pure (PrecededEffectfulRule 400 rule)

{- | An effectful rule that admits the version (its source vouches for it), wired
above the pure tier so it lifts a version the pure quarantine would otherwise hold.
-}
allowingEffectfulRule :: IO PrecededEffectfulRule
allowingEffectfulRule = do
    breaker <- newBreaker
    let rule =
            EffectfulRule
                { erName = "AllowAdvisory"
                , erEval = \_ -> pure (Allow "remediates a known advisory")
                , erConfig = defaultEffectfulConfig
                , erOnError = OnUnavailable
                , erBreaker = breaker
                }
    pure (PrecededEffectfulRule 400 rule)

effectfulSpec :: Spec
effectfulSpec = describe "effectful rule tier" $ do
    it "filters an Undecidable version out of a packument and 503s when nothing survives" $ do
        -- The single public version would clear the pure quarantine, but a needed
        -- effectful rule cannot be consulted, so it is fail-closed (Undecidable) and
        -- filtered out exactly like a denial. With no private survivors the
        -- no-survivors status is a transient 503, inviting a retry.
        rule <- downEffectfulRule
        privateUp <- servingUpstream (encodePackument (privatePackument [] "0.0.0"))
        publicUp <- servingUpstream (encodePackument (admittingPublic "1.0.0"))
        withProxyEffectful [rule] privateUp publicUp $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 503
            status resp `shouldNotBe` 404

    it "excludes a version a reachable effectful deny rejects (403 when nothing else survives)" $ do
        -- A reachable effectful deny outranks the pure allow, so the only public
        -- version is excluded; with no private survivors that is a by-policy 403.
        rule <- denyingEffectfulRule
        privateUp <- servingUpstream (encodePackument (privatePackument [] "0.0.0"))
        publicUp <- servingUpstream (encodePackument (admittingPublic "1.0.0"))
        withProxyEffectful [rule] privateUp publicUp $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 403

    it "serves a version a reachable effectful allow admits (a 200 with that version)" $ do
        -- A young public version the pure quarantine would hold is lifted by a
        -- higher-ranked effectful allow, so it survives the filter and is served.
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

    it "serves a tarball a reachable effectful allow admits on the artifact path" $ do
        -- The same admit on the concrete-artifact path: the gated version is admitted
        -- by the effectful allow and the public bytes stream through.
        rule <- allowingEffectfulRule
        privateUp <- privateArtifactMiss
        let young base = packument [("1.0.0", selfHostedVersion base "1.0.0")] "1.0.0" [("1.0.0", publishedDaysAgo 1)]
        publicUp <- artifactUpstreamServing (encodePackument . young) publicTarballBytes
        withProxyEffectful [rule] privateUp publicUp $ \app -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` publicTarballBytes

    it "maps a concrete-artifact Undecidable to a 503 on the tarball path" $ do
        -- A direct tarball fetch gates the one version through both tiers; the
        -- exhausted effectful rule fail-closes it, surfacing as a transient 503
        -- (not a 403 denial, not a 500) via the serve error model.
        rule <- downEffectfulRule
        privateUp <- privateArtifactMiss
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEffectful [rule] privateUp publicUp $ \app -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 503

-- ── response bounds through the real request path (security.md invariant 4) ────

{- | A tight response-bound budget for the bounds tests: small enough that the
fixtures below breach it while a normal packument clears it. Each ceiling stays
strictly positive (the config layer rejects a non-positive one), so what is asserted
is the breach behaviour, not a degenerate "everything is too big".
-}
tightLimits :: Limits
tightLimits =
    defaultLimits
        { maxBodyBytes = 4096
        , maxVersionCount = 3
        , maxNestingDepth = 8
        }

-- Set the mount's response-bound budget on the deps (the composition root's job in
-- production; here a per-test tweak to exercise the breach paths).
withLimits :: Limits -> PackumentDeps -> PackumentDeps
withLimits limits d = d{pdLimits = limits}

{- | A packument body larger than the tight @maxBodyBytes@: the same admitting
single-version document padded past the cap with a large unmodeled string. It still
parses and projects, so only the body-size bound — not a parse failure — can refuse
it.
-}
oversizedPackument :: Text -> LByteString
oversizedPackument v =
    encodePackument $ case admittingPublic v of
        Object o -> Object (KeyMap.insert "_padding" (String (T.replicate 8192 "x")) o)
        other -> other

{- | A packument carrying more versions than the tight @maxVersionCount@ (4 > 3),
every one old enough to clear the quarantine — so only the version-count bound can
refuse it.
-}
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

{- | A tiny body that is not valid JSON, so it fails the decode (not a bound) and
exercises the parse-failure log — the one bad-upstream case the bound guards leave
silent.
-}
undecodableBody :: LByteString
undecodableBody = "!"

{- | A JSON body nested deeper than the tight @maxNestingDepth@ (a chain of nested
arrays). It is valid JSON but not a packument; 'checkNestingDepth' refuses it at the
decode boundary, before projection ever runs — so the nesting bound, not a parse
failure, is what fails it closed.
-}
deeplyNestedBody :: LByteString
deeplyNestedBody = encodePackument (nest 32 (String "deep"))
  where
    nest :: Int -> Value -> Value
    nest 0 v = v
    nest n v = nest (n - 1) (Aeson.toJSON [v])

boundsSpec :: Spec
boundsSpec = describe "response bounds through the request path (security.md invariant 4)" $ do
    it "refuses an oversized private packument fail-closed, serving only the public set" $ do
        -- The private body exceeds maxBodyBytes, so its bounded read aborts: the private
        -- contribution degrades to nothing (the parse-failure path), never a partial
        -- merge. The good public version is still served, proving the oversized document
        -- was refused outright rather than truncated into the served document.
        privateUp <- servingUpstream (oversizedPackument "9.9.9")
        publicUp <- servingUpstream (encodePackument (admittingPublic "1.0.0"))
        queue <- newInMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing (withLimits tightLimits) $ \app _env _port -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            -- Only the public version survives; the oversized private 9.9.9 never enters.
            servedVersions resp `shouldBe` ["1.0.0"]

    it "503s when the only (public) packument is oversized and the private upstream is down" $ do
        -- With no private contribution and the public body over the cap, nothing
        -- resolves — a fail-closed refusal (a needed upstream unavailable, 503), never
        -- a partially-served oversized document.
        privateUp <- failingUpstream
        publicUp <- servingUpstream (oversizedPackument "1.0.0")
        queue <- newInMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing (withLimits tightLimits) $ \app _env _port -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 503

    it "refuses a version-flood public packument fail-closed (nothing served from it)" $ do
        -- The public packument carries more versions than maxVersionCount, so the whole
        -- contribution is refused after projection — not trimmed to the first N. With
        -- the private upstream down, that is a 503 (a needed upstream unavailable),
        -- never a partial serve of the flood.
        privateUp <- failingUpstream
        publicUp <- servingUpstream versionFloodPackument
        queue <- newInMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing (withLimits tightLimits) $ \app _env _port -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 503

    it "serves the public set when a version-flood document arrives on the private leg" $ do
        -- A version flood on the private leg degrades that contribution to nothing
        -- (refused after projection), so the served document is the gated public set —
        -- the flood never enters, partially or otherwise.
        privateUp <- servingUpstream versionFloodPackument
        publicUp <- servingUpstream (encodePackument (admittingPublic "1.0.0"))
        queue <- newInMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing (withLimits tightLimits) $ \app _env _port -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["1.0.0"]

    it "refuses a deeply-nested public document fail-closed (503 with the private upstream down)" $ do
        -- The public body is nested past maxNestingDepth, refused at the decode boundary
        -- before any projection. With no private contribution, that is a 503 — a
        -- fail-closed refusal, never a partial traversal of the nested document.
        privateUp <- failingUpstream
        publicUp <- servingUpstream deeplyNestedBody
        queue <- newInMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing (withLimits tightLimits) $ \app _env _port -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 503

    it "serves the public set when a deeply-nested document arrives on the private leg" $ do
        -- A deeply-nested body on the private leg is refused at decode and contributes
        -- nothing, so only the gated public version is served.
        privateUp <- servingUpstream deeplyNestedBody
        publicUp <- servingUpstream (encodePackument (admittingPublic "1.0.0"))
        queue <- newInMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing (withLimits tightLimits) $ \app _env _port -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["1.0.0"]

    it "serves normally when every document is within the bounds (no false refusal)" $ do
        -- The same tight budget admits an ordinary single-version packument on each
        -- leg: the bounds refuse only the pathological documents above, not a normal one.
        privateUp <- servingUpstream (encodePackument (privatePackument [("3.0.0", plainVersion "3.0.0")] "3.0.0"))
        publicUp <- servingUpstream (encodePackument (admittingPublic "1.0.0"))
        queue <- newInMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing (withLimits tightLimits) $ \app _env _port -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["1.0.0", "3.0.0"]

-- ── a bound breach is logged before degrading (observability) ──────────────────

{- | Run an 'IO' action with the process @stdout@ redirected to a temporary file,
returning everything written — so a katip scribe's output can be asserted with no
network. The original @stdout@ is restored on every exit path. (Mirrors the helper
in "Ecluse.LogSpec"; kept local rather than exported to avoid widening that module's
surface for a test-only utility.)
-}
captureStdout :: IO () -> IO Text
captureStdout act =
    withSystemTempFile "ecluse-pipeline-log.txt" $ \path tmpHandle ->
        bracket (hDuplicate stdout) restore $ \_saved -> do
            hFlush stdout
            hDuplicateTo tmpHandle stdout
            act
            hFlush stdout
            hClose tmpHandle
            decodeUtf8 <$> readFileBS path
  where
    restore saved = do
        hFlush stdout
        hDuplicateTo saved stdout
        hClose saved

{- | Drive @GET \/npm\/thing@ once against a proxy whose 'LogEnv' writes JSONL to the
real stdout scribe, with the given private-leg packument body and a __down__ public
leg, under the tight bounds — and return everything the scribe wrote. The private
body breaches a bound, so the serve path logs a WARNING before degrading; capturing
stdout (with the scribes drained) makes that line assertable.
-}
captureBreachLog :: LByteString -> IO Text
captureBreachLog privateBody = do
    privateUp <- servingUpstream privateBody
    publicUp <- failingUpstream
    queue <- newInMemoryQueue
    testWithApplication (pure (upApp privateUp)) $ \privatePort ->
        testWithApplication (pure (upApp publicUp)) $ \publicPort -> do
            manager <- newManager defaultManagerSettings
            metadataCache <- newMetadataCache defaultCacheConfig
            -- A LogEnv with the real JSONL stdout scribe, so the serve path's warning
            -- is actually written and capturable.
            logEnv <- newLogEnv JsonLog (Environment "test")
            heartbeat <- newWorkerHeartbeat
            env <- newEnv fakeRegistry queue fakeCredentials manager manager metadataCache logEnv telemetryDisabled heartbeat
            let cfg =
                    mkServerConfig
                        [ MountBinding
                            { bindingPrefix = "npm" :| []
                            , bindingClassifier = Npm.classify
                            , bindingPackumentDeps = Just (withLimits tightLimits (deps privatePort publicPort Nothing))
                            , bindingRenderer = npmRenderer
                            }
                        ]
            captureStdout $ do
                _ <- getThing Nothing (application cfg env)
                -- Draining the scribes blocks until the worker has flushed the queued
                -- line to stdout, making the capture deterministic (a katip handle
                -- scribe writes asynchronously). The returned, scribe-less LogEnv is
                -- discarded — the env is not reused.
                _ <- closeScribes logEnv
                pure ()

boundsLogSpec :: Spec
boundsLogSpec = describe "serve-path warnings are logged before degrading" $ do
    it "logs a WARNING naming the version-count bound, distinct from a plain parse failure" $ do
        -- A version flood on the private leg breaches the version-count bound; the
        -- public leg is down, so the request 503s (the degrade path). The breach must
        -- be logged at WARNING first, naming which ceiling and the package — so an
        -- operator can tell a bound breach from an ordinary parse failure or outage.
        logged <- captureBreachLog versionFloodPackument
        logged `shouldSatisfy` T.isInfixOf "\"sev\":\"Warning\""
        logged `shouldSatisfy` T.isInfixOf "\"bound\":\"version-count\""
        logged `shouldSatisfy` T.isInfixOf "\"package\":\"thing\""

    it "logs a WARNING naming the body-size bound on an oversized body" $ do
        -- The oversized private body breaches maxBodyBytes in the bounded read; the
        -- breach is raised from the fetch and logged here before the degrade.
        logged <- captureBreachLog (oversizedPackument "9.9.9")
        logged `shouldSatisfy` T.isInfixOf "\"sev\":\"Warning\""
        logged `shouldSatisfy` T.isInfixOf "\"bound\":\"body-size\""

    it "logs a WARNING naming the nesting-depth bound on a deeply-nested body" $ do
        -- The deeply-nested private body breaches maxNestingDepth at the decode check.
        logged <- captureBreachLog deeplyNestedBody
        logged `shouldSatisfy` T.isInfixOf "\"sev\":\"Warning\""
        logged `shouldSatisfy` T.isInfixOf "\"bound\":\"nesting-depth\""

    it "logs a WARNING on an undecodable upstream body (the case the bound guards leave silent)" $ do
        -- A body that is not valid JSON fails the decode, not a bound; it was silently
        -- degraded before, and must now log a WARNING distinct from a bound breach.
        logged <- captureBreachLog undecodableBody
        logged `shouldSatisfy` T.isInfixOf "\"sev\":\"Warning\""
        logged `shouldSatisfy` T.isInfixOf "did not decode"

    it "tags every serve-path log line with the emitting module" $ do
        -- The standardised `module` field names the emitter on the JSON line, so the
        -- stream can be filtered by source without leaning on the katip namespace.
        logged <- captureBreachLog versionFloodPackument
        logged `shouldSatisfy` T.isInfixOf "\"module\":\"Ecluse.Server.Pipeline\""

-- A mirror queue whose 'enqueue' always throws, for the best-effort assertion: the
-- serve path must swallow the failure and still serve the artifact. 'receive' is a
-- no-op returning no messages (nothing is ever enqueued).
newFailingQueue :: IO MirrorQueue
newFailingQueue = do
    queue <- newInMemoryQueue
    pure queue{enqueue = \_ -> throwString "enqueue failed (test double)"}

-- ── fixture/assert helpers shared across groups ───────────────────────────────

-- A private packument: like 'packument' but the trust split is the caller's, so it
-- carries old-enough times only incidentally (private versions are trusted
-- regardless of age — they skip the rules entirely).
privatePackument :: [(Text, Value)] -> Text -> Value
privatePackument versions latest =
    packument versions latest [(v, publishedDaysAgo 1) | (v, _) <- versions]

-- A private packument with explicit version objects (used for the divergence test).
privatePackumentWith :: [(Text, Value)] -> Text -> Value
privatePackumentWith = privatePackument

twoServingUpstreams :: IO (Upstream, Upstream)
twoServingUpstreams = do
    privateUp <- servingUpstream (encodePackument (privatePackument [("1.0.0", plainVersion "1.0.0")] "1.0.0"))
    publicUp <-
        servingUpstream
            (encodePackument (packument [("2.0.0", plainVersion "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 30)]))
    pure (privateUp, publicUp)

encodePackument :: Value -> LByteString
encodePackument = Aeson.encode

-- A served version object's @dist.integrity@.
servedIntegrity :: Text -> SResponse -> Maybe Text
servedIntegrity version resp = do
    Object o <- Just (decodedBody resp)
    Object vs <- KeyMap.lookup "versions" o
    Object vo <- KeyMap.lookup (Key.fromText version) vs
    Object dist <- KeyMap.lookup "dist" vo
    String i <- KeyMap.lookup "integrity" dist
    pure i

-- The value at a top-level @field@ within a served version object.
servedVersionKey :: Text -> Text -> SResponse -> Maybe Value
servedVersionKey version field resp = do
    Object o <- Just (decodedBody resp)
    Object vs <- KeyMap.lookup "versions" o
    Object vo <- KeyMap.lookup (Key.fromText version) vs
    KeyMap.lookup (Key.fromText field) vo
