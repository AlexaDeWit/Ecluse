{-# LANGUAGE RankNTypes #-}

module Ecluse.Server.Pipeline.TestSupport where

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
import Katip (Environment (Environment), Namespace (Namespace), initLogEnv)
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
import UnliftIO.Exception (throwString)

import Ecluse.Core.Credential (mkSecret)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Package.Integrity (defaultMinIntegrity, defaultMinTrustedIntegrity)
import Ecluse.Core.Queue (
    MirrorJob (jobArtifactUrl, jobMirrorTarget, jobPackage, jobVersion),
    MirrorQueue (enqueue, receive),
    QueueMessage (msgJob),
    newInMemoryQueue,
 )
import Ecluse.Core.Registry (ParseError (..), RegistryClient (..))
import Ecluse.Core.Registry.Npm (NpmClientConfig (..))
import Ecluse.Core.Registry.Npm.Filter (assembleMergedPackument)
import Ecluse.Core.Registry.Npm.Request (artifactRequestByFile, artifactRequestByUrl)
import Ecluse.Core.Registry.Npm.Route qualified as Npm
import Ecluse.Core.Registry.Npm.Serve (npmRenderer)
import Ecluse.Core.Rules (PreparedRule, prepare)
import Ecluse.Core.Rules.Types (
    PrecededRule,
    Rule (AllowIfOlderThan, DenyInstallTimeExecution),
    atDefaultPrecedence,
 )
import Ecluse.Core.Security (TarballHostPolicy (SameHostAsPackument), defaultLimits, lowerCaseHosts)
import Ecluse.Core.Server.Cache (defaultCacheConfig, newMetadataCache)
import Ecluse.Core.Server.Context (PackumentDeps (..))
import Ecluse.Core.Server.Metadata (newNpmMetadataClient)
import Ecluse.Core.Version (Version)
import Ecluse.Env (Env (envQueue), newEnv, newWorkerHeartbeat)
import Ecluse.Server (
    MountBinding (..),
    application,
    mkServerConfig,
 )
import Ecluse.Telemetry (telemetryDisabled)

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
    [ atDefaultPrecedence (AllowIfOlderThan (7 * nominalDay))
    , atDefaultPrecedence DenyInstallTimeExecution
    ]

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

{- | An upstream double that always answers @500@ -- a failed/unavailable upstream, for
the partial-upstream-availability and no-survivors paths.
-}
failingUpstream :: IO Upstream
failingUpstream = upstreamRespondingWith (responseLBS status500 [] "upstream error")

{- | An upstream double that serves a sequence of bodies -- its first for the first
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
-- in arrival order -- so a test can assert a HEAD request reached upstream as a HEAD
-- (never the body-pumping GET) and that no full-artifact GET was issued.
seenArtifactMethods :: Upstream -> IO [ByteString]
seenArtifactMethods up = reverse <$> readIORef (upSeenArtifactMethods up)

-- The @If-None-Match@ conditional validators an upstream double saw on its
-- artifact-slot (tarball) requests, in arrival order -- so a test can assert the
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

{- | Whether a request path is a tarball slot (@\/…\/-\/….tgz@) rather than a
packument. The artifact and packument fetches of a single upstream are distinguished
by this, so one double can answer both.
-}
isTarballPath :: ByteString -> Bool
isTarballPath path = "/-/" `BS.isInfixOf` path && ".tgz" `BS.isSuffixOf` path

{- | The base URL a request reached this in-process double at, recovered from its
@Host@ header (@http:\/\/{host:port}@). The serve path now honours the packument's
@dist.tarball@ rather than reconstructing it, so a double's packument must point its
tarball at __itself__ -- and only the @Host@ header names the ephemeral port the test
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
@dist.tarball@ points at @baseUrl@ -- the self-hosting form the artifact path fetches.
-}
selfHostedAdmitting :: Text -> Text -> Value
selfHostedAdmitting baseUrl v =
    packument [(v, selfHostedVersion baseUrl v)] v [(v, publishedDaysAgo 30)]

{- | A path-aware upstream double: it answers a tarball-slot path with @200@ and the
given artifact bytes, and any other path (the packument fetch) with @200@ and a
packument whose @dist.tarball@ for version @v@ points back at this double (so the
serve path's honour-the-URL fetch returns here). Records each request's
@Authorization@ header. The single double thus serves both fetches the public
artifact path consults -- the gating packument and the artifact itself.
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

{- | A private upstream double that has the artifact: it answers the conventional
tarball slot (@\/…\/-\/….tgz@) with @200@ and the given bytes (a __private hit__). The
private tarball leg reads that conventional URL directly (no packument fetch); the
double also serves a self-referential single-version packument on other paths, which the
tarball leg never requests.
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

{- | The shared private-hit double: a tarball-slot path returns @200@ with the given
artifact bytes and extra headers (a __private hit__ on the conventional read); a
non-tarball path returns a self-referential single-version packument the tarball leg
never requests.
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

{- | A private upstream double that has a __hashless__ artifact: a tarball-slot path
returns @200@ with the given bytes, and a non-tarball path a single-version packument
carrying neither @integrity@ nor @shasum@. The private tarball leg reads the conventional
URL and applies no serve-time integrity floor, so a hashless private artifact streams
through (no fall-through to public).
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

{- | A private upstream double that has a __SHA-1-only__ artifact: a tarball-slot path
returns @200@ with the given bytes, and a non-tarball path a single-version packument
carrying a legacy @shasum@ but no SRI @integrity@ (a digest below the default SHA-256
floor). The private tarball leg reads the conventional URL and applies no serve-time
integrity floor, so this is served from the private origin regardless of the floor.
-}
privateArtifactHitShasumOnly :: Text -> LByteString -> IO Upstream
privateArtifactHitShasumOnly version tarballBody = do
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
                                (packument [(version, selfHostedShasumOnly (selfBaseUrl req) version)] version [(version, publishedDaysAgo 1)])
    mkUpstream seen app

{- | A private upstream double that does __not__ hold the artifact bytes: a tarball-slot
path is a @404@ miss, so the private tarball leg's conventional read misses and the
request falls through to the public origin. (It also answers a non-tarball path with a
self-referential packument, unused by the tarball leg.)
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
@200@ that __promises far more than it delivers__ -- a large @Content-Length@ but
only a little body -- and then returns, so Warp closes the socket and the proxy's
read of the artifact hits EOF short of the promised length and fails at once. Any
other path is a @404@. The handler never throws (so @testWithApplication@ does not
surface a server-side error), and the immediate close keeps the short read from
stalling on a read timeout. It exercises the committed-stream case -- once the @200@
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
    -- fails immediately -- no exception thrown here, no timeout waited on.
    truncatedArtifact :: IO ByteString -> (ByteString -> IO ()) -> IO ()
    truncatedArtifact _recv send = do
        send "HTTP/1.1 200 OK\r\nContent-Length: 1048576\r\n\r\n"
        send (BS.replicate 1024 0x7a)

{- | A minimal npm packument body for @thing@ with the given version objects, a
@dist-tags.latest@, a @time@ map (built from the same versions), and an unmodeled
top-level key -- so a test can assert the unmodeled key is relayed unchanged.
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
dist-tag reconciliation -- not digest realism -- so a deterministic well-formed digest
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
digest is SHA-1 -- below the default SHA-256 floor. The integrity-floor admission policy
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
absent, so this projects to an artifact with empty @artHashes@ -- identical to
'hashlessVersion' -- and the integrity-presence admission policy refuses it from a public
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

{- | A hashless version object whose @dist.tarball@ points at @baseUrl@ -- the
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

{- | A registry-handle double whose fields are never invoked (the pipeline talks to
upstreams directly via the npm client over the shared 'Manager', not the handle).
-}
fakeRegistry :: RegistryClient
fakeRegistry =
    RegistryClient
        { fetchMetadata = const (refuse "fetchMetadata")
        , fetchArtifact = \_ _ -> refuse "fetchArtifact"
        , publishArtifact = \_ _ _ _ -> refuse "publishArtifact"
        , parsePackageInfo = \_ _ -> Left (ParseError "unused")
        , parseVersionDetails = \_ _ -> Left (ParseError "unused")
        , parseVersionList = const (Left (ParseError "unused"))
        }
  where
    refuse :: Text -> IO a
    refuse field = throwString (toString ("fakeRegistry: the pipeline must not use the handle field " <> field))

{- | A fresh 'Env' over handle doubles and a real (no-TLS) manager for the in-process
upstream doubles, carrying the given mirror queue (the in-memory double, or one
rigged to fail for the best-effort-enqueue assertion).
-}
newTestEnvWithQueue :: MirrorQueue -> Manager -> IO Env
newTestEnvWithQueue queue manager = do
    metadataCache <- newMetadataCache defaultCacheConfig
    logEnv <- initLogEnv (Namespace ["ecluse"]) (Environment "test")
    heartbeat <- newWorkerHeartbeat
    newEnv fakeRegistry queue manager manager metadataCache logEnv telemetryDisabled heartbeat

{- | The packument-serve dependencies pointing at two in-process upstream ports,
with the given inbound edge token (usually 'Nothing').

The in-process upstream doubles bind loopback, so @127.0.0.1@ is opted in to the
internal-range block for the __public__ leg's honoured-tarball gate -- the unit-test
analogue of an operator deliberately opting an internal /public/ host in. The
__private__ leg needs no opt-in: it gates as the trusted origin, exempt from the literal
internal-range block on the `dist.tarball` host gate; a test empties this set to assert
that exemption directly. The default tarball-host policy is the secure 'SameHostAsPackument';
a test overrides it where it exercises the cross-host relaxation.
-}
deps :: Int -> Int -> Maybe Text -> IO PackumentDeps
deps privatePort publicPort inbound = do
    prepared <- prepare policy
    pure
        PackumentDeps
            { pdPrivateBaseUrl = localhost privatePort
            , pdPublicBaseUrl = localhost publicPort
            , pdMountBaseUrl = "https://proxy.test"
            , pdMirrorTarget = "https://mirror.test"
            , pdRules = prepared
            , pdTarballHostPolicy = SameHostAsPackument
            , pdAllowedInternalHosts = lowerCaseHosts (Set.singleton "127.0.0.1")
            , pdLimits = defaultLimits
            , pdInboundToken = mkSecret <$> inbound
            , pdNow = pure now
            , pdHelp = Nothing
            , pdMinIntegrity = defaultMinIntegrity
            , pdMinTrustedIntegrity = defaultMinTrustedIntegrity
            , pdNewMetadataClient = \t p u c f1 f2 f3 l m b s -> newNpmMetadataClient t p u c f1 f2 f3 (NpmClientConfig b m s l)
            , pdBuildArtifactRequestByFile = \_ _ t s -> artifactRequestByFile t s
            , pdBuildArtifactRequestByUrl = \_ _ t s -> artifactRequestByUrl t s
            , pdAssemble = assembleMergedPackument
            }

{- | The packument-serve dependencies as 'deps', but with the given effectful prepared
rules appended to the prepared policy, so a test can drive an effectful rule end to end
through the unified engine.
-}
depsWith :: [PreparedRule] -> Int -> Int -> IO PackumentDeps
depsWith effectful privatePort publicPort = do
    base <- deps privatePort publicPort Nothing
    prepared <- prepare policy
    pure base{pdRules = prepared <> effectful}

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
the given transform first -- so a test can break one origin's base URL (an unformable
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
            baseDeps <- deps privatePort publicPort inbound
            let cfg =
                    mkServerConfig
                        [ MountBinding
                            { bindingPrefix = "npm" :| []
                            , bindingClassifier = Npm.classify
                            , bindingPackumentDeps = Just (tweakDeps baseDeps)
                            , bindingPublishDeps = Nothing
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
rules, so a request flows through the unified engine. The two upstream doubles are
hosted on ephemeral ports as elsewhere; the effectful rules see the public version.
-}
withProxyEffectful ::
    [PreparedRule] ->
    Upstream ->
    Upstream ->
    (forall a. (Application -> IO a) -> IO a)
withProxyEffectful effectful privateUp publicUp k = do
    queue <- newInMemoryQueue
    testWithApplication (pure (upApp privateUp)) $ \privatePort ->
        testWithApplication (pure (upApp publicUp)) $ \publicPort -> do
            manager <- newManager defaultManagerSettings
            env <- newTestEnvWithQueue queue manager
            effectfulDeps <- depsWith effectful privatePort publicPort
            let cfg =
                    mkServerConfig
                        [ MountBinding
                            { bindingPrefix = "npm" :| []
                            , bindingClassifier = Npm.classify
                            , bindingPackumentDeps = Just effectfulDeps
                            , bindingPublishDeps = Nothing
                            , bindingRenderer = npmRenderer
                            }
                        ]
            k (application cfg env)

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

{- | A @HEAD /npm/thing@ request carrying the given (optional) bearer credential -- the
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
(optional) bearer credential -- the tarball path for @thing@ at one version.
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
(optional) bearer credential -- the same tarball coordinate as 'getTarball', issued
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
status' message, which the @mkStatus@-built serve statuses carry as a lazy field --
so an assertion over it exercises the per-status reason mapping the serve path
threads through, not just the numeric code. -}
reason :: SResponse -> ByteString
reason = statusMessage . simpleStatus

header :: ByteString -> SResponse -> Maybe ByteString
header name resp = snd <$> find ((== CI.mk name) . fst) (simpleHeaders resp)

-- A private packument: like 'packument' but the trust split is the caller's, so it
-- carries old-enough times only incidentally (private versions are trusted
-- regardless of age -- they skip the rules entirely).
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
        -- names @crossHost@ at this same port -- so the policy sees a cross-host URL.
        crossHostPackument req =
            let port = snd (T.breakOnEnd ":" (decodeUtf8 (maybe "" snd (find ((== hHost) . fst) (requestHeaders req)))))
                tarballBase = "http://" <> crossHost <> ":" <> port
             in selfHostedAdmitting tarballBase version
    mkUpstream seen app

{- | A double whose version's @dist.tarball@ sits at a __non-conventional path__
(@\/files\/{filename}@, not the npm @\/-\/@ slot) on its own host, with the given
@filename@ as the artifact name. The serve path honours that exact URL rather than
reconstructing @{base}\/{pkg}\/-\/{file}@, so the double serves the bytes at the
honoured path (matched by @.tgz@ suffix) -- proving the location is honoured, not
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

{- | A private upstream double whose tarball lives __only__ at an off-convention
@\/files\/{filename}@ path (a separate files host / CDN shape), @404@ing every other
path including the conventional @\/-\/@ tarball slot. The private tarball leg reads the
conventional @{base}\/{pkg}\/-\/{file}@ URL directly, so it never reaches these bytes:
the conventional read @404@s and the request falls through to the public origin.
-}
offConventionPrivateUpstream :: Text -> LByteString -> IO Upstream
offConventionPrivateUpstream filename tarballBody = do
    seen <- newIORef []
    let app :: Application
        app req respond = do
            modifyIORef' seen (lookupAuth (requestHeaders req) :)
            respond $
                if rawPathInfo req == encodeUtf8 ("/files/" <> filename)
                    then responseLBS status200 [] tarballBody
                    else responseLBS status404 [] "not found"
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

newFailingQueue :: IO MirrorQueue
newFailingQueue = do
    queue <- newInMemoryQueue
    pure queue{enqueue = \_ -> throwString "enqueue failed (test double)"}
