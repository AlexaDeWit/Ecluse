-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE RankNTypes #-}

module Ecluse.Server.Pipeline.TestSupport where

import Prelude hiding (get)

import Data.Aeson (Value (Null, Object, String), eitherDecodeStrict, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian, nominalDay)
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

import Ecluse (mountBindingFor)
import Ecluse.Core.Credential (mkSecret)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Fault (TransportCause (TransportUnreachable), transportFault)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Queue (
    MirrorJob (jobArtifactFilename, jobArtifactUrl, jobPackage, jobVersion),
    MirrorQueue (enqueue, receive),
    QueueMessage (msgJob),
    queueTransportFault,
 )
import Ecluse.Core.Rules (PreparedRule, prepare)
import Ecluse.Core.Rules.Types (
    PrecededRule,
    Rule (AllowIfOlderThan, DenyInstallTimeExecution),
 )
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Context (PackumentDeps (..))
import Ecluse.Core.Server.Upstream (MirrorServePlan (MirrorOnAdmit))
import Ecluse.Core.Version (Version)
import Ecluse.Runtime.Env (Env (envQueue))
import Ecluse.Runtime.Server (
    application,
    mkServerConfig,
 )
import Ecluse.Runtime.Telemetry (telemetryDisabled)
import Ecluse.Runtime.Test.Support (newTestEnvWith)
import Ecluse.Test.Package (sriSha256Of, sriSha512Of)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Registry.Npm (VersionSpec (..), packumentValue, versionSpec, versionValue)
import Ecluse.Test.Registry.Npm qualified as NpmFixture (publishedDaysAgo)
import Ecluse.Test.Rules (atDefaultPrecedence, inertRuleDeps)
import Ecluse.Test.Server.Mount (npmServeDeps, withEcosystemHosts)

-- | A fixed "now" so the age-based admit/deny axis is deterministic under test.
now :: UTCTime
now = UTCTime (fromGregorian 2026 6 20) 0

{- | An ISO-8601 instant @ageDays@ before 'now' (the npm @time@ string), so only a
version's fixture time decides its survival under the quarantine.
-}
publishedDaysAgo :: Integer -> Text
publishedDaysAgo = NpmFixture.publishedDaysAgo now

-- | The policy under test: a 7-day publish-age quarantine plus an install-script deny.
policy :: [PrecededRule]
policy =
    [ atDefaultPrecedence (AllowIfOlderThan (7 * nominalDay))
    , atDefaultPrecedence DenyInstallTimeExecution
    ]

{- | An in-process upstream double that serves a fixed response and records what each request
carried. Tests read the @Authorization@ header, the artifact-slot method, and its @If-None-
Match@ validator.
-}
data Upstream = Upstream
    { upApp :: Application
    , upSeenAuth :: IORef [Maybe ByteString]
    , upSeenArtifactMethods :: IORef [ByteString]
    , upSeenArtifactValidators :: IORef [Maybe ByteString]
    }

-- | An upstream double serving a fixed packument body with @200@.
servingUpstream :: LByteString -> IO Upstream
servingUpstream body = upstreamRespondingWith (responseLBS status200 [] body)

{- | An upstream double that always answers @500@: a failed or unavailable upstream,
for the partial-upstream-availability and no-survivors paths.
-}
failingUpstream :: IO Upstream
failingUpstream = upstreamRespondingWith (responseLBS status500 [] "upstream error")

{- | An upstream double serving each given body in turn, holding on the last once exhausted.
A test changes what upstream returns between two requests inside the cache TTL.
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

-- The HTTP methods an upstream double saw on artifact-slot requests, in arrival order. A test
-- asserts a HEAD reached upstream as a HEAD, never as a body-pumping GET.
seenArtifactMethods :: Upstream -> IO [ByteString]
seenArtifactMethods up = reverse <$> readIORef (upSeenArtifactMethods up)

-- The @If-None-Match@ validators an upstream double saw on artifact-slot requests, in arrival
-- order. 'Nothing' for a request that carried none.
seenArtifactValidators :: Upstream -> IO [Maybe ByteString]
seenArtifactValidators up = reverse <$> readIORef (upSeenArtifactValidators up)

-- The @If-None-Match@ header value a request carried, if any.
lookupIfNoneMatch :: [Header] -> Maybe ByteString
lookupIfNoneMatch headers = snd <$> find ((== hIfNoneMatch) . fst) headers

{- | Assemble an 'Upstream' over a double that already records @Authorization@ into the given ref.
It layers the artifact-slot method and @If-None-Match@ recording on uniformly.
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

{- | Whether a request path is a tarball slot (@\/…\/-\/….tgz@) rather than a packument, so one
double can answer both.
-}
isTarballPath :: ByteString -> Bool
isTarballPath path = "/-/" `BS.isInfixOf` path && ".tgz" `BS.isSuffixOf` path

{- | The base URL a request reached this double at, from its @Host@ header, the only place the
harness's ephemeral port appears. An absent header falls back to @http:\/\/localhost@.
-}
selfBaseUrl :: Request -> Text
selfBaseUrl req =
    case find ((== hHost) . fst) (requestHeaders req) of
        Just (_, hostPort) -> "http://" <> decodeUtf8 hostPort
        Nothing -> "http://localhost"

{- | A version object whose @dist.tarball@ addresses the given base URL's tarball slot, with a
distinct integrity and no install script. The serve path fetches that exact URL.
-}
selfHostedVersion :: Text -> Text -> Value
selfHostedVersion baseUrl version =
    versionValue
        ( (versionFixture version (baseUrl <> "/thing/-/thing-" <> version <> ".tgz"))
            { vsIntegrity = Just (sriFor version)
            , vsShasum = Just validShasum
            }
        )

{- | An admitting public packument (single old-enough version @v@) whose
@dist.tarball@ points at @baseUrl@: the self-hosting form the artifact path fetches.
-}
selfHostedAdmitting :: Text -> Text -> Value
selfHostedAdmitting baseUrl v =
    packument [(v, selfHostedVersion baseUrl v)] v [(v, publishedDaysAgo 30)]

{- | A path-aware upstream double answering a tarball slot with the given artifact bytes, and any
other path with a packument whose @dist.tarball@ for @version@ points back at this double.
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

{- | Like 'artifactUpstream' but answering the artifact slot with the given arbitrary response, for
the public-relay verdict cases.
-}
artifactUpstreamAnswering :: Text -> Response -> IO Upstream
artifactUpstreamAnswering version artifactResponse = do
    seen <- newIORef []
    let app :: Application
        app req respond = do
            modifyIORef' seen (lookupAuth (requestHeaders req) :)
            respond $
                if isTarballPath (rawPathInfo req)
                    then artifactResponse
                    else responseLBS status200 [] (encodePackument (selfHostedAdmitting (selfBaseUrl req) version))
    mkUpstream seen app

{- | Like 'artifactUpstream' but serving the given packument body verbatim, for tests that shape the
gating packument themselves.
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

{- | A private upstream double that has the artifact: a tarball slot answers @200@ with the given
bytes. The private tarball leg reads that conventional URL directly, with no packument fetch.
-}
privateArtifactHit :: Text -> LByteString -> IO Upstream
privateArtifactHit version = privateArtifactHitWith version []

{- | A private hit that also tags the artifact with one upstream header, so a test can assert the
relay forwards the artifact's own content headers through.
-}
privateArtifactHitWithHeader :: ByteString -> ByteString -> Text -> LByteString -> IO Upstream
privateArtifactHitWithHeader headerName headerValue version =
    privateArtifactHitWith version [(CI.mk headerName, headerValue)]

{- | The shared private-hit double: a tarball slot answers @200@ with the given bytes and extra
headers, any other path a self-referential single-version packument.
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

{- | A private hit whose packument carries neither @integrity@ nor @shasum@. The private tarball leg
applies no serve-time integrity floor, so a hashless private artifact streams through.
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

{- | A private hit whose packument carries a legacy @shasum@ but no SRI @integrity@, below the
default SHA-256 floor. The private leg applies no serve-time floor, so this artifact still serves.
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

{- | A private upstream double that does not hold the artifact: a tarball slot is a @404@, so the
request falls through to the public origin.
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

{- | A private upstream double whose tarball slot answers @200@, declares far more body than it
sends, then closes. Once the @200@ is on the wire the serve path must fail, never fall through.
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
    -- Warp closes the raw socket once this returns, so the proxy reads EOF short of the declared
    -- Content-Length and fails immediately: no exception thrown here, no timeout waited on.
    truncatedArtifact :: IO ByteString -> (ByteString -> IO ()) -> IO ()
    truncatedArtifact _recv send = do
        send "HTTP/1.1 200 OK\r\nContent-Length: 1048576\r\n\r\n"
        send (BS.replicate 1024 0x7a)

{- | A minimal npm packument body for @thing@, carrying an unmodeled top-level key. A test asserts
the serve path relays that key unchanged.
-}
packument :: [(Text, Value)] -> Text -> [(Text, Text)] -> Value
packument versions latest times =
    packumentValue
        "thing"
        latest
        versions
        (("created" .= publishedDaysAgo 400) : [(Key.fromText version, String time) | (version, time) <- times])
        ["_id" .= ("thing" :: Text)] -- an unmodeled top-level key

{- | A packument like 'packument' but self-reporting a different top-level @name@. The pipeline
validates out a packument named anything but @thing@ and drops its contribution.
-}
packumentNamed :: Text -> [(Text, Value)] -> Text -> [(Text, Text)] -> Value
packumentNamed nm versions latest times =
    case packument versions latest times of
        Object o -> Object (KeyMap.insert "name" (String nm) o)
        v -> v

{- | A version object with a @dist@ tarball URL and @integrity@, plus an unmodeled per-version key.
The @scripts@ field flags an install script when asked.
-}
versionObject :: Text -> Text -> Bool -> Value
versionObject version integrity hasInstall =
    versionValue
        ( (versionFixture version ("https://upstream.example/thing/-/thing-" <> version <> ".tgz"))
            { vsIntegrity = Just integrity
            , vsShasum = Just validShasum
            , vsHasInstallScript = hasInstall
            }
        )

-- A @thing@ version fixture with the unmodelled field the relay assertions preserve.
versionFixture :: Text -> Text -> VersionSpec
versionFixture version tarballUrl =
    (versionSpec "thing" version tarballUrl)
        { vsExtraPairs = ["_unmodeled" .= ("kept" :: Text)]
        }

-- A plain (no-install-script) version object with a distinct integrity.
plainVersion :: Text -> Value
plainVersion version = versionObject version (sriFor version) False

{- | A well-formed sha512 (resp. sha256) SRI derived from a label. These tests cover admission and
the merge, never digest realism, so a deterministic 'mkHash'-constructible digest stands in.
-}
sriFor, sri256For :: Text -> Text
sriFor = sriSha512Of . encodeUtf8
sri256For = sriSha256Of . encodeUtf8

-- | A well-formed 40-hex SHA-1 shasum (sha1 of the empty string) for the dist fixtures.
validShasum :: Text
validShasum = "da39a3ee5e6b4b0d3255bfef95601890afd80709"

{- | A version object carrying only a legacy SHA-1 @shasum@ and no @integrity@. The integrity floor
refuses such a version from a public upstream, and exempts a private one.
-}
shasumOnlyVersion :: Text -> Value
shasumOnlyVersion version =
    versionValue
        ( (versionFixture version ("https://upstream.example/thing/-/thing-" <> version <> ".tgz"))
            { vsShasum = Just validShasum
            }
        )

{- | A version object carrying neither @integrity@ nor @shasum@. The integrity-presence policy
refuses such a version from a public upstream.
-}
hashlessVersion :: Text -> Value
hashlessVersion version =
    versionValue (versionFixture version ("https://upstream.example/thing/-/thing-" <> version <> ".tgz"))

{- | A version whose @dist@ carries empty-string @integrity@ and @shasum@. The projection normalises
an empty digest to absent, so admission refuses it as 'NoIntegrity', not 'BelowFloor'.
-}
emptyDigestVersion :: Text -> Value
emptyDigestVersion version =
    versionValue
        ( (versionFixture version ("https://upstream.example/thing/-/thing-" <> version <> ".tgz"))
            { vsIntegrity = Just ""
            , vsShasum = Just ""
            }
        )

{- | A hashless version whose @dist.tarball@ points at @baseUrl@, the self-hosting form the artifact
path fetches. The artifact gate must refuse it before anything fetches that URL.
-}
selfHostedHashless :: Text -> Text -> Value
selfHostedHashless baseUrl version =
    versionValue (versionFixture version (baseUrl <> "/thing/-/thing-" <> version <> ".tgz"))

{- | A self-hosting version object carrying __only a legacy SHA-1 shasum__ (no SRI
@integrity@), so its strongest digest is below the default floor. The artifact-gate
refusal (@BelowIntegrityFloor@) must fire before anything fetches its @dist.tarball@.
-}
selfHostedShasumOnly :: Text -> Text -> Value
selfHostedShasumOnly baseUrl version =
    versionValue
        ( (versionFixture version (baseUrl <> "/thing/-/thing-" <> version <> ".tgz"))
            { vsShasum = Just validShasum
            }
        )

{- | A self-hosting version object whose @dist@ carries __empty-string__ @integrity@ and
@shasum@, so it projects to no digest at all. The artifact-gate refusal ('MissingIntegrity')
must fire before anything fetches its @dist.tarball@.
-}
selfHostedEmptyDigest :: Text -> Text -> Value
selfHostedEmptyDigest baseUrl version =
    versionValue
        ( (versionFixture version (baseUrl <> "/thing/-/thing-" <> version <> ".tgz"))
            { vsIntegrity = Just ""
            , vsShasum = Just ""
            }
        )

-- | A fresh 'Env' over handle doubles and a real (no-TLS) manager, carrying the given mirror queue.
newTestEnvWithQueue :: MirrorQueue -> Manager -> IO Env
newTestEnvWithQueue queue manager = newTestEnvWith queue (manager, manager) telemetryDisabled

{- | The packument-serve dependencies over two in-process upstream ports, with the given inbound
edge token. The doubles bind loopback as @localhost@, never the @127.0.0.1@ literal, because
the internal-range block matches only an IP literal, so a hostname-addressed double never trips
it.
-}
deps :: Int -> Int -> Maybe Text -> IO PackumentDeps
deps privatePort publicPort inbound = do
    prepared <- prepare inertRuleDeps policy
    pure
        (npmServeDeps (Just (localhost privatePort)) (localhost publicPort) (MirrorOnAdmit "https://mirror.test") prepared (pure now))
            { pdInboundToken = mkSecret <$> inbound
            , pdEgressUrl = Right . loopbackRegistryUrl
            }

{- | As 'deps', but appends the given effectful prepared rules to the prepared policy, so a test can
drive an effectful rule end to end through the unified engine.
-}
depsWith :: [PreparedRule] -> Int -> Int -> IO PackumentDeps
depsWith effectful privatePort publicPort = do
    base <- deps privatePort publicPort Nothing
    prepared <- prepare inertRuleDeps policy
    pure base{pdRules = prepared <> effectful}

localhost :: Int -> Text
localhost port = "http://localhost:" <> show port

{- | Run an assertion against a proxy over the two in-process upstream doubles and the given mirror
queue. Warp hosts the doubles on ephemeral ports. The test drives the proxy through a WAI session.
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

{- | Like 'withProxyEnvQueue', but the mount's 'PackumentDeps' passes through the given transform
first, so a test can break one origin's base URL without a new harness.
-}
withProxyEnvQueueDeps ::
    MirrorQueue ->
    Upstream ->
    Upstream ->
    Maybe Text ->
    (PackumentDeps -> PackumentDeps) ->
    (forall a. (Application -> Env -> Int -> IO a) -> IO a)
withProxyEnvQueueDeps queue privateUp publicUp inbound =
    withProxyEnvQueueDepsHosts queue privateUp publicUp inbound (const [])

{- | Like 'withProxyEnvQueueDeps', but the tarball-host gate also declares ecosystem artifact hosts,
computed from the tweaked deps so a host can carry an upstream double's ephemeral port.
-}
withProxyEnvQueueDepsHosts ::
    MirrorQueue ->
    Upstream ->
    Upstream ->
    Maybe Text ->
    (PackumentDeps -> [Text]) ->
    (PackumentDeps -> PackumentDeps) ->
    (forall a. (Application -> Env -> Int -> IO a) -> IO a)
withProxyEnvQueueDepsHosts queue privateUp publicUp inbound hostsOf tweakDeps k =
    testWithApplication (pure (upApp privateUp)) $ \privatePort ->
        testWithApplication (pure (upApp publicUp)) $ \publicPort -> do
            manager <- newManager defaultManagerSettings
            env <- newTestEnvWithQueue queue manager
            baseDeps <- deps privatePort publicPort inbound
            let tweaked = tweakDeps baseDeps
                cfg = mkServerConfig (maybeToList (mountBindingFor Npm (withEcosystemHosts (hostsOf tweaked) tweaked) Nothing))
            k (application cfg env) env publicPort

{- | Run an assertion against a proxy over the two upstream doubles and the proxy's own 'Env'. That
'Env' carries an in-memory queue, so a test can drain the enqueued mirror jobs.
-}
withProxyEnv ::
    Upstream ->
    Upstream ->
    Maybe Text ->
    (forall a. (Application -> Env -> IO a) -> IO a)
withProxyEnv privateUp publicUp inbound k = do
    queue <- newTestMemoryQueue
    withProxyEnvQueue queue privateUp publicUp inbound (\app env _port -> k app env)

-- | Run an assertion against a proxy over the two in-process upstream doubles, without its 'Env'.
withProxy ::
    Upstream ->
    Upstream ->
    Maybe Text ->
    (forall a. (Application -> IO a) -> IO a)
withProxy privateUp publicUp inbound k =
    withProxyEnv privateUp publicUp inbound (\app _env -> k app)

{- | Run an assertion against a proxy whose npm mount carries the given effectful rules, so a
request flows through the unified engine. The effectful rules see the public version.
-}
withProxyEffectful ::
    [PreparedRule] ->
    Upstream ->
    Upstream ->
    (forall a. (Application -> IO a) -> IO a)
withProxyEffectful effectful privateUp publicUp k = do
    queue <- newTestMemoryQueue
    testWithApplication (pure (upApp privateUp)) $ \privatePort ->
        testWithApplication (pure (upApp publicUp)) $ \publicPort -> do
            manager <- newManager defaultManagerSettings
            env <- newTestEnvWithQueue queue manager
            effectfulDeps <- depsWith effectful privatePort publicPort
            let cfg = mkServerConfig (maybeToList (mountBindingFor Npm effectfulDeps Nothing))
            k (application cfg env)

-- | A @GET@ at the given path with no credential: the arbitrary-path generalisation of 'getThing'.
getPath :: ByteString -> Application -> IO SResponse
getPath path = runSession (request (setPath defaultRequest path))

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

{- | A @HEAD /npm/thing@ carrying the given optional bearer credential. The serve path must answer
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
(optional) bearer credential: the tarball path for @thing@ at one version.
-}
getTarball :: Text -> Maybe Text -> Application -> IO SResponse
getTarball version bearer =
    runSession (request (setPath baseRequest path))
  where
    path = "/npm/thing/-/thing-" <> encodeUtf8 version <> ".tgz"
    baseRequest =
        defaultRequest{requestHeaders = maybe [] (\t -> [(hAuthorization, "Bearer " <> encodeUtf8 t)]) bearer}

{- | A @GET /npm/thing/-/thing-{version}.tgz@ with no credential and the given extra request headers
(e.g. a conditional @If-None-Match@), to drive the pass-through conditional-GET relay.
-}
getTarballWith :: Text -> [Header] -> Application -> IO SResponse
getTarballWith version extra =
    runSession (request (setPath defaultRequest{requestHeaders = extra} path))
  where
    path = "/npm/thing/-/thing-" <> encodeUtf8 version <> ".tgz"

{- | A @HEAD /npm/thing/-/thing-{version}.tgz@ carrying the given optional bearer credential. The
serve path must answer without pumping the full artifact body.
-}
headTarball :: Text -> Maybe Text -> Application -> IO SResponse
headTarball version bearer =
    runSession (request (setPath baseRequest path){requestMethod = methodHead})
  where
    path = "/npm/thing/-/thing-" <> encodeUtf8 version <> ".tgz"
    baseRequest =
        defaultRequest{requestHeaders = maybe [] (\t -> [(hAuthorization, "Bearer " <> encodeUtf8 t)]) bearer}

{- | Drain every mirror job currently enqueued on the proxy's queue, in FIFO order. The backend
delivers batches up to its cap per receive, so this polls until an empty batch.
-}
drainJobs :: Env -> IO [MirrorJob]
drainJobs env = go []
  where
    go acc =
        receive (envQueue env) >>= \case
            Right [] -> pure (reverse acc)
            Right messages -> go (reverse (map msgJob messages) <> acc)
            Left fault -> fail ("drainJobs: the in-memory queue faulted: " <> show fault)

-- The decoded JSON body of a proxy response, or 'Null' if it did not decode. A
-- non-JSON body then surfaces as a plain assertion mismatch, not a crash.
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

{- The HTTP reason phrase of a response (e.g. @"Forbidden"@). Reading it forces the status' lazy
message, so an assertion covers the per-status reason mapping, not just the numeric code. -}
reason :: SResponse -> ByteString
reason = statusMessage . simpleStatus

header :: ByteString -> SResponse -> Maybe ByteString
header name resp = snd <$> find ((== CI.mk name) . fst) (simpleHeaders resp)

-- A private packument. Its publish times are incidental: the pipeline skips the rules for a
-- private version, so it trusts one whatever its age.
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

-- A public packument whose single version clears the quarantine. On the packument path the serve
-- path only relays its dist.tarball under the mount base and never fetches it.
admittingPublic :: Text -> Value
admittingPublic v = packument [(v, plainVersion v)] v [(v, publishedDaysAgo 30)]

{- | A path-aware public double whose packument names its @dist.tarball@ on a different host than
the one that served the packument, while it still serves the tarball bytes itself. Pass a
@*.localhost@ alias, which RFC 6761 reserves for loopback, so it resolves with no hosts entry.
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
        -- The dist.tarball names @crossHost@ at this same port, so the policy sees a cross-host
        -- URL.
        crossHostPackument req =
            let port = snd (T.breakOnEnd ":" (decodeUtf8 (maybe "" snd (find ((== hHost) . fst) (requestHeaders req)))))
                tarballBase = "http://" <> crossHost <> ":" <> port
             in selfHostedAdmitting tarballBase version
    mkUpstream seen app

{- | A double whose @dist.tarball@ sits at an off-convention @\/files\/{filename}@ path.
It serves the bytes at any @.tgz@ path, so a test proves the serve path honours that URL rather than
rebuilding @{base}\/{pkg}\/-\/{file}@.
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
            let vo =
                    versionValue
                        ( (versionSpec "thing" version (base <> "/files/" <> filename))
                            { vsIntegrity = Just (sriFor version)
                            }
                        )
             in packument [(version, vo)] version [(version, publishedDaysAgo 30)]
    mkUpstream seen app

{- | A private double whose tarball lives only at @\/files\/{filename}@ and @404@s every other path,
including the conventional @\/-\/@ slot. The private leg reads the conventional URL, so it
misses and the request falls through to the public origin.
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

{- | A path-aware double that honours a conditional artifact request. A tarball path answers a
bodiless @304@ with an @ETag@ when the request carries @If-None-Match@, and @200@ with the bytes
otherwise, so a test drives the pass-through conditional-GET relay end to end.
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
    -- A relayed client validator turns the upstream artifact fetch into a 304. An
    -- unconditional fetch still serves the bytes.
    conditionalTarball :: [Header] -> Response
    conditionalTarball headers
        | any ((== hIfNoneMatch) . fst) headers = responseLBS status304 [(hETag, "\"v1\"")] ""
        | otherwise = responseLBS status200 [] tarballBody

-- A flat projection of a mirror job, for an order-stable equality assertion over
-- the coordinates the queued worker consumes.
jobShape :: MirrorJob -> (PackageName, Version, Text, Text)
jobShape job = (jobPackage job, jobVersion job, registryUrlText (jobArtifactUrl job), jobArtifactFilename job)

newFailingQueue :: IO MirrorQueue
newFailingQueue = do
    queue <- newTestMemoryQueue
    -- The typed producer channel: a backend fault is the 'Left' value the serve
    -- path's best-effort enqueue counts and swallows.
    pure queue{enqueue = \_ -> pure (Left (queueTransportFault (transportFault TransportUnreachable "enqueue failed (test double)")))}
