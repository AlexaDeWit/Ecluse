{-# LANGUAGE RankNTypes #-}

module Ecluse.Server.PipelineSpec (spec) where

import Prelude hiding (get)

import Data.Aeson (Value (Null, Object, String), eitherDecodeStrict, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.Time (UTCTime (UTCTime), addUTCTime, fromGregorian, nominalDay)
import Data.Time.Format.ISO8601 (iso8601Show)
import Katip (Environment (Environment), Namespace (Namespace), initLogEnv)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.HTTP.Types (Header, hAuthorization, status200, status404, status500, statusCode)
import Network.Wai (Application, Request (rawPathInfo, requestHeaders), Response, responseLBS, responseRaw)
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Test (
    SResponse (simpleBody, simpleHeaders, simpleStatus),
    defaultRequest,
    request,
    runSession,
    setPath,
 )
import Test.Hspec
import UnliftIO.Exception (throwString, try)

import Ecluse.Credential (AuthToken (..), CredentialProvider, mkSecret, staticProvider)
import Ecluse.Ecosystem (Ecosystem (Npm))
import Ecluse.Env (Env (envQueue), newEnv)
import Ecluse.Package (PackageName, mkPackageName)
import Ecluse.Queue (
    MirrorJob (jobArtifactUrl, jobMirrorTarget, jobPackage, jobVersion),
    MirrorQueue (enqueue, receive),
    QueueMessage (msgJob),
    newInMemoryQueue,
 )
import Ecluse.Registry (ParseError (..), RegistryClient (..))
import Ecluse.Registry.Npm.Route qualified as Npm
import Ecluse.Registry.Npm.Serve (npmRenderer)
import Ecluse.Rules.Types (PrecededRule, Rule (AllowIfPublishedBefore, DenyInstallTimeExecution), atDefaultPrecedence)
import Ecluse.Server (
    MountBinding (..),
    application,
    mkServerConfig,
 )
import Ecluse.Server.Cache (defaultCacheConfig, newMetadataCache)
import Ecluse.Server.Context (PackumentDeps (..))
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
request it receives (so the credential-authority invariant is assertable) and
serves a fixed response.
-}
data Upstream = Upstream
    { upApp :: Application
    , upSeenAuth :: IORef [Maybe ByteString]
    }

{- | An upstream double serving a fixed packument body with @200@, recording each
request's @Authorization@ header.
-}
servingUpstream :: LByteString -> IO Upstream
servingUpstream body = upstreamRespondingWith (responseLBS status200 [] body)

{- | An upstream double that always answers @500@ — a failed/unavailable leg, for
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
    pure Upstream{upApp = app, upSeenAuth = seen}
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
    pure Upstream{upApp = app, upSeenAuth = seen}

-- The @Authorization@ header value a request carried, if any.
lookupAuth :: [Header] -> Maybe ByteString
lookupAuth headers = snd <$> find ((== hAuthorization) . fst) headers

-- The auth headers an upstream double saw, in arrival order.
seenAuth :: Upstream -> IO [Maybe ByteString]
seenAuth up = reverse <$> readIORef (upSeenAuth up)

-- ── tarball upstream doubles (path-aware) ─────────────────────────────────────

{- | Whether a request path is a tarball slot (@\/…\/-\/….tgz@) rather than a
packument. The artifact and packument legs of a single upstream are distinguished
by this, so one double can answer both.
-}
isTarballPath :: ByteString -> Bool
isTarballPath path = "/-/" `BS.isInfixOf` path && ".tgz" `BS.isSuffixOf` path

{- | A path-aware upstream double: it answers a tarball-slot path with @200@ and the
given artifact bytes, and any other path (the packument fetch) with @200@ and the
given packument body. Records each request's @Authorization@ header. The single
double thus serves both legs the public artifact path consults — the gating
packument and the artifact itself.
-}
artifactUpstream :: LByteString -> LByteString -> IO Upstream
artifactUpstream packumentBody tarballBody = do
    seen <- newIORef []
    let app :: Application
        app req respond = do
            modifyIORef' seen (lookupAuth (requestHeaders req) :)
            respond $
                if isTarballPath (rawPathInfo req)
                    then responseLBS status200 [] tarballBody
                    else responseLBS status200 [] packumentBody
    pure Upstream{upApp = app, upSeenAuth = seen}

{- | A private upstream double that has the artifact: it answers a tarball-slot path
with @200@ and the given bytes (a __private hit__), and any other path with @404@
(the private leg is never consulted for a packument on the artifact path).
-}
privateArtifactHit :: LByteString -> IO Upstream
privateArtifactHit tarballBody = do
    seen <- newIORef []
    let app :: Application
        app req respond = do
            modifyIORef' seen (lookupAuth (requestHeaders req) :)
            respond $
                if isTarballPath (rawPathInfo req)
                    then responseLBS status200 [] tarballBody
                    else responseLBS status404 [] "not found"
    pure Upstream{upApp = app, upSeenAuth = seen}

{- | A private upstream double that does __not__ have the artifact: every path is a
@404@ miss, so the artifact path falls through to the public leg.
-}
privateArtifactMiss :: IO Upstream
privateArtifactMiss = upstreamRespondingWith (responseLBS status404 [] "not found")

{- | A private upstream double that begins serving the artifact then fails
__mid-stream__: a tarball-slot path is answered (over the raw connection) with a
@200@ that __promises far more than it delivers__ — a large @Content-Length@ but
only a little body — and then returns, so Warp closes the socket and the proxy's
read of the artifact hits EOF short of the promised length and fails at once. Any
other path is a @404@. The handler never throws (so @testWithApplication@ does not
surface a server-side error), and the immediate close keeps the short read from
stalling on a read timeout. It exercises the committed-stream case — once the @200@
is on the wire the serve path must fail internally, not fall through to the public
leg.
-}
privateArtifactMidStreamFailure :: IO Upstream
privateArtifactMidStreamFailure = do
    seen <- newIORef []
    let app :: Application
        app req respond = do
            modifyIORef' seen (lookupAuth (requestHeaders req) :)
            if isTarballPath (rawPathInfo req)
                then respond (responseRaw truncatedArtifact (responseLBS status500 [] "raw unsupported"))
                else respond (responseLBS status404 [] "not found")
    pure Upstream{upApp = app, upSeenAuth = seen}
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
                    , "shasum" .= ("deadbeef" :: Text)
                    ]
          , "_unmodeled" .= ("kept" :: Text) -- a per-version unmodeled key
          ]
            <> ["scripts" .= object ["postinstall" .= ("node build.js" :: Text)] | hasInstall]
        )

-- A plain (no-install-script) version object with a distinct integrity.
plainVersion :: Text -> Value
plainVersion version = versionObject version ("sha512-" <> version <> "-int") False

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
        , parsePackageInfo = const (Left (ParseError "unused"))
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
    newEnv fakeRegistry queue fakeCredentials manager metadataCache logEnv

{- | The packument-serve dependencies pointing at two in-process upstream ports,
with the given inbound edge token (usually 'Nothing').
-}
deps :: Int -> Int -> Maybe Text -> PackumentDeps
deps privatePort publicPort inbound =
    PackumentDeps
        { pdPrivateBaseUrl = localhost privatePort
        , pdPublicBaseUrl = localhost publicPort
        , pdMountBaseUrl = "https://proxy.test"
        , pdMirrorTarget = "https://mirror.test"
        , pdRules = policy
        , pdInboundToken = mkSecret <$> inbound
        , pdNow = pure now
        , pdHelp = Nothing
        }

localhost :: Int -> Text
localhost port = "http://127.0.0.1:" <> show port

{- | Run an assertion against a proxy whose two upstream legs are the given
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
withProxyEnvQueue queue privateUp publicUp inbound k =
    testWithApplication (pure (upApp privateUp)) $ \privatePort ->
        testWithApplication (pure (upApp publicUp)) $ \publicPort -> do
            manager <- newManager defaultManagerSettings
            env <- newTestEnvWithQueue queue manager
            let cfg =
                    mkServerConfig
                        [ MountBinding
                            { bindingPrefix = "npm" :| []
                            , bindingClassifier = Npm.classify
                            , bindingPackumentDeps = Just (deps privatePort publicPort inbound)
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

{- | Run an assertion against a proxy whose two upstream legs are the given
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
    losslessSpec
    tarballSpec

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
                        [("1.0.0", versionObject "1.0.0" "sha512-public-int" True)]
                        "1.0.0"
                        [("1.0.0", publishedDaysAgo 30)]
                    )
                )
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["1.0.0"]
            -- The served 1.0.0 is the private (trusted) copy: its integrity wins.
            servedIntegrity "1.0.0" resp `shouldBe` Just "sha512-1.0.0-int"

    it "serves the private copy on an integrity divergence (private wins; flagged in the merge)" $ do
        -- Same version key, differing integrity across upstreams: the private copy
        -- wins the served document. The divergence is recorded in the MergePlan
        -- (asserted directly in Ecluse.Package.MergeSpec); here we pin that the
        -- served bytes are the trusted copy's, never the public one's.
        privateUp <-
            servingUpstream
                (encodePackument (privatePackumentWith [("1.0.0", versionObject "1.0.0" "sha512-PRIVATE" False)] "1.0.0"))
        publicUp <-
            servingUpstream
                ( encodePackument
                    ( packument
                        [("1.0.0", versionObject "1.0.0" "sha512-PUBLIC" False)]
                        "1.0.0"
                        [("1.0.0", publishedDaysAgo 30)]
                    )
                )
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedIntegrity "1.0.0" resp `shouldBe` Just "sha512-PRIVATE"

    it "repoints dist-tags.latest to a survivor when the public latest is denied (public-only)" $ do
        -- Public latest points at 2.0.0 (3 days old → denied by the quarantine);
        -- 1.0.0 (30 days) survives. With the private leg down, the served document
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
            -- The private leg received the client's bearer credential verbatim.
            privAuth `shouldBe` [Just "Bearer client-secret-token"]
            -- The public leg was queried ANONYMOUSLY — the internal token never left
            -- for the public registry. This is the load-bearing security assertion.
            pubAuth `shouldBe` [Nothing]

-- ── private leg is the per-client authority (uncached across clients) ──────────

privateAuthoritySpec :: Spec
privateAuthoritySpec = describe "private leg is the per-client authority (not cached across clients)" $
    it "re-consults the private upstream per client within the TTL — each client's token reaches it" $ do
        -- Two requests to the SAME proxy with DIFFERENT client bearer tokens, well
        -- within the 60s metadata-cache TTL. The private upstream is the authority for
        -- who may read what, so its metadata must not be shared across clients: each
        -- request must re-consult it with that client's OWN forwarded token. Were the
        -- private leg cached (keyed by base URL, with no credential dimension), the
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
            -- The anonymous public leg IS cached: it was hit once, anonymously, and the
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

-- ── cache read-through coherence ──────────────────────────────────────────────

cacheSpec :: Spec
cacheSpec = describe "metadata cache (read-through coherence)" $ do
    it "reuses the cached document within the TTL — a second request does not re-fetch" $ do
        -- The public upstream would gain 2.0.0 on a second fetch, but within the 60s
        -- TTL the packument path reads through: the second request is served from the
        -- cached entry, so the upstream is hit exactly once and the served set stays
        -- {1.0.0}. The mutating double is the witness — 2.0.0 appears only if a second
        -- fetch occurred, so its absence proves the single-flight reuse. The private
        -- leg is absent, so the served set is exactly the cached public versions.
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
            servedIntegrity "1.0.0" secondResp `shouldBe` Just "sha512-1.0.0-int"
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
                    -- all denied → no survivors. With a failed private leg this
                    -- would be 503; here the private leg failing makes it transient.
                    ( packument
                        [("1.0.0", plainVersion "1.0.0"), ("2.0.0", plainVersion "2.0.0")]
                        "2.0.0"
                        [("1.0.0", publishedDaysAgo 1), ("2.0.0", publishedDaysAgo 1)]
                    )
                )
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            -- A failed private leg is a needed-upstream-unavailable signal (transient)
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
                        [("2.0.0", versionObject "2.0.0" "sha512-x" True)]
                        "2.0.0"
                        [("2.0.0", publishedDaysAgo 30)]
                    )
                )
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 403
            -- Never a 404: the package exists; its versions were withheld.
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

-- The opaque bytes a tarball double serves, distinct per leg so a test can pin
-- which upstream the served artifact came from.
privateTarballBytes :: LByteString
privateTarballBytes = "PRIVATE-TGZ-BYTES"

publicTarballBytes :: LByteString
publicTarballBytes = "PUBLIC-TGZ-BYTES"

-- A public packument whose single version @v@ clears the quarantine (admitted on
-- the artifact path).
admittingPublic :: Text -> Value
admittingPublic v = packument [(v, plainVersion v)] v [(v, publishedDaysAgo 30)]

-- A flat projection of a mirror job, for an order-stable equality assertion over
-- the four coordinates the job carries (package, version, public artifact URL,
-- mirror target).
jobShape :: MirrorJob -> (PackageName, Version, Text, Text)
jobShape job = (jobPackage job, jobVersion job, jobArtifactUrl job, jobMirrorTarget job)

tarballSpec :: Spec
tarballSpec = describe "artifact (tarball) path" $ do
    it "streams the private artifact unfiltered on a private hit (public never consulted)" $ do
        -- The private upstream has the artifact: it is streamed straight through,
        -- the public leg never queried and no mirror job enqueued (the bytes are
        -- already vetted; mirroring is for public-sourced artifacts).
        privateUp <- privateArtifactHit privateTarballBytes
        publicUp <- artifactUpstream (encodePackument (admittingPublic "1.0.0")) publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- getTarball "1.0.0" (Just "client-token") app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` privateTarballBytes
            -- The public upstream was never touched on a private hit.
            seenAuth publicUp `shouldReturn` []
            -- A private hit enqueues nothing — only a public-sourced admit does.
            drainJobs env `shouldReturn` []

    it "forwards the client credential to the private leg, never to the public" $ do
        -- On a private MISS the artifact still comes from public, but the gating
        -- packument and artifact fetches must be anonymous; the private leg saw the
        -- client's bearer.
        privateUp <- privateArtifactMiss
        publicUp <- artifactUpstream (encodePackument (admittingPublic "1.0.0")) publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app _env -> do
            _ <- getTarball "1.0.0" (Just "client-secret-token") app
            privAuth <- seenAuth privateUp
            pubAuth <- seenAuth publicUp
            -- The private leg received the client's bearer credential.
            privAuth `shouldBe` [Just "Bearer client-secret-token"]
            -- Both public-leg requests (packument gate + artifact fetch) were anonymous.
            pubAuth `shouldBe` [Nothing, Nothing]

    it "on a private miss: gates the version, streams from public, and enqueues a mirror job" $ do
        privateUp <- privateArtifactMiss
        publicUp <- artifactUpstream (encodePackument (admittingPublic "1.0.0")) publicTarballBytes
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
        let tooNew = packument [("2.0.0", plainVersion "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 3)]
        publicUp <- artifactUpstream (encodePackument tooNew) publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- getTarball "2.0.0" Nothing app
            status resp `shouldBe` 403
            drainJobs env `shouldReturn` []

    it "503s when the public upstream is unavailable (transient), enqueuing nothing" $ do
        privateUp <- privateArtifactMiss
        publicUp <- failingUpstream
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 503
            drainJobs env `shouldReturn` []

    it "404s a version absent from the public metadata (forwarded miss), enqueuing nothing" $ do
        -- The package resolves but the requested 9.9.9 is not among its versions: a
        -- forwarded upstream miss → 404, never a fabricated admit.
        privateUp <- privateArtifactMiss
        publicUp <- artifactUpstream (encodePackument (admittingPublic "1.0.0")) publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- getTarball "9.9.9" Nothing app
            status resp `shouldBe` 404
            drainJobs env `shouldReturn` []

    it "serves the artifact even when the enqueue fails (best-effort, non-blocking)" $ do
        -- The queue's enqueue throws; the client must still receive the streamed
        -- artifact with a 200. The enqueue failure is swallowed, never surfaced.
        privateUp <- privateArtifactMiss
        publicUp <- artifactUpstream (encodePackument (admittingPublic "1.0.0")) publicTarballBytes
        failingQueue <- newFailingQueue
        withProxyEnvQueue failingQueue privateUp publicUp Nothing $ \app _env _port -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` publicTarballBytes

    it "gates a lockfile install hitting the tarball URL with no preceding packument request" $ do
        -- An `npm ci` install fetches the tarball directly; the gate decides on this
        -- path alone (the version metadata is fetched here), with no prior packument
        -- request. A denied version is still refused.
        privateUp <- privateArtifactMiss
        let tooNew = packument [("2.0.0", plainVersion "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 3)]
        publicUp <- artifactUpstream (encodePackument tooNew) publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app _env ->
            getTarball "2.0.0" Nothing app >>= \resp -> status resp `shouldBe` 403

    it "fails internally on a mid-stream private failure, never falling through to public" $ do
        -- The private leg commits a 200 then drops the connection mid-body. Because
        -- the response is already on the wire, the serve path must fail (the broken
        -- stream surfaces as an error here) rather than swallow it and respond a
        -- second time over the half-sent artifact — so the public leg is never
        -- consulted and nothing is enqueued.
        privateUp <- privateArtifactMidStreamFailure
        publicUp <- artifactUpstream (encodePackument (admittingPublic "1.0.0")) publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            (_ :: Either SomeException SResponse) <- try (getTarball "1.0.0" (Just "client-token") app)
            seenAuth publicUp `shouldReturn` []
            drainJobs env `shouldReturn` []

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
