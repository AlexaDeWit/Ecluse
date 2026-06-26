{- | The end-to-end harness: bring the whole system up as containers, drive it with
the real @npm@ CLI, and tear it all down.

The topology runs the __real OCI image__ (the artifact we publish), an __nginx__
public-upstream stub, a __Verdaccio__ private upstream + mirror target, and a
__ministack__ SQS emulator on a docker network whose subnet is RFC 5737 documentation
space (@203.0.113.0\/24@). That range is __not__ in the egress guard's internal-range
block, so the proxy reaches the stub at a non-internal address with no production code
change — see @planning\/slices\/S53-e2e-ecosystem.md@. Custom-subnet networks are
beyond @testcontainers-hs@, so the harness drives @docker@ directly through
@typed-process@.

The proxy's mirror queue is the __real AWS SQS backend__ pointed at ministack through
the production @AWS_ENDPOINT_URL_SQS@ override (no test-only code path — the released image
is exercised exactly as deployed, just with the endpoint aimed at the emulator). The
harness creates the queue in ministack over the plain SQS query API (the emulator
needs no signed request) and passes its URL to the proxy; the proxy reaches the
emulator by its network alias while routing by the queue's path, so the queue URL's
host is immaterial.

The suite only runs when @ECLUSE_E2E_IMAGE@ names a loaded image and a docker daemon
is reachable; 'e2eUnavailable' reports the reason otherwise so the spec can mark its
cases @pending@ rather than fail on a machine without the setup.
-}
module Ecluse.E2E.Harness (
    E2E (..),
    e2eUnavailable,
    withE2E,

    -- * Driving the system
    NpmResult (..),
    NpmProject,
    npmInstall,
    withNpmProject,
    npmInstallIn,
    npmCiIn,
    withUpstreamPaused,
    proxyStatus,
    proxyGet,
    proxyHead,
    verdaccioHasVersion,
    verdaccioHasVersionNow,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Client (
    Manager,
    Request (method),
    Response,
    brConsume,
    defaultManagerSettings,
    httpLbs,
    newManager,
    parseRequest,
    responseBody,
    responseHeaders,
    responseStatus,
    withResponse,
 )
import Network.HTTP.Types (hContentLength, statusCode)
import Network.Socket (
    Family (AF_INET),
    SockAddr (SockAddrInet),
    SocketType (Stream),
    bind,
    close,
    defaultProtocol,
    getSocketName,
    socket,
    tupleToHostAddress,
 )
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Process.Typed (proc, readProcess, readProcessStdout, setEnv, setWorkingDir)
import UnliftIO (bracket, bracket_, handleAny)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Environment (getEnvironment)

import Ecluse.E2E.Fixtures (buildFixtures, fixturePackages)

-- | A booted end-to-end environment, handed to each spec case.
data E2E = E2E
    { e2eRegistry :: Text
    -- ^ The npm registry URL to point a client at (the proxy's npm mount).
    , e2eBaseUrl :: Text
    -- ^ The proxy's base URL on host loopback (no trailing slash).
    , e2eVerdaccio :: Text
    -- ^ The Verdaccio base URL on host loopback (the mirror, for polling).
    , e2eStubContainer :: String
    {- ^ The public-upstream stub container name, so a test can pause and resume it
    ('withUpstreamPaused') to simulate a public-registry outage.
    -}
    , e2eManager :: Manager
    -- ^ A shared HTTP manager for the harness's own probes.
    }

-- ── availability ──────────────────────────────────────────────────────────────

{- | 'Nothing' when the suite can run; @Just reason@ when it must be skipped — no
docker daemon, or @ECLUSE_E2E_IMAGE@ unset (the image is built and named by
@make test-e2e@ / the CI e2e job).
-}
e2eUnavailable :: IO (Maybe String)
e2eUnavailable =
    lookupEnv imageVar >>= \case
        Nothing -> pure (Just (imageVar <> " is unset — run via `make test-e2e`"))
        Just "" -> pure (Just (imageVar <> " is empty — run via `make test-e2e`"))
        Just _ -> do
            ok <- dockerDaemonReachable
            pure (if ok then Nothing else Just "no reachable docker daemon")

imageVar :: String
imageVar = "ECLUSE_E2E_IMAGE"

dockerDaemonReachable :: IO Bool
dockerDaemonReachable =
    handleAny (\_ -> pure False) (exitOk <$> readProcess (proc "docker" ["info"]))

-- ── lifecycle ───────────────────────────────────────────────────────────────

{- | Bring the network + three containers up, wait for proxy readiness, run the
action, then tear everything down on every exit path. Assumes 'e2eUnavailable'
returned 'Nothing'.
-}
withE2E :: (E2E -> IO ()) -> IO ()
withE2E action = do
    image <- maybe (fail (imageVar <> " unset")) pure =<< lookupEnv imageVar
    sfx <- uniqueSuffix
    tmpRoot <- getTemporaryDirectory
    let net = "ecluse-e2e-net-" <> sfx
        stub = "ecluse-e2e-stub-" <> sfx
        verd = "ecluse-e2e-verd-" <> sfx
        mini = "ecluse-e2e-mini-" <> sfx
        prox = "ecluse-e2e-proxy-" <> sfx
        workDir = tmpRoot </> ("ecluse-e2e-" <> sfx)
        htmlDir = workDir </> "html"
        verdConf = workDir </> "verdaccio.yaml"
        nginxConf = workDir </> "nginx.conf"
    bracket
        (pure ())
        (\_ -> teardown net [prox, verd, stub, mini] workDir)
        ( \_ -> do
            createDirectoryIfMissing True htmlDir
            buildFixtures htmlDir fixturePackages
            writeFileText verdConf verdaccioConfig
            writeFileText nginxConf nginxStubConfig
            dockerOk ["network", "create", "--subnet", "203.0.113.0/24", net]
            -- nginx public-upstream stub, reachable by the proxy as `upstream`. The
            -- config maps /<pkg> to the package's packument.json (the tarball lives
            -- under /<pkg>/-/, so /<pkg> cannot be a file and a directory both).
            dockerOk
                [ "run"
                , "-d"
                , "--name"
                , stub
                , "--network"
                , net
                , "--network-alias"
                , "upstream"
                , "-v"
                , htmlDir <> ":/usr/share/nginx/html:ro"
                , "-v"
                , nginxConf <> ":/etc/nginx/conf.d/default.conf:ro"
                , "nginx:alpine"
                ]
            -- Verdaccio private upstream + mirror target, reachable as `mirror`.
            dockerOk
                [ "run"
                , "-d"
                , "--name"
                , verd
                , "--network"
                , net
                , "--network-alias"
                , "mirror"
                , "-p"
                , "127.0.0.1:0:4873"
                , "-v"
                , verdConf <> ":/verdaccio/conf/config.yaml:ro"
                , "verdaccio/verdaccio:5"
                ]
            -- ministack SQS emulator, reachable by the proxy as `ministack` and by the
            -- harness on a published host port (to create the queue). The image is used
            -- directly (no testcontainers-hs label-parsing workaround needed here).
            dockerOk
                [ "run"
                , "-d"
                , "--name"
                , mini
                , "--network"
                , net
                , "--network-alias"
                , "ministack"
                , "-p"
                , "127.0.0.1:0:4566"
                , "ministackorg/ministack@sha256:5164592def36af01b8ac76364028e27c5ecd8f1494c8a53d5fcd811cc7dfb594"
                ]
            manager <- newManager defaultManagerSettings
            -- Create the mirror queue in ministack and learn its URL. The proxy routes to
            -- ministack via AWS_ENDPOINT_URL_SQS and matches the queue by its path, so the
            -- URL's host (here ministack's own `localhost:4566`) is immaterial.
            miniPort <- publishedPort mini "4566/tcp"
            queueUrl <- createMinistackQueue manager miniPort "ecluse-e2e"
            -- Pick the host port up front so PROXY_PUBLIC_URL (which makes the proxy
            -- rewrite dist.tarball to an absolute, npm-fetchable URL) is known before
            -- the container starts — the assigned port is only readable after.
            proxyPort <- freeHostPort
            -- The real proxy image: server ‖ worker over the real SQS backend, pointed
            -- at ministack through the production AWS_ENDPOINT_URL_SQS override.
            dockerOk $
                [ "run"
                , "-d"
                , "--name"
                , prox
                , "--network"
                , net
                , "-p"
                , "127.0.0.1:" <> show proxyPort <> ":4873"
                ]
                    <> concatMap (\(k, v) -> ["-e", toString (k <> "=" <> v)]) (proxyEnv proxyPort queueUrl)
                    <> [image]
            verdPort <- publishedPort verd "4873/tcp"
            let base = "http://127.0.0.1:" <> show proxyPort
                e2e =
                    E2E
                        { e2eRegistry = base <> "/npm/"
                        , e2eBaseUrl = base
                        , e2eVerdaccio = "http://127.0.0.1:" <> show verdPort
                        , e2eStubContainer = stub
                        , e2eManager = manager
                        }
            ready <- waitFor manager (base <> "/readyz") 200
            unless ready (fail "proxy did not become ready on /readyz within the timeout")
            action e2e
        )

{- | The proxy's environment, given the host port it is published on and the mirror
queue URL created in ministack. The real SQS backend is pointed at ministack through
the production @AWS_ENDPOINT_URL_SQS@ override and signs with the standard
@AWS_ACCESS_KEY_ID@\/@AWS_SECRET_ACCESS_KEY@ (the emulator ignores them). Both upstream
legs and the mirror target point at the stub containers by their network aliases.
@PROXY_PUBLIC_URL@ is the host-loopback address npm reaches the proxy on, so each
served @dist.tarball@ is rewritten to an absolute URL npm can fetch.
-}
proxyEnv :: Int -> Text -> [(Text, Text)]
proxyEnv hostPort queueUrl =
    [ ("PROXY_PORT", "4873")
    , ("PROXY_PUBLIC_URL", "http://127.0.0.1:" <> show hostPort)
    , ("PUBLIC_UPSTREAM_URL", "http://upstream/")
    , ("PRIVATE_UPSTREAM_URL", "http://mirror:4873/")
    , ("MIRROR_TARGET_URL", "http://mirror:4873/")
    , ("MIRROR_TARGET_TOKEN", "e2e-publish-token")
    , ("MIRROR_QUEUE_PROVIDER", "sqs")
    , ("MIRROR_QUEUE_URL", queueUrl)
    , -- The production endpoint override (AWS-SDK-standard), aimed at the ministack
      -- alias; the dummy keys sign the request the emulator does not validate.
      ("AWS_ENDPOINT_URL_SQS", "http://ministack:4566")
    , ("AWS_REGION", "us-east-1")
    , ("AWS_ACCESS_KEY_ID", "test")
    , ("AWS_SECRET_ACCESS_KEY", "test")
    , ("PROXY_LOG_FORMAT", "json")
    , -- Add DenyInstallTimeExecution to the default min-age policy so the deny
      -- scenario has a rule to fire; the document carries only this rule patch.
      ("PROXY_CONFIG", "{\"rules\":{\"deny-install-scripts\":{\"type\":\"DenyInstallTimeExecution\"}}}")
    ]

teardown :: String -> [String] -> FilePath -> IO ()
teardown net containers workDir = do
    for_ containers (\c -> void (readProcess (proc "docker" ["rm", "-f", c])))
    void (readProcess (proc "docker" ["network", "rm", net]))
    handleAny (const pass) (removePathForcibly workDir)

-- ── npm driver ──────────────────────────────────────────────────────────────

-- | The outcome of an @npm@ invocation: exit code plus captured output.
data NpmResult = NpmResult
    { npmExit :: ExitCode
    , npmStdout :: Text
    , npmStderr :: Text
    }
    deriving stock (Show)

{- | An isolated, throwaway @npm@ project: its directory plus the fully isolated
environment (own cache, userconfig, prefix, @HOME@) so a developer's global npm state
cannot leak in and the only registry is the proxy. The lockfile is left __enabled__, so
an @npm install@ here writes a @package-lock.json@ a later 'npmCiIn' installs from.
-}
data NpmProject = NpmProject
    { npDir :: FilePath
    , npEnv :: [(String, String)]
    }

{- | Bracket an isolated npm project (see 'NpmProject'): create the dirs and the pinned
environment, run the action, then remove the project tree on every exit path.
-}
withNpmProject :: E2E -> (NpmProject -> IO a) -> IO a
withNpmProject e2e use = do
    sfx <- uniqueSuffix
    tmpRoot <- getTemporaryDirectory
    let projectDir = tmpRoot </> ("ecluse-e2e-npm-" <> sfx)
        cacheDir = projectDir </> "cache"
        prefixDir = projectDir </> "prefix"
        npmrc = projectDir </> ".npmrc"
    bracket
        ( do
            createDirectoryIfMissing True cacheDir
            createDirectoryIfMissing True prefixDir
            writeFileText (projectDir </> "package.json") consumerPackageJson
            writeFileText npmrc ""
            baseEnv <- getEnvironment
            let overrides =
                    [ ("npm_config_registry", toString (e2eRegistry e2e))
                    , ("npm_config_cache", cacheDir)
                    , ("npm_config_userconfig", npmrc)
                    , ("npm_config_prefix", prefixDir)
                    , ("npm_config_audit", "false")
                    , ("npm_config_fund", "false")
                    , ("npm_config_update_notifier", "false")
                    , ("npm_config_progress", "false")
                    , ("HOME", projectDir)
                    ]
                cleanEnv =
                    filter
                        (\(k, _) -> k `notElem` map fst overrides && not ("npm_config_" `isPrefixOf` k))
                        baseEnv
                        <> overrides
            pure NpmProject{npDir = projectDir, npEnv = cleanEnv}
        )
        (\_ -> handleAny (const pass) (removePathForcibly projectDir))
        use

-- | Run @npm@ with the given args in a project, capturing its exit and output.
runNpm :: NpmProject -> [String] -> IO NpmResult
runNpm proj args = do
    let cmd = setWorkingDir (npDir proj) . setEnv (npEnv proj) $ proc "npm" args
    (code, out, err) <- readProcess cmd
    pure
        NpmResult
            { npmExit = code
            , npmStdout = decodeUtf8 (LBS.toStrict out)
            , npmStderr = decodeUtf8 (LBS.toStrict err)
            }

{- | @npm install \<pkg\>@ in a project — resolves via the packument and writes the
lockfile (@package.json@ + @package-lock.json@) for a later 'npmCiIn'.
-}
npmInstallIn :: NpmProject -> Text -> IO NpmResult
npmInstallIn proj pkg = runNpm proj ["install", toString pkg]

{- | @npm ci@ in a project — a deterministic install from the lockfile. It fetches each
artifact from the lockfile's @resolved@ URL (the proxy's __private-first__ tarball path)
and checks @integrity@, without re-resolving via the packument — so once a version is
mirrored it never contacts the public upstream.
-}
npmCiIn :: NpmProject -> IO NpmResult
npmCiIn proj = runNpm proj ["ci"]

{- | @npm install \<pkg\>@ against the proxy in a throwaway project (see 'withNpmProject'),
for the one-shot cases that only need the install's outcome.
-}
npmInstall :: E2E -> Text -> IO NpmResult
npmInstall e2e pkg = withNpmProject e2e (`npmInstallIn` pkg)

consumerPackageJson :: Text
consumerPackageJson = "{\"name\":\"e2e-consumer\",\"version\":\"1.0.0\",\"private\":true}\n"

{- | Pause the public-upstream stub for the duration of an action, then resume it
(@docker pause@ / @docker unpause@). Used to prove an install is served from the private
mirror while the public registry is unreachable: with the stub frozen, the only source
that can answer is the mirror. Resumed on every exit path so later cases see it again.
-}
withUpstreamPaused :: E2E -> IO a -> IO a
withUpstreamPaused e2e =
    bracket_
        (dockerOk ["pause", e2eStubContainer e2e])
        (dockerOk ["unpause", e2eStubContainer e2e])

-- ── HTTP probes ─────────────────────────────────────────────────────────────

-- | The HTTP status of a @GET@ to a proxy path (e.g. @\/npm\/e2e-allow@).
proxyStatus :: E2E -> Text -> IO Int
proxyStatus e2e path = fst <$> proxyGet e2e path

-- | @GET@ a proxy path, returning the status and body.
proxyGet :: E2E -> Text -> IO (Int, LByteString)
proxyGet e2e path = do
    req <- parseRequest (toString (e2eBaseUrl e2e <> path))
    resp <- httpLbs req (e2eManager e2e)
    pure (statusCode (responseStatus resp), responseBody resp)

{- | @HEAD@ a proxy path, returning the status, the declared @Content-Length@ (if
any), and how many body bytes actually arrived — so a test can assert a @HEAD@ does
not stream a body.
-}
proxyHead :: E2E -> Text -> IO (Int, Maybe Int, Int)
proxyHead e2e path = do
    base <- parseRequest (toString (e2eBaseUrl e2e <> path))
    let req = base{method = "HEAD"}
    withResponse req (e2eManager e2e) $ \resp -> do
        chunks <- brConsume (responseBody resp)
        let declared = do
                raw <- lookup hContentLength (responseHeaders resp)
                readMaybe (toString (decodeUtf8 raw :: Text))
        pure (statusCode (responseStatus resp), declared, sum (map BS.length chunks))

{- | Poll Verdaccio (the mirror) until it serves the given version of a package, or
the timeout lapses. Used to await an asynchronous mirror, and to assert one never
happens (a 'False' after the patience window).
-}
verdaccioHasVersion :: E2E -> Text -> Text -> IO Bool
verdaccioHasVersion e2e pkg version = go (40 :: Int)
  where
    go 0 = pure False
    go n = do
        present <- verdaccioHasVersionNow e2e pkg version
        if present then pure True else threadDelay 500000 >> go (n - 1)

{- | A single, non-retrying check of whether the mirror already serves a version —
the precondition probe (\"absent now\") without the patience window
'verdaccioHasVersion' spends to confirm an absence.
-}
verdaccioHasVersionNow :: E2E -> Text -> Text -> IO Bool
verdaccioHasVersionNow e2e pkg version =
    handleAny (\_ -> pure False) $ do
        req <- parseRequest (toString (e2eVerdaccio e2e <> "/" <> pkg))
        resp <- httpLbs req (e2eManager e2e)
        pure
            ( statusCode (responseStatus resp) == 200
                && version `T.isInfixOf` decodeUtf8 (LBS.toStrict (responseBody resp))
            )

-- ── docker helpers ────────────────────────────────────────────────────────────

-- | Run a docker command, failing the test loudly if it exits non-zero.
dockerOk :: [String] -> IO ()
dockerOk args = do
    (code, _, err) <- readProcess (proc "docker" args)
    unless (code == ExitSuccess) $
        fail ("docker command " <> show args <> " failed: " <> toString (decodeUtf8 (LBS.toStrict err) :: Text))

-- | The host loopback port a container's given @\<port\>\/tcp@ is published to.
publishedPort :: String -> String -> IO Int
publishedPort cname containerPort = do
    (_, out) <- readProcessStdout (proc "docker" ["port", cname, containerPort])
    let firstLine = fromMaybe "" (listToMaybe (lines (decodeUtf8 (LBS.toStrict out))))
        portText = T.takeWhileEnd (/= ':') (T.strip firstLine)
    maybe
        (fail ("could not parse published port from " <> show firstLine))
        pure
        (readMaybe (toString portText))

{- | Create (idempotently) the mirror queue in the ministack SQS emulator on its
host-published port and return the queue URL. Uses the plain SQS query API — the
emulator needs no signed request — and retries while the emulator's SQS service warms
up. @CreateQueue@ is idempotent (a repeat returns the existing URL), so the retry is
safe. The returned URL's host is the emulator's own (@localhost:4566@); the proxy
routes to ministack via @AWS_ENDPOINT_URL_SQS@ and matches the queue by its path, so
that host is never dialled.
-}
createMinistackQueue :: Manager -> Int -> Text -> IO Text
createMinistackQueue manager hostPort queueName = go (60 :: Int)
  where
    endpoint =
        "http://127.0.0.1:"
            <> show hostPort
            <> "/?Action=CreateQueue&QueueName="
            <> queueName
            <> "&Version=2012-11-05"
    go :: Int -> IO Text
    go 0 = fail "ministack SQS CreateQueue never succeeded within the timeout"
    go n =
        handleAny (\_ -> retry n) $ do
            base <- parseRequest (toString endpoint)
            resp <- httpLbs base{method = "POST"} manager
            let body = decodeUtf8 (LBS.toStrict (responseBody resp)) :: Text
            case (statusCode (responseStatus resp), between "<QueueUrl>" "</QueueUrl>" body) of
                (200, Just url) | not (T.null url) -> pure url
                _ -> retry n
    retry :: Int -> IO Text
    retry n = threadDelay 500000 >> go (n - 1)

-- | The text between the first @opening@ and the following @closing@ marker, or 'Nothing'.
between :: Text -> Text -> Text -> Maybe Text
between opening closing t =
    let afterOpen = snd (T.breakOn opening t)
     in if T.null afterOpen
            then Nothing
            else
                let (inner, rest) = T.breakOn closing (T.drop (T.length opening) afterOpen)
                 in if T.null rest then Nothing else Just inner

-- ── waiting ───────────────────────────────────────────────────────────────────

-- | Poll a URL until it returns the wanted status, up to ~30s.
waitFor :: Manager -> Text -> Int -> IO Bool
waitFor manager url want = go (100 :: Int)
  where
    go 0 = pure False
    go n = do
        got <-
            handleAny (\_ -> pure Nothing) $ do
                req <- parseRequest (toString url)
                Just . statusCode . responseStatus <$> (httpLbs req manager :: IO (Response LByteString))
        if got == Just want then pure True else threadDelay 300000 >> go (n - 1)

-- ── misc ──────────────────────────────────────────────────────────────────────

exitOk :: (ExitCode, a, b) -> Bool
exitOk (code, _, _) = code == ExitSuccess

{- | A free host loopback port: bind to @127.0.0.1:0@, read the port the OS assigned,
release it. The brief window before docker rebinds it is a tolerable race for a
loopback test. Picked up front so PROXY_PUBLIC_URL can name it before boot.
-}
freeHostPort :: IO Int
freeHostPort =
    bracket (socket AF_INET Stream defaultProtocol) close $ \sock -> do
        bind sock (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
        getSocketName sock >>= \case
            SockAddrInet port _ -> pure (fromIntegral port)
            other -> fail ("unexpected socket address: " <> show other)

-- | A unique, monotonic-ish suffix for network/container/temp names.
uniqueSuffix :: IO String
uniqueSuffix = do
    t <- getPOSIXTime
    pure (show (round (t * 1000) :: Integer))

{- | The nginx stub config. A package's packument is served at @\/\<pkg\>@ from
@\<pkg\>\/packument.json@ (a regex location), while its tarball is served from
@\/\<pkg\>\/-\/\<file\>.tgz@ by the default root — so @\<pkg\>@ can be both the
packument path and the tarball path prefix without a file/directory clash.
-}
nginxStubConfig :: Text
nginxStubConfig =
    T.unlines
        [ "server {"
        , "    listen 80;"
        , "    server_name _;"
        , "    root /usr/share/nginx/html;"
        , "    location ~ ^/(?<pkg>[^/]+)$ {"
        , "        default_type application/json;"
        , "        alias /usr/share/nginx/html/$pkg/packument.json;"
        , "    }"
        , "    location / {"
        , "        try_files $uri =404;"
        , "    }"
        , "}"
        ]

{- | The Verdaccio config: anonymous read + publish, no uplinks (a sealed local
mirror), listening on all interfaces so a peer container can reach it.
-}
verdaccioConfig :: Text
verdaccioConfig =
    T.unlines
        [ "listen: 0.0.0.0:4873"
        , "storage: /verdaccio/storage/data"
        , "auth:"
        , "  htpasswd:"
        , "    file: /verdaccio/storage/htpasswd"
        , "    max_users: -1"
        , "uplinks: {}"
        , "packages:"
        , "  '@*/*':"
        , "    access: $all"
        , "    publish: $all"
        , "    unpublish: $all"
        , "  '**':"
        , "    access: $all"
        , "    publish: $all"
        , "    unpublish: $all"
        , "log: { type: stdout, format: pretty, level: warn }"
        ]
