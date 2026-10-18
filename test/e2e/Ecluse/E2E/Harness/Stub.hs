-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | TLS registry routes, deterministic faults, and access-log parsing for the product image.
module Ecluse.E2E.Harness.Stub (
    -- * Routes
    StubRoute (..),
    stubHosts,
    stubUrl,
    nativeUploadLimit,

    -- * Faults
    StubFault (..),
    stubConfig,

    -- * Reload progress
    startedWorkers,
    retiredWorkers,

    -- * Answered requests
    StubRead (..),
    stubReads,
    answeredByPublic,
) where

import Data.Text qualified as T

-- | A registry name the stub answers for.
data StubRoute
    = -- | The npm public upstream, served from the fixture tree.
      NpmPublic
    | -- | The PyPI public upstream, served from the fixture tree.
      PyPIPublic
    | -- | A private upstream that holds nothing: it answers every read with a 404.
      EmptyPrivate
    | -- | The front of the second Verdaccio a Dredger case runs as a private cache.
      PrivateCache
    | -- | The front of the Verdaccio every writing role mirrors into.
      Mirror
    | -- | The native PyPI upload/read fixture.
      PyPIMirror
    | -- | Native PyPI repositories behind the deterministic AWS control plane.
      PyPICodeArtifactData
    | -- | The AWS API host resolved on the isolated fixture network.
      PyPICodeArtifactControl
    deriving stock (Eq, Show, Enum, Bounded)

-- | Every name the stub answers to: its network aliases, and the names its certificate carries.
stubHosts :: [Text]
stubHosts = map stubHost universe

-- | The base URL a product role dials a route at.
stubUrl :: StubRoute -> Text
stubUrl route = "https://" <> stubHost route <> "/"

-- The name a route answers to, which is also its @server_name@.
stubHost :: StubRoute -> Text
stubHost = \case
    NpmPublic -> "upstream"
    PyPIPublic -> "pypi-upstream"
    EmptyPrivate -> "private-upstream"
    PrivateCache -> "private-cache"
    Mirror -> "mirror"
    PyPIMirror -> "pypi-mirror"
    PyPICodeArtifactData -> "contract-domain-123456789012.d.codeartifact.us-east-1.amazonaws.com"
    PyPICodeArtifactControl -> "codeartifact.us-east-1.amazonaws.com"

-- | A fault the stub holds on its routes. It holds one at a time.
data StubFault
    = -- | Close both public upstreams without answering while private routes continue serving.
      PublicUpstreamsDown
    | -- | The private cache refuses every write method and still answers reads.
      PrivateCacheWritesRefused
    | -- | Forward PyPI writes to the host fixture, optionally holding public unavailable.
      PyPIMirrorBackend Int Bool
    | -- | Route native PyPI stores and the AWS API to one host fixture.
      PyPICodeArtifactBackend Int
    deriving stock (Eq, Show)

-- | The stub's configuration: every route in declaration order, under the fault if there is one.
stubConfig :: Maybe StubFault -> Text
stubConfig fault =
    T.unlines $
        -- The CodeArtifact fixture hostname exceeds nginx's default 64-byte hash bucket.
        ["server_names_hash_bucket_size 128;", readLogFormat]
            <> concatMap (serverBlock fault) universe

{- One line for each finished request, in the fields 'stubReads' reads back. Each coding field is
named, so an empty value keeps its place, and Accept-Encoding comes last because it can hold spaces.
-}
readLogFormat :: Text
readLogFormat = "log_format stub_read '" <> readMarker <> " $server_name $request_method $status $request_uri coding=$sent_http_content_encoding accepts=$http_accept_encoding';"

readMarker :: Text
readMarker = "stub-read"

serverBlock :: Maybe StubFault -> StubRoute -> [Text]
serverBlock fault route =
    [ "server {"
    , "    listen 443 ssl;"
    , "    server_name " <> stubHost route <> ";"
    , "    ssl_certificate /certs/server.crt;"
    , "    ssl_certificate_key /certs/server.key;"
    , "    access_log /var/log/nginx/access.log stub_read;"
    ]
        <> maybe [] (`faultLines` route) fault
        <> [line | isPublic route, line <- publicCoding]
        <> ( case (fault, route) of
                (Just (PyPIMirrorBackend port _), PyPIMirror) -> forwarded nativeUploadLimit port
                (Just (PyPICodeArtifactBackend port), PyPICodeArtifactData) -> forwarded 16777216 port
                (Just (PyPICodeArtifactBackend port), PyPICodeArtifactControl) -> forwarded 16777216 port
                _ -> routeLines route
           )
        <> ["}"]

forwarded :: Int -> Int -> [Text]
forwarded limit port =
    [ "    client_max_body_size " <> show limit <> ";"
    , "    location / { proxy_pass http://203.0.113.1:" <> show port <> "; }"
    ]

-- Gzip any response a request permits: more than a registry CDN, which gzips JSON and companions.
publicCoding :: [Text]
publicCoding = ["    gzip on;", "    gzip_types *;"]

-- nginx evaluates these before it picks a location, so a fault covers its whole route.
faultLines :: StubFault -> StubRoute -> [Text]
faultLines = \case
    PublicUpstreamsDown -> \route -> [closeUnanswered | isPublic route]
    PrivateCacheWritesRefused -> \route -> [refuseWrites | route == PrivateCache]
    PyPIMirrorBackend _ down -> \route -> [closeUnanswered | down && isPublic route]
    PyPICodeArtifactBackend _ -> const []
  where
    closeUnanswered = "    return " <> show unansweredStatus <> ";"
    refuseWrites = "    if ($request_method !~ ^(GET|HEAD)$) { return 503; }"

-- nginx's own status for closing a connection with no response. The client connects and completes
-- TLS, and the server then closes the connection in place of answering the request.
unansweredStatus :: Int
unansweredStatus = 444

isPublic :: StubRoute -> Bool
isPublic = \case
    NpmPublic -> True
    PyPIPublic -> True
    EmptyPrivate -> False
    PrivateCache -> False
    Mirror -> False
    PyPIMirror -> False
    PyPICodeArtifactData -> False
    PyPICodeArtifactControl -> False

routeLines :: StubRoute -> [Text]
routeLines = \case
    NpmPublic ->
        [ "    root /usr/share/nginx/html;"
        , "    location ~ ^/(?<pkg>[^/]+)$ {"
        , "        default_type application/json;"
        , "        alias /usr/share/nginx/html/$pkg/packument.json;"
        , "    }"
        , "    location / {"
        , "        try_files $uri =404;"
        , "    }"
        ]
    PyPIPublic ->
        [ "    root /usr/share/nginx/pypi;"
        , "    index index.json;"
        , -- A `types` block replaces the inherited map rather than extending it, so the
          -- index gets the PEP 691 media type and every other file needs the default below.
          "    types {"
        , "        application/vnd.pypi.simple.v1+json json;"
        , "    }"
        , "    default_type application/octet-stream;"
        , "    location / {"
        , -- An index request carries a trailing slash, so the file form is tried first: a
          -- bare $uri matches the directory, which nginx answers 403 with no index file.
          "        try_files $uri/index.json $uri =404;"
        , "    }"
        ]
    PyPIMirror -> ["    location / { return 404; }"]
    EmptyPrivate ->
        [ "    location / {"
        , "        return 404;"
        , "    }"
        ]
    PrivateCache ->
        [ "    resolver 127.0.0.11 valid=5s;"
        , "    location / {"
        , -- A variable defers the lookup to the request, so the stub starts before the cache does.
          "        set $cache_backend cache-verdaccio:4873;"
        , "        proxy_pass http://$cache_backend;"
        , "        proxy_set_header Host $host;"
        , "        proxy_set_header X-Forwarded-Proto https;"
        , "    }"
        ]
    Mirror ->
        [ "    resolver 127.0.0.11 valid=5s;"
        , "    client_max_body_size 0;" -- admits a published tarball of any size
        , "    location / {"
        , "        set $mirror_backend verdaccio:4873;"
        , "        proxy_pass http://$mirror_backend;"
        , "        proxy_set_header Host $host;"
        , "        proxy_set_header X-Forwarded-Proto https;" -- keeps Verdaccio writing https URLs
        , "        proxy_set_header X-Forwarded-For $remote_addr;"
        , "    }"
        ]
    PyPICodeArtifactData -> ["    location / { return 503; }"]
    PyPICodeArtifactControl -> ["    location / { return 503; }"]

-- | Count started workers so a reload can wait for the previous generation to retire.
startedWorkers :: Text -> Int
startedWorkers = T.count "start worker process "

-- | How many workers the stub's log shows have stopped accepting connections.
retiredWorkers :: Text -> Int
retiredWorkers = T.count "gracefully shutting down"

-- | One request a route finished, as the stub's access log records it.
data StubRead = StubRead
    { srRoute :: StubRoute
    , srMethod :: Text
    , srStatus :: Int
    , srPath :: Text
    , srContentEncoding :: Maybe Text
    -- ^ The response's @Content-Encoding@.
    , srAcceptEncoding :: Maybe Text
    -- ^ The request's @Accept-Encoding@, as the client sent it.
    }
    deriving stock (Eq, Show)

-- | Every request the stub's log records, oldest first. Lines of any other shape are skipped.
stubReads :: Text -> [StubRead]
stubReads = mapMaybe (readOf . words) . lines
  where
    readOf = \case
        marker : host : method : status : path : coding : accepted | marker == readMarker -> do
            route <- find ((== host) . stubHost) universe
            answered <- readMaybe (toString status)
            responseCoding <- T.stripPrefix "coding=" coding
            requestCodings <- T.stripPrefix "accepts=" (unwords accepted)
            pure
                StubRead
                    { srRoute = route
                    , srMethod = method
                    , srStatus = answered
                    , srPath = path
                    , srContentEncoding = logged responseCoding
                    , srAcceptEncoding = logged requestCodings
                    }
        _ -> Nothing
    -- nginx logs "-" for an absent field and nothing for an empty one.
    logged value = if value `elem` ["-", ""] then Nothing else Just value

-- | Whether a public upstream answered the request, where an outage leaves it unanswered.
answeredByPublic :: StubRead -> Bool
answeredByPublic r = isPublic (srRoute r) && srStatus r /= unansweredStatus

-- | The native fixture's maximum multipart request body, shared with its TLS forwarder.
nativeUploadLimit :: Int
nativeUploadLimit = 1024 * 1024
