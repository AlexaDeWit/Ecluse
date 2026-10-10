-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The nginx stub's routes. One nginx terminates TLS for every registry name the product dials
and tells them apart by @server_name@, so the product reaches https-only endpoints. A case holds
a fault on a route by rendering the configuration with it, and reads what each route answered
from the access log. "Ecluse.E2E.Harness.Docker" applies the configuration and fetches the log.
-}
module Ecluse.E2E.Harness.Stub (
    -- * Routes
    StubRoute (..),
    stubHosts,
    stubUrl,

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

-- | A fault the stub holds on its routes. It holds one at a time.
data StubFault
    = {- | Both public upstreams, npm's and PyPI's, close every connection with no answer. The
      other routes keep answering, so only a private store can supply a client.
      -}
      PublicUpstreamsDown
    | -- | The private cache refuses every write method and still answers reads.
      PrivateCacheWritesRefused
    deriving stock (Eq, Show)

-- | The stub's configuration: every route in declaration order, under the fault if there is one.
stubConfig :: Maybe StubFault -> Text
stubConfig fault = T.unlines (readLogFormat : concatMap (serverBlock fault) universe)

-- One line for each finished request, in the fields 'stubReads' reads back.
readLogFormat :: Text
readLogFormat = "log_format stub_read '" <> readMarker <> " $server_name $request_method $status $request_uri';"

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
        <> routeLines route
        <> ["}"]

-- nginx evaluates these before it picks a location, so a fault covers its whole route.
faultLines :: StubFault -> StubRoute -> [Text]
faultLines = \case
    PublicUpstreamsDown -> \route -> [closeUnanswered | isPublic route]
    PrivateCacheWritesRefused -> \route -> [refuseWrites | route == PrivateCache]
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
        [ "    client_max_body_size 0;" -- admits a published tarball of any size
        , "    location / {"
        , "        proxy_pass http://verdaccio:4873;"
        , "        proxy_set_header Host $host;"
        , "        proxy_set_header X-Forwarded-Proto https;" -- keeps Verdaccio writing https URLs
        , "        proxy_set_header X-Forwarded-For $remote_addr;"
        , "    }"
        ]

{- | How many worker processes the stub's log shows nginx has started, over every reload. A reload
starts a new set, so the count taken before one is the number it must retire.
-}
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
    }
    deriving stock (Eq, Show)

-- | Every request the stub's log records, oldest first. Lines of any other shape are skipped.
stubReads :: Text -> [StubRead]
stubReads = mapMaybe (readOf . words) . lines
  where
    readOf = \case
        [marker, host, method, status, path] | marker == readMarker -> do
            route <- find ((== host) . stubHost) universe
            answered <- readMaybe (toString status)
            pure StubRead{srRoute = route, srMethod = method, srStatus = answered, srPath = path}
        _ -> Nothing

-- | Whether a public upstream answered the request, where an outage leaves it unanswered.
answeredByPublic :: StubRead -> Bool
answeredByPublic r = isPublic (srRoute r) && srStatus r /= unansweredStatus
