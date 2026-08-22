-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.E2E.Harness.Types (
    E2E (..),
    E2EConfig (..),
    defaultE2EConfig,
    GlobalDataPlane (..),
    NpmResult (..),
    NpmProject (..),
) where

import Network.HTTP.Client (Manager)
import System.Exit (ExitCode)

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
    , e2eProxyContainer :: String
    {- ^ The proxy container name, so a test can read the proxy's own JSONL log stream
    ('proxyContainerLogs'): what it wrote to stdout\/stderr.
    -}
    , e2eCollectorContainer :: Maybe String
    {- ^ The OTLP collector container name when the environment booted one ('ecCollector'), so
    a test can read the collector's debug-exporter output. 'Nothing' when no collector booted.
    -}
    , e2eManager :: Manager
    -- ^ A shared HTTP manager for the harness's own probes.
    }

{- | What an end-to-end environment boots beyond the base topology: an optional OTLP collector
plus extra proxy environment that layers over 'proxyEnv'. The default boots neither.
-}
data E2EConfig = E2EConfig
    { ecCollector :: Bool
    -- ^ Stand up the OTLP collector container (reached by the proxy as @otelcol@).
    , ecExtraEnv :: [(Text, Text)]
    -- ^ Extra proxy environment, appended over (and so overriding) the base 'proxyEnv'.
    }

-- | The base configuration: the plain topology, no collector and no extra environment.
defaultE2EConfig :: E2EConfig
defaultE2EConfig = E2EConfig{ecCollector = False, ecExtraEnv = []}

data GlobalDataPlane = GlobalDataPlane
    { gdpNet :: String
    , gdpStub :: String
    , gdpVerd :: String
    , gdpMini :: String
    , gdpVerdPort :: Int
    , gdpMiniPort :: Int
    , gdpWorkDir :: FilePath
    }

-- | The outcome of an @npm@ invocation: exit code plus captured output.
data NpmResult = NpmResult
    { npmExit :: ExitCode
    , npmStdout :: Text
    , npmStderr :: Text
    }
    deriving stock (Show)

{- | An isolated, throwaway @npm@ project: its own cache, userconfig, prefix and @HOME@ keep
global npm state out and the proxy the only registry. The lockfile stays on for 'npmCiIn'.
-}
data NpmProject = NpmProject
    { npDir :: FilePath
    , npEnv :: [(String, String)]
    }
