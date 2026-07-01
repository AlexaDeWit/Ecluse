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
    ('proxyContainerLogs') -- what it wrote to stdout\/stderr.
    -}
    , e2eCollectorContainer :: Maybe String
    {- ^ The OTLP collector container name when one was booted ('ecCollector'), so a
    test can read the collector's debug-exporter output to assert which signals arrived.
    'Nothing' for an environment booted without a collector.
    -}
    , e2eManager :: Manager
    -- ^ A shared HTTP manager for the harness's own probes.
    }

{- | What an end-to-end environment boots beyond the base topology: whether to stand
up an OTLP collector for the proxy to export to, and any extra proxy environment
(telemetry switches, the OTLP\/Datadog dialect) layered over the base 'proxyEnv'. The
default boots neither -- the plain topology the non-telemetry scenarios use.
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

{- | An isolated, throwaway @npm@ project: its directory plus the fully isolated
environment (own cache, userconfig, prefix, @HOME@) so a developer's global npm state
cannot leak in and the only registry is the proxy. The lockfile is left __enabled__, so
an @npm install@ here writes a @package-lock.json@ a later 'npmCiIn' installs from.
-}
data NpmProject = NpmProject
    { npDir :: FilePath
    , npEnv :: [(String, String)]
    }
