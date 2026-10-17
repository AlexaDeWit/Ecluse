-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.E2E.Harness.Types (
    E2E (..),
    E2EConfig (..),
    Service (..),
    Topology (..),
    npmTopology,
    pypiTopology,
    pypiAdvisoryTopology,
    dredgerTopology,
    mixedTopology,
    clientEndpoint,
    packageStore,
    serviceUrl,
    ministackPort,
    defaultE2EConfig,
    GlobalDataPlane (..),
    ClientResult (..),
    NpmProject (..),
    PipProject (..),
) where

import Data.Map.Strict qualified as Map
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Test.Contract.Install (ClientResult (..))
import Network.HTTP.Client (Manager)

-- | Only the processes and endpoints acquired for this group.
data E2E = E2E
    { e2eClientEndpoints :: Map Ecosystem Text
    , e2eBaseUrl :: Text
    , e2eStore :: Maybe Service
    , e2ePlane :: GlobalDataPlane
    , e2eProxyContainer :: String
    , e2eCollectorContainer :: Maybe String
    , e2eManager :: Manager
    , e2eAdvisoryDirectory :: Maybe FilePath
    }

-- | Extra role configuration layered over the group's topology.
data E2EConfig = E2EConfig
    { ecCollector :: Bool
    , ecExtraEnv :: [(Text, Text)]
    , ecQueueUrl :: Maybe Text
    , ecArgs :: [String]
    , ecAdvisoryDirectory :: Maybe FilePath
    }

-- | A service with its owned container and host-loopback endpoint.
data Service = Service
    { serviceContainer :: String
    , servicePort :: Int
    }

-- | Resources and mounted ecosystems required by one atomic group.
data Topology = Topology
    { topologyMounts :: [Ecosystem]
    , topologyStore :: Bool
    , topologyAws :: Bool
    }

-- | npm reads with an observed mirror and an in-process queue.
npmTopology :: Topology
npmTopology = Topology [Npm] True False

-- | PyPI reads need neither a package store nor an AWS emulator.
pypiTopology :: Topology
pypiTopology = Topology [PyPI] False False

-- | Advisory compilation and reads share the S3 emulator.
pypiAdvisoryTopology :: Topology
pypiAdvisoryTopology = pypiTopology{topologyAws = True}

-- | Maintenance and independent workers share S3 and SQS.
dredgerTopology :: Topology
dredgerTopology = npmTopology{topologyAws = True}

-- | The supported shared-proxy configuration enables both ecosystems.
mixedTopology :: Topology
mixedTopology = npmTopology{topologyMounts = [Npm, PyPI]}

-- | Resolve a required client capability before constructing a project.
clientEndpoint :: Ecosystem -> E2E -> IO Text
clientEndpoint ecosystem environment =
    maybe
        (fail ("topology has no client endpoint for " <> show ecosystem))
        pure
        (Map.lookup ecosystem (e2eClientEndpoints environment))

-- | Resolve the required package-store capability before observing it.
packageStore :: E2E -> IO Service
packageStore environment = maybe (fail "topology has no package store") pure (e2eStore environment)

-- | The host-loopback address of a concrete service.
serviceUrl :: Service -> Text
serviceUrl service = "http://127.0.0.1:" <> show (servicePort service)

-- | Resolve the AWS emulator capability for advisory or durable-queue operations.
ministackPort :: GlobalDataPlane -> IO Int
ministackPort plane = maybe (fail "topology has no AWS emulator") (pure . servicePort) (gdpMinistack plane)

-- | The base configuration: the plain topology, no collector and no extra environment.
defaultE2EConfig :: E2EConfig
defaultE2EConfig = E2EConfig{ecCollector = False, ecExtraEnv = [], ecQueueUrl = Nothing, ecArgs = [], ecAdvisoryDirectory = Nothing}

-- | The stores and the stub one spec group boots once: every proxy of the group runs on them.
data GlobalDataPlane = GlobalDataPlane
    { gdpNet :: String
    , gdpStub :: String
    , gdpStore :: Maybe Service
    , gdpMinistack :: Maybe Service
    , gdpWorkDir :: FilePath
    , gdpTopology :: Topology
    }

{- | An isolated, throwaway @npm@ project: its own cache, userconfig, prefix and @HOME@ keep
global npm state out and the proxy the only registry.
-}
data NpmProject = NpmProject
    { npDir :: FilePath
    , npEnv :: [(String, String)]
    }

{- | An isolated, throwaway @pip@ project: its own install target and @HOME@ keep global pip
state out and the proxy the only index.
-}
data PipProject = PipProject
    { ppDir :: FilePath
    , ppEnv :: [(String, String)]
    , ppIndex :: Text
    -- ^ The Simple-index URL this project resolves through.
    }
