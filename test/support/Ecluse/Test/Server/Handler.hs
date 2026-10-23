-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Run a serve handler over a test runtime and capture the response it answered with.
module Ecluse.Test.Server.Handler (
    captureServe,
    captureServeWithLog,
    deliverServe,
    responseBytes,
    testServeRuntime,
    testServeRuntimeWith,
    MissingFixtureResponse (..),
) where

import Data.ByteString.Builder (toLazyByteString)
import Katip (LogEnv)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Internal (Response (ResponseBuilder), ResponseReceived (ResponseReceived))
import UnliftIO.Exception (throwIO)

import Ecluse.Core.Registry.Public (newPublicControl)
import Ecluse.Core.Server.Admission (ServeAdmission, newServeAdmission)
import Ecluse.Core.Server.Context (Handler, MountBinding, RequestCtx (RequestCtx), ServeRuntime (ServeRuntime), runHandler)
import Ecluse.Core.Server.Contract (ResponseContract, responseToWai)
import Ecluse.Core.Telemetry.Record (MetricsPort (mpUpstreamRequest))
import Ecluse.Test.Deadline (unsweptClient)
import Ecluse.Test.Log (newTestLogEnv)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Server.Cache (defaultCacheConfig, newMetadataCache)
import Ecluse.Test.Support (testMemoryMeter)

-- | Run a handler under the given runtime and mount, and return the response it rendered.
captureServe :: ResponseContract response -> ServeRuntime -> MountBinding -> ((response -> IO ResponseReceived) -> Handler ResponseReceived) -> IO Response
captureServe contract rt binding mkHandler = do
    logEnv <- newTestLogEnv
    captureServeWithLog logEnv contract rt binding mkHandler

-- | 'captureServe' into the given log environment.
captureServeWithLog :: LogEnv -> ResponseContract response -> ServeRuntime -> MountBinding -> ((response -> IO ResponseReceived) -> Handler ResponseReceived) -> IO Response
captureServeWithLog logEnv contract rt binding mkHandler = do
    captured <- newIORef Nothing
    deliverServe logEnv contract rt binding mkHandler (writeIORef captured . Just)
    maybe (throwIO MissingFixtureResponse) pure =<< readIORef captured

-- | Run a handler and pass the response it renders to @deliver@, which stands in for the server's send.
deliverServe :: LogEnv -> ResponseContract response -> ServeRuntime -> MountBinding -> ((response -> IO ResponseReceived) -> Handler ResponseReceived) -> (Response -> IO ()) -> IO ()
deliverServe logEnv contract rt binding mkHandler deliver =
    void (runHandler logEnv mempty (RequestCtx rt binding) (mkHandler (\value -> deliver (responseToWai contract value) $> ResponseReceived)))

-- | The bytes of a captured buffered response. A streamed or file response fails the example.
responseBytes :: Response -> IO LByteString
responseBytes = \case
    ResponseBuilder _ _ builder -> pure (toLazyByteString builder)
    _ -> fail "expected a buffered response"

-- | A handler that returned without answering: a fixture failure, never an upstream outcome.
data MissingFixtureResponse = MissingFixtureResponse
    deriving stock (Show)

instance Exception MissingFixtureResponse

-- | A runtime whose admission, meter, and cache never bind, over the given metrics port.
testServeRuntime :: MetricsPort -> IO ServeRuntime
testServeRuntime metricsPort = do
    admission <- newServeAdmission 1_000_000
    testServeRuntimeWith admission metricsPort

-- | 'testServeRuntime' under the given serve admission.
testServeRuntimeWith :: ServeAdmission -> MetricsPort -> IO ServeRuntime
testServeRuntimeWith admission metricsPort = do
    client <- unsweptClient =<< newManager defaultManagerSettings
    cache <- newMetadataCache defaultCacheConfig
    queue <- newTestMemoryQueue
    meter <- testMemoryMeter
    publicControl <- newPublicControl (mpUpstreamRequest metricsPort) (\_ _ -> pass)
    pure (ServeRuntime admission meter client publicControl client cache queue metricsPort passthroughTracingPort)
