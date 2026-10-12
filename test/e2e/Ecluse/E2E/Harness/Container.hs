-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Owned Docker resources, diagnostics and cancellation cleanup.
module Ecluse.E2E.Harness.Container (
    DockerRun (..),
    pinnedExternal,
    dockerRun,
    runArgs,
    withDockerContainer,
    withDockerNetwork,
    reportContainer,
    removeContainerIfPresent,
    commandOk,
    publishedPort,
    freeHostPort,
    withBridgeApplication,
    uniqueSuffix,
    awaitContainerLog,
    containerLogs,
) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Network.Socket (Family (AF_INET), HostAddress, SockAddr (SockAddrInet), Socket, SocketType (Stream), bind, close, defaultProtocol, listen, maxListenQueue, socket, socketPort, tupleToHostAddress)
import Network.Wai (Application)
import Network.Wai.Handler.Warp qualified as Warp
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Process.Typed (proc, readProcess, readProcessStdout)
import System.Random (randomIO)
import UnliftIO (bracket, finally, handleAny, onException, race, throwIO, withException)
import UnliftIO.Concurrent (myThreadId, throwTo)

import Ecluse.Test.Container.Image (ImageRef (PinnedExternal), PinnedImageRef, renderImageRef)
import Ecluse.Test.Poll (pollUntil)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))

-- | A concrete container specification. Its surrounding bracket owns removal.
data DockerRun = DockerRun
    { drName :: String
    , drNetwork :: String
    , drAliases :: [String]
    , drPorts :: [String]
    , drMounts :: [(FilePath, String)]
    , drTmpfs :: [String]
    , drEnv :: [(Text, Text)]
    , drImage :: String
    , drCmd :: [String]
    }

-- | Reject an invalid image pin before acquiring a resource.
pinnedExternal :: Either Text PinnedImageRef -> IO ImageRef
pinnedExternal = fmap PinnedExternal . either (fail . toString) pure

-- | A container specification with no implicit mounts, aliases or ports.
dockerRun :: String -> String -> ImageRef -> DockerRun
dockerRun name net image =
    DockerRun
        { drName = name
        , drNetwork = net
        , drAliases = []
        , drPorts = []
        , drMounts = []
        , drTmpfs = []
        , drEnv = []
        , drImage = toString (renderImageRef image)
        , drCmd = []
        }

runDetached :: [String] -> DockerRun -> IO ()
runDetached labelArgs = commandOk "docker" . runArgs ["-d"] labelArgs

-- | Render the exact Docker invocation from the resource specification.
runArgs :: [String] -> [String] -> DockerRun -> [String]
runArgs extra labelArgs spec =
    ["run"]
        <> extra
        <> ["--name", drName spec, "--network", drNetwork spec]
        <> concatMap (\a -> ["--network-alias", a]) (drAliases spec)
        <> concatMap (\p -> ["-p", p]) (drPorts spec)
        <> concatMap (\(h, c) -> ["-v", h <> ":" <> c]) (drMounts spec)
        <> concatMap (\t -> ["--tmpfs", t]) (drTmpfs spec)
        <> concatMap (\(k, v) -> ["-e", toString (k <> "=" <> v)]) (drEnv spec)
        <> labelArgs
        <> (drImage spec : drCmd spec)

-- | Bracket startup and teardown, recording logs before removing a failed container.
withDockerContainer :: [String] -> DockerRun -> (String -> IO a) -> IO a
withDockerContainer labelArgs spec action =
    bracket acquire release $ \name ->
        action name `withException` \(err :: SomeException) -> do
            putTextLn ("E2E action failed: " <> show err)
            reportContainer name
  where
    release name = captureContainer name `finally` removeContainer name
    acquire =
        (runDetached labelArgs spec >> pure (drName spec))
            `onException` (reportContainer (drName spec) >> removeContainerIfPresent (drName spec))

captureContainer :: String -> IO ()
captureContainer name = do
    root <- fromMaybe "scratchpad/e2e" <$> lookupEnv "ECLTEST_ARTIFACT_DIR"
    let directory = root </> "reports" </> "containers"
    createDirectoryIfMissing True directory
    (status, out, err) <- readProcess (proc "docker" ["logs", name])
    writeFileText
        (directory </> name <> ".log")
        ( "docker logs "
            <> toText name
            <> " exited "
            <> show status
            <> "\n"
            <> decodeUtf8 (LBS.toStrict (out <> err))
        )
    unless (status == ExitSuccess) (fail ("could not capture logs before removing " <> name))

-- | Emit captured container logs while the resource still exists.
reportContainer :: String -> IO ()
reportContainer name = do
    logs <- containerLogs name
    putTextLn ("E2E container " <> toText name <> " failed. Captured logs:\n" <> logs)

-- | Remove an owned name after failed or interrupted acquisition.
removeContainerIfPresent :: String -> IO ()
removeContainerIfPresent = removeResourceIfPresent "container"

removeResourceIfPresent :: String -> String -> IO ()
removeResourceIfPresent kind name = do
    (status, _, err) <- readProcess (proc "docker" [kind, "inspect", name])
    case status of
        ExitSuccess
            | kind == "container" -> captureContainer name `finally` removeContainer name
            | otherwise -> commandOk "docker" [kind, "rm", name]
        _ | "No such" `T.isInfixOf` decodeUtf8 (LBS.toStrict err) -> pass
        _ -> fail ("cannot inspect owned " <> kind <> " " <> name <> ": " <> toString (decodeUtf8 (LBS.toStrict err) :: Text))

-- | Acquire one labelled network and propagate cleanup failures.
withDockerNetwork :: [String] -> String -> [String] -> (String -> IO a) -> IO a
withDockerNetwork labelArgs name createArgs = bracket acquire removeNetwork
  where
    acquire =
        (commandOk "docker" (["network", "create"] <> createArgs <> labelArgs <> [name]) >> pure name)
            `onException` removeResourceIfPresent "network" name

removeContainer :: String -> IO ()
removeContainer name = commandOk "docker" ["rm", "-f", name]

removeNetwork :: String -> IO ()
removeNetwork name = commandOk "docker" ["network", "rm", name]

-- | Reject a failed command with its exact arguments and captured stderr.
commandOk :: String -> [String] -> IO ()
commandOk command args = do
    (code, _, err) <- readProcess (proc command args)
    unless (code == ExitSuccess) $
        fail (command <> " command " <> show args <> " failed: " <> toString (decodeUtf8 (LBS.toStrict err) :: Text))

-- | Read the host port Docker assigned to the named container port.
publishedPort :: String -> String -> IO Int
publishedPort cname containerPort = do
    (_, out) <- readProcessStdout (proc "docker" ["port", cname, containerPort])
    let firstLine = fromMaybe "" (listToMaybe (lines (decodeUtf8 (LBS.toStrict out))))
        portText = T.takeWhileEnd (/= ':') (T.strip firstLine)
    maybe
        (fail ("could not parse published port from " <> show firstLine))
        pure
        (readMaybe (toString portText))

-- The window between releasing this port and Docker binding it is a loopback-test race.

-- | Reserve and release a host port before declaring the proxy public URL.
freeHostPort :: IO Int
freeHostPort = withHostSocket (tupleToHostAddress (127, 0, 0, 1)) (\port _ -> pure port)

withHostSocket :: HostAddress -> (Int -> Socket -> IO a) -> IO a
withHostSocket address action =
    bracket (socket AF_INET Stream defaultProtocol) close $ \sock -> do
        bind sock (SockAddrInet 0 address)
        port <- fromIntegral <$> socketPort sock
        action port sock

-- | Serve a fixture to both bridge containers and local observers, propagating server failures.
withBridgeApplication :: Application -> (Int -> IO a) -> IO a
withBridgeApplication application action =
    withHostSocket (tupleToHostAddress (0, 0, 0, 0)) $ \port sock -> do
        listen sock maxListenQueue
        owner <- myThreadId
        ready <- newEmptyMVar
        let settings =
                Warp.setBeforeMainLoop (putMVar ready ())
                    . Warp.setOnException (\_ fault -> when (Warp.defaultShouldDisplayException fault) (throwTo owner fault))
                    $ Warp.defaultSettings
        -- Warp's test helpers always bind loopback, even when setHost names another address.
        race (Warp.runSettingsSocket settings sock application) (readMVar ready >> action port) >>= \case
            Right result -> pure result
            -- An early listener exit invalidates the fixture's dependent observations.
            Left () -> throwIO (TestContractEscape "bridge fixture server stopped before its action finished")

-- | A random identifier for concurrently acquired resources across processes.
uniqueSuffix :: IO String
uniqueSuffix = show <$> (randomIO :: IO Word64)

-- | Poll logs at 250 ms intervals, returning False when the attempt budget expires.
awaitContainerLog :: String -> (Text -> Bool) -> Int -> IO Bool
awaitContainerLog cname matches attempts =
    pollUntil attempts 250000 id (matches <$> containerLogs cname)

-- | Read combined container output. Missing resources produce empty diagnostic evidence.
containerLogs :: String -> IO Text
containerLogs cname =
    handleAny (\_ -> pure "") $ do
        (_, out, err) <- readProcess (proc "docker" ["logs", cname])
        pure (decodeUtf8 (LBS.toStrict out) <> decodeUtf8 (LBS.toStrict err))
