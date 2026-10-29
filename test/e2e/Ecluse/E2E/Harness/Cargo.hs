-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native Cargo clients share the E2E command runner and own fresh registry and compilation state.
module Ecluse.E2E.Harness.Cargo (
    -- * Isolated clients
    CargoToolchain (..),
    currentCargo,
    floorCargo,
    CargoView (..),
    CargoProject (..),
    withCargoProject,
    cargoUseFixture,
    cargoSelectDependency,
    cargoSelectRequirement,
    runCargo,
    cargoVersions,

    -- * Native operations
    cargoResolve,
    cargoFetch,
    cargoMetadata,
    cargoBuild,
    cargoRun,
    cargoInstall,
    cargoPublish,
    cargoInstalledExecutable,

    -- * Independent observations
    cargoGraph,
    cargoLock,
    cargoTree,
    CargoRequest (..),
    CargoTraffic (..),
    CargoInvocation (..),
    observeCargo,
    cargoTrafficSince,
) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable, removeFile, removePathForcibly)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeDirectory, (</>))
import UnliftIO.Environment (getEnvironment)

import Ecluse.E2E.Harness.Client (compilerEnvironmentKeys, runClient, withClientDir)
import Ecluse.E2E.Harness.Types (ClientResult (..), E2E, clientCertificateBundle)
import Ecluse.Test.InstalledTree (InstalledTree, snapshotTree)
import Ecluse.Test.Registry.Cargo.Fixtures (CargoCrate (..), cargoRoot, cargoVersion)
import Ecluse.Test.Registry.Cargo.Reference (CargoGraph, CargoLocked, referenceGraph, referenceLock)

-- | Both clients use the pinned compiler. The floor observes Cargo compatibility, not an old compiler.
data CargoToolchain = CargoToolchain
    { cargoCommand :: String
    , cargoCompiler :: String
    }
    deriving stock (Eq, Show)

-- | The default Nix toolchain supplies the current native client.
currentCargo :: CargoToolchain
currentCargo = CargoToolchain "cargo" "rustc"

-- | The official pinned 1.74 client uses the same controlled compiler as the current client.
floorCargo :: CargoToolchain
floorCargo = CargoToolchain "cargo-1.74" "rustc"

-- | The public replacement and private alternate retain distinct native source identities.
data CargoView = CargoReplacement | CargoAlternate
    deriving stock (Eq, Show)

-- | Every project owns its HOME, cache, target directory and installation root.
data CargoProject = CargoProject
    { cargoDirectory :: FilePath
    , cargoEnvironment :: [(String, String)]
    , cargoToolchain :: CargoToolchain
    , cargoIndex :: Text
    , cargoInstallRoot :: FilePath
    }

-- | A required missing tool fails setup. The sparse endpoint and token belong to this fixture only.
withCargoProject :: CargoToolchain -> CargoView -> E2E -> Text -> Maybe Text -> (CargoProject -> IO a) -> IO a
withCargoProject toolchain view proxy index token use = withClientDir "cargo" $ \directory -> do
    unless ("sparse+https://" `T.isPrefixOf` index) (fail "Cargo fixture requires an explicit sparse HTTPS endpoint")
    unless ("/" `T.isSuffixOf` index) (fail "Cargo fixture sparse endpoint lacks a trailing slash")
    compiler <- required (cargoCompiler toolchain)
    _ <- required (cargoCommand toolchain)
    let home = directory </> "home"
        target = directory </> "target"
        installation = directory </> "installed"
    traverse_ (createDirectoryIfMissing True) [home, target, installation, directory </> "src"]
    writeFileText (home </> "config.toml") (configuration view index)
    forM_ token $ \value -> writeFileText (home </> "credentials.toml") ("[registries.ecluse-test]\ntoken = " <> tomlString value <> "\n")
    writeFileText (directory </> "Cargo.toml") (consumerManifest view cargoRoot ("=" <> cargoVersion))
    writeFileText (directory </> "src/main.rs") "fn main() { println!(\"{}\", ecluse_e2e_root::value()); }\n"
    environment <- getEnvironment
    let overrides =
            [ ("HOME", home)
            , ("CARGO_HOME", home)
            , ("CARGO_TARGET_DIR", target)
            , ("CARGO_INSTALL_ROOT", installation)
            , ("RUSTC", compiler)
            , ("CARGO_HTTP_PROXY", "")
            , ("CARGO_HTTP_CAINFO", clientCertificateBundle proxy)
            , ("CARGO_NET_RETRY", "0")
            , ("CARGO_TERM_COLOR", "never")
            , ("GIT_CONFIG_NOSYSTEM", "1")
            , ("XDG_CONFIG_HOME", home </> "config")
            ]
        clean = filter ((`elem` inheritedKeys) . fst) environment <> overrides
    use (CargoProject directory clean toolchain index installation)
  where
    required command = findExecutable command >>= maybe (fail ("required Cargo tool is absent: " <> command)) pure

inheritedKeys :: [String]
inheritedKeys =
    [ "PATH"
    , "TMPDIR"
    , "TEMP"
    , "TMP"
    , "LANG"
    , "LC_ALL"
    , "SSL_CERT_FILE"
    , "NIX_SSL_CERT_FILE"
    ]
        <> compilerEnvironmentKeys

configuration :: CargoView -> Text -> Text
configuration view index =
    T.unlines $
        [ "[registry]"
        , "global-credential-providers = [\"cargo:token\"]"
        , "[registries.ecluse-test]"
        , "index = " <> tomlString index
        , "credential-provider = \"cargo:token\""
        , "[net]"
        , "retry = 0"
        , "[http]"
        , "proxy = \"\""
        , "ssl-verify = true"
        ]
            <> case view of
                CargoAlternate -> []
                CargoReplacement ->
                    [ "[source.crates-io]"
                    , "replace-with = \"ecluse-test\""
                    , "[source.ecluse-test]"
                    , "registry = " <> tomlString index
                    ]

consumerManifest :: CargoView -> Text -> Text -> Text
consumerManifest view name requirement =
    T.unlines
        [ "[package]"
        , "name = \"ecluse-e2e-consumer\""
        , "version = \"0.0.0\""
        , "edition = \"2021\""
        , "resolver = \"2\""
        , "[dependencies]"
        , name <> " = { version = \"" <> requirement <> "\"" <> registry <> " }"
        ]
  where
    registry = case view of
        CargoReplacement -> ""
        CargoAlternate -> ", registry = \"ecluse-test\""

tomlString :: Text -> Text
tomlString = decodeUtf8 . LBS.toStrict . Aeson.encode

-- | Copy only the declared repository-owned source files for a native publication observation.
cargoUseFixture :: CargoProject -> FilePath -> CargoCrate -> IO ()
cargoUseFixture project fixtureRoot spec = do
    removePathForcibly (cargoDirectory project </> "src")
    forM_ ["build.rs", "Cargo.lock"] $ \path -> do
        let full = cargoDirectory project </> path
        present <- doesFileExist full
        when present (removeFile full)
    forM_ (crateFiles spec) $ \path -> do
        let destination = cargoDirectory project </> path
        createDirectoryIfMissing True (takeDirectory destination)
        readFileBS (fixtureRoot </> crateDirectory spec </> path) >>= writeFileBS destination

-- | Select one native dependency without inheriting the graph consumer's executable source.
cargoSelectDependency :: CargoView -> Text -> Text -> CargoProject -> IO ()
cargoSelectDependency view name version = cargoSelectRequirement view name ("=" <> version)

-- | Select a complete native version requirement for fresh exact or unpinned client observations.
cargoSelectRequirement :: CargoView -> Text -> Text -> CargoProject -> IO ()
cargoSelectRequirement view name requirement project = do
    writeFileText (cargoDirectory project </> "Cargo.toml") (consumerManifest view name requirement)
    writeFileText (cargoDirectory project </> "src/main.rs") "fn main() {}\n"

-- | All native commands use the shared E2E runner and return their exact exit status and output.
runCargo :: CargoProject -> [String] -> IO ClientResult
runCargo project = runClient (cargoDirectory project) (cargoEnvironment project) (cargoCommand (cargoToolchain project))

-- | Observe both actual executable versions before making a compatibility claim.
cargoVersions :: CargoProject -> IO (ClientResult, ClientResult)
cargoVersions project =
    (,) <$> runCargo project ["--version"] <*> runClient (cargoDirectory project) (cargoEnvironment project) (cargoCompiler (cargoToolchain project)) ["--version", "--verbose"]

-- | Resolve into an initially empty native cache and create the consumer lockfile.
cargoResolve :: CargoProject -> [String] -> IO ClientResult
cargoResolve project options = runCargo project (["generate-lockfile"] <> options)

-- | Fetch every locked target archive without compiling foreign code.
cargoFetch :: CargoProject -> IO ClientResult
cargoFetch project = runCargo project ["fetch", "--locked"]

-- | Metadata resolves from the existing lockfile and includes target and feature observations.
cargoMetadata :: CargoProject -> [String] -> IO ClientResult
cargoMetadata project options = runCargo project (["metadata", "--format-version", "1", "--locked"] <> options)

-- | Compile only repository-owned graph sources in the controlled client environment.
cargoBuild :: CargoProject -> [String] -> IO ClientResult
cargoBuild project options = runCargo project (["build", "--locked", "--message-format=json"] <> options)

-- | The owned consumer observes the library graph's feature and platform behaviour.
cargoRun :: CargoProject -> [String] -> IO ClientResult
cargoRun project options = runCargo project (["run", "--quiet", "--locked"] <> options)

-- | Registry installs use the isolated CARGO_HOME because Cargo ignores ordinary project config here.
cargoInstall :: CargoProject -> Text -> Text -> [String] -> IO ClientResult
cargoInstall project name version options =
    runCargo project (["install", "--registry", "ecluse-test", "--version", toString ("=" <> version), "--locked", "--root", cargoInstallRoot project, toString name] <> options)

-- | Publish an owned fixture through the configured native API, including Cargo's package verification.
cargoPublish :: CargoProject -> IO ClientResult
cargoPublish project = runCargo project ["publish", "--registry", "ecluse-test", "--manifest-path", cargoDirectory project </> "Cargo.toml", "--allow-dirty"]

-- | Only an executable installed from the repository-owned graph may be selected here.
cargoInstalledExecutable :: CargoProject -> Text -> IO ClientResult
cargoInstalledExecutable project executableName =
    runClient (cargoDirectory project) (cargoEnvironment project) (cargoInstallRoot project </> "bin" </> toString executableName) []

-- | Parse native metadata without depending on production index or graph readers.
cargoGraph :: ClientResult -> Either Text CargoGraph
cargoGraph result = case crExit result of
    ExitSuccess -> referenceGraph (encodeUtf8 (crStdout result))
    _ -> Left ("Cargo metadata failed: " <> crStderr result)

-- | Missing lockfiles remain an explicit observation rather than an empty successful graph.
cargoLock :: CargoProject -> IO (Either Text [CargoLocked])
cargoLock project = do
    let path = cargoDirectory project </> "Cargo.lock"
    present <- doesFileExist path
    if present then referenceLock . decodeUtf8 <$> readFileBS path else pure (Left "Cargo wrote no lockfile")

-- | Retain source files, exact archives, install metadata and the consumer lockfile for contract assertions.
cargoTree :: CargoProject -> IO InstalledTree
cargoTree project = snapshotTree (cargoDirectory project) ["home/registry", "installed", "Cargo.lock"]

-- | A captured request keeps its scheme, authority, method, path and status for bypass assertions.
data CargoRequest = CargoRequest
    { cargoRequestScheme :: Text
    , cargoRequestHost :: Text
    , cargoRequestMethod :: Text
    , cargoRequestPath :: Text
    , cargoRequestStatus :: Int
    }
    deriving stock (Eq, Show)

-- | The existing fixture observers supply independent incoming, proxy and upstream request inventories.
data CargoTraffic = CargoTraffic
    { cargoClientRequests :: [CargoRequest]
    , cargoProxyRequests :: [CargoRequest]
    , cargoUpstreamRequests :: [CargoRequest]
    }
    deriving stock (Eq, Show)

-- | Native output and all three request inventories belong to one isolated command interval.
data CargoInvocation = CargoInvocation
    { cargoInvocationClient :: ClientResult
    , cargoInvocationTraffic :: CargoTraffic
    }
    deriving stock (Show)

-- | Slice append-only fixture logs around a native command. Shared contracts own the resulting assertions.
observeCargo :: IO CargoTraffic -> CargoProject -> [String] -> IO CargoInvocation
observeCargo observations project arguments = do
    before <- observations
    result <- runCargo project arguments
    after <- observations
    traffic <- cargoTrafficSince before after
    pure (CargoInvocation result traffic)

-- | Keep the same append-only checks for one command or a complete native installation.
cargoTrafficSince :: CargoTraffic -> CargoTraffic -> IO CargoTraffic
cargoTrafficSince before after = do
    client <- delta (cargoClientRequests before) (cargoClientRequests after)
    proxy <- delta (cargoProxyRequests before) (cargoProxyRequests after)
    upstream <- delta (cargoUpstreamRequests before) (cargoUpstreamRequests after)
    pure (CargoTraffic client proxy upstream)
  where
    delta previous current = do
        unless (length current >= length previous) (fail "Cargo request inventory reset during a command")
        pure (drop (length previous) current)
