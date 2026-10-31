-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native Cargo clients share the E2E command runner and own fresh registry and compilation state.
module Ecluse.E2E.Harness.Cargo (
    -- * Isolated clients
    CargoToolchain (..),
    currentCargo,
    floorCargo,
    CargoRole (..),
    CargoProject (..),
    withCargoProject,
    cargoUseFixture,
    cargoRestrictPublication,
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
    CargoInvocation (..),
    observeCargo,
) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable, removeFile, removePathForcibly)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeDirectory, (</>))
import UnliftIO.Environment (getEnvironment)

import Ecluse.E2E.Harness.Client (compilerEnvironmentKeys, runClient, withClientDir)
import Ecluse.E2E.Harness.NativeEdge (NativeTraffic, nativeTrafficSince)
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

-- | Every role replaces crates.io with Écluse. A publisher also names Écluse its default registry.
data CargoRole = CargoConsumer | CargoPublisher
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
withCargoProject :: CargoToolchain -> CargoRole -> E2E -> Text -> Maybe Text -> (CargoProject -> IO a) -> IO a
withCargoProject toolchain role proxy index token use = withClientDir "cargo" $ \directory -> do
    unless ("sparse+https://" `T.isPrefixOf` index) (fail "Cargo fixture requires an explicit sparse HTTPS endpoint")
    unless ("/" `T.isSuffixOf` index) (fail "Cargo fixture sparse endpoint lacks a trailing slash")
    compiler <- required (cargoCompiler toolchain)
    _ <- required (cargoCommand toolchain)
    let home = directory </> "home"
        target = directory </> "target"
        installation = directory </> "installed"
    traverse_ (createDirectoryIfMissing True) [home, target, installation, directory </> "src"]
    writeFileText (home </> "config.toml") (configuration role index)
    forM_ token $ \value -> writeFileText (home </> "credentials.toml") ("[registries.ecluse-test]\ntoken = " <> tomlString value <> "\n")
    writeFileText (directory </> "Cargo.toml") (consumerManifest cargoRoot ("=" <> cargoVersion))
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

-- The documented client setup: replace-with names the registries entry, with no separate source table.
configuration :: CargoRole -> Text -> Text
configuration role index =
    T.unlines $
        [ "[registry]"
        , "global-credential-providers = [\"cargo:token\"]"
        ]
            <> ["default = \"ecluse-test\"" | role == CargoPublisher]
            <> [ "[registries.ecluse-test]"
               , "index = " <> tomlString index
               , "credential-provider = \"cargo:token\""
               , "[net]"
               , "retry = 0"
               , "[http]"
               , "proxy = \"\""
               , "ssl-verify = true"
               , "[source.crates-io]"
               , "replace-with = \"ecluse-test\""
               ]

consumerManifest :: Text -> Text -> Text
consumerManifest name requirement =
    T.unlines
        [ "[package]"
        , "name = \"ecluse-e2e-consumer\""
        , "version = \"0.0.0\""
        , "edition = \"2021\""
        , "resolver = \"2\""
        , "[dependencies]"
        , name <> " = { version = \"" <> requirement <> "\" }"
        ]

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

-- | Allow publication only to Écluse, as the documented first-party crate setup does.
cargoRestrictPublication :: CargoProject -> IO ()
cargoRestrictPublication project = do
    let path = cargoDirectory project </> "Cargo.toml"
    manifest <- decodeUtf8 <$> readFileBS path
    writeFileText path (T.replace "[package]\n" "[package]\npublish = [\"ecluse-test\"]\n" manifest)

-- | Select one native dependency without inheriting the graph consumer's executable source.
cargoSelectDependency :: Text -> Text -> CargoProject -> IO ()
cargoSelectDependency name version = cargoSelectRequirement name ("=" <> version)

-- | Select a complete native version requirement for fresh exact or unpinned client observations.
cargoSelectRequirement :: Text -> Text -> CargoProject -> IO ()
cargoSelectRequirement name requirement project = do
    writeFileText (cargoDirectory project </> "Cargo.toml") (consumerManifest name requirement)
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

-- | Publish an owned fixture to the default registry, including Cargo's package verification.
cargoPublish :: CargoProject -> IO ClientResult
cargoPublish project = runCargo project ["publish", "--manifest-path", cargoDirectory project </> "Cargo.toml", "--allow-dirty"]

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

-- | Native output and all three request inventories belong to one isolated command interval.
data CargoInvocation = CargoInvocation
    { cargoInvocationClient :: ClientResult
    , cargoInvocationTraffic :: NativeTraffic
    }
    deriving stock (Show)

-- | Slice append-only fixture logs around a native command. Shared contracts own the resulting assertions.
observeCargo :: IO NativeTraffic -> CargoProject -> [String] -> IO CargoInvocation
observeCargo observations project arguments = do
    before <- observations
    result <- runCargo project arguments
    after <- observations
    traffic <- nativeTrafficSince before after
    pure (CargoInvocation result traffic)
