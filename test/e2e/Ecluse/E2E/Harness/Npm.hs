-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | npm clients for end-to-end scenarios.
Each project isolates npm state and disables package lifecycle scripts.
-}
module Ecluse.E2E.Harness.Npm (
    npmInstall,
    npmInstallIn,
    npmPublishIn,
    withNpmProject,
    withPublishProject,
    installWithLifecycleProbe,
    installedVersion,
    npmPublicReachable,

    -- * What an install left behind
    Resolved (..),
    resolvedGraph,
    loadedExport,
    linkedExecutable,
    installedTree,
    lockedSources,
    redactedProxy,
    localInstallSources,

    -- * Constants
    consumerName,
    npmArtifactPath,
    npmTarballPath,
    publishTargetEnv,
    publishScope,
    publishInScopeName,
    publishOutOfScopeName,
    publishDredgerName,
    publishVersion,
) where

import Data.Aeson (FromJSON (parseJSON), Object, Value (Object), decodeFileStrict', decodeStrict, eitherDecodeStrict, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing, doesFileExist, doesPathExist, listDirectory)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import UnliftIO.Environment (getEnvironment)

import Ecluse.E2E.Fixtures.Npm (psName, publicOnlyPkg)
import Ecluse.E2E.Harness.Client (clientReport, runClient, withClientDir)
import Ecluse.E2E.Harness.Proxy (proxyStatus)
import Ecluse.E2E.Harness.Stub (StubRoute (Mirror), stubUrl)
import Ecluse.E2E.Harness.Types
import Ecluse.Test.InstalledTree (InstalledTree, TreeEntry (TreeFile), normaliseNpmSources, snapshotTree)

-- | Isolate a consumer's npm state and remove its project directory after the action.
withNpmProject :: E2E -> (NpmProject -> IO a) -> IO a
withNpmProject e2e = withProjectContents e2e consumerPackageJson ""

withProjectContents :: E2E -> Text -> Text -> (NpmProject -> IO a) -> IO a
withProjectContents e2e packageJson npmrcContents use =
    withClientDir "npm" $ \projectDir -> do
        let cacheDir = projectDir </> cacheDirName
            prefixDir = projectDir </> "prefix"
            npmrc = projectDir </> ".npmrc"
        createDirectoryIfMissing True cacheDir
        createDirectoryIfMissing True prefixDir
        writeFileText (projectDir </> "package.json") packageJson
        writeFileText npmrc npmrcContents
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
                , -- No npm child may run an upstream package's lifecycle scripts, an arbitrary-code-execution
                  -- surface. This project sits outside the repo tree, beyond the root @.npmrc@'s reach.
                  ("npm_config_ignore_scripts", "true")
                , -- npm's 10 s then 60 s retry backoff is sized for the public internet, and
                  -- every registry here is a container on a local network. Keep the retries.
                  ("npm_config_fetch_retry_mintimeout", "200")
                , ("npm_config_fetch_retry_maxtimeout", "1000")
                , ("HOME", projectDir)
                ]
            cleanEnv =
                filter
                    (\(k, _) -> k `notElem` map fst overrides && not ("npm_config_" `isPrefixOf` k))
                    baseEnv
                    <> overrides
        use NpmProject{npDir = projectDir, npEnv = cleanEnv}

-- | Prepare an isolated publisher with the token npm requires before contacting the proxy.
withPublishProject :: E2E -> Text -> Text -> (NpmProject -> IO a) -> IO a
withPublishProject e2e name version =
    withProjectContents
        e2e
        (publishPackageJson name version)
        (npmAuthLine (e2eRegistry e2e) publishAuthToken)

runNpm :: NpmProject -> [String] -> IO ClientResult
runNpm proj = runClient (npDir proj) (npEnv proj) "npm"

{- | The version a project resolved for an installed package, read from its own manifest.
'Nothing' when the package is absent or its manifest declares no version.
-}
installedVersion :: NpmProject -> Text -> IO (Maybe Text)
installedVersion proj pkg = do
    present <- doesFileExist manifest
    if present
        then do
            decoded <- decodeFileStrict' manifest
            pure (parseMaybe (.: "version") =<< (decoded :: Maybe Object))
        else pure Nothing
  where
    manifest = npDir proj </> "node_modules" </> toString pkg </> "package.json"

-- | @npm install \<pkg\>@ in a project, resolving the package's metadata through the proxy.
npmInstallIn :: NpmProject -> Text -> IO ClientResult
npmInstallIn proj pkg = runNpm proj ["install", toString pkg]

-- | Publish through the configured proxy with package lifecycle scripts disabled.
npmPublishIn :: NpmProject -> IO ClientResult
npmPublishIn proj = runNpm proj ["publish"]

-- | Install through the proxy in a temporary project that is removed after the command.
npmInstall :: E2E -> Text -> IO ClientResult
npmInstall e2e pkg = withNpmProject e2e (`npmInstallIn` pkg)

{- | Whether the proxy can serve 'publicOnlyPkg', which only the npm public upstream holds: 'True'
while that upstream answers the proxy, 'False' through an outage of it.
-}
npmPublicReachable :: E2E -> IO Bool
npmPublicReachable e2e = (== 200) <$> proxyStatus e2e ("/npm/" <> psName publicOnlyPkg)

-- | One edge of the graph npm resolved: a dependent, and the version installed for one of its needs.
data Resolved = Resolved
    { rsDependent :: Text
    , rsPackage :: Text
    , rsVersion :: Text
    }
    deriving stock (Eq, Ord, Show)

{- | The dependency graph npm resolved for a project, as @npm ls --all@ reports it: ordinary and
peer edges alike. 'Left' carries the client's output when the listing fails or cannot be read.
-}
resolvedGraph :: NpmProject -> IO (Either Text (Set Resolved))
resolvedGraph proj = do
    listed <- runNpm proj ["ls", "--all", "--json"]
    pure $ case (crExit listed, eitherDecodeStrict (encodeUtf8 (crStdout listed))) of
        (ExitSuccess, Right (ListedProject name root)) -> Right (Set.fromList (edgesFrom name root))
        _ -> Left (clientReport listed "listed no readable tree")

-- The slice of @npm ls --json@ the graph needs: a node's version and what it resolved, by name.
data Listed = Listed Text (Map Text Listed)

instance FromJSON Listed where
    parseJSON = withObject "npm ls node" $ \node ->
        Listed <$> node .:? "version" .!= "" <*> node .:? "dependencies" .!= mempty

-- The root node, which alone states its own name. Every other node is keyed by its name.
data ListedProject = ListedProject Text Listed

instance FromJSON ListedProject where
    parseJSON = withObject "npm ls project" $ \project ->
        ListedProject <$> project .: "name" <*> parseJSON (Object project)

edgesFrom :: Text -> Listed -> [Resolved]
edgesFrom dependent (Listed _ needs) =
    concat [Resolved dependent name version : edgesFrom name need | (name, need@(Listed version _)) <- Map.toList needs]

{- | Load a package from the project with @node@ and decode what its module exports. 'Left' carries
the runtime's output when the load fails, as it does when a package the module requires is absent.
-}
loadedExport :: NpmProject -> Text -> IO (Either Text Value)
loadedExport proj package =
    printedValue <$> runClient (npDir proj) (npEnv proj) "node" ["-e", "process.stdout.write(JSON.stringify(require(" <> show package <> ")))"]

{- | Run an executable npm linked into the project, by the name its package's @bin@ gives it, and
decode the JSON it prints. 'Nothing' when npm linked no executable of that name.
-}
linkedExecutable :: NpmProject -> Text -> IO (Maybe (Either Text Value))
linkedExecutable proj name = do
    linked <- doesFileExist link
    if linked then Just . printedValue <$> runClient (npDir proj) (npEnv proj) link [] else pure Nothing
  where
    link = npDir proj </> "node_modules" </> ".bin" </> toString name

-- The JSON value a successful command printed, or the command's whole output.
printedValue :: ClientResult -> Either Text Value
printedValue res = case (crExit res, decodeStrict (encodeUtf8 (crStdout res))) of
    (ExitSuccess, Just value) -> Right value
    _ -> Left (clientReport res "printed no JSON value")

-- | Snapshot the installed tree, normalising only npm lockfile package source URLs.
installedTree :: E2E -> NpmProject -> IO InstalledTree
installedTree e2e proj =
    normaliseNpmSources (e2eBaseUrl e2e) redactedProxy
        <$> snapshotTree (npDir proj) ["node_modules", lockfileName, "package.json"]

-- | What stands for the proxy's address in an 'installedTree'.
redactedProxy :: Text
redactedProxy = "<proxy>"

{- | The location the lockfile of an 'installedTree' records for each installed package, which names
the registry that supplied it. Empty when the tree holds no readable lockfile.
-}
lockedSources :: InstalledTree -> [Text]
lockedSources tree = fromMaybe [] $ do
    TreeFile _ bytes <- Map.lookup lockfileName tree
    lockfile <- decodeStrict bytes
    packages <- parseMaybe (.: "packages") (lockfile :: Object)
    pure (mapMaybe (parseMaybe (.: "resolved")) (Map.elems (packages :: Map Text Object)))

{- | Everything in a project that could supply an install without the registry: npm cache entries,
a lockfile, and @node_modules@. A new project holds none.
-}
localInstallSources :: NpmProject -> IO [FilePath]
localInstallSources proj = do
    cached <- listDirectory (npDir proj </> cacheDirName)
    kept <- filterM (doesPathExist . (npDir proj </>)) [lockfileName, "node_modules"]
    pure (map (cacheDirName </>) cached <> kept)

cacheDirName :: FilePath
cacheDirName = "cache"

lockfileName :: FilePath
lockfileName = "package-lock.json"

-- | Report whether installation executed a sentinel-writing lifecycle script that should be disabled.
installWithLifecycleProbe :: E2E -> IO (ClientResult, Bool)
installWithLifecycleProbe e2e =
    withProjectContents e2e lifecycleProbePackageJson "" $ \proj -> do
        res <- runNpm proj ["install"]
        ran <- doesFileExist (npDir proj </> lifecycleSentinel)
        pure (res, ran)

lifecycleSentinel :: FilePath
lifecycleSentinel = "lifecycle-script-ran"

lifecycleProbePackageJson :: Text
lifecycleProbePackageJson =
    "{\"name\":\"e2e-lifecycle-probe\",\"version\":\"1.0.0\",\"private\":true,\"scripts\":{\"postinstall\":\"touch lifecycle-script-ran\"}}\n"

consumerPackageJson :: Text
consumerPackageJson = "{\"name\":\"" <> consumerName <> "\",\"version\":\"1.0.0\",\"private\":true}\n"

-- | The name of the project 'withNpmProject' installs into, which heads the graph npm resolves.
consumerName :: Text
consumerName = "e2e-consumer"

-- | The path a registry serves one unscoped package version's artifact at, under its own base.
npmArtifactPath :: Text -> Text -> Text
npmArtifactPath name version = "/" <> name <> "/-/" <> name <> "-" <> version <> ".tgz"

-- | The proxy path one npm package version's artifact is served at.
npmTarballPath :: Text -> Text -> Text
npmTarballPath name version = "/npm" <> npmArtifactPath name version

-- | Publish first-party packages into the Verdaccio store used by the proxy's private upstream.
publishTargetEnv :: [(Text, Text)]
publishTargetEnv =
    [ ("ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET__VERDACCIO__URL", stubUrl Mirror)
    , ("ECLUSE_MOUNTS__NPM__FIRST_PARTY", publishScope)
    ]

-- | The configured first-party namespace, shared by all in-scope fixture names.
publishScope :: Text
publishScope = "@acme"

-- | An in-scope package for publication through the proxy.
publishInScopeName :: Text
publishInScopeName = publishScope <> "/e2e-publish"

-- | An out-of-scope package whose publication must stop before an upstream write.
publishOutOfScopeName :: Text
publishOutOfScopeName = "@rogue/e2e-shadow"

-- | A first-party package reserved for the Dredger's protection scenario.
publishDredgerName :: Text
publishDredgerName = publishScope <> "/e2e-dredger-first-party"

-- | The single version the publish scenarios publish (and read back).
publishVersion :: Text
publishVersion = "1.0.0"

-- The target accepts any token, but npm requires one before publishing.
publishAuthToken :: Text
publishAuthToken = "e2e-publisher-token"

-- npm refuses to publish a package marked private.
-- The allowlist excludes npm's project-local cache and logs from the fixture artifact.
publishPackageJson :: Text -> Text -> Text
publishPackageJson name version =
    "{\"name\":\"" <> name <> "\",\"version\":\"" <> version <> "\",\"files\":[\"package.json\"]}\n"

-- npm requires a trailing slash on the host/path authentication key, including a registry root.
npmAuthLine :: Text -> Text -> Text
npmAuthLine registry token =
    "//" <> T.dropWhileEnd (== '/') (withoutScheme registry) <> "/:_authToken=" <> token <> "\n"
  where
    withoutScheme u = fromMaybe u (T.stripPrefix "http://" u <|> T.stripPrefix "https://" u)
