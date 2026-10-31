-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Isolated pip clients for digest checks, dependency installs, and platform selection.
module Ecluse.E2E.Harness.Pip (
    withPipProject,
    pipCredentialUrl,
    pipInstallIn,
    pipInstalled,
    pipInstalledTree,
    pipInstallsWheel,
    pipInstallGraph,
    pipInstallRequirement,
    pipInstallAndInspect,
    pipInstallThenRun,
    PythonGraph,
    installedPythonGraph,
    pipObserveGraph,
    PipTargetInstall (..),
    pipInstallForPlatform,
    pipRefusedForPlatform,
    pipSingleAttempt,

    -- * The served index
    advertisedFiles,
    advertisedCompanions,
) where

import Data.Aeson (Value (Array, Object, String))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeDirectory, (</>))
import Test.Hspec (Expectation, expectationFailure)
import UnliftIO.Environment (getEnvironment)

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.E2E.Fixtures.PyPI (pypiDistInfo, pypiProject, pypiVersion, pypiWheelFile)
import Ecluse.E2E.Harness.Client (runClient, withClientDir)
import Ecluse.E2E.Harness.Proxy (logTail, logTailLines, proxyContainerLogs, proxyGet, shouldSucceedThroughProxy)
import Ecluse.E2E.Harness.Types
import Ecluse.Test.Contract.Install (InstallDriver (..), installsObserved)
import Ecluse.Test.Contract.Mirror (Observed (..))
import Ecluse.Test.InstalledTree (InstalledTree, TreeEntry (TreeFile), snapshotTree)

-- | Add the fixture's URL-safe Basic credentials to its local HTTP index URL.
pipCredentialUrl :: Text -> Text -> Text -> Text
pipCredentialUrl username password = T.replace "http://" ("http://" <> username <> ":" <> password <> "@")

-- | Pin @project==version@ to @digest@ in an isolated project removed after the action.
withPipProject :: E2E -> Text -> Text -> Text -> (PipProject -> IO a) -> IO a
withPipProject e2e project version digest use =
    withPipClient e2e $ \client -> do
        writeFileText (ppDir client </> requirementsFile) (requirement project version digest)
        use client

-- A fresh home directory gives each client its own empty cache and configuration.
withPipClient :: E2E -> (PipProject -> IO a) -> IO a
withPipClient e2e use =
    withClientDir "pip" $ \projectDir -> do
        index <- clientEndpoint PyPI e2e
        baseEnv <- getEnvironment
        -- --isolated drops PIP_* and the user config but still reads the global and site
        -- config files, which PIP_CONFIG_FILE is the only way to silence.
        let cleanEnv =
                filter ((`notElem` ["HOME", "PIP_CONFIG_FILE"]) . fst) baseEnv
                    <> [("HOME", projectDir), ("PIP_CONFIG_FILE", "/dev/null")]
        use PipProject{ppDir = projectDir, ppEnv = cleanEnv, ppIndex = index, ppCertificateBundle = if "https://" `T.isPrefixOf` e2eBaseUrl e2e then Just (clientCertificateBundle e2e) else Nothing}

-- | Install into the project target with @--require-hashes@ checking the advertised digest.
pipInstallIn :: PipProject -> IO ClientResult
pipInstallIn proj =
    runClient
        (ppDir proj)
        (ppEnv proj)
        "python3"
        ( [ "-m"
          , "pip"
          , "--isolated"
          , "install"
          , "--no-cache-dir"
          , "--disable-pip-version-check"
          , "--no-input"
          , "--require-hashes"
          , -- An sdist executes its build backend. This fixture permits only wheels.
            "--only-binary=:all:"
          , "--index-url"
          , toString (ppIndex proj)
          , "--target"
          , ppDir proj </> targetDir
          , "--requirement"
          , ppDir proj </> requirementsFile
          ]
            <> pipCertificateArguments proj
        )

pipCertificateArguments :: PipProject -> [String]
pipCertificateArguments project = maybe [] (\path -> ["--cert", path]) (ppCertificateBundle project)

-- | Install the fixture wheel with its advertised digest and assert its installed metadata exists.
pipInstallsWheel :: E2E -> [(Text, Text)] -> Expectation
pipInstallsWheel e2e advertised =
    case filter ((== pypiWheelFile) . fst) advertised of
        [(_, digest)] ->
            withPipProject e2e pypiProject pypiVersion digest $ \proj -> do
                installsObserved
                    InstallDriver
                        { installRun = (,) <$> pipInstallIn proj <*> pipInstalled proj pypiDistInfo
                        , installExpected = True
                        }
        other ->
            expectationFailure ("the served index advertised " <> show (map fst other) <> ", not one digested wheel")

-- | Whether a wheel's @.dist-info@ directory landed in the project's install target.
pipInstalled :: PipProject -> Text -> IO Bool
pipInstalled proj distInfo = doesDirectoryExist (ppDir proj </> targetDir </> toString distInfo)

-- | Observe all installation output, including unexpected distributions and partial files.
pipInstalledTree :: PipProject -> IO InstalledTree
pipInstalledTree proj = snapshotTree (ppDir proj) [targetDir]

-- | Read advertised @(filename, sha256)@ pairs through the proxy, failing if the mount refuses.
advertisedFiles :: E2E -> Text -> IO [(Text, Text)]
advertisedFiles = advertisedDigests "hashes"

-- | Read each distribution's advertised @(filename, sha256)@ for its metadata companion.
advertisedCompanions :: E2E -> Text -> IO [(Text, Text)]
advertisedCompanions = advertisedDigests "core-metadata"

advertisedDigests :: Aeson.Key -> E2E -> Text -> IO [(Text, Text)]
advertisedDigests field e2e project = do
    (status, body) <- proxyGet e2e ("/pypi/simple/" <> project)
    unless (status == 200) $ do
        logs <- proxyContainerLogs e2e
        fail (toString (indexRefusal project status logs))
    pure (digestedFiles field body)

-- A refusal reaches the wire as a bare status, so its reason exists only in the proxy's own
-- JSONL. An index that never resolved leaves the failed metadata read there, naming the upstream
-- and its status or transport cause. A denial leaves "denied" lines naming the rule, at debug
-- level only.
indexRefusal :: Text -> Int -> Text -> Text
indexRefusal project status logs =
    "the pypi mount answered "
        <> show status
        <> " for the "
        <> project
        <> " index. Last "
        <> show logTailLines
        <> " proxy log lines:\n"
        <> logTail logTailLines logs

-- The served PEP 691 document's file entries, keeping only those whose digest map carries a sha256.
digestedFiles :: Aeson.Key -> LByteString -> [(Text, Text)]
digestedFiles field body =
    [ (filename, digest)
    | Object entry <- servedFiles (Aeson.decode body)
    , Just (String filename) <- [KeyMap.lookup "filename" entry]
    , Just (Object hashes) <- [KeyMap.lookup field entry]
    , Just (String digest) <- [KeyMap.lookup "sha256" hashes]
    ]

servedFiles :: Maybe Value -> [Value]
servedFiles = \case
    Just (Object top) | Just (Array files) <- KeyMap.lookup "files" top -> toList files
    _ -> []

-- pip's hash-checking mode needs every requirement pinned and digested, and it resolves
-- no dependency the file does not name.
requirement :: Text -> Text -> Text -> Text
requirement project version digest =
    project <> "==" <> version <> " --hash=sha256:" <> digest <> "\n"

requirementsFile :: FilePath
requirementsFile = "requirements.txt"

targetDir :: FilePath
targetDir = "site"

-- | Capture every site-package file and package-installed script from a fresh virtual environment.
pipInstallGraph :: E2E -> Text -> Text -> IO InstalledTree
pipInstallGraph proxy project version = pipInstallRequirement proxy (pinned project version)

-- | Install one requirement, such as a bare project name that leaves the release to pip.
pipInstallRequirement :: E2E -> Text -> IO InstalledTree
pipInstallRequirement proxy requirementText = do
    (result, tree) <- inspectRequirement proxy requirementText
    void (shouldSucceedThroughProxy proxy result)
    pure tree

-- | Return the native install outcome, all site packages, and the installed package scripts.
pipInstallAndInspect :: E2E -> Text -> Text -> IO (ClientResult, InstalledTree)
pipInstallAndInspect proxy project version = inspectRequirement proxy (pinned project version)

inspectRequirement :: E2E -> Text -> IO (ClientResult, InstalledTree)
inspectRequirement proxy requirementText = withInstalledRequirement proxy requirementText $ \installed ->
    (piClient installed,) <$> snapshotInstallation installed

pinned :: Text -> Text -> Text
pinned project version = project <> "==" <> version

-- | Install one release into a fresh virtual environment, then run @python -c source@ inside it.
pipInstallThenRun :: E2E -> Text -> Text -> Text -> IO (ClientResult, ClientResult)
pipInstallThenRun proxy project version source = withInstalledRequirement proxy (pinned project version) $ \installed ->
    (piClient installed,) <$> runClient (piDirectory installed) (piEnvironment installed) (piPython installed) ["-B", "-c", toString source]

-- | Installed versions and dependency declarations from each package's own metadata.
type PythonGraph = Map Text (Text, [Text])

-- | Observe graph, root exports, and the installed executable through the native virtual environment.
pipObserveGraph :: E2E -> Text -> Text -> Text -> IO (Observed PythonGraph Value, Either Text [(Text, Text)])
pipObserveGraph proxy project version executableName = withInstalledRequirement proxy (pinned project version) $ \installed -> do
    tree <- snapshotInstallation installed
    exported <-
        observeJson
            installed
            (piPython installed)
            ["-B", "-c", "import importlib,json,sys\nprint(json.dumps(importlib.import_module(sys.argv[1]).export(), sort_keys=True))", toString (T.replace "-" "_" project)]
    executed <- observeJson installed (takeDirectory (piPython installed) </> toString executableName) []
    pure (Observed (installedPythonGraph tree) exported (Just executed) tree (piClient installed) (piInitial installed), piSources installed)

data PipInstallation = PipInstallation
    { piClient :: ClientResult
    , piDirectory :: FilePath
    , piPython :: FilePath
    , piSite :: FilePath
    , piScriptRoots :: [FilePath]
    , piEnvironment :: [(String, String)]
    , piInitial :: [FilePath]
    , piSources :: Either Text [(Text, Text)]
    }

withInstalledRequirement :: E2E -> Text -> (PipInstallation -> IO a) -> IO a
withInstalledRequirement proxy requirementText use = withPipClient proxy $ \client -> do
    let virtualenv = ppDir client </> "venv"
        python = virtualenv </> "bin" </> "python"
        report = ppDir client </> "report.json"
        run executable arguments = runClient (ppDir client) (ppEnv client) executable arguments >>= shouldSucceedThroughProxy proxy
    void $ run "python3" ["-m", "venv", "--without-pip", virtualenv]
    location <- run python ["-B", "-c", "import sysconfig\nprint(sysconfig.get_path('purelib'))"]
    scriptLocation <- run python ["-B", "-c", "import os,sysconfig\nprint(os.path.relpath(sysconfig.get_path('scripts'),sysconfig.get_path('purelib')))"]
    let site = toString (T.strip (crStdout location))
        scriptDirectory = toString (T.strip (crStdout scriptLocation))
    bootstrapScripts <- listDirectory (site </> scriptDirectory)
    initial <- listDirectory site
    unless (null initial) (fail "new virtual environment contains installed packages")
    result <-
        runClient
            (ppDir client)
            (ppEnv client)
            "python3"
            ( [ "-m"
              , "pip"
              , "--isolated"
              , "--python"
              , python
              , "install"
              , "--no-cache-dir"
              , "--no-compile"
              , "--disable-pip-version-check"
              , "--no-input"
              , "--only-binary=:all:"
              , "--index-url"
              , toString (ppIndex client)
              , "--report"
              , report
              , toString requirementText
              ]
                <> pipCertificateArguments client
            )
    scriptNames <- listDirectory (site </> scriptDirectory)
    let scriptRoots = [scriptDirectory </> name | name <- scriptNames, name `notElem` bootstrapScripts]
    reported <- doesFileExist report
    sources <- if reported then reportSources <$> readFileBS report else pure (Left "pip wrote no installation report")
    let environment =
            filter ((`notElem` ["PATH", "PYTHONPATH", "PYTHONDONTWRITEBYTECODE"]) . fst) (ppEnv client)
                <> [("PATH", takeDirectory python <> ":" <> fromMaybe "" (lookup "PATH" (ppEnv client))), ("PYTHONDONTWRITEBYTECODE", "1")]
    use (PipInstallation result (ppDir client) python site scriptRoots environment initial sources)

snapshotInstallation :: PipInstallation -> IO InstalledTree
snapshotInstallation installed = do
    roots <- listDirectory (piSite installed)
    snapshotTree (piSite installed) (roots <> piScriptRoots installed)

observeJson :: PipInstallation -> FilePath -> [String] -> IO (Either Text Value)
observeJson installed executable arguments = do
    result <- runClient (piDirectory installed) (piEnvironment installed) executable arguments
    pure $ case crExit result of
        ExitSuccess -> first toText (Aeson.eitherDecodeStrict (encodeUtf8 (crStdout result)))
        _ -> Left (crCommand result <> " failed: " <> crStderr result)

-- | Read the actual installed metadata, rejecting missing identities and duplicate project names.
installedPythonGraph :: InstalledTree -> Either Text PythonGraph
installedPythonGraph tree = do
    packages <- traverse metadata [bytes | (path, TreeFile _ bytes) <- Map.toList tree, ".dist-info/METADATA" `T.isSuffixOf` toText path]
    let graph = Map.fromList packages
    unless (Map.size graph == length packages) (Left "installed metadata repeats a project")
    pure graph
  where
    metadata bytes = do
        document <- first (const "installed metadata is not UTF-8") (TE.decodeUtf8' bytes)
        let headers = T.lines (fst (T.breakOn "\n\n" document))
            fields name = mapMaybe (T.stripPrefix (name <> ": ")) headers
        name <- uniqueField "Name" (fields "Name")
        version <- uniqueField "Version" (fields "Version")
        pure (name, (version, fields "Requires-Dist"))
    uniqueField _ [value] = Right value
    uniqueField name _ = Left ("installed metadata lacks one " <> name)

-- | Cross-target installation evidence. The target files are inspected without executing foreign code.
data PipTargetInstall = PipTargetInstall
    { ptiClient :: ClientResult
    , ptiTree :: InstalledTree
    , ptiSources :: Either Text [(Text, Text)]
    }
    deriving stock (Show)

-- | Install the complete dependency graph for a named platform into a fresh target directory.
pipInstallForPlatform :: E2E -> Text -> Text -> Text -> IO PipTargetInstall
pipInstallForPlatform = platformInstall []

-- | 'pipInstallForPlatform' in one attempt, for an install the proxy refuses with a 503.
pipRefusedForPlatform :: E2E -> Text -> Text -> Text -> IO PipTargetInstall
pipRefusedForPlatform = platformInstall pipSingleAttempt

-- | pip's options for one attempt. Its default retries back off for about 8 s on a 503.
pipSingleAttempt :: [String]
pipSingleAttempt = ["--retries", "0"]

platformInstall :: [String] -> E2E -> Text -> Text -> Text -> IO PipTargetInstall
platformInstall attempts proxy project version platform = withPipClient proxy $ \client -> do
    let destination = ppDir client </> "target"
        report = ppDir client </> "report.json"
    result <-
        runClient
            (ppDir client)
            (ppEnv client)
            "python3"
            ( ["-m", "pip", "--isolated", "install"]
                <> attempts
                <> [ "--no-cache-dir"
                   , "--no-compile"
                   , "--disable-pip-version-check"
                   , "--no-input"
                   , "--ignore-installed"
                   , "--platform"
                   , toString platform
                   , "--python-version"
                   , "3.12"
                   , "--only-binary=:all:"
                   , "--index-url"
                   , toString (ppIndex client)
                   , "--target"
                   , destination
                   , "--report"
                   , report
                   , toString (pinned project version)
                   ]
                <> pipCertificateArguments client
            )
    exists <- doesDirectoryExist destination
    roots <- if exists then listDirectory destination else pure []
    tree <- snapshotTree destination roots
    reported <- doesFileExist report
    sources <- if reported then reportSources <$> readFileBS report else pure (Left "pip wrote no installation report")
    pure PipTargetInstall{ptiClient = result, ptiTree = tree, ptiSources = sources}

reportSources :: ByteString -> Either Text [(Text, Text)]
reportSources bytes =
    first toText (Aeson.eitherDecodeStrict bytes) >>= \case
        Object report | Just (Array installed) <- KeyMap.lookup "install" report -> traverse source (toList installed)
        _ -> Left "pip report has no install array"
  where
    source (Object entry)
        | Just (Object metadata) <- KeyMap.lookup "metadata" entry
        , Just (String name) <- KeyMap.lookup "name" metadata
        , Just (Object download) <- KeyMap.lookup "download_info" entry
        , Just (String url) <- KeyMap.lookup "url" download =
            Right (name, url)
    source _ = Left "pip report omits a project's name or source URL"
