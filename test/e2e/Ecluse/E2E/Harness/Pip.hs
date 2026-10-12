-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Isolated pip clients for digest checks, dependency installs, and platform selection.
module Ecluse.E2E.Harness.Pip (
    withPipProject,
    pipInstallIn,
    pipResolveIn,
    pipInstalled,
    pipInstallsWheel,
    pipInstallGraph,
    PipTargetInstall (..),
    pipInstallForPlatform,

    -- * The served index
    advertisedFiles,
) where

import Data.Aeson (Value (Array, Object, String))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as T
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
import Test.Hspec (Expectation, expectationFailure, shouldReturn)
import UnliftIO.Environment (getEnvironment)

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.E2E.Fixtures.PyPI (pypiDistInfo, pypiProject, pypiVersion, pypiWheelFile)
import Ecluse.E2E.Harness.Client (runClient, withClientDir)
import Ecluse.E2E.Harness.Proxy (logTail, logTailLines, proxyContainerLogs, proxyGet, shouldSucceedThroughProxy)
import Ecluse.E2E.Harness.Types
import Ecluse.Test.Contract.Install (InstallDriver (..), installsObserved)
import Ecluse.Test.InstalledTree (InstalledTree, snapshotTree)

-- | Pin @project==version@ to @digest@ in an isolated project removed after the action.
withPipProject :: E2E -> Text -> Text -> Text -> (PipProject -> IO a) -> IO a
withPipProject e2e project version digest use =
    withClientDir "pip" $ \projectDir -> do
        index <- clientEndpoint PyPI e2e
        writeFileText (projectDir </> requirementsFile) (requirement project version digest)
        baseEnv <- getEnvironment
        -- --isolated drops PIP_* and the user config but still reads the global and site
        -- config files, which PIP_CONFIG_FILE is the only way to silence.
        let cleanEnv =
                filter ((`notElem` ["HOME", "PIP_CONFIG_FILE"]) . fst) baseEnv
                    <> [("HOME", projectDir), ("PIP_CONFIG_FILE", "/dev/null")]
        use PipProject{ppDir = projectDir, ppEnv = cleanEnv, ppIndex = index}

-- | Install into the project target with @--require-hashes@ checking the advertised digest.
pipInstallIn :: PipProject -> IO ClientResult
pipInstallIn proj =
    runClient
        (ppDir proj)
        (ppEnv proj)
        "python3"
        [ "-m"
        , "pip"
        , "--isolated"
        , "install"
        , "--no-cache-dir"
        , "--disable-pip-version-check"
        , "--no-input"
        , "--require-hashes"
        , -- An sdist runs its own build backend on install, so a wheel is the only
          -- admissible form here, as npm_config_ignore_scripts is on the npm side.
          "--only-binary=:all:"
        , "--index-url"
        , toString (ppIndex proj)
        , "--target"
        , ppDir proj </> targetDir
        , "--requirement"
        , ppDir proj </> requirementsFile
        ]

-- | Resolve without hash mode, which makes pip fetch the declared metadata companion.
pipResolveIn :: PipProject -> Text -> Text -> IO ClientResult
pipResolveIn proj project version =
    runClient
        (ppDir proj)
        (ppEnv proj)
        "python3"
        [ "-m"
        , "pip"
        , "--isolated"
        , "install"
        , "--dry-run"
        , "--ignore-installed"
        , "--no-cache-dir"
        , "--disable-pip-version-check"
        , "--no-input"
        , "--only-binary=:all:"
        , "--index-url"
        , toString (ppIndex proj)
        , "--target"
        , ppDir proj </> targetDir
        , toString (project <> "==" <> version)
        ]

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

-- | Read advertised @(filename, sha256)@ pairs through the proxy, failing if the mount refuses.
advertisedFiles :: E2E -> Text -> IO [(Text, Text)]
advertisedFiles e2e project = do
    (status, body) <- proxyGet e2e ("/pypi/simple/" <> project)
    unless (status == 200) $ do
        logs <- proxyContainerLogs e2e
        fail (toString (indexRefusal project status logs))
    pure (digestedFiles body)

-- A refusal reaches the wire as a bare status, so its reason exists only in the proxy's own
-- JSONL. "no versions are available" means the index never resolved. A rule name means a denial.
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

-- The served PEP 691 document's file entries, keeping only those carrying a sha256.
digestedFiles :: LByteString -> [(Text, Text)]
digestedFiles body =
    [ (filename, digest)
    | Object entry <- servedFiles (Aeson.decode body)
    , Just (String filename) <- [KeyMap.lookup "filename" entry]
    , Just (Object hashes) <- [KeyMap.lookup "hashes" entry]
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

-- | Install a root and its dependencies into a new empty virtual environment, preserving every installed byte.
pipInstallGraph :: E2E -> Text -> Text -> IO InstalledTree
pipInstallGraph proxy project version = withPipProject proxy project version "" $ \client -> do
    let virtualenv = ppDir client </> "venv"
        python = virtualenv </> "bin" </> "python"
        run executable arguments = runClient (ppDir client) (ppEnv client) executable arguments >>= shouldSucceedThroughProxy proxy
    void $ run "python3" ["-m", "venv", "--without-pip", virtualenv]
    location <- run python ["-c", "import sysconfig; print(sysconfig.get_path('purelib'))"]
    let site = toString (T.strip (crStdout location))
    listDirectory site `shouldReturn` []
    void $
        run
            "python3"
            [ "-m"
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
            , toString (project <> "==" <> version)
            ]
    roots <- listDirectory site
    snapshotTree site roots

-- | Cross-target installation evidence. The target files are inspected without executing foreign code.
data PipTargetInstall = PipTargetInstall
    { ptiClient :: ClientResult
    , ptiTree :: InstalledTree
    , ptiSources :: Either Text [(Text, Text)]
    }
    deriving stock (Show)

-- | Install the complete dependency graph for a named platform into a fresh target directory.
pipInstallForPlatform :: E2E -> Text -> Text -> Text -> IO PipTargetInstall
pipInstallForPlatform proxy project version platform = withPipProject proxy project version "" $ \client -> do
    let destination = ppDir client </> "target"
        report = ppDir client </> "report.json"
    result <-
        runClient
            (ppDir client)
            (ppEnv client)
            "python3"
            [ "-m"
            , "pip"
            , "--isolated"
            , "install"
            , "--no-cache-dir"
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
            , toString (project <> "==" <> version)
            ]
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
