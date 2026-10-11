-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | pip clients for end-to-end scenarios. Each project pins one release to the digest the mount's own index
advertised, so pip's hash-checking mode refuses any byte that is not the one the client was promised.
-}
module Ecluse.E2E.Harness.Pip (
    withPipProject,
    pipInstallIn,
    pipResolveIn,
    pipInstalled,
    pipInstallsWheel,

    -- * The served index
    advertisedFiles,
) where

import Data.Aeson (Value (Array, Object, String))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import Test.Hspec (Expectation, expectationFailure)
import UnliftIO.Environment (getEnvironment)

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.E2E.Fixtures.PyPI (pypiDistInfo, pypiProject, pypiVersion, pypiWheelFile)
import Ecluse.E2E.Harness.Client (runClient, withClientDir)
import Ecluse.E2E.Harness.Proxy (logTail, logTailLines, proxyContainerLogs, proxyGet)
import Ecluse.E2E.Harness.Types
import Ecluse.Test.Contract.Install (InstallDriver (..), installsObserved)

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
