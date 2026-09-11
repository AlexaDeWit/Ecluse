-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | pip clients for end-to-end scenarios.
Each project pins one release to the digest the mount's own index advertised, so pip's
hash-checking mode refuses any byte that is not the one the client was promised.
-}
module Ecluse.E2E.Harness.Pip (
    withPipProject,
    pipInstallIn,
    pipInstalled,

    -- * The served index
    advertisedFiles,
) where

import Data.Aeson (Value (Array, Object, String))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as T
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import UnliftIO.Environment (getEnvironment)

import Ecluse.E2E.Harness.Client (runClient, withClientDir)
import Ecluse.E2E.Harness.Proxy (proxyContainerLogs, proxyGet)
import Ecluse.E2E.Harness.Types

{- | Isolate a consumer's pip state, pinning @project==version@ to @digest@, and remove the
project directory after the action.
-}
withPipProject :: E2E -> Text -> Text -> Text -> (PipProject -> IO a) -> IO a
withPipProject e2e project version digest use =
    withClientDir "pip" $ \projectDir -> do
        writeFileText (projectDir </> requirementsFile) (requirement project version digest)
        baseEnv <- getEnvironment
        -- --isolated drops PIP_* and the user config but still reads the global and site
        -- config files, which PIP_CONFIG_FILE is the only way to silence.
        let cleanEnv =
                filter ((`notElem` ["HOME", "PIP_CONFIG_FILE"]) . fst) baseEnv
                    <> [("HOME", projectDir), ("PIP_CONFIG_FILE", "/dev/null")]
        use PipProject{ppDir = projectDir, ppEnv = cleanEnv, ppIndex = e2ePypiIndex e2e}

{- | Install the pinned requirement through the proxy into the project's own target directory.
@--require-hashes@ makes the advertised digest the download's acceptance test.
-}
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

-- | Whether a wheel's @.dist-info@ directory landed in the project's install target.
pipInstalled :: PipProject -> Text -> IO Bool
pipInstalled proj distInfo = doesDirectoryExist (ppDir proj </> targetDir </> toString distInfo)

{- | The @(filename, sha256)@ pairs the pypi mount advertises for a project, read through
the proxy exactly as a client reads them. A mount that does not answer fails the setup.
-}
advertisedFiles :: E2E -> Text -> IO [(Text, Text)]
advertisedFiles e2e project = do
    (status, body) <- proxyGet e2e ("/pypi/simple/" <> project)
    unless (status == 200) $ do
        logs <- proxyContainerLogs e2e
        fail (toString (indexRefusal project status logs))
    pure (digestedFiles body)

-- A refusal reaches the wire as a bare status, so its reason exists only in the proxy's own
-- JSONL. "no versions are available" means the index never resolved; a rule name means a denial.
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

logTail :: Int -> Text -> Text
logTail n = T.intercalate "\n" . reverse . take n . reverse . lines

logTailLines :: Int
logTailLines = 50

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
