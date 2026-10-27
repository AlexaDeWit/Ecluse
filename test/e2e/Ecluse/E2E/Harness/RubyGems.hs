-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Real Bundler clients keep credentials, lockfiles and installed gems inside one fresh project.
module Ecluse.E2E.Harness.RubyGems (
    BundlerClient (..),
    minimumBundler,
    currentBundler,
    RubyProject (..),
    withRubyProject,
    rubyDependency,
    runBundler,
    rubyLock,
    rubyInstall,
    rubyUpdate,
    rubyEvaluate,
    rubyInstalledTree,
    rubyLockfile,
    rubyGemCommand,
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import UnliftIO.Environment (getEnvironment)

import Ecluse.E2E.Harness.Client (compilerEnvironmentKeys, runClient, withClientDir)
import Ecluse.E2E.Harness.Types (ClientResult (..), E2E, clientCertificateBundle)
import Ecluse.Test.InstalledTree (InstalledTree, TreeEntry (TreeDirectory), snapshotTree)

-- | A packaging wrapper supplies one exact client version without network installation.
data BundlerClient = BundlerClient
    { bundlerVariable :: String
    , bundlerVersion :: Text
    }
    deriving stock (Eq, Show)

-- | The minimum supported client must run independently of the current client.
minimumBundler :: BundlerClient
minimumBundler = BundlerClient "ECLTEST_BUNDLER_269" "2.6.9"

-- | This pinned release defines the current-client observations for this delivery.
currentBundler :: BundlerClient
currentBundler = BundlerClient "ECLTEST_BUNDLER_4023" "4.0.23"

-- | No user configuration or cache participates in the native observations.
data RubyProject = RubyProject
    { rubyDirectory :: FilePath
    , rubyEnvironment :: [(String, String)]
    , rubyBundler :: FilePath
    , rubyInterpreter :: FilePath
    , rubySource :: Text
    }

-- | Required wrapper paths fail acquisition when absent, including outside CI.
withRubyProject :: BundlerClient -> E2E -> Text -> Maybe Text -> (RubyProject -> IO a) -> IO a
withRubyProject client proxy source credentials action = withClientDir "rubygems" $ \directory -> do
    bundler <- required (bundlerVariable client)
    interpreter <- required "ECLTEST_RUBY"
    inherited <- getEnvironment
    let home = directory </> "home"
        installed = directory </> "installed"
        retained = filter ((`elem` (["PATH", "TMPDIR", "TEMP", "TMP", "LANG", "LC_ALL"] <> compilerEnvironmentKeys)) . fst) inherited
        overrides =
            [ ("HOME", home)
            , ("BUNDLE_USER_HOME", home </> ".bundle")
            , ("BUNDLE_APP_CONFIG", directory </> ".bundle")
            , ("BUNDLE_PATH", installed)
            , ("BUNDLE_DISABLE_SHARED_GEMS", "true")
            , ("BUNDLE_DISABLE_VERSION_CHECK", "true")
            , ("BUNDLE_RETRY", "0")
            , ("BUNDLE_TIMEOUT", "15")
            , ("BUNDLE_SSL_CA_CERT", clientCertificateBundle proxy)
            , ("SSL_CERT_FILE", clientCertificateBundle proxy)
            ]
        project = RubyProject directory (retained <> overrides) bundler interpreter source
    traverse_ (createDirectoryIfMissing True) [home, directory </> ".bundle"]
    version <- runBundler project ["--version"]
    let output = T.strip (crStdout version)
        reported = fromMaybe output (T.stripPrefix "Bundler version " output)
    unless (crExit version == ExitSuccess && reported == bundlerVersion client) $
        fail ("expected Bundler " <> toString (bundlerVersion client) <> ", got " <> show version)
    forM_ credentials $ \value -> do
        configured <- runBundler project ["config", "set", "--local", toString source, toString value]
        unless (crExit configured == ExitSuccess) (fail (toString (crStderr configured)))
    rubyDependency project "ecluse-fixture-root" "= 1.0.0"
    action project
  where
    required variable = do
        command <- lookupEnv variable >>= maybe (fail ("required Ruby client path is absent: " <> variable)) pure
        findExecutable command >>= maybe (fail ("required Ruby client executable is absent: " <> command)) pure

-- | The fixture source is explicit and never adds a public fallback registry.
rubyDependency :: RubyProject -> Text -> Text -> IO ()
rubyDependency project name requirement =
    writeFileText (rubyDirectory project </> "Gemfile") $
        "source " <> rubyLiteral (rubySource project) <> "\ngem " <> rubyLiteral name <> ", " <> rubyLiteral requirement <> "\n"

rubyLiteral :: Text -> Text
rubyLiteral value = "'" <> T.replace "'" "\\'" (T.replace "\\" "\\\\" value) <> "'"

-- | The wrapper chooses the version. Every native command retains its actual output and exit code.
runBundler :: RubyProject -> [String] -> IO ClientResult
runBundler project = runClient (rubyDirectory project) (rubyEnvironment project) (rubyBundler project)

-- | Enable lockfile checksums. Named dependency records need installation to populate archive digests.
rubyLock :: RubyProject -> IO ClientResult
rubyLock project = runBundler project ["lock", "--add-checksums"]

-- | Frozen installs preserve the existing lockfile instead of resolving a replacement.
rubyInstall :: RubyProject -> Bool -> IO ClientResult
rubyInstall project frozen =
    runClient
        (rubyDirectory project)
        ([entry | entry <- rubyEnvironment project, fst entry /= "BUNDLE_FROZEN"] <> [("BUNDLE_FROZEN", if frozen then "true" else "false")])
        (rubyBundler project)
        ["install"]

-- | An update uses the same source and private installation directory as the lock operation.
rubyUpdate :: RubyProject -> IO ClientResult
rubyUpdate project = runBundler project ["update"]

-- | Ruby evaluates only the installed owned fixture, never inspected archive metadata.
rubyEvaluate :: RubyProject -> String -> IO ClientResult
rubyEvaluate project expression = runBundler project ["exec", rubyInterpreter project, "-e", expression]

-- | Installed bytes prove both positive contents and refusal without cache inference.
rubyInstalledTree :: RubyProject -> IO InstalledTree
rubyInstalledTree project = Map.filter (/= TreeDirectory) <$> snapshotTree (rubyDirectory project) ["installed"]

-- | A missing lockfile stays explicit in independent client observations.
rubyLockfile :: RubyProject -> IO (Maybe Text)
rubyLockfile project = do
    let path = rubyDirectory project </> "Gemfile.lock"
    exists <- doesFileExist path
    if exists then Just . decodeUtf8 <$> readFileBS path else pure Nothing

-- | RubyGems sends the native API key through its documented isolated environment variable.
rubyGemCommand :: RubyProject -> Text -> [String] -> IO ClientResult
rubyGemCommand project token arguments =
    runClient (rubyDirectory project) (rubyEnvironment project <> [("GEM_HOST_API_KEY", toString token)]) (rubyInterpreter project) (["-S", "gem"] <> arguments)
