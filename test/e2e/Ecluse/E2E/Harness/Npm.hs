-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.E2E.Harness.Npm (
    npmInstall,
    npmInstallIn,
    npmCiIn,
    npmPublishIn,
    withNpmProject,
    withPublishProject,
    installWithLifecycleProbe,

    -- * Assertions
    shouldSucceed,
    shouldFail,

    -- * Constants
    publishTargetEnv,
    publishInScopeName,
    publishOutOfScopeName,
    publishVersion,
) where

import Data.ByteString.Lazy qualified as LBS

import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing, doesFileExist, getTemporaryDirectory, removePathForcibly)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Process.Typed (proc, readProcess, setEnv, setWorkingDir)
import Test.Hspec (expectationFailure)
import UnliftIO (bracket, handleAny)
import UnliftIO.Environment (getEnvironment)

import Ecluse.E2E.Harness.Docker (uniqueSuffix)
import Ecluse.E2E.Harness.Types

shouldSucceed :: (MonadIO m) => NpmResult -> m NpmResult
shouldSucceed res = liftIO $ case npmExit res of
    ExitSuccess -> pure res
    _ -> expectationFailure ("npm failed!\nSTDOUT:\n" <> T.unpack (npmStdout res) <> "\nSTDERR:\n" <> T.unpack (npmStderr res)) >> pure res

shouldFail :: (MonadIO m) => NpmResult -> m NpmResult
shouldFail res = liftIO $ case npmExit res of
    ExitSuccess -> expectationFailure ("npm incorrectly succeeded!\nSTDOUT:\n" <> T.unpack (npmStdout res) <> "\nSTDERR:\n" <> T.unpack (npmStderr res)) >> pure res
    _ -> pure res

{- | Bracket an isolated npm consumer project (see 'NpmProject'): a fixed consumer
@package.json@ and an __empty__ @.npmrc@, the pinned isolated environment, run the
action, then remove the project tree on every exit path. For a publish-capable project
(a scoped name plus an authorising @.npmrc@), see 'withPublishProject'.
-}
withNpmProject :: E2E -> (NpmProject -> IO a) -> IO a
withNpmProject e2e = withProjectContents e2e consumerPackageJson ""

{- An isolated npm project with the given @package.json@ and @.npmrc@ contents -- the
shared body behind 'withNpmProject' (a consumer project, empty @.npmrc@) and
'withPublishProject' (a publishable project, an authorising @.npmrc@). Creates the dirs
and the pinned, fully isolated environment (own cache, userconfig, prefix, @HOME@ -- no
developer global state leaks in, the only registry is the proxy), runs the action, then
removes the tree on every exit path. -}
withProjectContents :: E2E -> Text -> Text -> (NpmProject -> IO a) -> IO a
withProjectContents e2e packageJson npmrcContents use = do
    sfx <- uniqueSuffix
    tmpRoot <- getTemporaryDirectory
    let projectDir = tmpRoot </> ("ecluse-e2e-npm-" <> sfx)
        cacheDir = projectDir </> "cache"
        prefixDir = projectDir </> "prefix"
        npmrc = projectDir </> ".npmrc"
    bracket
        ( do
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
                    , -- Écluse is a supply-chain-security proxy, so no npm child it spawns may
                      -- execute an upstream package's lifecycle scripts (an arbitrary-code-
                      -- execution surface). This project lives outside the repo tree, where the
                      -- committed root @.npmrc@'s @ignore-scripts@ is unreachable, so the guard
                      -- is set in the child environment instead -- unconditionally, every npm call.
                      ("npm_config_ignore_scripts", "true")
                    , ("HOME", projectDir)
                    ]
                cleanEnv =
                    filter
                        (\(k, _) -> k `notElem` map fst overrides && not ("npm_config_" `isPrefixOf` k))
                        baseEnv
                        <> overrides
            pure NpmProject{npDir = projectDir, npEnv = cleanEnv}
        )
        (\_ -> handleAny (const pass) (removePathForcibly projectDir))
        use

{- | Bracket an isolated, __publishable__ npm project: a @package.json@ carrying the
given scoped name and version (npm packs the project directory, so no prebuilt tarball
fixture is needed) and an @.npmrc@ authorising the proxy registry with a bearer token.
The token satisfies npm's client-side publish gate -- without one npm refuses the publish
with @ENEEDAUTH@ before it reaches the proxy -- and is forwarded through the publish
relay. The publication target accepts the publish on its own terms, so the token's
identity is immaterial here (asserting the forwarded-credential identity is the
integration tier's concern); the @.npmrc@ exists to exercise the forward, not to prove it.
-}
withPublishProject :: E2E -> Text -> Text -> (NpmProject -> IO a) -> IO a
withPublishProject e2e name version =
    withProjectContents
        e2e
        (publishPackageJson name version)
        (npmAuthLine (e2eRegistry e2e) publishAuthToken)

-- | Run @npm@ with the given args in a project, capturing its exit and output.
runNpm :: NpmProject -> [String] -> IO NpmResult
runNpm proj args = do
    let cmd = setWorkingDir (npDir proj) . setEnv (npEnv proj) $ proc "npm" args
    (code, out, err) <- readProcess cmd
    pure
        NpmResult
            { npmExit = code
            , npmStdout = decodeUtf8 (LBS.toStrict out)
            , npmStderr = decodeUtf8 (LBS.toStrict err)
            }

{- | @npm install \<pkg\>@ in a project -- resolves via the packument and writes the
lockfile (@package.json@ + @package-lock.json@) for a later 'npmCiIn'.
-}
npmInstallIn :: NpmProject -> Text -> IO NpmResult
npmInstallIn proj pkg = runNpm proj ["install", toString pkg]

{- | @npm ci@ in a project -- a deterministic install from the lockfile. It fetches each
artifact from the lockfile's @resolved@ URL (the proxy's __private-first__ tarball path)
and checks @integrity@, without re-resolving via the packument -- so once a version is
mirrored it never contacts the public upstream.
-}
npmCiIn :: NpmProject -> IO NpmResult
npmCiIn proj = runNpm proj ["ci"]

{- | @npm publish@ in a publishable project (see 'withPublishProject') -- packs the
project directory and @PUT \/{pkg}@s the publish document to the proxy, which gates it on
the publish-scope allow-list before relaying it to the publication target. The exit code
reflects what the proxy returned: success when the publish was admitted and the target
accepted it, non-zero when the anti-shadowing guard refused the name (a @403@).
-}
npmPublishIn :: NpmProject -> IO NpmResult
npmPublishIn proj = runNpm proj ["publish"]

{- | @npm install \<pkg\>@ against the proxy in a throwaway project (see 'withNpmProject'),
for the one-shot cases that only need the install's outcome.
-}
npmInstall :: E2E -> Text -> IO NpmResult
npmInstall e2e pkg = withNpmProject e2e (`npmInstallIn` pkg)

{- | Install an isolated project whose own @postinstall@ would create a sentinel file in the
project root, returning the install result and whether that sentinel was created. npm runs a
root package's lifecycle scripts on @npm install@ unless they are disabled, and the harness
disables them for every npm child it spawns, so a faithful harness creates no sentinel -- the
returned 'Bool' is 'False' even on a successful install. The regression guard that script
suppression holds: were the guard dropped, the @postinstall@ would run and the 'Bool' flip to
'True'.
-}
installWithLifecycleProbe :: E2E -> IO (NpmResult, Bool)
installWithLifecycleProbe e2e =
    withProjectContents e2e lifecycleProbePackageJson "" $ \proj -> do
        res <- runNpm proj ["install"]
        ran <- doesFileExist (npDir proj </> lifecycleSentinel)
        pure (res, ran)

-- The file a lifecycle script would create; its absence after an install proves no script
-- ran. Relative, so it lands in the project root -- npm's working directory for a root
-- package's own lifecycle scripts.
lifecycleSentinel :: FilePath
lifecycleSentinel = "lifecycle-script-ran"

-- A minimal project whose own @postinstall@ would @touch@ 'lifecycleSentinel'. npm runs a
-- root package's lifecycle scripts on @npm install@ unless they are disabled, so the
-- sentinel is a faithful probe for whether script execution was suppressed.
lifecycleProbePackageJson :: Text
lifecycleProbePackageJson =
    "{\"name\":\"e2e-lifecycle-probe\",\"version\":\"1.0.0\",\"private\":true,\"scripts\":{\"postinstall\":\"touch lifecycle-script-ran\"}}\n"

consumerPackageJson :: Text
consumerPackageJson = "{\"name\":\"e2e-consumer\",\"version\":\"1.0.0\",\"private\":true}\n"

{- | The extra proxy environment that turns the first-party publish path __on__, layered
over the base 'proxyEnv' through 'E2EConfig'\'s @ecExtraEnv@ -- so only the scenarios that
ask for it see a publication target, and the base topology keeps the implicit
publish→@405@ default. The target is Verdaccio, the same registry the base topology reads
as the private upstream (@mirror@), so a published package is then readable back over the
private leg. @ECLUSE_MOUNTS__NPM__PUBLISH_ALLOW@ is the anti-shadowing allow-list, required once a target is
set. The publish is __passthrough__: the relay forwards the client's own bearer (the
project @.npmrc@\'s 'publishAuthToken'), so no static publication-target token is configured.
-}
publishTargetEnv :: [(Text, Text)]
publishTargetEnv =
    [ ("ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET", "https://mirror/")
    , ("ECLUSE_MOUNTS__NPM__PUBLISH_ALLOW", publishScope)
    ]

-- The publish-scope allow-list value 'publishTargetEnv' configures. 'publishInScopeName'
-- is derived from it, so the configured scope and the in-scope name cannot drift apart.
publishScope :: Text
publishScope = "@acme"

{- | A first-party package __within__ the configured 'publishTargetEnv' scope: an
@npm publish@ of it is admitted by the anti-shadowing guard and relayed to the
publication target.
-}
publishInScopeName :: Text
publishInScopeName = publishScope <> "/e2e-publish"

{- | A package in a scope __outside__ the allow-list: an @npm publish@ of it must be
refused by the anti-shadowing guard __before__ any upstream write -- the security
property the refuse-before-write scenario proves.
-}
publishOutOfScopeName :: Text
publishOutOfScopeName = "@rogue/e2e-shadow"

-- | The single version the publish scenarios publish (and read back).
publishVersion :: Text
publishVersion = "1.0.0"

-- The bearer token written into a publishable project's @.npmrc@. Its only job is to
-- satisfy npm's client-side publish gate (no token ⇒ @ENEEDAUTH@) and exercise the
-- forward; the publication target accepts the publish regardless, so the identity is
-- immaterial at this tier (the forwarded-credential identity is integration-tier work).
publishAuthToken :: Text
publishAuthToken = "e2e-publisher-token"

-- A publishable project's @package.json@: the scoped name and version npm packs and
-- publishes. Deliberately not @private@ -- npm refuses to publish a private package.
publishPackageJson :: Text -> Text -> Text
publishPackageJson name version =
    "{\"name\":\"" <> name <> "\",\"version\":\"" <> version <> "\"}\n"

-- The npm @.npmrc@ line authorising a registry with a bearer token. npm keys auth by the
-- registry URL's host and path with the scheme stripped and a leading @\/\/@, so a
-- publish to the proxy registry carries this token.
npmAuthLine :: Text -> Text -> Text
npmAuthLine registry token =
    "//" <> withoutScheme registry <> ":_authToken=" <> token <> "\n"
  where
    withoutScheme u = fromMaybe u (T.stripPrefix "http://" u <|> T.stripPrefix "https://" u)
