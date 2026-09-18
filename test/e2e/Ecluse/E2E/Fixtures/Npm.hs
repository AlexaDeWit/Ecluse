-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | npm fixtures for the nginx upstream in end-to-end tests.
Versions predate quarantine, and artifact integrity matches the served bytes
except in the tampering fixture.
-}
module Ecluse.E2E.Fixtures.Npm (
    PkgSpec (..),
    defaultPkgSpec,
    psVersions,
    allowPkg,
    denyPkg,
    mirrorPkg,
    mirrorAuthorFields,
    mirrorRegistryFields,
    mirrorRegistryDistFields,
    latestPkg,
    dredgerPkg,
    dredgerKeepPkg,
    dredgerDryRunPkg,
    recoveryPkg,
    recoveryFaultPkg,
    recoveryReadmitPkg,
    recoveryLostPkg,
    recoveryLatePkg,
    corpusRevokedPkg,
    tamperPkg,
    headPkg,
    telemetryPkg,
    telemetryDdPkg,
    fixturePackages,
    buildFixtures,
    artifactFile,
) where

import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (Pair)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Process.Typed (proc, runProcess_)

import Ecluse.Test.Package (sriSha512Of)
import Ecluse.Test.Registry.Npm (VersionSpec (..), packumentValue, versionSpec, versionValue)

-- | One fixture package: its identity plus the two behaviours the scenarios turn on.
data PkgSpec = PkgSpec
    { psName :: Text
    -- ^ The package name (also the mount-relative path the stub serves it at).
    , psVersion :: Text
    -- ^ The version @dist-tags.latest@ points at.
    , psOlderVersions :: [Text]
    -- ^ Further versions the packument publishes, all below 'psVersion'.
    , psInstallScript :: Bool
    -- ^ Declare an install script: the @DenyInstallTimeExecution@ trigger.
    , psTamper :: Bool
    -- ^ Corrupt the served bytes after computing their declared integrity.
    , psVersionFields :: [Pair]
    -- ^ Further fields on every version object, beside the identity, @dist@, and script fields.
    , psDistFields :: [Pair]
    -- ^ Further @dist@ fields on every version object, beside the location and integrity.
    }
    deriving stock (Eq, Show)

-- | One backdated version with no install script or altered artifact bytes.
defaultPkgSpec :: Text -> PkgSpec
defaultPkgSpec name =
    PkgSpec
        { psName = name
        , psVersion = "1.0.0"
        , psOlderVersions = []
        , psInstallScript = False
        , psTamper = False
        , psVersionFields = []
        , psDistFields = []
        }

-- | Every version the packument publishes, newest first.
psVersions :: PkgSpec -> [Text]
psVersions spec = psVersion spec : psOlderVersions spec

-- | An allow-listed package for the install path.
allowPkg :: PkgSpec
allowPkg = defaultPkgSpec "e2e-allow"

-- | A package with an install script: denied at the public surface.
denyPkg :: PkgSpec
denyPkg = (defaultPkgSpec "e2e-deny"){psInstallScript = True}

{- | A package used to exercise the mirror round-trip (served, then mirrored). Its version object
carries the author fields the mirror write keeps and the registry fields it strips.
-}
mirrorPkg :: PkgSpec
mirrorPkg =
    (defaultPkgSpec "e2e-mirror")
        { psVersionFields = mirrorAuthorFields <> mirrorRegistryFields
        , psDistFields = mirrorRegistryDistFields
        }

-- | What the author of 'mirrorPkg' wrote: every field must reach the mirror verbatim.
mirrorAuthorFields :: [Pair]
mirrorAuthorFields =
    [ "dependencies" .= object [Key.fromText (psName allowPkg) .= psVersion allowPkg]
    , "bin" .= object ["e2e-mirror" .= ("index.js" :: Text)]
    , "engines" .= object ["node" .= (">=18" :: Text)]
    , "license" .= ("MIT" :: Text)
    , "scripts" .= object ["test" .= ("node -e \"\"" :: Text)]
    , "deprecated" .= ("superseded by a later release" :: Text)
    , "gitHead" .= ("0123456789abcdef0123456789abcdef01234567" :: Text)
    ]

-- | What the public registry wrote about itself on 'mirrorPkg': none of it reaches the mirror.
mirrorRegistryFields :: [Pair]
mirrorRegistryFields =
    [ "_npmUser" .= object ["name" .= ("fixture-publisher" :: Text)]
    , "_nodeVersion" .= ("20.11.0" :: Text)
    , "_npmVersion" .= ("10.2.4" :: Text)
    ]

-- | The public registry's own signatures and attestations on 'mirrorPkg', stripped at the mirror.
mirrorRegistryDistFields :: [Pair]
mirrorRegistryDistFields =
    [ "signatures" .= [object ["keyid" .= ("SHA256:fixture" :: Text), "sig" .= ("MEUCIQ" :: Text)]]
    , "attestations" .= object ["url" .= ("https://upstream/-/npm/v1/attestations/e2e-mirror@1.0.0" :: Text)]
    ]

{- | A two-version package whose upstream @latest@ is @2.0.0@. Mirroring @1.0.0@ after @2.0.0@
must not retag the store.
-}
latestPkg :: PkgSpec
latestPkg = (defaultPkgSpec "e2e-latest"){psVersion = "2.0.0", psOlderVersions = ["1.0.0"]}

-- | A package with tampered artifact bytes: the worker must refuse to mirror it.
tamperPkg :: PkgSpec
tamperPkg = (defaultPkgSpec "e2e-tamper"){psTamper = True}

-- | A package reserved for @HEAD@ probes, so no install can seed its mirror entry.
headPkg :: PkgSpec
headPkg = defaultPkgSpec "e2e-head"

-- | A package reserved for deletion by the Dredger scenario.
dredgerPkg :: PkgSpec
dredgerPkg = defaultPkgSpec "e2e-dredger"

-- | A mirrored package that the Dredger's deny rule does not name.
dredgerKeepPkg :: PkgSpec
dredgerKeepPkg = defaultPkgSpec "e2e-dredger-keep"

-- | A package condemned only by a dry run, which must preserve it.
dredgerDryRunPkg :: PkgSpec
dredgerDryRunPkg = defaultPkgSpec "e2e-dredger-dry-run"

{- | A two-version package for the recovery cases. A deny names @1.0.0@ alone, so @2.0.0@ is the
sibling a next read must still serve.
-}
recoveryPkg :: PkgSpec
recoveryPkg = (defaultPkgSpec "e2e-recovery"){psVersion = "2.0.0", psOlderVersions = ["1.0.0"]}

-- | A package whose private-cache deletion the recovery case makes fail.
recoveryFaultPkg :: PkgSpec
recoveryFaultPkg = defaultPkgSpec "e2e-recovery-fault"

-- | A package a recovery case installs again once the policy that removed it is lifted.
recoveryReadmitPkg :: PkgSpec
recoveryReadmitPkg = defaultPkgSpec "e2e-recovery-readmit"

-- | A package whose public artifact a recovery case withholds, so no source bytes remain.
recoveryLostPkg :: PkgSpec
recoveryLostPkg = defaultPkgSpec "e2e-recovery-lost"

-- | A package a recovery case republishes into both stores after a completed removal.
recoveryLatePkg :: PkgSpec
recoveryLatePkg = defaultPkgSpec "e2e-recovery-late"

{- | The package the OSV corpus names in its V2 delta alone. The name matches that advisory, so a
generation swap condemns @1.0.0@ and leaves @1.2.0@, its stated fix.
-}
corpusRevokedPkg :: PkgSpec
corpusRevokedPkg = (defaultPkgSpec "corpus-revoked"){psVersion = "1.2.0", psOlderVersions = ["1.0.0"]}

-- | A package used to exercise telemetry domain-span emission.
telemetryPkg :: PkgSpec
telemetryPkg = defaultPkgSpec "e2e-telemetry"

-- | A package for correlating mirrored requests with Datadog telemetry.
telemetryDdPkg :: PkgSpec
telemetryDdPkg = defaultPkgSpec "e2e-telemetry-datadog"

-- | The full fixture set the stub serves.
fixturePackages :: [PkgSpec]
fixturePackages =
    [ allowPkg
    , denyPkg
    , mirrorPkg
    , latestPkg
    , tamperPkg
    , headPkg
    , dredgerPkg
    , dredgerKeepPkg
    , dredgerDryRunPkg
    , recoveryPkg
    , recoveryFaultPkg
    , recoveryReadmitPkg
    , recoveryLostPkg
    , recoveryLatePkg
    , corpusRevokedPkg
    , telemetryPkg
    , telemetryDdPkg
    ]

{- | Where the stub serves one version's artifact, under the root 'buildFixtures' writes into.
A case that withholds an artifact brackets this path.
-}
artifactFile :: FilePath -> Text -> Text -> FilePath
artifactFile root name version =
    root </> toString name </> "-" </> toString (name <> "-" <> version <> ".tgz")

-- | Write nginx fixtures with matching artifact integrity, then apply any requested tampering.
buildFixtures :: FilePath -> [PkgSpec] -> IO ()
buildFixtures root = traverse_ (buildOne root)

buildOne :: FilePath -> PkgSpec -> IO ()
buildOne root spec = do
    let pkgDir = root </> toString (psName spec)
    createDirectoryIfMissing True (pkgDir </> "-")
    digests <- traverse (buildArtifact root spec) (psVersions spec)
    -- @<name>@ cannot be both a file and a directory, so the packument sits inside the package
    -- directory and the nginx stub config maps @/<name>@ to it.
    writeFileLBS (pkgDir </> "packument.json") (Aeson.encode (packument spec digests))

-- Archive one version's package tree and return the integrity its packument entry declares.
buildArtifact :: FilePath -> PkgSpec -> Text -> IO (Text, Text)
buildArtifact root spec version = do
    let name = toString (psName spec)
        tgzPath = artifactFile root (psName spec) version
        -- A scratch directory holding the package tree `tar` archives.
        workRoot = root </> (".work-" <> name <> "-" <> toString version)
        workPkg = workRoot </> "package"
    createDirectoryIfMissing True workPkg
    -- The artifact's package.json (npm tarballs root everything under `package/`).
    writeFileLBS (workPkg </> "package.json") (Aeson.encode (tarballPackageJson spec version))
    writeFileLBS (workPkg </> "index.js") "module.exports = {};\n"
    -- Deterministic gzip (fixed mtime) so a rebuild yields identical bytes.
    runProcess_ $
        proc
            "tar"
            [ "--sort=name"
            , "--mtime=2020-01-01 00:00:00Z"
            , "--owner=0"
            , "--group=0"
            , "--numeric-owner"
            , "-czf"
            , tgzPath
            , "-C"
            , workRoot
            , "package"
            ]
    bytes <- BS.readFile tgzPath
    when (psTamper spec) $
        -- Corrupt the served artifact after the SRI is fixed: the worker's integrity
        -- gate and npm's own check must now reject these bytes.
        BS.appendFile tgzPath "tampered"
    pure (version, sriSha512Of bytes)

-- The archived package.json carries the author fields too, so the tree matches its manifest.
tarballPackageJson :: PkgSpec -> Text -> Value
tarballPackageJson spec version =
    object $
        [ "name" .= psName spec
        , "version" .= version
        ]
            <> ["scripts" .= object ["install" .= ("node -e \"\"" :: Text)] | psInstallScript spec]
            <> filter (not . T.isPrefixOf "_" . Key.toText . fst) (psVersionFields spec)

packument :: PkgSpec -> [(Text, Text)] -> Value
packument spec digests =
    packumentValue
        (psName spec)
        (psVersion spec)
        [(version, versionMeta version sri) | (version, sri) <- digests]
        ( ["created" .= backdated, "modified" .= backdated]
            <> [fromString (toString version) .= backdated | (version, _) <- digests]
        )
        []
  where
    backdated :: Text
    backdated = "2020-01-01T00:00:00.000Z"

    versionMeta :: Text -> Text -> Value
    versionMeta version sri =
        versionValue
            ( (versionSpec (psName spec) version (tarballUrl version))
                { vsIntegrity = Just sri
                , vsHasInstallScript = psInstallScript spec
                , vsExtraPairs = psVersionFields spec <> installScriptFields
                , vsDistPairs = psDistFields spec
                }
            )

    tarballUrl :: Text -> Text
    tarballUrl version =
        "https://upstream/"
            <> psName spec
            <> "/-/"
            <> psName spec
            <> "-"
            <> version
            <> ".tgz"

    installScriptFields :: [Pair]
    installScriptFields
        | psInstallScript spec =
            [ "hasInstallScript" .= True
            , "scripts" .= object ["install" .= ("node -e \"\"" :: Text)]
            ]
        | otherwise = []
