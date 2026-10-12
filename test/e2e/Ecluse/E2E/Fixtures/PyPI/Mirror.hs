-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | A dependency graph with distinct wheel metadata and every supported source archive format.
module Ecluse.E2E.Fixtures.PyPI.Mirror (
    Distribution (..),
    mirrorRoot,
    mirrorDependency,
    mirrorVersion,
    mirrorPlatformX86,
    mirrorPlatformArm,
    mirrorWheelFile,
    mirrorArchitectures,
    buildMirrorGraph,
    buildPublication,
    distributionEntry,
    distributionIndex,
) where

import Data.Aeson (Value, object, (.=))
import Data.ByteString qualified as BS
import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Info qualified as System
import System.Process.Typed (proc, runProcess_, setWorkingDir)

import Ecluse.E2E.Fixtures.PyPI (buildWheelWithFiles)
import Ecluse.Test.Package (hexSha256Of)

-- | Original bytes and supported source fields for one distribution.
data Distribution = Distribution
    { distributionProject :: Text
    , distributionFilename :: Text
    , distributionBytes :: ByteString
    , distributionMetadata :: ByteString
    }

-- | The package pip installs explicitly.
mirrorRoot :: Text
mirrorRoot = "e2e-mirror-root"

-- | The dependency pip must discover from the root's metadata.
mirrorDependency :: Text
mirrorDependency = "e2e-mirror-dependency"

-- | The version shared by the fixture graph.
mirrorVersion :: Text
mirrorVersion = "1.0.0"

-- | Build both projects under the existing nginx public fixture directory.
buildMirrorGraph :: FilePath -> IO [Distribution]
buildMirrorGraph root = concat <$> traverse (buildProject root) [mirrorRoot, mirrorDependency]

-- | Build a separate first-party project for publication relay cases.
buildPublication :: FilePath -> IO [Distribution]
buildPublication root = buildProject root "e2e-published"

buildProject :: FilePath -> Text -> IO [Distribution]
buildProject root project = do
    let projectDir = root </> "simple" </> toString project
    createDirectoryIfMissing True projectDir
    wheels <- forM [("x86_64", mirrorPlatformX86), ("aarch64", mirrorPlatformArm)] $ \(architecture, platform) -> do
        let tag = "py3-none-" <> platform
            filename = mirrorWheelFile project platform
            metadata = projectMetadata project ("Wheel " <> tag)
            path = projectDir </> toString filename
        buildWheelWithFiles
            project
            mirrorVersion
            metadata
            tag
            [(toString (T.replace "-" "_" project) </> "architecture.txt", architecture <> "\n")]
            (root </> toString (".build-" <> filename))
            path
        bytes <- BS.readFile path
        let companion = encodeUtf8 (metadata <> "\nPublic companion payload.\n")
        BS.writeFile (path <> ".metadata") companion
        pure (Distribution project filename bytes companion)
    sources <- forM ["tar.gz", "tgz", "tar.bz2", "tar.xz", "zip"] $ \suffix -> do
        let filename = project <> "-" <> mirrorVersion <> "." <> suffix
            metadata = projectMetadata project ("Source " <> suffix)
            work = root </> toString (".build-" <> filename)
            folder = toString (project <> "-" <> mirrorVersion)
            path = projectDir </> toString filename
        createDirectoryIfMissing True (work </> folder)
        writeFileText (work </> folder </> "PKG-INFO") metadata
        writeFileText (work </> folder </> "setup.py") "raise RuntimeError('source archive must never execute')\n"
        case suffix of
            "zip" -> runProcess_ . setWorkingDir work $ proc "python3" ["-m", "zipfile", "-c", path, folder]
            _ -> runProcess_ (proc "tar" [compressionFlag suffix, path, "-C", work, folder])
        bytes <- BS.readFile path
        pure (Distribution project filename bytes (encodeUtf8 metadata))
    pure (wheels <> sources)

compressionFlag :: Text -> String
compressionFlag = \case
    "tar.bz2" -> "-cjf"
    "tar.xz" -> "-cJf"
    _ -> "-czf"

projectMetadata :: Text -> Text -> Text
projectMetadata project summary =
    T.unlines $
        [ "Metadata-Version: 2.2"
        , "Name: " <> project
        , "Version: " <> mirrorVersion
        , "Summary: " <> summary
        , "Requires-Python: >=3.8"
        , "Provides-Extra: test"
        , "Classifier: Programming Language :: Python :: 3"
        , "Classifier: License :: OSI Approved :: MIT License"
        ]
            <> ["Requires-Dist: " <> mirrorDependency <> "==" <> mirrorVersion | project == mirrorRoot]
            <> ["", "Fixture description with a second line.", "Retain this body verbatim."]

-- | The public Simple entry retains fields the native target deliberately loses.
distributionEntry :: Text -> Distribution -> Value
distributionEntry base distribution =
    object $
        [ "filename" .= distributionFilename distribution
        , "url" .= (base <> "simple/" <> distributionProject distribution <> "/" <> distributionFilename distribution)
        , "hashes" .= object ["sha256" .= hexSha256Of (distributionBytes distribution)]
        , "requires-python" .= (">=3.8" :: Text)
        , "upload-time" .= ("2020-01-01T00:00:00Z" :: Text)
        , "size" .= BS.length (distributionBytes distribution)
        , "yanked" .= False
        , "fixture-field" .= ("preserved source value" :: Text)
        ]
            <> ["core-metadata" .= object ["sha256" .= hexSha256Of (distributionMetadata distribution)] | ".whl" `T.isSuffixOf` distributionFilename distribution]

-- | Render one project's full expected inventory.
distributionIndex :: Text -> Text -> [Distribution] -> Value
distributionIndex base project distributions =
    object
        [ "name" .= project
        , "meta" .= object ["api-version" .= ("1.1" :: Text)]
        , "versions" .= [mirrorVersion]
        , "files" .= map (distributionEntry base) (filter ((== project) . distributionProject) distributions)
        ]

-- | The Linux x86_64 compatibility tag used for the first cross-target installation.
mirrorPlatformX86 :: Text
mirrorPlatformX86 = "manylinux_2_17_x86_64"

-- | The distinct Linux aarch64 target installed after mirroring and public shutdown.
mirrorPlatformArm :: Text
mirrorPlatformArm = "manylinux_2_17_aarch64"

-- | The architecture-specific wheel selected for one project and pip platform value.
mirrorWheelFile :: Text -> Text -> Text
mirrorWheelFile project platform = T.replace "-" "_" project <> "-" <> mirrorVersion <> "-py3-none-" <> platform <> ".whl"

-- | The native client architecture first and the distinct Linux target second.
mirrorArchitectures :: Either Text ((Text, Text), (Text, Text))
mirrorArchitectures = case System.arch of
    "x86_64" -> Right (x86, arm)
    "aarch64" -> Right (arm, x86)
    other -> Left ("unsupported E2E client architecture: " <> toText other)
  where
    x86 = ("x86_64", mirrorPlatformX86)
    arm = ("aarch64", mirrorPlatformArm)
