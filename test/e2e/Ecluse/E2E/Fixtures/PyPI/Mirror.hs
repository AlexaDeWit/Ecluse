-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native PyPI releases: a dependency graph with every supported archive format, and plain wheel-and-sdist releases.
module Ecluse.E2E.Fixtures.PyPI.Mirror (
    Distribution (..),
    mirrorRoot,
    mirrorDependency,
    mirrorVersion,
    mirrorExecutable,
    mirrorExport,
    mirrorGraph,
    mirrorPlatformX86,
    mirrorPlatformArm,
    mirrorWheelFile,
    mirrorArchitectures,
    distributionFields,
    distributionUploadBody,
    builtByCurrentTool,
    hiddenPartUploadBody,
    releaseRemovalBody,
    nativeUploadContentType,
    buildMirrorGraph,
    buildNativeProjects,
    buildRelease,
    mirrorFileNames,
    distributionIndex,
    yankedIndex,
    yankProject,
    yankKeptVersion,
    yankGreatestVersion,
    yankReason,
) where

import Data.Aeson (Value (Bool, String), object, (.=))
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Info qualified as System
import System.Process.Typed (proc, runProcess_, setWorkingDir)

import Ecluse.E2E.Fixtures.PyPI (WheelCompression (Bzip2Wheel, LzmaWheel), buildSdistFor, buildWheelFor, buildWheelWithCompression, metadataFileFor, pypiDistInfoFor, pypiWheelFor)
import Ecluse.Test.Package (hexSha256Of)

-- | Original bytes and supported source fields for one distribution.
data Distribution = Distribution
    { distributionProject :: Text
    , distributionVersion :: Text
    , distributionFilename :: Text
    , distributionBytes :: ByteString
    , distributionMetadata :: ByteString
    , distributionArchiveMetadata :: ByteString
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
buildMirrorGraph root = buildNativeProjects root [mirrorRoot, mirrorDependency]

-- | Build independent native projects with the same complete multi-file release shape.
buildNativeProjects :: FilePath -> [Text] -> IO [Distribution]
buildNativeProjects root = fmap concat . traverse (buildProject root)

buildProject :: FilePath -> Text -> IO [Distribution]
buildProject root project = do
    let projectDir = root </> "simple" </> toString project
    createDirectoryIfMissing True projectDir
    wheels <- forM [("x86_64", mirrorPlatformX86, Bzip2Wheel), ("aarch64", mirrorPlatformArm, LzmaWheel)] $ \(architecture, platform, compression) -> do
        let tag = "py3-none-" <> platform
            filename = mirrorWheelFile project platform
            metadata = projectMetadata project ("Wheel " <> tag)
            path = projectDir </> toString filename
        buildWheelWithCompression
            compression
            project
            mirrorVersion
            metadata
            tag
            (wheelFiles project architecture)
            (root </> toString (".build-" <> filename))
            path
        bytes <- BS.readFile path
        let companion = encodeUtf8 (metadata <> "\nPublic companion payload.\n")
        BS.writeFile (path <> ".metadata") companion
        pure (Distribution project mirrorVersion filename bytes companion (encodeUtf8 metadata))
    sources <- forM sourceFormats $ \suffix -> do
        let filename = sourceStem project suffix <> "." <> suffix
            metadata = projectMetadata project ("Source " <> suffix)
            work = root </> toString (".build-" <> filename)
            folder = toString (sourceStem project suffix)
            path = projectDir </> toString filename
        createDirectoryIfMissing True (work </> folder)
        writeFileText (work </> folder </> "PKG-INFO") metadata
        writeFileText (work </> folder </> "setup.py") "raise RuntimeError('source archive must never execute')\n"
        case suffix of
            "zip" -> runProcess_ . setWorkingDir work $ proc "python3" ["-m", "zipfile", "-c", path, folder]
            _ -> runProcess_ (proc "tar" [compressionFlag suffix, path, "-C", work, folder])
        bytes <- BS.readFile path
        pure (Distribution project mirrorVersion filename bytes (encodeUtf8 metadata) (encodeUtf8 metadata))
    pure (wheels <> sources)

-- | Build one release as a pure-Python wheel with its metadata companion and a source archive.
buildRelease :: FilePath -> Text -> Text -> IO [Distribution]
buildRelease root project version = do
    let filename = pypiWheelFor project version
        sourceName = project <> "-" <> version <> ".tar.gz"
        metadata = metadataFileFor project version
        directory = root </> "simple" </> toString project
        path = directory </> toString filename
        sourcePath = directory </> toString sourceName
    createDirectoryIfMissing True directory
    buildWheelFor project version metadata (root </> toString (".wheel-" <> filename)) path
    buildSdistFor project version metadata (root </> toString (".source-" <> sourceName)) sourcePath
    bytes <- BS.readFile path
    sourceBytes <- BS.readFile sourcePath
    pure [Distribution project version filename bytes (encodeUtf8 metadata) (encodeUtf8 metadata), Distribution project version sourceName sourceBytes (encodeUtf8 metadata) (encodeUtf8 metadata)]

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

-- | The public Simple entry, yanked when given a reason, retains fields the native target deliberately loses.
distributionEntry :: Text -> Maybe Text -> Distribution -> Value
distributionEntry base yank distribution =
    object $
        [ "filename" .= distributionFilename distribution
        , "url" .= (base <> "simple/" <> distributionProject distribution <> "/" <> distributionFilename distribution)
        , "hashes" .= object ["sha256" .= hexSha256Of (distributionBytes distribution)]
        , "requires-python" .= (">=3.8" :: Text)
        , "upload-time" .= ("2020-01-01T00:00:00Z" :: Text)
        , "size" .= BS.length (distributionBytes distribution)
        , "yanked" .= maybe (Bool False) String yank
        , "fixture-field" .= ("preserved source value" :: Text)
        ]
            <> ["core-metadata" .= object ["sha256" .= hexSha256Of (distributionMetadata distribution)] | ".whl" `T.isSuffixOf` distributionFilename distribution]

-- | Render one project's full expected inventory.
distributionIndex :: Text -> Text -> [Distribution] -> Value
distributionIndex base = yankedIndex base Map.empty

-- | Render the inventory with every file of each keyed version yanked for that version's reason.
yankedIndex :: Text -> Map Text Text -> Text -> [Distribution] -> Value
yankedIndex base yanks project distributions =
    object
        [ "name" .= project
        , "meta" .= object ["api-version" .= ("1.1" :: Text)]
        , "versions" .= ordNub (map distributionVersion released)
        , "files" .= [distributionEntry base (Map.lookup (distributionVersion distribution) yanks) distribution | distribution <- released]
        ]
  where
    released = filter ((== project) . distributionProject) distributions

-- | A project whose source can yank its greatest release before the mirror copies it.
yankProject :: Text
yankProject = "e2e-mirror-yank"

-- | The release an unpinned install must select while the greatest release is yanked.
yankKeptVersion :: Text
yankKeptVersion = "1.0.0"

-- | The greatest release. A pin on it copies its whole version to the mirror target.
yankGreatestVersion :: Text
yankGreatestVersion = "2.0.0"

-- | The source's reason for yanking the greatest release.
yankReason :: Text
yankReason = "withdrawn before the mirror copy"

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

-- | Native upload fields declared by the fixture's archive metadata, independent of the product parser.
distributionFields :: Distribution -> [(ByteString, ByteString)]
distributionFields distribution =
    [ (encodeUtf8 (fieldName name), encodeUtf8 value)
    | line <- T.lines headers
    , let (name, rest) = T.breakOn ":" line
    , Just value <- [T.stripPrefix ": " rest]
    ]
        <> [("description", encodeUtf8 (T.drop 2 body)) | not (T.null body)]
  where
    (headers, body) = T.breakOn "\n\n" (decodeUtf8 (distributionArchiveMetadata distribution))
    fieldName "Classifier" = "classifiers"
    fieldName "Project-URL" = "project_urls"
    fieldName name = T.replace "-" "_" (T.toLower name)

-- | The boundary used by the independent fixture multipart encoder.
nativeUploadContentType :: ByteString
nativeUploadContentType = "multipart/form-data; boundary=fixture-upload"

-- | Encode an actual client upload, with explicit replacement fields for refusal controls.
distributionUploadBody :: Distribution -> [(Text, Text)] -> ByteString
distributionUploadBody distribution overrides = uploadWithData distribution overrides (distributionBytes distribution)

-- | Whether a current build tool writes this file, which the publish guard requires: a wheel, or an sdist under its PEP 625 name.
builtByCurrentTool :: Distribution -> Bool
builtByCurrentTool distribution = ".whl" `T.isSuffixOf` filename || filename == T.replace "-" "_" (distributionProject distribution) <> "-" <> distributionVersion distribution <> ".tar.gz"
  where
    filename = distributionFilename distribution

-- | A client upload whose content data hides a second content part after the given line end and delimiter padding.
hiddenPartUploadBody :: Distribution -> Distribution -> ByteString -> ByteString -> ByteString
hiddenPartUploadBody carrier hidden lineEnd padding =
    uploadWithData carrier [] (distributionBytes carrier <> lineEnd <> "--fixture-upload" <> padding <> lineEnd <> disposition <> lineEnd <> lineEnd <> distributionBytes hidden)
  where
    disposition = "Content-Disposition: form-data; name=\"content\"; filename=\"" <> encodeUtf8 (distributionFilename hidden) <> "\""

uploadWithData :: Distribution -> [(Text, Text)] -> ByteString -> ByteString
uploadWithData distribution overrides bytes =
    BS.concat (map fieldPart (controls <> fields <> replacements))
        <> "--fixture-upload\r\nContent-Disposition: form-data; name=\"content\"; filename=\""
        <> encodeUtf8 (distributionFilename distribution)
        <> "\"\r\nContent-Type: application/octet-stream\r\n\r\n"
        <> bytes
        <> "\r\n--fixture-upload--\r\n"
  where
    replacements = [(encodeUtf8 name, encodeUtf8 value) | (name, value) <- overrides]
    fields = filter ((`notElem` map fst replacements) . fst) (distributionFields distribution)
    controls =
        [ (":action", "file_upload")
        , ("protocol_version", "1")
        , ("filetype", if ".whl" `T.isSuffixOf` distributionFilename distribution then "bdist_wheel" else "sdist")
        , ("pyversion", if ".whl" `T.isSuffixOf` distributionFilename distribution then "py3" else "source")
        , ("sha256_digest", encodeUtf8 (hexSha256Of (distributionBytes distribution)))
        ]

-- | A raw @remove_pkg@ of one release, which pypiserver honours and no uploader sends.
releaseRemovalBody :: Text -> Text -> ByteString
releaseRemovalBody project version =
    BS.concat (map fieldPart [(":action", "remove_pkg"), ("name", encodeUtf8 project), ("version", encodeUtf8 version)])
        <> "--fixture-upload--\r\n"

fieldPart :: (ByteString, ByteString) -> ByteString
fieldPart (name, value) = "--fixture-upload\r\nContent-Disposition: form-data; name=\"" <> name <> "\"\r\n\r\n" <> value <> "\r\n"

-- | The wheel's installed console command, executed only in a native virtual environment.
mirrorExecutable :: Text
mirrorExecutable = "e2e-mirror"

-- | The root must load its dependency to produce this export.
mirrorExport :: Value
mirrorExport = object ["root" .= mirrorVersion, "dependency" .= mirrorVersion]

-- | Installed package versions and dependency declarations, read independently from METADATA.
mirrorGraph :: Map Text (Text, [Text])
mirrorGraph = Map.fromList [(mirrorRoot, (mirrorVersion, [mirrorDependency <> "==" <> mirrorVersion])), (mirrorDependency, (mirrorVersion, []))]

wheelFiles :: Text -> Text -> [(FilePath, Text)]
wheelFiles project architecture =
    [ (toString escaped </> "architecture.txt", architecture <> "\n")
    , (toString escaped </> "__init__.py", moduleBody)
    ]
        <> [(toString dataDirectory </> "scripts" </> toString mirrorExecutable, executableBody) | project == mirrorRoot]
  where
    escaped = T.replace "-" "_" project
    dataDirectory = fromMaybe "" (T.stripSuffix ".dist-info" (pypiDistInfoFor project mirrorVersion)) <> ".data"
    moduleBody =
        T.unlines $
            ["VERSION = " <> show mirrorVersion]
                <> if project == mirrorRoot
                    then ["from e2e_mirror_dependency import export as dependency_export", "def export():", "    return {'root': VERSION, 'dependency': dependency_export()}"]
                    else ["def export():", "    return VERSION"]
    executableBody = T.unlines ["#!/usr/bin/env python3", "import json", "from e2e_mirror_root import export", "print(json.dumps(export(), sort_keys=True))"]

sourceFormats :: [Text]
sourceFormats = ["tar.gz", "tgz", "tar.bz2", "tar.xz", "zip"]

-- A current build tool writes a .tar.gz under its PEP 625 name. The older archive formats keep the legacy spelling.
sourceStem :: Text -> Text -> Text
sourceStem project suffix = (if suffix == "tar.gz" then T.replace "-" "_" project else project) <> "-" <> mirrorVersion

-- | The independent expected filenames for every supported wheel and source archive in one release.
mirrorFileNames :: Text -> [Text]
mirrorFileNames project =
    map (mirrorWheelFile project) [mirrorPlatformX86, mirrorPlatformArm]
        <> [sourceStem project suffix <> "." <> suffix | suffix <- sourceFormats]
