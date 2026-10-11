-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | PyPI wheels, source distributions, and Simple indexes for end-to-end tests.
module Ecluse.E2E.Fixtures.PyPI (
    pypiUpstreamUrl,
    pypiProject,
    pypiVersion,
    pypiWheelFile,
    pypiSdistFile,
    pypiDistInfo,
    pypiWheelFor,
    pypiDistInfoFor,
    buildPyPIFixtures,
) where

import Data.Aeson (Value (Bool, Object, String), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Process.Typed (proc, runProcess_, setWorkingDir)

import Ecluse.Core.Text.Iso8601 (renderIso8601Utc)
import Ecluse.E2E.Harness.Stub (StubRoute (PyPIPublic), stubUrl)
import Ecluse.Test.Package (hexSha256Of)

-- | The in-network URL the nginx stub answers this project's Simple index on.
pypiUpstreamUrl :: Text
pypiUpstreamUrl = stubUrl PyPIPublic

-- | The ordinary-install project, in PEP 503 canonical form.
pypiProject :: Text
pypiProject = "e2e-sample"

-- | The single release the fixture index names.
pypiVersion :: Text
pypiVersion = "1.0.0"

-- | The wheel's on-the-wire name, which encodes the project, the release, and its tags.
pypiWheelFile :: Text
pypiWheelFile = pypiWheelFor pypiProject pypiVersion

-- | The source distribution's advertised filename.
pypiSdistFile :: Text
pypiSdistFile = pypiProject <> "-" <> pypiVersion <> ".tar.gz"

-- | The metadata directory a wheel install leaves in the client's target.
pypiDistInfo :: Text
pypiDistInfo = pypiDistInfoFor pypiProject pypiVersion

-- | Write ordinary-install artifacts, compatibility controls, and old/recent advisory releases.
buildPyPIFixtures :: FilePath -> IO ()
buildPyPIFixtures root = do
    let projectDir = root </> "simple" </> toString pypiProject
        wheelPath = projectDir </> toString pypiWheelFile
        sdistPath = projectDir </> toString pypiSdistFile
    createDirectoryIfMissing True projectDir
    buildWheel (root </> ".work-wheel") wheelPath
    buildSdist (root </> ".work-sdist") sdistPath
    bytes <- BS.readFile wheelPath
    sourceBytes <- BS.readFile sdistPath
    writeFileLBS (projectDir </> indexFile) (Aeson.encode (simpleIndex (hexSha256Of bytes) (hexSha256Of sourceBytes)))
    now <- renderIso8601Utc <$> getCurrentTime
    for_ ["flask", "e2e-raw-fix"] $ \project -> buildAdvisoryWheels root project now

-- The filename the nginx stub aliases a project's index request onto.
indexFile :: FilePath
indexFile = "index.json"

-- PEP 427 escapes each run of non-alphanumerics in the project part to one underscore.
escapedProject :: Text
escapedProject = T.replace "-" "_" pypiProject

buildWheel :: FilePath -> FilePath -> IO ()
buildWheel = buildWheelFor pypiProject pypiVersion metadataFile

buildWheelFor :: Text -> Text -> Text -> FilePath -> FilePath -> IO ()
buildWheelFor project version metadata work wheelPath = do
    let escaped = T.replace "-" "_" project
        distInfo = pypiDistInfoFor project version
        moduleDir = work </> toString escaped
        metadataDir = work </> toString distInfo
    createDirectoryIfMissing True moduleDir
    createDirectoryIfMissing True metadataDir
    writeFileText (moduleDir </> "__init__.py") ("VERSION = \"" <> version <> "\"\n")
    writeFileText (metadataDir </> "METADATA") metadata
    writeFileText (metadataDir </> "WHEEL") wheelMetadataFile
    writeFileText (metadataDir </> "RECORD") (recordFileFor project version)
    runProcess_ . setWorkingDir work $
        proc "python3" ["-m", "zipfile", "-c", wheelPath, toString escaped, toString distInfo]

buildSdist :: FilePath -> FilePath -> IO ()
buildSdist work sdistPath = do
    let distribution = pypiProject <> "-" <> pypiVersion
        sourceDir = work </> toString distribution
    createDirectoryIfMissing True sourceDir
    writeFileText (sourceDir </> "PKG-INFO") metadataFile
    writeFileText (sourceDir </> "setup.py") "from setuptools import setup\nsetup(name='e2e-sample', version='1.0.0')\n"
    runProcess_ (proc "tar" ["-czf", sdistPath, "-C", work, toString distribution])

metadataFile :: Text
metadataFile = metadataFileFor pypiProject pypiVersion

metadataFileFor :: Text -> Text -> Text
metadataFileFor project version =
    T.unlines
        [ "Metadata-Version: 2.1"
        , "Name: " <> project
        , "Version: " <> version
        , "Summary: An Ecluse end-to-end fixture distribution"
        ]

wheelMetadataFile :: Text
wheelMetadataFile =
    T.unlines
        [ "Wheel-Version: 1.0"
        , "Generator: ecluse-e2e"
        , "Root-Is-Purelib: true"
        , "Tag: py3-none-any"
        ]

-- pip reads RECORD to write its own install manifest and checks no digest in it, so the
-- entries carry the filename alone. The wheel's own sha256 is what this tier verifies.
recordFileFor :: Text -> Text -> Text
recordFileFor project version =
    T.unlines
        [ T.replace "-" "_" project <> "/__init__.py,,"
        , pypiDistInfoFor project version <> "/METADATA,,"
        , pypiDistInfoFor project version <> "/WHEEL,,"
        , pypiDistInfoFor project version <> "/RECORD,,"
        ]

simpleIndex :: Text -> Text -> Value
simpleIndex digest sourceDigest =
    object
        [ "name" .= pypiProject
        , "meta" .= object ["api-version" .= ("1.1" :: Text)]
        , "versions" .= [pypiVersion]
        , "files" .= [sdistEntry sourceDigest, wheelEntry digest, excludedWheel "2" "requires-python" (String ">=99") digest, excludedWheel "3" "yanked" (Bool True) digest]
        ]

wheelEntry :: Text -> Value
wheelEntry = wheelEntryFor pypiProject pypiVersion "2020-01-01T00:00:00Z"

wheelEntryFor :: Text -> Text -> Text -> Text -> Value
wheelEntryFor project version uploaded digest =
    object
        [ "filename" .= filename
        , "url" .= (pypiUpstreamUrl <> "simple/" <> project <> "/" <> filename)
        , "hashes" .= object ["sha256" .= digest]
        , "requires-python" .= (">=3.8" :: Text)
        , "upload-time" .= uploaded
        , "core-metadata" .= True
        ]
  where
    filename = pypiWheelFor project version

sdistEntry :: Text -> Value
sdistEntry digest =
    object
        [ "filename" .= pypiSdistFile
        , "url" .= (pypiUpstreamUrl <> "simple/" <> pypiProject <> "/" <> pypiSdistFile)
        , "hashes" .= object ["sha256" .= digest]
        , "upload-time" .= ("2020-01-01T00:00:00Z" :: Text)
        ]

-- These files are absent, so an installer that loses compatibility or yank fields cannot succeed.
excludedWheel :: Text -> Aeson.Key -> Value -> Text -> Value
excludedWheel build key value digest = case wheelEntry digest of
    Object fields -> Object (KeyMap.insert key value (KeyMap.insert "filename" (String filename) (KeyMap.insert "url" (String url) fields)))
    other -> other
  where
    filename = escapedProject <> "-" <> pypiVersion <> "-" <> build <> "-py3-none-any.whl"
    url = pypiUpstreamUrl <> "simple/" <> pypiProject <> "/" <> filename

-- | The wheel filename for a PEP 503 canonical project and its release spelling.
pypiWheelFor :: Text -> Text -> Text
pypiWheelFor project version = T.replace "-" "_" project <> "-" <> version <> "-py3-none-any.whl"

-- | The installed metadata directory for a fixture wheel.
pypiDistInfoFor :: Text -> Text -> Text
pypiDistInfoFor project version = T.replace "-" "_" project <> "-" <> version <> ".dist-info"

buildAdvisoryWheels :: FilePath -> Text -> Text -> IO ()
buildAdvisoryWheels root project now = do
    let projectDir = root </> "simple" </> toString project
    createDirectoryIfMissing True projectDir
    files <- forM [("1.18", "2020-01-01T00:00:00Z"), ("1.19", now)] $ \(version, uploaded) -> do
        let filename = pypiWheelFor project version
            wheelPath = projectDir </> toString filename
            work = root </> toString (".work-" <> project <> "-" <> version)
        buildWheelFor project version (metadataFileFor project version) work wheelPath
        bytes <- BS.readFile wheelPath
        pure (wheelEntryFor project version uploaded (hexSha256Of bytes))
    writeFileLBS (projectDir </> indexFile) . Aeson.encode $
        object ["name" .= project, "meta" .= object ["api-version" .= ("1.1" :: Text)], "versions" .= (["1.18", "1.19"] :: [Text]), "files" .= files]
