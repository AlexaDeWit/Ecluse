-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | PyPI fixtures for the nginx upstream in end-to-end tests.
The release is backdated past quarantine and carries a wheel alone: an sdist beside it
reads as install-time code execution, which the harness deny rule refuses.
-}
module Ecluse.E2E.Fixtures.PyPI (
    pypiUpstreamUrl,
    pypiProject,
    pypiVersion,
    pypiWheelFile,
    pypiDistInfo,
    buildPyPIFixtures,
) where

import Data.Aeson (Value (Bool, Object, String), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Process.Typed (proc, runProcess_, setWorkingDir)

import Ecluse.Test.Package (hexSha256Of)

-- | The in-network URL the nginx stub answers this project's Simple index on.
pypiUpstreamUrl :: Text
pypiUpstreamUrl = "https://pypi-upstream/"

-- | The one project the fixture index serves, in PEP 503 canonical form.
pypiProject :: Text
pypiProject = "e2e-sample"

-- | The single release the fixture index names.
pypiVersion :: Text
pypiVersion = "1.0.0"

-- | The wheel's on-the-wire name, which encodes the project, the release, and its tags.
pypiWheelFile :: Text
pypiWheelFile = escapedProject <> "-" <> pypiVersion <> "-py3-none-any.whl"

-- | The metadata directory a wheel install leaves in the client's target.
pypiDistInfo :: Text
pypiDistInfo = escapedProject <> "-" <> pypiVersion <> ".dist-info"

-- | Write one wheel and an index with higher-build candidates that pip must exclude.
buildPyPIFixtures :: FilePath -> IO ()
buildPyPIFixtures root = do
    let projectDir = root </> "simple" </> toString pypiProject
        wheelPath = projectDir </> toString pypiWheelFile
    createDirectoryIfMissing True projectDir
    buildWheel (root </> ".work-wheel") wheelPath
    bytes <- BS.readFile wheelPath
    writeFileLBS (projectDir </> indexFile) (Aeson.encode (simpleIndex (hexSha256Of bytes)))

-- The filename the nginx stub aliases a project's index request onto.
indexFile :: FilePath
indexFile = "index.json"

-- PEP 427 escapes each run of non-alphanumerics in the project part to one underscore.
escapedProject :: Text
escapedProject = T.replace "-" "_" pypiProject

{- A wheel is a zip of the importable package beside its .dist-info metadata, staged in
@work@ and archived by `python3 -m zipfile`, as the npm fixtures archive with `tar`. -}
buildWheel :: FilePath -> FilePath -> IO ()
buildWheel work wheelPath = do
    let moduleDir = work </> toString escapedProject
        metadataDir = work </> toString pypiDistInfo
    createDirectoryIfMissing True moduleDir
    createDirectoryIfMissing True metadataDir
    writeFileText (moduleDir </> "__init__.py") ("VERSION = \"" <> pypiVersion <> "\"\n")
    writeFileText (metadataDir </> "METADATA") metadataFile
    writeFileText (metadataDir </> "WHEEL") wheelMetadataFile
    writeFileText (metadataDir </> "RECORD") recordFile
    runProcess_ . setWorkingDir work $
        proc "python3" ["-m", "zipfile", "-c", wheelPath, toString escapedProject, toString pypiDistInfo]

metadataFile :: Text
metadataFile =
    T.unlines
        [ "Metadata-Version: 2.1"
        , "Name: " <> pypiProject
        , "Version: " <> pypiVersion
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
recordFile :: Text
recordFile =
    T.unlines
        [ escapedProject <> "/__init__.py,,"
        , pypiDistInfo <> "/METADATA,,"
        , pypiDistInfo <> "/WHEEL,,"
        , pypiDistInfo <> "/RECORD,,"
        ]

simpleIndex :: Text -> Value
simpleIndex digest =
    object
        [ "name" .= pypiProject
        , "meta" .= object ["api-version" .= ("1.1" :: Text)]
        , "versions" .= [pypiVersion]
        , "files" .= [wheelEntry digest, excludedWheel "2" "requires-python" (String ">=99") digest, excludedWheel "3" "yanked" (Bool True) digest]
        ]

wheelEntry :: Text -> Value
wheelEntry digest =
    object
        [ "filename" .= pypiWheelFile
        , "url" .= (pypiUpstreamUrl <> "simple/" <> pypiProject <> "/" <> pypiWheelFile)
        , "hashes" .= object ["sha256" .= digest]
        , "requires-python" .= (">=3.8" :: Text)
        , "upload-time" .= ("2020-01-01T00:00:00Z" :: Text)
        , "core-metadata" .= True
        ]

-- These files are absent, so an installer that loses compatibility or yank fields cannot succeed.
excludedWheel :: Text -> Aeson.Key -> Value -> Text -> Value
excludedWheel build key value digest = case wheelEntry digest of
    Object fields -> Object (KeyMap.insert key value (KeyMap.insert "filename" (String filename) (KeyMap.insert "url" (String url) fields)))
    other -> other
  where
    filename = escapedProject <> "-" <> pypiVersion <> "-" <> build <> "-py3-none-any.whl"
    url = pypiUpstreamUrl <> "simple/" <> pypiProject <> "/" <> filename
