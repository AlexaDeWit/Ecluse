-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Local OSV exports and the EPSS slice for end-to-end Pilot runs.
module Ecluse.E2E.Fixtures.Advisories (
    advisoryExportBase,
    advisoryEpssPath,
    pypiAdvisoryExportBase,
    pypiAdvisoryId,
    buildAdvisoryFixtures,
) where

import Data.Aeson (encode, object, (.=))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))

import Ecluse.Test.Osv (CorpusVersion (CorpusV1, CorpusV2), osvCorpusZip, osvZipOf)
import Ecluse.Test.OsvDb (epssFixtureFile)

-- | The stub-relative base for one corpus generation in osv.dev's layout.
advisoryExportBase :: CorpusVersion -> Text
advisoryExportBase generation = "advisories/" <> tag
  where
    tag = case generation of
        CorpusV1 -> "v1"
        CorpusV2 -> "v2"

-- | The stub-relative path of the EPSS feed slice Pilot joins onto every generation.
advisoryEpssPath :: Text
advisoryEpssPath = "advisories/epss_scores-current.csv.gz"

-- | The stub-relative base for the PyPI advisory proof's export.
pypiAdvisoryExportBase :: Text
pypiAdvisoryExportBase = "advisories/pypi-proof"

-- | The scored advisory whose package spelling differs from the served canonical name.
pypiAdvisoryId :: Text
pypiAdvisoryId = "GHSA-e2e-flask"

-- | Write the local OSV archives and shared EPSS slice without reaching public feeds.
buildAdvisoryFixtures :: FilePath -> IO ()
buildAdvisoryFixtures root = do
    for_ [minBound .. maxBound] $ \generation ->
        osvCorpusZip generation >>= writeUnder (advisoryExportBase generation <> "/npm/all.zip")
    osvZipOf
        [ ("flask.json", pypiAdvisory pypiAdvisoryId "Flask" "1.19")
        , ("raw-fix.json", pypiAdvisory "GHSA-e2e-raw-fix" "e2e-raw-fix" "1.19.0")
        ]
        >>= writeUnder (pypiAdvisoryExportBase <> "/PyPI/all.zip")
    readFileLBS epssFixtureFile >>= writeUnder advisoryEpssPath
  where
    writeUnder relative bytes = do
        let path = root </> toString relative
        createDirectoryIfMissing True (takeDirectory path)
        writeFileLBS path bytes

pypiAdvisory :: Text -> Text -> Text -> LByteString
pypiAdvisory advisory name fixed =
    encode $
        object
            [ "id" .= advisory
            , "aliases" .= ["CVE-2026-10001" :: Text]
            , "severity" .= [object ["type" .= ("CVSS_V3" :: Text), "score" .= ("CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:H/I:H/A:H" :: Text)]]
            , "affected"
                .= [ object
                        [ "package" .= object ["name" .= name, "ecosystem" .= ("PyPI" :: Text)]
                        , "ranges" .= [object ["type" .= ("ECOSYSTEM" :: Text), "events" .= [object ["introduced" .= ("0" :: Text)], object ["fixed" .= fixed]]]]
                        ]
                   ]
            ]
