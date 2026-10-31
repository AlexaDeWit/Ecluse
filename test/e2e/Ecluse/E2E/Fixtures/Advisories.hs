-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Local OSV exports and the EPSS slice for end-to-end Pilot runs.
module Ecluse.E2E.Fixtures.Advisories (
    advisoryExportBase,
    advisoryEpssPath,
    pypiAdvisoryExportBase,
    npmAdvisoryExportBase,
    npmAdvisoryId,
    pypiAdvisoryId,
    cargoAdvisoryExportBase,
    cargoAdvisoryId,
    rubyAdvisoryExportBase,
    rubyAdvisoryId,
    maliciousExportBase,
    maliciousAdvisoryId,
    partialFixExportBase,
    partialFixAdvisoryId,
    partialFixRemainingAdvisoryId,
    epssProofExportBase,
    epssControlExportBase,
    buildAdvisoryFixtures,
    ecosystemAdvisory,
) where

import Data.Aeson (Value, encode, object, (.=))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems))
import Ecluse.E2E.Fixtures.Npm (corpusRevokedPkg, psName)
import Ecluse.Test.Osv (CorpusVersion (CorpusV1, CorpusV2), osvCorpusZip, osvZipOf)
import Ecluse.Test.Osv.Withdrawal (maliciousZip)
import Ecluse.Test.OsvDb (epssFixtureWith)

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

-- | The npm read-policy proof has a separate export from the revocation corpus.
npmAdvisoryExportBase :: Text
npmAdvisoryExportBase = "advisories/npm-proof"

-- | The advisory linked to the npm affected and fixed releases.
npmAdvisoryId :: Text
npmAdvisoryId = "GHSA-e2e-npm"

-- | The scored advisory whose package spelling differs from the served canonical name.
pypiAdvisoryId :: Text
pypiAdvisoryId = "GHSA-e2e-flask"

-- | Cargo uses its native OSV source and exact SemVer fixes.
cargoAdvisoryExportBase, cargoAdvisoryId :: Text
cargoAdvisoryExportBase = "advisories/cargo-proof"
cargoAdvisoryId = "GHSA-e2e-cargo"

-- | RubyGems uses its native OSV identity and ecosystem version ranges.
rubyAdvisoryExportBase, rubyAdvisoryId :: Text
rubyAdvisoryExportBase = "advisories/rubygems-proof"
rubyAdvisoryId = "GHSA-e2e-rubygems"

-- | The stub-relative base for derived MAL records targeting inert client fixtures.
maliciousExportBase :: Text
maliciousExportBase = "advisories/mal-derived"

-- | The identifier 'maliciousZip' gives its first derived target.
maliciousAdvisoryId :: Text
maliciousAdvisoryId = "MAL-derived-1"

-- | The stub-relative base for each ecosystem's partial-fix export.
partialFixExportBase :: Text
partialFixExportBase = "advisories/partial-fix"

-- | The severe advisory the partial fix resolves. A minor one still affects that release, inside the shipped limits.
partialFixAdvisoryId :: Text
partialFixAdvisoryId = "GHSA-e2e-partial-severe"

-- | The minor advisory that still affects the partial fix, which a denial of that release names.
partialFixRemainingAdvisoryId :: Text
partialFixRemainingAdvisoryId = "GHSA-e2e-partial-minor"

-- | Each ecosystem's EPSS denial export: its advisory with no CVSS vector, so only EPSS 0.875 can deny.
epssProofExportBase :: Text
epssProofExportBase = "advisories/epss-proof"

-- | Each ecosystem's EPSS control export: its advisory with CVSS 9.8 and EPSS 0.0625, below a 0.5 deny.
epssControlExportBase :: Text
epssControlExportBase = "advisories/epss-control"

-- | Write the local OSV archives and shared EPSS slice without reaching public feeds.
buildAdvisoryFixtures :: FilePath -> IO ()
buildAdvisoryFixtures root = do
    for_ [minBound .. maxBound] $ \generation ->
        osvCorpusZip generation >>= writeUnder (advisoryExportBase generation <> "/npm/all.zip")
    osvZipOf
        [ ("flask.json", ecosystemAdvisory PyPI pypiAdvisoryId "Flask" "1.19")
        , ("raw-fix.json", ecosystemAdvisory PyPI "GHSA-e2e-raw-fix" "e2e-raw-fix" "1.19.0")
        ]
        >>= writeUnder (pypiAdvisoryExportBase <> "/PyPI/all.zip")
    osvZipOf [("npm.json", ecosystemAdvisory Npm npmAdvisoryId "e2e-advisory" "1.19.0")]
        >>= writeUnder (npmAdvisoryExportBase <> "/npm/all.zip")
    osvZipOf [("cargo.json", ecosystemAdvisory Cargo cargoAdvisoryId "ecluse-e2e-advisory" "1.19.0")]
        >>= writeUnder (cargoAdvisoryExportBase <> "/crates.io/all.zip")
    osvZipOf [("rubygems.json", ecosystemAdvisory RubyGems rubyAdvisoryId "ecluse-fixture-advisory" "1.19.0")]
        >>= writeUnder (rubyAdvisoryExportBase <> "/RubyGems/all.zip")
    maliciousZip RubyGems [("ecluse-fixture-advisory", ["1.18.0"])] Nothing >>= writeUnder (maliciousExportBase <> "/RubyGems/all.zip")
    maliciousZip Cargo [("ecluse-e2e-advisory", ["1.18.0"])] Nothing >>= writeUnder (maliciousExportBase <> "/crates.io/all.zip")
    maliciousZip Npm [(psName corpusRevokedPkg, ["1.0.0"])] Nothing >>= writeUnder (maliciousExportBase <> "/npm/all.zip")
    maliciousZip PyPI [("Flask", ["1.18"])] Nothing >>= writeUnder (maliciousExportBase <> "/PyPI/all.zip")
    for_ [(Npm, "npm", "e2e-advisory", "1.19.0", npmAdvisoryId), (PyPI, "PyPI", "Flask", "1.19", pypiAdvisoryId), (Cargo, "crates.io", "ecluse-e2e-advisory", "1.19.0", cargoAdvisoryId), (RubyGems, "RubyGems", "ecluse-fixture-advisory", "1.19.0", rubyAdvisoryId)] $ \(ecosystem, directory, name, fixed, advisory) -> do
        osvZipOf [("severe.json", ecosystemAdvisory ecosystem partialFixAdvisoryId name fixed), ("minor.json", minorAdvisory ecosystem name fixed)]
            >>= writeUnder (partialFixExportBase <> "/" <> directory <> "/all.zip")
        osvZipOf [("epss.json", scoredAdvisory ecosystem advisory "CVE-2026-10001" Nothing name (fixedAt fixed))]
            >>= writeUnder (epssProofExportBase <> "/" <> directory <> "/all.zip")
        osvZipOf [("epss.json", scoredAdvisory ecosystem advisory "CVE-2026-10004" (Just severeVector) name (fixedAt fixed))]
            >>= writeUnder (epssControlExportBase <> "/" <> directory <> "/all.zip")
    epssFixtureWith [(minorAlias, "0.005")] >>= writeUnder advisoryEpssPath
  where
    writeUnder relative bytes = do
        let path = root </> toString relative
        createDirectoryIfMissing True (takeDirectory path)
        writeFileLBS path bytes

-- | A scored native advisory with one fixed boundary and no feed-specific decoration.
ecosystemAdvisory :: Ecosystem -> Text -> Text -> Text -> LByteString
ecosystemAdvisory ecosystem advisory name fixed =
    scoredAdvisory ecosystem advisory "CVE-2026-10001" (Just severeVector) name (fixedAt fixed)

-- Every release before the fix is affected.
fixedAt :: Text -> [Value]
fixedAt fixed = [object ["introduced" .= ("0" :: Text)], object ["fixed" .= fixed]]

-- The CVSS 3.1 vector scoring 9.8.
severeVector :: Text
severeVector = "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:H/I:H/A:H"

-- The partial fix's remaining advisory: CVSS 4.2 and the EPSS 'minorAlias' carries, from the fix on.
minorAdvisory :: Ecosystem -> Text -> Text -> LByteString
minorAdvisory ecosystem name fixed =
    scoredAdvisory ecosystem partialFixRemainingAdvisoryId minorAlias (Just "CVSS:3.1/AV:N/AC:H/PR:N/UI:R/S:U/C:L/I:L/A:N") name [object ["introduced" .= fixed]]

minorAlias :: Text
minorAlias = "CVE-2026-10007"

scoredAdvisory :: Ecosystem -> Text -> Text -> Maybe Text -> Text -> [Value] -> LByteString
scoredAdvisory ecosystem advisory alias vector name events =
    encode . object $
        [ "id" .= advisory
        , "aliases" .= [alias]
        ]
            <> ["severity" .= [object ["type" .= ("CVSS_V3" :: Text), "score" .= score]] | Just score <- [vector]]
            <> [ "affected"
                    .= [ object
                            [ "package" .= object ["name" .= name, "ecosystem" .= (case ecosystem of PyPI -> "PyPI"; Cargo -> "crates.io"; RubyGems -> "RubyGems"; _ -> "npm" :: Text)]
                            , "ranges" .= [object ["type" .= (if ecosystem `elem` [PyPI, RubyGems] then "ECOSYSTEM" else "SEMVER" :: Text), "events" .= events]]
                            ]
                       ]
               ]
