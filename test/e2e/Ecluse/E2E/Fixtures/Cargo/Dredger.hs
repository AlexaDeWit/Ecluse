-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Owned Cargo archives and independent advisory generations for the common Dredger laws.
module Ecluse.E2E.Fixtures.Cargo.Dredger (
    buildDredgerReleases,
    dredgerPackages,
    deniedPackage,
    previewPackage,
    keptPackage,
    recoveryPackages,
    siblingPackage,
    refusedPackage,
    readmittedPackage,
    missingPackage,
    latePackage,
    sentinelPackage,
    advisoryPackage,
    firstPartyName,
    dredgerAdvisoryId,
    dredgerExportBase,
    dredgerMaliciousExportBase,
) where

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.E2E.Fixtures.Advisories (ecosystemAdvisory)
import Ecluse.E2E.Harness.Stub (StubRoute (CargoPublic), stubUrl)
import Ecluse.Test.Contract.Sweep.Types (PackageFixture (..))
import Ecluse.Test.Osv (CorpusVersion (..), osvZipOf)
import Ecluse.Test.Osv.Withdrawal (maliciousZip)
import Ecluse.Test.Registry.Cargo.Fixtures

-- | Every coordinate contains native Cargo.toml, README and repository-owned library source.
buildDredgerReleases :: FilePath -> IO [CargoRelease]
buildDredgerReleases work = do
    releases <- traverse (uncurry cargoLeafRelease) coordinates
    writeCargoRegistry (work </> "cargo") (stubUrl CargoPublic) Nothing False releases
    buildGenerations work
    pure releases
  where
    coordinates =
        [(packageName package, version) | package <- dredgerPackages, version <- packageVersions package]
            <> [(packageName previewPackage, version) | version <- ["2.0.0", "3.0.0"]]
            <> [(firstPartyName, version) | version <- ["1.0.0", "2.0.0"]]

-- | The complete native corpus covers each independent mutable-store lifetime.
dredgerPackages :: [PackageFixture]
dredgerPackages = [deniedPackage, previewPackage, keptPackage, sentinelPackage, advisoryPackage] <> recoveryPackages

-- | Identity denial removes this single native release.
deniedPackage :: PackageFixture
deniedPackage = PackageFixture "ecluse-dredger-denied" "1.0.0" ["1.0.0"]

-- | Extra releases distinguish mirror, shared and cache-only contents.
previewPackage :: PackageFixture
previewPackage = PackageFixture "ecluse-dredger-preview" "1.0.0" ["1.0.0"]

-- | Every cleanup preserves the unrelated native release.
keptPackage :: PackageFixture
keptPackage = PackageFixture "ecluse-dredger-kept" "1.0.0" ["1.0.0"]

-- | A completed sentinel proves that the real worker drained an admitted request.
sentinelPackage :: PackageFixture
sentinelPackage = PackageFixture "ecluse-dredger-sentinel" "1.0.0" ["1.0.0"]

-- | Distinct names keep the recovery cases independent.
recoveryPackages :: [PackageFixture]
recoveryPackages = [siblingPackage, refusedPackage, readmittedPackage, missingPackage, latePackage]

-- | The permitted sibling survives physical removal of the older release.
siblingPackage :: PackageFixture
siblingPackage = PackageFixture "ecluse-dredger-sibling" "2.0.0" ["1.0.0", "2.0.0"]

-- | The cache reports a native per-version refusal until the next sweep.
refusedPackage :: PackageFixture
refusedPackage = PackageFixture "ecluse-dredger-refused" "1.0.0" ["1.0.0"]

-- | Native build metadata remains part of the retained raw version identity.
readmittedPackage :: PackageFixture
readmittedPackage = PackageFixture "ecluse-dredger-readmitted" "1.0.1+readmitted" ["1.0.1+readmitted"]

-- | Withholding this public archive prevents recovery after deletion.
missingPackage :: PackageFixture
missingPackage = PackageFixture "ecluse-dredger-missing" "1.0.0" ["1.0.0"]

-- | A later publication tests physical rediscovery after an earlier sweep.
latePackage :: PackageFixture
latePackage = PackageFixture "ecluse-dredger-late" "1.0.0" ["1.0.0"]

-- | Both releases predate the scored and malicious advisory generations.
advisoryPackage :: PackageFixture
advisoryPackage = PackageFixture "ecluse-dredger-advisory" "1.2.0" ["1.0.0", "1.2.0"]

-- | The explicit native first-party declaration protects every stored version.
firstPartyName :: Text
firstPartyName = "ecluse-dredger-firstparty"

-- | Revocation audit evidence must retain this independent advisory identifier.
dredgerAdvisoryId :: Text
dredgerAdvisoryId = "GHSA-e2e-cargo-dredger"

-- | Distinct exports permit a real Pilot compile and remote generation swap.
dredgerExportBase :: CorpusVersion -> Text
dredgerExportBase = \case
    CorpusV1 -> "advisories/cargo-dredger-v1"
    CorpusV2 -> "advisories/cargo-dredger-v2"

-- | The derived MAL record condemns the same exact native vulnerable version.
dredgerMaliciousExportBase :: Text
dredgerMaliciousExportBase = "advisories/cargo-dredger-mal"

buildGenerations :: FilePath -> IO ()
buildGenerations work = do
    for_ [CorpusV1, CorpusV2] $ \generation -> do
        let unrelated = ("unrelated.json", ecosystemAdvisory Cargo "GHSA-e2e-unrelated" "ecluse-unrelated" "2.0.0")
            revoked = ("revoked.json", ecosystemAdvisory Cargo dredgerAdvisoryId (packageName advisoryPackage) "1.2.0")
        osvZipOf (unrelated : [revoked | generation == CorpusV2]) >>= writeExport (dredgerExportBase generation)
    maliciousZip Cargo [(packageName advisoryPackage, ["1.0.0"])] Nothing >>= writeExport dredgerMaliciousExportBase
  where
    writeExport base archive = do
        let directory = work </> "html" </> toString base </> "crates.io"
        createDirectoryIfMissing True directory
        writeFileLBS (directory </> "all.zip") archive
