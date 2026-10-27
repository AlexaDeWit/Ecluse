-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Genuine RubyGems coordinates and independent advisory generations for maintenance laws.
module Ecluse.E2E.Harness.RubyGemsCodeArtifact.Fixtures (
    deniedPackage,
    previewPackage,
    keptPackage,
    siblingPackage,
    refusedPackage,
    readmittedPackage,
    missingPackage,
    latePackage,
    sentinelPackage,
    advisoryPackage,
    recoveryPackages,
    firstPartyName,
    dredgerAdvisoryId,
    dredgerExportBase,
    dredgerMaliciousExportBase,
    buildSweepAdvisories,
) where

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.E2E.Fixtures.Advisories (ecosystemAdvisory)
import Ecluse.Test.Contract.Sweep.Types (PackageFixture (..))
import Ecluse.Test.Osv (CorpusVersion (..), osvZipOf)
import Ecluse.Test.Osv.Withdrawal (maliciousZip)

-- | Independent lifetimes reuse a native leaf with stable versions.
deniedPackage :: PackageFixture
deniedPackage = PackageFixture "ecluse-fixture-leaf" "1.0.0" ["1.0.0"]

-- | Three native root versions distinguish mirror and cache-only inventory.
previewPackage :: PackageFixture
previewPackage = PackageFixture "ecluse-fixture-root" "1.0.0" ["1.0.0"]

-- | Whole-version maintenance must preserve all four platform artifacts of this unrelated version.
keptPackage :: PackageFixture
keptPackage = PackageFixture "ecluse-fixture-platform" "1.0.0" ["1.0.0"]

-- | The newer native leaf survives deletion of its older sibling.
siblingPackage :: PackageFixture
siblingPackage = PackageFixture "ecluse-fixture-leaf" "2.0.0" ["1.0.0", "2.0.0"]

-- | The cache refuses this root's first physical delete until the next cycle.
refusedPackage :: PackageFixture
refusedPackage = previewPackage

-- | The prerelease spelling stays part of the exact persisted native coordinate.
readmittedPackage :: PackageFixture
readmittedPackage = PackageFixture "ecluse-fixture-readmitted" "1.0.1.pre.readmitted" ["1.0.1.pre.readmitted"]

-- | A withheld native archive prevents recovery despite a retained metadata row.
missingPackage :: PackageFixture
missingPackage = PackageFixture "ecluse-fixture-missing" "1.0.0" ["1.0.0"]

-- | A genuine post-sweep publication must appear in physical inventory again.
latePackage :: PackageFixture
latePackage = PackageFixture "ecluse-fixture-late" "1.0.0" ["1.0.0"]

-- | A completed worker publication proves that queued work passed the refused request.
sentinelPackage :: PackageFixture
sentinelPackage = PackageFixture "ecluse-fixture-sentinel" "1.0.0" ["1.0.0"]

-- | Scored and MAL generations condemn the old native leaf while preserving its fixed version.
advisoryPackage :: PackageFixture
advisoryPackage = siblingPackage

-- | These distinct names keep every recovery case inside its own mutable target identity.
recoveryPackages :: [PackageFixture]
recoveryPackages = [siblingPackage, refusedPackage, readmittedPackage, missingPackage, latePackage]

-- | Native first-party uploads have their own genuine source identity.
firstPartyName :: Text
firstPartyName = "ecluse-fixture-firstparty"

-- | Scored revocation evidence must name this independently generated advisory.
dredgerAdvisoryId :: Text
dredgerAdvisoryId = "GHSA-e2e-rubygems-dredger"

-- | Each Pilot run reads a different controlled source generation.
dredgerExportBase :: CorpusVersion -> Text
dredgerExportBase = \case
    CorpusV1 -> "advisories/rubygems-dredger-v1"
    CorpusV2 -> "advisories/rubygems-dredger-v2"

-- | The MAL generation uses the same vulnerable coordinate through the native RubyGems source.
dredgerMaliciousExportBase :: Text
dredgerMaliciousExportBase = "advisories/rubygems-dredger-mal"

-- | Advisory bytes come from the independent OSV fixture tools, not production matching code.
buildSweepAdvisories :: FilePath -> IO ()
buildSweepAdvisories work = do
    for_ [CorpusV1, CorpusV2] $ \generation -> do
        let unrelated = ("unrelated.json", ecosystemAdvisory RubyGems "GHSA-e2e-unrelated" "ecluse-unrelated" "2.0.0")
            revoked = ("revoked.json", ecosystemAdvisory RubyGems dredgerAdvisoryId (packageName advisoryPackage) "2.0.0")
        osvZipOf (unrelated : [revoked | generation == CorpusV2]) >>= writeExport (dredgerExportBase generation)
    maliciousZip RubyGems [(packageName advisoryPackage, ["1.0.0"])] Nothing >>= writeExport dredgerMaliciousExportBase
  where
    writeExport base archive = do
        let directory = work </> "html" </> toString base </> "RubyGems"
        createDirectoryIfMissing True directory
        writeFileLBS (directory </> "all.zip") archive
