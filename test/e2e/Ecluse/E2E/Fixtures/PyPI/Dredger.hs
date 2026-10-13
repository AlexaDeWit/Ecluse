-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Native releases and advisory generations for the canonical Dredger laws.
module Ecluse.E2E.Fixtures.PyPI.Dredger (
    buildDredgerDistributions,
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
) where

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.E2E.Fixtures.Advisories (ecosystemAdvisory)
import Ecluse.E2E.Fixtures.PyPI (buildSdistFor, buildWheelFor, metadataFileFor, pypiWheelFor)
import Ecluse.E2E.Fixtures.PyPI.Mirror (Distribution (..), distributionIndex)
import Ecluse.E2E.Harness.Stub (StubRoute (PyPIPublic), stubUrl)
import Ecluse.Test.Contract.Sweep.Types (PackageFixture (..))
import Ecluse.Test.Osv (CorpusVersion (..), osvZipOf)

-- | Each native version holds both a real installable wheel and a source archive.
buildDredgerDistributions :: FilePath -> IO [Distribution]
buildDredgerDistributions work = do
    distributions <- fmap concat $ forM coordinates $ \(name, version) -> do
        let filename = pypiWheelFor name version
            sourceName = name <> "-" <> version <> ".tar.gz"
            metadata = metadataFileFor name version
            directory = work </> "pypi" </> "simple" </> toString name
            path = directory </> toString filename
            sourcePath = directory </> toString sourceName
        createDirectoryIfMissing True directory
        buildWheelFor name version metadata (work </> toString (".wheel-" <> filename)) path
        buildSdistFor name version metadata (work </> toString (".source-" <> sourceName)) sourcePath
        bytes <- BS.readFile path
        sourceBytes <- BS.readFile sourcePath
        BS.writeFile (path <> ".metadata") (encodeUtf8 metadata)
        pure [Distribution name version filename bytes (encodeUtf8 metadata) (encodeUtf8 metadata), Distribution name version sourceName sourceBytes (encodeUtf8 metadata) (encodeUtf8 metadata)]
    for_ (map packageName dredgerPackages <> [firstPartyName]) $ \name ->
        writeFileLBS
            (work </> "pypi" </> "simple" </> toString name </> "index.json")
            (Aeson.encode (distributionIndex (stubUrl PyPIPublic) name distributions))
    buildGenerations work
    pure distributions
  where
    coordinates =
        [(packageName package, version) | package <- dredgerPackages, version <- packageVersions package]
            <> [(packageName previewPackage, version) | version <- ["2.0.0", "3.0.0"]]
            <> [(firstPartyName, version) | version <- ["1.0.0", "2.0.0"]]

-- | The complete independent corpus accepted by the native upload fixture.
dredgerPackages :: [PackageFixture]
dredgerPackages = [deniedPackage, previewPackage, keptPackage, sentinelPackage, advisoryPackage] <> recoveryPackages

-- | A single release that an identity denial removes.
deniedPackage :: PackageFixture
deniedPackage = release "e2e-dredger-denied"

-- | Its extra versions distinguish mirror, shared and cache-only inventory.
previewPackage :: PackageFixture
previewPackage = release "e2e-dredger-preview"

-- | An unrelated release that every cleanup preserves.
keptPackage :: PackageFixture
keptPackage = release "e2e-dredger-kept"

-- | The queue sentinel proves that the worker drained a real admitted request.
sentinelPackage :: PackageFixture
sentinelPackage = release "e2e-dredger-sentinel"

-- | Separate projects preserve the canonical recovery cases' initial conditions.
recoveryPackages :: [PackageFixture]
recoveryPackages = [siblingPackage, refusedPackage, readmittedPackage, missingPackage, latePackage]

-- | The permitted sibling survives removal of the earlier version.
siblingPackage :: PackageFixture
siblingPackage = PackageFixture "e2e-dredger-sibling" "2.0.0" ["1.0.0", "2.0.0"]

-- | The cache refuses deletion until the next product sweep.
refusedPackage :: PackageFixture
refusedPackage = release "e2e-dredger-refused"

-- | A later permissive role can fetch this release again.
readmittedPackage :: PackageFixture
readmittedPackage = PackageFixture "e2e-dredger-readmitted" "1.0.post1" ["1.0.post1"]

-- | Withheld source files make recovery impossible after native deletion.
missingPackage :: PackageFixture
missingPackage = release "e2e-dredger-missing"

-- | A later native publish tests rediscovery after an earlier sweep.
latePackage :: PackageFixture
latePackage = release "e2e-dredger-late"

-- | Both versions predate the newly published advisory generation.
advisoryPackage :: PackageFixture
advisoryPackage = PackageFixture "e2e-dredger-advisory" "1.2.0" ["1.0.0", "1.2.0"]

-- | An exact PEP 503 name is the mount's first-party declaration.
firstPartyName :: Text
firstPartyName = "e2e-dredger-firstparty"

-- | The identifier the product must retain in its revocation audit.
dredgerAdvisoryId :: Text
dredgerAdvisoryId = "GHSA-e2e-pypi-dredger"

-- | Each generation uses an independent source export under the fixture's public route.
dredgerExportBase :: CorpusVersion -> Text
dredgerExportBase = \case
    CorpusV1 -> "advisories/pypi-dredger-v1"
    CorpusV2 -> "advisories/pypi-dredger-v2"

release :: Text -> PackageFixture
release name = PackageFixture name "1.0.0" ["1.0.0"]

buildGenerations :: FilePath -> IO ()
buildGenerations work = for_ [CorpusV1, CorpusV2] $ \generation -> do
    let directory = work </> "html" </> toString (dredgerExportBase generation) </> "PyPI"
        unrelated = ("unrelated.json", ecosystemAdvisory PyPI "GHSA-e2e-unrelated" "e2e-unrelated" "2.0.0")
        revoked = ("revoked.json", ecosystemAdvisory PyPI dredgerAdvisoryId (packageName advisoryPackage) "1.2.0")
    createDirectoryIfMissing True directory
    osvZipOf (unrelated : [revoked | generation == CorpusV2]) >>= writeFileLBS (directory </> "all.zip")
