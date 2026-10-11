-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Live PyPI protocol drift checks through the proxy's adapter and bounded transport.
module Ecluse.Core.Registry.PyPISmokeSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Package (Artifact (..), Hash (hashAlg), HashAlg (SHA256), PackageDetails (pkgArtifacts, pkgName, pkgVersion), PackageInfo (infoName, infoVersions), mkPackageName, renderPackageName)
import Ecluse.Core.Registry (FetchFault (FetchTransport), RegistryResponse (responseBody, responseStatusCode))
import Ecluse.Core.Registry.Adapter.Capability (AdapterArtifact (artifactByUrl), AdapterMetadata (metadataRead))
import Ecluse.Core.Registry.Adapter.Types (RegistryAdapter (adapterArtifact, adapterMetadata))
import Ecluse.Core.Registry.Exchange (boundedFetch)
import Ecluse.Core.Registry.Metadata (Manifest (manifestInfo), MetadataError (MetadataFetch))
import Ecluse.Core.Registry.Metadata.Fetch (fetchManifest)
import Ecluse.Core.Registry.Origin (originClient)
import Ecluse.Core.Registry.PyPI.Adapter (pypiAdapter)
import Ecluse.Core.Security (BodyLimit (MirrorArtifactBodyLimit), Limits (progressFloor), defaultLimits)
import Ecluse.Core.Security.Egress (mkRegistryUrl)
import Ecluse.Core.Version (renderVersion)
import Ecluse.Core.Worker.Integrity (IntegrityResult (IntegrityVerified), verifyIntegrity)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Support (expectRight, expectRightText)

-- | Only transport unavailability skips a live document or integrity check.
spec :: Spec
spec = describe "live PyPI registry protocol" $
    it "reads a bounded Simple index and verifies one advertised wheel" $ do
        manager <- newManager tlsManagerSettings
        base <- expectRightText (mkRegistryUrl "https://pypi.org")
        let name = mkPackageName PyPI Nothing "sampleproject"
            origin = originClient defaultLimits manager base Nothing
        fetched <- fetchManifest (metadataRead (adapterMetadata pypiAdapter)) passthroughTracingPort origin name
        case fetched of
            Left (MetadataFetch (FetchTransport fault)) ->
                pendingWith ("PyPI registry unreachable: " <> show fault)
            _ -> do
                manifest <- expectRight fetched
                let info = manifestInfo manifest
                renderPackageName (infoName info) `shouldBe` "sampleproject"
                version <- maybe (fail "live index omitted sampleproject 4.0.0") pure (Map.lookup "4.0.0" (infoVersions info))
                pkgName version `shouldBe` name
                renderVersion (pkgVersion version) `shouldBe` "4.0.0"
                artifact <-
                    maybe (fail "live index omitted the sampleproject 4.0.0 wheel") pure $
                        find ((== wheelFilename) . artFilename) (NE.toList (pkgArtifacts version))
                downloadWheel manager artifact

wheelFilename :: Text
wheelFilename = "sampleproject-4.0.0-py3-none-any.whl"

-- PyPI's sample project wheel is 4661 bytes. Keep drift bounded without fetching a whole release.
wheelByteLimit :: Int
wheelByteLimit = 65_536

downloadWheel :: Manager -> Artifact -> IO ()
downloadWheel manager artifact = do
    hashes <- maybe (fail "live wheel omitted integrity hashes") pure (nonEmpty (artHashes artifact))
    NE.toList hashes `shouldSatisfy` any ((== SHA256) . hashAlg)
    request <- expectRight (artifactByUrl (adapterArtifact pypiAdapter) Nothing (artUrl artifact))
    fetched <- boundedFetch manager (progressFloor defaultLimits) (MirrorArtifactBodyLimit wheelByteLimit) request
    case fetched of
        Left (FetchTransport fault) -> pendingWith ("PyPI files host unreachable: " <> show fault)
        _ -> do
            response <- expectRight fetched
            responseStatusCode response `shouldBe` 200
            responseBody response `shouldNotBe` ""
            verifyIntegrity hashes (responseBody response) `shouldBe` IntegrityVerified
