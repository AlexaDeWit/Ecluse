-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | An owned archive and paired index snapshot shared by native publication and recovery tests.
module Ecluse.Test.Registry.Cargo.Publish (
    cargoPublicationFixture,
    cargoSourceVersion,
    observeCargoRelay,
) where

import Codec.Compression.GZip qualified as GZip
import Data.Aeson (Value (..))
import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types.Status (status200)
import Test.Hspec (shouldBe)

import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.Adapter.Capability (AdapterPublish (publishRelay), PublishRequestBody (PublishRequestBody))
import Ecluse.Core.Registry.CachedDocument (cargoCached)
import Ecluse.Core.Registry.Cargo.Document (cargoDocument)
import Ecluse.Core.Registry.Cargo.Project (projectName)
import Ecluse.Core.Registry.Cargo.Projection (projectCargoRecord)
import Ecluse.Core.Registry.Cargo.Publish (cargoPublish)
import Ecluse.Core.Registry.Metadata (VersionDoc (..))
import Ecluse.Core.Registry.Origin (originClient)
import Ecluse.Core.Registry.ServedDocument (stringField)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress (RegistryUrl)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (ChargeFactors))
import Ecluse.Test.Deadline (withExchangeClient)
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Registry.Cargo.Fixtures (CargoRelease (..), minimalCargoRelease)
import Ecluse.Test.Registry.Cargo.Metadata (cargoTestDiscovery, cargoTestName)
import Ecluse.Test.Registry.Cargo.Reference (UploadFrame, referenceUpload)
import Ecluse.Test.Registry.PyPI.Archive (tarBytes)
import Ecluse.Test.Stub (Captured (capBody, capMethod), allCaptured, stubBaseUrl, withRoutedStub)
import Ecluse.Test.Support (expectRight)

-- | Observe production relay bytes at the destination before decoding their native metadata independently.
observeCargoRelay :: (Text -> RegistryUrl) -> ByteString -> IO UploadFrame
observeCargoRelay registryAt bytes =
    withRoutedStub (const (status200, [], "{}")) $ \target ->
        withExchangeClient (newManager defaultManagerSettings) $ \client -> do
            let origin = originClient defaultLimits client (registryAt (stubBaseUrl target)) Nothing
            void (publishRelay (cargoPublish (ChargeFactors 1000 1000 1000)) origin cargoTestName (PublishRequestBody bytes Nothing) >>= expectRight)
            captured <-
                allCaptured target >>= \case
                    [written] -> pure written
                    _ -> fail "Cargo relay did not issue exactly one request"
            capMethod captured `shouldBe` "PUT"
            capBody captured `shouldBe` bytes
            expectRight (referenceUpload (capBody captured))

-- | The README precedes Cargo.toml so publication must perform its bounded selected-text pass.
cargoPublicationFixture :: IO CargoRelease
cargoPublicationFixture = do
    let manifest = "[package]\nname='fixture'\nversion='1.0.0'\nauthors=['Ecluse contributors']\ndescription='Owned Cargo registry contract fixture'\nlicense='MIT'\nrust-version='1.74'\n"
    bytes <- LBS.toStrict . GZip.compress <$> tarBytes [("fixture-1.0.0/README.md", "owned readme"), ("fixture-1.0.0/Cargo.toml", manifest), ("fixture-1.0.0/src/lib.rs", "pub fn owned() {}\n")]
    pure (minimalCargoRelease "fixture" "1.0.0"){releaseArchive = bytes, releaseChecksum = hexSha256Of bytes, releaseReadme = "owned readme"}

-- | Project the supplied native row with a nonzero original source coordinate.
cargoSourceVersion :: Value -> IO VersionDoc
cargoSourceVersion row@(Object fields) = do
    name <- maybe (fail "fixture row lacks its name") (expectRight . projectName) (stringField "name" fields)
    discovery <- expectRight cargoTestDiscovery
    details <- expectRight (projectCargoRecord name (RecordEntry 7) discovery row)
    pure (VersionDoc details (Just (fst cargoCached (cargoDocument [(RecordEntry 7, row)]))))
cargoSourceVersion _ = fail "fixture row is not an object"
