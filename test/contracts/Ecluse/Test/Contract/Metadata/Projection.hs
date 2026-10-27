-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Full and selected projection laws distinguish document rejection from native record quarantine.
module Ecluse.Test.Contract.Metadata.Projection (
    MetadataFraming (..),
    ProjectionDriver (..),
    metadataProjectionContract,
    projectionFailureLaw,
) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Package (PackageDetails, PackageInfo (infoInvalidEntries, infoVersions))
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataBoundExceeded, MetadataNameMismatch, MetadataUndecodable))
import Ecluse.Core.Security (LimitError (TooManyArtifacts, TooManyVersions), Limits (maxArtifactCount, maxVersionCount), defaultLimits)
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Support (expectRight)

-- | Native framing fixes whether one invalid value rejects the document or only that record.
data MetadataFraming
    = -- | A document owns one envelope and one reported identity.
      SingleDocument
    | -- | Every record owns its identity. Invalid records must leave explicit evidence.
      RecordStream
    deriving stock (Eq, Show)

-- | Projection operations and native fixtures. Each valid release contains one artifact.
data ProjectionDriver = ProjectionDriver
    { projectFull :: Limits -> ByteString -> IO (Either MetadataError PackageInfo)
    , projectSelected :: Limits -> Text -> ByteString -> IO (Either MetadataError (Maybe PackageDetails))
    , projectionFraming :: MetadataFraming
    , projectionDocument :: Text -> [Text] -> ByteString
    , projectionPackageName :: Text
    , projectionOtherName :: Text
    , projectionVersions :: [Text]
    , projectionMissingName :: ByteString
    }

-- | Compare native refusal evidence and identical count bounds through full and selected reads.
metadataProjectionContract :: Text -> ProjectionDriver -> Spec
metadataProjectionContract provider driver = describe "shared metadata projection" $ do
    contractExample "malformed" "rejects malformed input on full and selected reads" $
        projectionFailureLaw driver MetadataUndecodable "{not json"
    contractExample "missing-name" "rejects a missing identity on full and selected reads" $
        projectionFailureLaw driver MetadataUndecodable (projectionMissingName driver)
    contractExample "name-mismatch" "rejects another package's identity on full and selected reads" $
        projectionFailureLaw driver (MetadataNameMismatch (projectionOtherName driver)) (projectionDocument driver (projectionOtherName driver) versions)
    when (projectionFraming driver == RecordStream) $
        contractExample "record-siblings" "retains healthy records beside malformed and foreign siblings" $
            for_ ["{broken}", projectionMissingName driver, projectionDocument driver (projectionOtherName driver) ["4.0.0"]] $ \invalid -> do
                let mixed = invalid <> "\n" <> document
                info <- expectRight =<< full defaultLimits mixed
                Map.keys (infoVersions info) `shouldBe` sort versions
                infoInvalidEntries info `shouldSatisfy` not . null
                for_ versions $ \version ->
                    selected defaultLimits version mixed `shouldReturn` Right (Map.lookup version (infoVersions info))
    contractExample "version-bound" "rejects two entries above a one-entry version ceiling" $ do
        let limits = defaultLimits{maxVersionCount = 1}
            expected = Left (MetadataBoundExceeded (TooManyVersions 2 1))
        length versions `shouldBe` 2
        fmap void (full limits document) `shouldReturn` expected
        for_ versions $ \version ->
            fmap void (selected limits version document) `shouldReturn` expected
    contractExample "artifact-bound" "bounds full-read artifacts and accepts the exact ceiling" $ do
        fmap void (full defaultLimits{maxArtifactCount = 1} document) `shouldReturn` Left (MetadataBoundExceeded (TooManyArtifacts 2 1))
        full defaultLimits{maxArtifactCount = 2} document >>= (`shouldSatisfy` isRight)
    contractExample "missing-version" "returns no details for a missing version" $
        selected defaultLimits "9.9.9" document `shouldReturn` Right Nothing
    contractExample "selected-parity" "projects every selected version identically to the full document" $ do
        info <- expectRight =<< full defaultLimits document
        Map.keys (infoVersions info) `shouldBe` sort versions
        for_ versions $ \version -> do
            Map.lookup version (infoVersions info) `shouldSatisfy` isJust
            selected defaultLimits version document `shouldReturn` Right (Map.lookup version (infoVersions info))
  where
    contractExample identifier description = it (caseLabel ("core.metadata.projection." <> provider <> "." <> identifier) description)
    full = projectFull driver
    selected = projectSelected driver
    versions = projectionVersions driver
    document = projectionDocument driver (projectionPackageName driver) versions

-- | Invalid native records admit nothing and leave evidence. Single documents retain their exact failure.
projectionFailureLaw :: ProjectionDriver -> MetadataError -> ByteString -> Expectation
projectionFailureLaw driver failure bytes = do
    full <- projectFull driver defaultLimits bytes
    case projectionFraming driver of
        SingleDocument -> void full `shouldBe` Left failure
        RecordStream -> do
            info <- expectRight full
            infoVersions info `shouldBe` Map.empty
            infoInvalidEntries info `shouldSatisfy` not . null
    for_ (projectionVersions driver) $ \version -> do
        selected <- projectSelected driver defaultLimits version bytes
        case projectionFraming driver of
            SingleDocument -> void selected `shouldBe` Left failure
            RecordStream -> selected `shouldBe` Right Nothing
