-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | npm wire types shared by tree decoding and typed extraction during packing.
module Ecluse.Core.Registry.Npm.Wire (
    -- * Shared scalars
    Person (..),
    License (..),

    -- * The @dist@ object
    Dist (..),
    distFields,

    -- * Per-version manifest
    VersionManifest (..),
    availability,
) where

import Data.Aeson (
    FromJSON (parseJSON),
    Value (Bool, Object, String),
    withObject,
    (.!=),
    (.:),
    (.:?),
 )

import Ecluse.Core.Json.Lenient (lenientOptional, typeMismatchOneOf)
import Ecluse.Core.Package (Availability (Available, Deprecated))

-- | A publisher or contributor. Packed person strings stay verbatim in 'personName'.
data Person = Person
    { personName :: Text
    -- ^ The name, or the whole packed string as sent.
    , personEmail :: Maybe Text
    , personUrl :: Maybe Text
    }
    deriving stock (Eq, Ord, Show)

instance FromJSON Person where
    parseJSON = \case
        String name -> pure (Person name Nothing Nothing)
        Object o ->
            Person
                <$> o .:? "name" .!= ""
                <*> o .:? "email"
                <*> o .:? "url"
        other -> typeMismatchOneOf "Person (object or string)" other

-- | A declared license, in npm's modern string form or its legacy object form.
data License
    = -- | An SPDX expression or identifier (@"MIT"@, @"(MIT OR Apache-2.0)"@).
      LicenseSpdx Text
    | -- | The legacy @{type, url?}@ object: a license name and a URL to its text.
      LicenseObject Text (Maybe Text)
    deriving stock (Eq, Ord, Show)

instance FromJSON License where
    parseJSON = \case
        String spdx -> pure (LicenseSpdx spdx)
        Object o ->
            LicenseObject
                <$> o .:? "type" .!= ""
                <*> o .:? "url"
        other -> typeMismatchOneOf "License (object or string)" other

-- | Artifact coordinates and digests that serving and mirroring preserve.
data Dist = Dist
    { distTarball :: Text
    -- ^ Absolute URL of the @.tgz@ artifact. Always present.
    , distShasum :: Maybe Text
    -- ^ The tarball's SHA-1, hex-encoded (legacy integrity).
    , distIntegrity :: Maybe Text
    -- ^ The Subresource-Integrity string (@"sha512-..."@), preferred over the shasum.
    , distUnpackedSize :: Maybe Int
    -- ^ Advisory bytes. Fractional, huge or wrong-typed values read as 'Nothing'.
    }
    deriving stock (Eq, Ord, Show)

-- | The @dist@ members the 'Dist' parser reads.
distFields :: [Text]
distFields = ["tarball", "shasum", "integrity", "unpackedSize"]

instance FromJSON Dist where
    parseJSON = withObject "Dist" $ \o ->
        Dist
            <$> o .: "tarball"
            <*> o .:? "shasum"
            <*> o .:? "integrity"
            <*> lenientOptional o "unpackedSize"

-- | A release's policy inputs. Dependency maps and maintainers stay outside the typed facts.
data VersionManifest = VersionManifest
    { vmName :: Text
    -- ^ The package name as sent, possibly scoped (@"\@scope\/name"@).
    , vmVersion :: Text
    -- ^ The exact version string, kept opaque at this layer.
    , vmDist :: Dist
    , vmAvailability :: Availability
    -- ^ 'Deprecated' when @deprecated@ is a nonempty string or @true@, else 'Available'.
    , vmHasInstallScript :: Maybe Bool
    -- ^ Abbreviated form only. 'Nothing' in the full form, where 'vmScripts' carries it.
    , vmScripts :: Map Text Text
    -- ^ The @scripts@ map, lifecycle name to command, empty when absent.
    , vmLicense :: Maybe License
    }
    deriving stock (Eq, Show)

instance FromJSON VersionManifest where
    parseJSON = withObject "VersionManifest" $ \o ->
        VersionManifest
            <$> o .: "name"
            <*> o .: "version"
            <*> o .: "dist"
            <*> (availability <$> o .:? "deprecated")
            <*> o .:? "hasInstallScript"
            <*> o .:? "scripts" .!= mempty
            <*> o .:? "license"

-- | Only a nonempty string or @true@ declares deprecation. Other values remain available.
availability :: Maybe Value -> Availability
availability = \case
    Just (String "") -> Available
    Just (String _) -> Deprecated
    Just (Bool True) -> Deprecated
    _ -> Available
