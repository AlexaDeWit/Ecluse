-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The npm wire JSON types and their decoders: the decode-only protocol boundary.

Every decoder is lenient in the ways the public registry requires. Unknown keys are
ignored, @license@ and a person arrive as a string or an object, @deprecated@ as a string
or a boolean, and an undecodable advisory @unpackedSize@ reads as absent rather than
failing the version. The shapes come from live captures of @registry.npmjs.org@, kept as
fixtures under @core\/test\/unit\/fixtures\/npm\/@.
-}
module Ecluse.Core.Registry.Npm.Wire (
    -- * Shared scalars
    Person (..),
    License (..),

    -- * The @dist@ object
    Dist (..),

    -- * Per-version manifest
    VersionManifest (..),
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

{- | A person on a package: an author, maintainer, contributor, or a version's publisher.
A packed @"Name \<email\> (url)"@ string, npm's other form, stays verbatim in 'personName'.
-}
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

{- | The artifact descriptor every version manifest carries. A client fails the install
when the bytes mismatch, so a mirror or a URL rewrite preserves the digests byte for byte.
-}
data Dist = Dist
    { distTarball :: Text
    -- ^ Absolute URL of the @.tgz@ artifact. Always present.
    , distShasum :: Maybe Text
    -- ^ The tarball's SHA-1, hex-encoded (legacy integrity).
    , distIntegrity :: Maybe Text
    -- ^ The Subresource-Integrity string (@"sha512-..."@), preferred over the shasum.
    , distUnpackedSize :: Maybe Int
    {- ^ Unpacked size in bytes. Advisory, so a fractional, huge, or wrong-typed value
    reads as 'Nothing' instead of denying the whole packument.
    -}
    }
    deriving stock (Eq, Ord, Show)

instance FromJSON Dist where
    parseJSON = withObject "Dist" $ \o ->
        Dist
            <$> o .: "tarball"
            <*> o .:? "shasum"
            <*> o .:? "integrity"
            <*> lenientOptional o "unpackedSize"

{- | One version's manifest, decoding all three wire forms alike. The dependency maps and
the maintainer list stay unparsed: nothing reads them, and the cost falls on every version.
-}
data VersionManifest = VersionManifest
    { vmName :: Text
    -- ^ The package name as sent, possibly scoped (@"\@scope\/name"@).
    , vmVersion :: Text
    -- ^ The exact version string, kept opaque at this layer.
    , vmDist :: Dist
    , vmDeprecated :: Maybe Text
    {- ^ The deprecation message. A boolean @true@ reads as @""@, and @false@, @null@,
    absence, or any other shape as 'Nothing'.
    -}
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
            <*> (deprecatedNotice <$> o .:? "deprecated")
            <*> o .:? "hasInstallScript"
            <*> o .:? "scripts" .!= mempty
            <*> o .:? "license"

deprecatedNotice :: Maybe Value -> Maybe Text
deprecatedNotice = \case
    Just (String message) -> Just message
    Just (Bool True) -> Just ""
    _ -> Nothing
