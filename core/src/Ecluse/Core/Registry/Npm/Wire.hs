-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The npm registry __wire__ JSON types and their lenient decoders.

This module is the npm protocol __boundary__. It models the JSON the registry
actually sends and parses it with deliberately forgiving 'FromJSON' instances. It is
the raw-wire layer of "parse, don't validate". It captures /what the registry said/ as
faithfully as the rules and serving need, and __nothing more__. Projecting these wire
types into the ecosystem-agnostic domain model ("Ecluse.Core.Package":
@PackageDetails@ et al.) is a separate concern. Keeping the two apart is what keeps
the lenient\/faithful handle clean.

The shapes here are reverse-engineered from live captures of @registry.npmjs.org@. The
fixtures under @core\/test\/unit\/fixtures\/npm\/@ are those captures.

== Lenient on input

The public registry drifted from its own spec and is inconsistent across endpoints.
Every decoder here is forgiving in four specific ways, matching the documented reality:

* __Unknown keys__. Manifests carry arbitrary author keys (@gitHead@, @exports@,
  tool-config blocks like @is-odd@'s @verb@) and registry bookkeeping
  (@_npmOperationalInternal@). A decoder must not choke on them. The @aeson@ record
  decoders already ignore extra keys, so this falls out of using @(.:?)@\/@(.:)@ rather
  than enumerating the whole object.
* __String-or-object scalars__. Some slots arrive as /either/ a bare string /or/ an
  object, depending on the package's age and tooling. They are @license@ and the
  @author@\/maintainer person fields. Each corresponding type ('License', 'Person')
  therefore parses both shapes.
* __The string-or-boolean @deprecated@ flag__. The @deprecated@ key is conventionally
  the deprecation message string. Some published versions carry a boolean instead:
  @true@ = deprecated without a message, @false@ = not deprecated. 'vmDeprecated'
  reads every form, so a boolean never fails the whole packument decode. A real
  packument such as react's mixes the string and boolean forms across versions.
* __The advisory @unpackedSize@ degrades rather than denies__. The @unpackedSize@ field
  decides no rule and no serve. A hostile value (a fractional\/huge\/@Int@-overflowing
  number, or a wrong-typed field) reads as absent rather than failing the version. One
  poisoned value therefore cannot deny the whole packument ('Dist').

== Faithful on the rule-decisive fields

This module captures the fields the rules engine and the serving path need precisely.
They are the abbreviated-only 'vmHasInstallScript' flag, the 'vmDeprecated' notice, the
whole 'vmScripts' map, and the 'Dist' integrity triple
(@tarball@\/@shasum@\/@integrity@). It captures the @scripts@ map whole so the full
form's install-script presence can be /derived/, because the full manifest has no
@hasInstallScript@ key.

This module models only the decode path (@FromJSON@).
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

{- | A person associated with a package: an author, maintainer, contributor, or the
per-version publisher (@_npmUser@). Distinct from "Ecluse.Core.Package"'s domain @Person@.

__Lenient:__ npm sends either an object @{name, email?, url?}@ or a packed string of the
form @"Name \<email\> (url)"@. The decoder keeps a packed string __verbatim__ in
'personName' and leaves 'personEmail'\/'personUrl' 'Nothing'.
-}
data Person = Person
    { personName :: Text
    -- ^ The person's name, or the entire packed string as sent.
    , personEmail :: Maybe Text
    -- ^ Their email address, if given as an object field.
    , personUrl :: Maybe Text
    -- ^ A homepage \/ profile URL, if given as an object field.
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

{- | A declared license.

__Lenient:__ modern packages send a bare SPDX __string__ ('LicenseSpdx'), legacy packages
the object @{type, url?}@ ('LicenseObject'). The sum keeps the two distinguishable.
-}
data License
    = {- | An SPDX expression or identifier, sent as a bare string (@"MIT"@,
      @"Apache-2.0"@, @"(MIT OR Apache-2.0)"@). The modern form.
      -}
      LicenseSpdx Text
    | {- | The legacy object form @{type, url?}@: a license name plus an optional
      URL to the license text.
      -}
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

{- | The @dist@ object: the artifact descriptor carried by every version manifest, full and
abbreviated.

The integrity triple ('distTarball', 'distShasum', 'distIntegrity') is rule-decisive and
serving-decisive. A client __fails the install__ when the downloaded bytes do not match
@integrity@\/@shasum@, so any mirror or URL rewrite must preserve these byte for byte.

'distUnpackedSize' is __advisory__ and reads __leniently__. An undecodable number
(fractional, huge, or 'Int'-overflowing) reads as 'Nothing', so a hostile value in one
version degrades that field alone instead of denying the whole packument.
-}
data Dist = Dist
    { distTarball :: Text
    -- ^ Absolute URL of the @.tgz@ artifact. Always present.
    , distShasum :: Maybe Text
    -- ^ The tarball's SHA-1, hex-encoded (legacy integrity).
    , distIntegrity :: Maybe Text
    {- ^ The Subresource-Integrity string (@"\<alg\>-\<base64\>"@, e.g.
    @"sha512-…"@). The modern integrity check, preferred over the shasum.
    -}
    , distUnpackedSize :: Maybe Int
    -- ^ Unpacked size in bytes, if reported.
    }
    deriving stock (Eq, Ord, Show)

instance FromJSON Dist where
    parseJSON = withObject "Dist" $ \o ->
        Dist
            <$> o .: "tarball"
            <*> o .:? "shasum"
            <*> o .:? "integrity"
            <*> lenientOptional o "unpackedSize"

{- | A single version's manifest: the package's @package.json@ at publish time plus
registry-injected fields. One type decodes all three wire forms, a full packument's
@versions[v]@ entry, an abbreviated packument's trimmed subset, and the standalone
@GET \/{pkg}\/{version}@ body.

This type models only the fields Écluse's rules and serving need (see the module header).
The publish timestamp is __not__ one of them. It lives in the packument's @time@ map.
-}
data VersionManifest = VersionManifest
    { vmName :: Text
    -- ^ The package name, possibly scoped (@"\@scope\/name"@), verbatim.
    , vmVersion :: Text
    -- ^ The exact version string (e.g. @"1.2.3"@), kept opaque at this layer.
    , vmDist :: Dist
    -- ^ The artifact descriptor (always present).
    , vmDeprecated :: Maybe Text
    {- ^ The deprecation message, or 'Nothing' when the version is not deprecated. npm sends
    @deprecated@ as the message string or as a boolean. @true@ is deprecated with no message
    (captured as @""@), and @false@, @null@, absence, or any other shape reads as 'Nothing'.
    -}
    , vmHasInstallScript :: Maybe Bool
    {- ^ Whether the version declares install scripts. Present in the
    __abbreviated__ form only, and 'Nothing' in the full form (derive from
    'vmScripts' there).
    -}
    , vmScripts :: Map Text Text
    {- ^ The @scripts@ map (lifecycle name to command), empty when absent. The
    source for deriving install-script presence from the full form.
    -}
    , vmLicense :: Maybe License
    {- ^ The declared license, if any (a string or the legacy object, see 'License').

    The manifest's dependency maps and maintainer list are __deliberately not parsed__. No rule
    or serve path consults them, the raw document relays them to the client untouched, and a
    heavy packument would pay that parse cost on thousands of per-version entries.
    -}
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
