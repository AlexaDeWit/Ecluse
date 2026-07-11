-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The npm registry __wire__ JSON types and their lenient decoders.

This module is the npm protocol __boundary__: it models the JSON the registry
actually sends and parses it with deliberately forgiving 'FromJSON' instances.
It is the raw-wire layer of "parse, don't validate" -- it captures /what the
registry said/ as faithfully as the rules and serving need, and __nothing
more__. Projecting these wire types into the ecosystem-agnostic domain model
("Ecluse.Core.Package": @PackageDetails@ et al.) is a separate concern; keeping the
two apart is what keeps the lenient\/faithful handle clean.

The shapes here are reverse-engineered from live captures of
@registry.npmjs.org@; the authoritative reference (with real bodies) is
@docs\/research\/reverse-engineering\/npm.md@ (§4 full packument, §5 abbreviated,
§7 @dist@, §11 type model, §3 errors).

== Lenient on input

The public registry has drifted from its own spec and is inconsistent across
endpoints, so every decoder here is forgiving in five specific ways, matching
the documented reality:

* __Unknown keys are ignored.__ Manifests carry arbitrary author keys
  (@gitHead@, @exports@, tool-config blocks like @is-odd@'s @verb@) and
  registry bookkeeping (@_npmOperationalInternal@); a decoder must not choke on
  them. aeson's record decoders already ignore extra keys, so this falls out of
  using @(.:?)@\/@(.:)@ rather than enumerating the whole object.
* __String-or-object scalars.__ @license@, @bugs@, @repository@, and the
  @author@\/maintainer person fields each arrive as /either/ a bare string /or/
  an object, depending on the package's age and tooling. Each corresponding
  type ('License', 'Bugs', 'Repository', 'Person') therefore parses both shapes.
* __The bare-string error body.__ npm's per-version 404 is a bare JSON
  __string__ (@"version not found: ^3.0.0"@), not the documented
  @{error|message}@ object. 'ErrorResponse' tolerates both.
* __The string-or-boolean @deprecated@ flag.__ @deprecated@ is conventionally the
  deprecation message string, but some published versions carry a boolean instead
  (@true@ = deprecated without a message, @false@ = not deprecated). 'vmDeprecated'
  reads every form, so a boolean never fails the whole packument decode (a real
  packument such as react's mixes the string and boolean forms across versions).
* __Advisory @dist@ sub-fields degrade rather than deny.__ @fileCount@,
  @unpackedSize@, and @signatures@ are advisory -- they decide no rule and no
  serve -- so a hostile value in one (a fractional\/huge\/@Int@-overflowing number,
  a wrong-typed field, or a malformed\/non-array @signatures@) reads as
  absent\/empty rather than failing the version. One poisoned value therefore
  cannot deny the whole packument ('Dist').

== Faithful on the rule-decisive fields

The fields the rules engine and the serving path actually need are captured
precisely: the abbreviated-only 'vmHasInstallScript' flag, the 'vmDeprecated'
notice, the whole 'vmScripts' map (so the full form's install-script presence
can be /derived/ -- the full manifest has no @hasInstallScript@ key), the
'Dist' integrity triple (@tarball@\/@shasum@\/@integrity@), and the full
packument's 'pkmtTime' map (the source of truth for publish age, which the
abbreviated form drops).

Only the decode path (@FromJSON@) is modelled here.
-}
module Ecluse.Core.Registry.Npm.Wire (
    -- * Shared scalars
    Person (..),
    Repository (..),
    Bugs (..),
    License (..),

    -- * The @dist@ object
    Dist (..),
    Signature (..),

    -- * Per-version manifest
    VersionManifest (..),

    -- * Packuments
    Packument (..),
    AbbreviatedPackument (..),

    -- * Errors
    ErrorResponse (..),
    ErrorBody (..),
    errorMessage,
) where

import Data.Aeson (
    FromJSON (parseJSON),
    Object,
    Value (Array, Bool, Null, Number, Object, String),
    withObject,
    (.!=),
    (.:),
    (.:?),
 )
import Data.Aeson.Key (Key)
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime)

{- | A person associated with a package -- an author, maintainer, contributor, or
the per-version publisher (@_npmUser@).

__Lenient:__ npm sends a person as /either/ an object @{name, email?, url?}@ /or/
a single packed string of the conventional form
@"Name \<email\> (url)"@. The packed form is captured __verbatim__ in
'personName' (with 'personEmail'\/'personUrl' left 'Nothing'); this wire layer
does not attempt to split it, leaving that to the domain projection if it is ever
needed. Distinct from "Ecluse.Core.Package"'s domain @Person@ -- this is the raw wire
shape.
-}
data Person = Person
    { personName :: Text
    {- ^ The person's name. For the packed-string form, the entire string as
    sent (e.g. @"Mikeal Rogers \<mikeal\@example.com\>"@).
    -}
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

{- | An SCM location for a package.

__Lenient:__ npm sends @repository@ as /either/ an object @{type?, url}@ /or/ a
bare string (a shorthand URL such as @"github:user\/repo"@). Both are captured;
the bare-string form fills 'repoUrl' and leaves 'repoType' 'Nothing'.
-}
data Repository = Repository
    { repoType :: Maybe Text
    -- ^ The SCM type (e.g. @"git"@), if given.
    , repoUrl :: Text
    -- ^ The repository URL, as sent.
    }
    deriving stock (Eq, Ord, Show)

instance FromJSON Repository where
    parseJSON = \case
        String url -> pure (Repository Nothing url)
        Object o ->
            Repository
                <$> o .:? "type"
                <*> o .:? "url" .!= ""
        other -> typeMismatchOneOf "Repository (object or string)" other

{- | The issue tracker for a package.

__Lenient:__ npm sends @bugs@ as /either/ an object @{url?, email?}@ /or/ a bare
string (just the tracker URL). The bare-string form fills 'bugsUrl'.
-}
data Bugs = Bugs
    { bugsUrl :: Maybe Text
    -- ^ The issue-tracker URL, if given.
    , bugsEmail :: Maybe Text
    -- ^ A contact email, if given as an object field.
    }
    deriving stock (Eq, Ord, Show)

instance FromJSON Bugs where
    parseJSON = \case
        String url -> pure (Bugs (Just url) Nothing)
        Object o ->
            Bugs
                <$> o .:? "url"
                <*> o .:? "email"
        other -> typeMismatchOneOf "Bugs (object or string)" other

{- | A declared license.

__Lenient:__ modern packages send a bare SPDX __string__ (@"MIT"@); legacy
packages send an object @{type, url?}@. Both are preserved as a sum so the
distinction is not lost: 'LicenseSpdx' for the string, 'LicenseObject' for the
legacy object.
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

{- | One registry signature over a published artifact: an ECDSA signature and
the id of the key that produced it. Verifiable against npm's published public
keys (@GET \/-\/npm\/v1\/keys@) -- the basis of @npm audit signatures@.
-}
data Signature = Signature
    { sigSig :: Text
    -- ^ The base64-encoded signature value.
    , sigKeyid :: Text
    -- ^ The id of the signing key (e.g. @"SHA256:jl3bwswu80…"@).
    }
    deriving stock (Eq, Ord, Show)

instance FromJSON Signature where
    parseJSON = withObject "Signature" $ \o ->
        Signature
            <$> o .: "sig"
            <*> o .: "keyid"

{- | The @dist@ object: the artifact descriptor carried by every version
manifest (full and abbreviated). It is the gateway to the tarball bytes and the
integrity guarantee.

The integrity triple ('distTarball', 'distShasum', 'distIntegrity') is
rule-decisive and serving-decisive -- a client __fails the install__ if the
downloaded bytes do not match @integrity@\/@shasum@, so any mirror or URL rewrite
must preserve these byte-for-byte. Prefer 'distIntegrity' (SRI) over the legacy
SHA-1 'distShasum'.

The remaining sub-fields ('distFileCount', 'distUnpackedSize',
'distSignatures') are __advisory__ -- they inform reporting but decide no rule and
no serve -- and so are decoded __leniently__: a present-but-undecodable number
(fractional, huge, or 'Int'-overflowing) reads as absent ('Nothing'), a malformed
signature element is skipped rather than failing the array, and a
@signatures@ value that is not even an array reads as empty. A hostile value in
one version therefore degrades that field alone, never denying the whole
packument.
-}
data Dist = Dist
    { distTarball :: Text
    -- ^ Absolute URL of the @.tgz@ artifact. Always present.
    , distShasum :: Maybe Text
    -- ^ The tarball's SHA-1, hex-encoded (legacy integrity).
    , distIntegrity :: Maybe Text
    {- ^ The Subresource-Integrity string (@"\<alg\>-\<base64\>"@, e.g.
    @"sha512-…"@). The modern integrity check; prefer it over the shasum.
    -}
    , distFileCount :: Maybe Int
    -- ^ Number of files in the tarball, if reported.
    , distUnpackedSize :: Maybe Int
    -- ^ Unpacked size in bytes, if reported.
    , distSignatures :: [Signature]
    -- ^ Registry ECDSA signatures; empty when none are present.
    }
    deriving stock (Eq, Ord, Show)

instance FromJSON Dist where
    parseJSON = withObject "Dist" $ \o ->
        Dist
            <$> o .: "tarball"
            <*> o .:? "shasum"
            <*> o .:? "integrity"
            <*> lenientOptional o "fileCount"
            <*> lenientOptional o "unpackedSize"
            <*> lenientSignatures o

{- Decode an optional field __leniently__: an absent, @null@, or
present-but-undecodable value all yield 'Nothing'. Where @(.:?)@ fails the whole
decode on a present-but-wrong value, this degrades a hostile value (wrong-typed,
fractional, or outside the target's range) to 'Nothing' instead. Reserved for the
advisory @dist@ sub-fields, so one poisoned value cannot deny the whole packument;
the load-bearing integrity fields keep @(.:?)@\/@(.:)@. -}
lenientOptional :: (FromJSON a) => Object -> Key -> Parser (Maybe a)
lenientOptional o k = do
    mv <- o .:? k -- Parser (Maybe Value): a present junk value still arrives here
    pure (mv >>= parseMaybe parseJSON) -- but a Value that will not decode becomes Nothing

{- Decode the advisory @signatures@ array __leniently__: skip any element that
does not parse as a 'Signature' rather than failing the array, and treat a
present-but-non-array value (or absence\/@null@) as no signatures. The 'Signature'
instance itself stays strict; only its aggregation here tolerates a malformed
entry, so one bad signature cannot deny the version. -}
lenientSignatures :: Object -> Parser [Signature]
lenientSignatures o = do
    mv <- o .:? "signatures"
    pure $ case mv of
        Just (Array xs) -> mapMaybe (parseMaybe parseJSON) (toList xs)
        _ -> []

{- | A single version's manifest -- the per-version object that is essentially
the package's @package.json@ at publish time plus registry-injected fields. It
appears three ways on the wire and this one type decodes all of them: embedded in
a full 'Packument' (@versions[v]@), embedded in an 'AbbreviatedPackument' (a
trimmed subset of the same shape), and standalone (@GET \/{pkg}\/{version}@).

Only the fields Écluse's rules and serving need are modelled; everything else is
ignored (see the module header). The two rule-decisive optionals deserve note:

* 'vmHasInstallScript' is __abbreviated-only__ -- the registry sets it when the
  version declares @preinstall@\/@install@\/@postinstall@ scripts. It is the
  cleanest install-script signal, but it is __absent from the full manifest__.
* 'vmScripts' is therefore captured whole so that, when only the full form is
  available, install-script presence can be /derived/
  (@scripts@ has any of @preinstall@\/@install@\/@postinstall@). That derivation
  is a domain-projection concern, not this layer's.

The publish timestamp is __not__ here -- it lives in the packument's
'pkmtTime' map, not the manifest (see §8 of the protocol reference).
-}
data VersionManifest = VersionManifest
    { vmName :: Text
    -- ^ The package name, possibly scoped (@"\@scope\/name"@), verbatim.
    , vmVersion :: Text
    -- ^ The exact version string (e.g. @"1.2.3"@), kept opaque at this layer.
    , vmDist :: Dist
    -- ^ The artifact descriptor (always present).
    , vmDeprecated :: Maybe Text
    {- ^ The deprecation message when the version is deprecated, else 'Nothing'.
    npm sends @deprecated@ as the message string, or as a boolean (@true@ =
    deprecated with no message, captured as @""@; @false@ = not deprecated); an
    absent, @null@, @false@, or otherwise-shaped value reads as 'Nothing'.
    -}
    , vmHasInstallScript :: Maybe Bool
    {- ^ Whether the version declares install scripts. Present in the
    __abbreviated__ form only; 'Nothing' in the full form (derive from
    'vmScripts' there).
    -}
    , vmScripts :: Map Text Text
    {- ^ The @scripts@ map (lifecycle name to command), empty when absent. The
    source for deriving install-script presence from the full form.
    -}
    , vmLicense :: Maybe License
    {- ^ The declared license, if any (string or legacy object; see 'License').

    The manifest's dependency maps and maintainer list are __deliberately not
    parsed__: no rule or serve path consults them, the raw document relays them
    to the client untouched, and a heavy packument carries thousands of
    per-version entries of pure parse cost (architect ruling, 2026-07-02 --
    including that a malformed entry there may degrade rather than deny). Restore
    them from history if a dependency-reading rule ever lands.
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

{- Decode the @deprecated@ field leniently (see the module header's "string-or-boolean
@deprecated@ flag"): a string is the deprecation message, the boolean @true@ a
deprecated version with no message ('Just' @""@), and @false@, @null@, absence, or any
other shape a non-deprecated one ('Nothing'). Total, so a boolean never fails the decode. -}
deprecatedNotice :: Maybe Value -> Maybe Text
deprecatedNotice = \case
    Just (String message) -> Just message
    Just (Bool True) -> Just ""
    _ -> Nothing

{- Decode the @versions@ map __element-wise leniently__: read it as a raw map of
version key to 'Value', then keep only the entries that parse as a 'VersionManifest',
dropping any that do not. A version whose manifest is missing or malformed in a
required field (no @dist@\/@tarball@, an unusable @name@\/@version@) is __dropped__
rather than failing the whole packument, so one poisoned version cannot deny the
others. An absent @versions@ is the empty map; a @versions@ that is not an object at
all still fails the decode (the document is not a usable packument). -}
lenientVersionMap :: Object -> Parser (Map Text VersionManifest)
lenientVersionMap o = do
    raw <- o .:? "versions" .!= mempty -- Map Text Value: each version object kept raw
    pure (Map.mapMaybe (parseMaybe parseJSON) raw) -- drop the entries that will not decode

{- | The __full__ packument: @GET \/{pkg}@ with @Accept: application\/json@ (or
no @Accept@). One document describing the package and __every__ published
version.

The field that earns the full form its place in the pipeline is 'pkmtTime': the
map of publish timestamps (@created@, @modified@, and one per version), the
source of truth for publish age that age-based rules need. The abbreviated form
(§5) drops it, keeping only a top-level @modified@. Package-level
@description@\/@license@\/@author@ are hoisted from the @latest@ version for
convenience; the authoritative copy is the per-version one in 'pkmtVersions'.

@_attachments@ is intentionally not modelled -- it is populated only on the
publish document, not on reads.
-}
data Packument = Packument
    { pkmtName :: Text
    -- ^ The package name (may be @"\@scope\/name"@).
    , pkmtDistTags :: Map Text Text
    -- ^ The @dist-tags@ map (tag to version); __always__ includes @"latest"@.
    , pkmtVersions :: Map Text VersionManifest
    {- ^ Every published version that decodes, keyed by its exact version string. A
    version whose manifest is malformed in a required field is __dropped__ (see
    'lenientVersionMap'), so one poisoned version never denies the rest.
    -}
    , pkmtTime :: Map Text UTCTime
    {- ^ Publish timestamps: @"created"@, @"modified"@, and one entry per
    version key. The source of truth for publish age.
    -}
    , pkmtMaintainers :: [Person]
    -- ^ Current package maintainers; empty when absent.
    , pkmtDescription :: Maybe Text
    -- ^ Package description (hoisted from @latest@).
    , pkmtHomepage :: Maybe Text
    -- ^ Homepage URL, if given.
    , pkmtRepository :: Maybe Repository
    -- ^ SCM location, if given (string or object; see 'Repository').
    , pkmtBugs :: Maybe Bugs
    -- ^ Issue tracker, if given (string or object; see 'Bugs').
    , pkmtLicense :: Maybe License
    -- ^ Package-level license, if given (string or object; see 'License').
    , pkmtKeywords :: [Text]
    -- ^ Search keywords; empty when absent.
    }
    deriving stock (Eq, Show)

instance FromJSON Packument where
    parseJSON = withObject "Packument" $ \o ->
        Packument
            <$> o .: "name"
            <*> o .:? "dist-tags" .!= mempty
            <*> lenientVersionMap o
            <*> o .:? "time" .!= mempty
            <*> o .:? "maintainers" .!= []
            <*> o .:? "description"
            <*> o .:? "homepage"
            <*> o .:? "repository"
            <*> o .:? "bugs"
            <*> o .:? "license"
            <*> o .:? "keywords" .!= []

{- | The __abbreviated__ packument: @GET \/{pkg}@ with
@Accept: application\/vnd.npm.install-v1+json@. The install-optimised view and
the one the proxy treats as primary.

It carries exactly four top-level fields. Notably the full @time@ map is dropped
(only a top-level 'apkmtModified' remains), so publish-age rules need the full
'Packument'. Its 'apkmtVersions' manifests are the trimmed subset of
'VersionManifest' -- the same type, with the install-only fields populated
(including the abbreviated-only 'vmHasInstallScript').
-}
data AbbreviatedPackument = AbbreviatedPackument
    { apkmtName :: Text
    -- ^ The package name.
    , apkmtModified :: UTCTime
    {- ^ Equivalent to the full form's @time.modified@; the only timestamp the
    abbreviated form carries.
    -}
    , apkmtDistTags :: Map Text Text
    -- ^ The @dist-tags@ map (tag to version), as in the full form.
    , apkmtVersions :: Map Text VersionManifest
    {- ^ Every published version that decodes (abbreviated subset of fields), keyed
    by exact version string. As in the full form, a version whose manifest is
    malformed in a required field is __dropped__ (see 'lenientVersionMap').
    -}
    }
    deriving stock (Eq, Show)

instance FromJSON AbbreviatedPackument where
    parseJSON = withObject "AbbreviatedPackument" $ \o ->
        AbbreviatedPackument
            <$> o .: "name"
            <*> o .: "modified"
            <*> o .:? "dist-tags" .!= mempty
            <*> lenientVersionMap o

{- | An npm error body.

__Lenient:__ the documented shape is an object @{ message?, error?, ok?: false
}@ and clients "should check for @message@, then @error@". But the registry is
inconsistent -- its per-version 404 is a bare JSON __string__
(@"version not found: ^3.0.0"@), not an object. This type tolerates both: the
object form keeps its fields in an 'ErrorBody', and a bare string is captured
whole as 'ErrorString'. Read the human-facing reason via 'errorMessage', which
applies npm's "@message@, then @error@" precedence across both shapes.
-}
data ErrorResponse
    = -- | The documented object form @{ message?, error? }@.
      ErrorObject ErrorBody
    | -- | A bare JSON string body (npm's per-version 404), captured whole.
      ErrorString Text
    deriving stock (Eq, Show)

{- | The fields of npm's object-form error body. A product type (not inline
constructor fields on 'ErrorResponse') so its selectors are __total__ -- there is
no @ErrorString@ case for them to be partial over.
-}
data ErrorBody = ErrorBody
    { errMessage :: Maybe Text
    -- ^ The @message@ field -- the preferred human-facing reason.
    , errError :: Maybe Text
    -- ^ The @error@ field -- the fallback reason.
    }
    deriving stock (Eq, Show)

instance FromJSON ErrorResponse where
    parseJSON = \case
        String msg -> pure (ErrorString msg)
        Object o ->
            fmap ErrorObject $
                ErrorBody
                    <$> o .:? "message"
                    <*> o .:? "error"
        other -> typeMismatchOneOf "ErrorResponse (object or string)" other

{- | The human-facing reason carried by an error body, applying npm's documented
precedence: prefer @message@, then @error@, and for the bare-string form the
string itself. 'Nothing' only when an object form carried neither field. Total.
-}
errorMessage :: ErrorResponse -> Maybe Text
errorMessage = \case
    ErrorString msg -> Just msg
    ErrorObject body -> errMessage body <|> errError body

{- Fail a lenient string-or-object decoder with a descriptive message,
naming the accepted shapes and reporting what was actually found. A small wrapper
that keeps the @other ->@ branch of each tolerant instance to one readable line.
-}
typeMismatchOneOf :: String -> Value -> Parser a
typeMismatchOneOf expected actual =
    fail ("expected " <> expected <> ", but encountered " <> valueKind actual)

-- A short, human description of a JSON value's kind, for parse-error messages.
valueKind :: Value -> String
valueKind = \case
    Object{} -> "an object"
    String{} -> "a string"
    Array{} -> "an array"
    Number{} -> "a number"
    Bool{} -> "a boolean"
    Null -> "null"
