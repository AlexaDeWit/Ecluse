-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Compact specifications retain the original zlib bytes after data-only schema checks.
module Ecluse.Core.Registry.RubyGems.CompactSpec (
    GemSpecification (..),
    CompactSpec,
    decodeGemSpecification,
    decodeCompactSpec,
    validateCompactSpec,
    checkGemSpecification,
    compactSpecBytes,
    compactSpecification,
    specificationCoordinate,
) where

import Data.ByteString qualified as BS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import UnliftIO.Exception (evaluate)
import UnliftIO.Timeout (timeout)

import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.Core.Package (PackageName, renderPackageName)
import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.Archive.Inflate (InflateFormat (ZlibStream), withInflatedChunks)
import Ecluse.Core.Registry.RubyGems.Document
import Ecluse.Core.Registry.RubyGems.Marshal
import Ecluse.Core.Registry.RubyGems.Project
import Ecluse.Core.Security.Limits (Limits (..), requestTimeoutSeconds)
import Ecluse.Core.Version (Version, mkVersion, renderVersion)
import Ecluse.Core.Version.Gem (parseGem)

-- | Native installation facts from the supported historical and current specification layouts.
data GemSpecification = GemSpecification
    { specificationName :: PackageName
    , specificationVersion :: Version
    , specificationPlatform :: GemPlatform
    , specificationDependencies :: [GemDependency]
    , specificationRequiredRuby :: GemRequirement
    , specificationRequiredRubyGems :: GemRequirement
    , specificationMetadata :: Map Text Text
    }
    deriving stock (Eq, Show)

-- | The compressed bytes and parsed facts refer to the same fully consumed stream.
data CompactSpec = CompactSpec
    { compactSpecBytes :: ByteString
    , compactSpecification :: GemSpecification
    }
    deriving stock (Eq, Show)

-- | Parse the native Marshal wrapper and its bounded nested Marshal payload.
decodeGemSpecification :: Limits -> ByteString -> Either ParseError GemSpecification
decodeGemSpecification limits bytes = do
    outer <- decodeMarshal limits bytes
    validateClasses outer
    payload <- case stripInstanceVariables outer of
        MUserDefined "Gem::Specification" body -> Right body
        _ -> bad "compact specification lacks Gem::Specification framing"
    inner <- decodeMarshal limits payload
    validateClasses inner
    normalised <- normaliseLayout inner
    case normalised of
        MArray [rubyGems, MInteger _, name, version, date, summary, ruby, gems, original, dependencies, rubyforge, email, authors, description, homepage, rdoc, platform, licences, metadata] -> do
            traverse_ scalar [rubyGems, summary, original, rubyforge]
            traverse_ nullable [email, description, homepage]
            stringArray authors
            stringArray licences
            case rdoc of MBool _ -> Right (); _ -> bad "invalid specification documentation flag"
            case stripInstanceVariables date of
                MUserDefined "Time" raw | BS.length raw >= 8 && BS.length raw <= 122 -> Right ()
                MUserMarshal "Date" (MArray values) | length values == 6 -> traverse_ dateNumber values
                _ -> bad "invalid specification date"
            checkedName <- marshalText name >>= projectName
            checkedVersion <- gemVersion version
            checkedPlatform <- nativePlatform original
            void (nativePlatform platform)
            requiredRuby <- requirement ruby
            requiredGems <- requirement gems
            runtime <- array dependencies >>= traverse dependency
            extras <- textMap metadata
            pure (GemSpecification checkedName checkedVersion checkedPlatform (catMaybes runtime) requiredRuby requiredGems extras)
        _ -> bad "unsupported compact specification layout"
  where
    dateNumber (MInteger _) = Right ()
    dateNumber (MFloat _) = Right ()
    dateNumber _ = bad "invalid native Date component"
    scalar = void . marshalText
    nullable value = case stripInstanceVariables value of MNil -> Right (); MArray values -> traverse_ scalar values; other -> scalar other
    stringArray value = case stripInstanceVariables value of MNil -> Right (); other -> array other >>= traverse_ scalar

normaliseLayout :: MarshalValue -> Either ParseError MarshalValue
normaliseLayout value = case stripInstanceVariables value of
    MArray values@(_ : MInteger schema : _ : _ : _ : _ : _ : _ : original : _)
        | schema `elem` [-1, 1, 2, 3, 4] -> do
            let count = length values
                minimumFields
                    | schema <= 2 = 16
                    | schema == 3 = 17
                    | otherwise = 18
            unless (count >= minimumFields && count <= 19) (bad "invalid historical compact specification field count")
            let additions = drop (count - 16) [original, MArray [], MHash []]
            pure (MArray (values <> additions))
    _ -> bad "unsupported compact specification layout"

-- | Inflate one bounded zlib stream and preserve its exact original representation.
decodeCompactSpec :: Limits -> (Int -> IO ()) -> ByteString -> IO (Either ParseError CompactSpec)
decodeCompactSpec limits charge bytes
    | BS.length bytes > maxMetadataBytes limits = pure (bad "compact specification exceeds its compressed byte bound")
    | otherwise = do
        result <- timeout (requestTimeoutSeconds * 1000000) (withInflatedChunks ZlibStream charge bytes (collect 0 []))
        pure (fromMaybe (bad "compact specification validation timed out") result)
  where
    collect seen held pull =
        pull >>= \case
            Left fault -> pure (Left fault)
            Right chunk
                | BS.null chunk -> do
                    charge (73728 + 64 * seen)
                    evaluate (CompactSpec bytes <$> decodeGemSpecification limits (BS.concat (reverse held)))
                | BS.length chunk > maxMetadataBytes limits - seen -> pure (bad "compact specification exceeds its inflated byte bound")
                | otherwise -> do
                    let next = seen + BS.length chunk
                    charge (73728 + 3 * next)
                    collect next (chunk : held) pull

-- | Bind selected source bytes to the admitted physical coordinate and native installation facts.
validateCompactSpec :: Limits -> (Int -> IO ()) -> GemCoordinate -> GemRow -> ByteString -> IO (Either ParseError CompactSpec)
validateCompactSpec limits charge coordinate row bytes = do
    result <- timeout (requestTimeoutSeconds * 1000000) $ do
        decoded <- decodeCompactSpec limits charge bytes
        evaluate $ do
            compact <- decoded
            void (checkGemSpecification coordinate row (compactSpecification compact))
            pure compact
    pure (fromMaybe (bad "compact specification agreement timed out") result)

-- | Bind native archive or compact facts to the selected source coordinate and installation metadata.
checkGemSpecification :: GemCoordinate -> GemRow -> GemSpecification -> Either ParseError GemSpecification
checkGemSpecification coordinate row specification = do
    actual <- specificationCoordinate specification
    unless (actual == coordinate && grCoordinate row == coordinate) (bad "native specification identity disagrees with its selected coordinate")
    unless (normalDependencies (specificationDependencies specification) == normalDependencies (grDependencies row)) (bad "native specification runtime dependencies disagree with source metadata")
    unless (normalRequirement (specificationRequiredRuby specification) == normalRequirement (grRequiredRuby row)) (bad "native specification required Ruby disagrees with source metadata")
    unless (normalRequirement (specificationRequiredRubyGems specification) == normalRequirement (grRequiredRubyGems row)) (bad "native specification required RubyGems disagrees with source metadata")
    pure specification

-- | Build the checked physical artifact coordinate from native specification facts.
specificationCoordinate :: GemSpecification -> Either ParseError GemCoordinate
specificationCoordinate specification = mkCoordinate (specificationName specification) (specificationVersion specification) (specificationPlatform specification)

normalDependencies :: [GemDependency] -> [(Text, [Text])]
normalDependencies = sort . map (\dependencyValue -> (renderPackageName (gdName dependencyValue), normalRequirement (gdRequirement dependencyValue)))

normalRequirement :: GemRequirement -> [Text]
normalRequirement = sort . map canonical . toList
  where
    canonical clause =
        let (operator, rest) = T.span (`elem` ['=', '!', '<', '>', '~']) (T.strip clause)
         in operator <> " " <> T.strip rest

gemVersion :: MarshalValue -> Either ParseError Version
gemVersion value = case stripInstanceVariables value of
    MUserMarshal "Gem::Version" (MArray [raw]) -> do
        text <- marshalText raw
        unless (isJust (parseGem text)) (bad "invalid native gem version")
        pure (mkVersion RubyGems text)
    _ -> bad "invalid Gem::Version data"

requirement :: MarshalValue -> Either ParseError GemRequirement
requirement value = case stripInstanceVariables value of
    MUserMarshal "Gem::Requirement" (MArray [MArray clauses]) -> do
        parsed <- traverse clause clauses
        maybeToRight (ParseError "empty Gem::Requirement") (nonEmpty parsed)
    _ -> bad "invalid Gem::Requirement data"
  where
    clause (MArray [operator, version]) = do
        op <- marshalText operator
        unless (op `elem` ["=", "!=", ">", "<", ">=", "<=", "~>"]) (bad "unsupported gem requirement operator")
        raw <- renderVersion <$> gemVersion version
        pure (op <> " " <> raw)
    clause _ = bad "invalid gem requirement clause"

dependency :: MarshalValue -> Either ParseError (Maybe GemDependency)
dependency value = case stripInstanceVariables value of
    MObject "Gem::Dependency" attributes -> do
        unless (all ((`elem` ["@name", "@requirement", "@type", "@prerelease", "@version_requirements", "@force_ruby_platform"]) . fst) attributes) (bad "unsupported Gem::Dependency field")
        name <- field "@name" attributes >>= marshalText >>= projectName
        wanted <- case lookup "@requirement" attributes of
            Just MNil -> field "@version_requirements" attributes >>= requirement
            Just raw -> requirement raw
            Nothing -> field "@version_requirements" attributes >>= requirement
        case lookup "@version_requirements" attributes of
            Nothing -> Right ()
            Just other -> do
                old <- requirement other
                unless (normalRequirement old == normalRequirement wanted) (bad "conflicting gem dependency requirements")
        for_ (lookup "@force_ruby_platform" attributes) $ \case MBool _ -> Right (); _ -> bad "invalid dependency platform flag"
        for_ (lookup "@prerelease" attributes) $ \case MBool _ -> Right (); _ -> bad "invalid dependency prerelease flag"
        field "@type" attributes >>= \case
            MSymbol "runtime" -> Right (Just (GemDependency name wanted))
            MSymbol "development" -> Right Nothing
            _ -> bad "unsupported gem dependency type"
    _ -> bad "invalid Gem::Dependency data"

nativePlatform :: MarshalValue -> Either ParseError GemPlatform
nativePlatform value = case stripInstanceVariables value of
    MString _ -> marshalText value >>= mkGemPlatform
    MObject "Gem::Platform" attributes -> do
        unless (sort (map fst attributes) == ["@cpu", "@os", "@version"]) (bad "invalid Gem::Platform fields")
        cpu <- field "@cpu" attributes >>= part
        os <- field "@os" attributes >>= marshalText
        version <- field "@version" attributes >>= part
        mkGemPlatform (T.intercalate "-" (catMaybes [cpu, Just os, version]))
    _ -> bad "invalid specification platform"
  where
    part MNil = Right Nothing
    part other = Just <$> marshalText other

textMap :: MarshalValue -> Either ParseError (Map Text Text)
textMap value = case stripInstanceVariables value of
    MNil -> Right Map.empty
    MHash entries -> do
        pairs <- traverse (\(key, item) -> (,) <$> marshalText key <*> marshalText item) entries
        let result = Map.fromList pairs
        unless (Map.size result == length pairs) (bad "duplicate specification metadata key")
        pure result
    _ -> bad "invalid specification metadata"

validateClasses :: MarshalValue -> Either ParseError ()
validateClasses = \case
    MUserDefined className _ | className `elem` ["Gem::Specification", "Time"] -> Right ()
    MUserDefined _ _ -> bad "unsupported Marshal user-defined class"
    MUserMarshal className value | className `elem` ["Gem::Version", "Gem::Requirement", "Date"] -> validateClasses value
    MUserMarshal _ _ -> bad "unsupported Marshal user-marshal class"
    MObject className attributes | className `elem` ["Gem::Dependency", "Gem::Platform"] -> traverse_ (validateClasses . snd) attributes
    MObject _ _ -> bad "unsupported Marshal object class"
    MInstance value attributes -> do
        unless (all ((`elem` ["E", "encoding", "zone", "offset", "nano_num", "nano_den", "submicro", "@_zone", "@marshal_with_utc_coercion", "@taguri", "@debug_created_info"]) . fst) attributes) (bad "unsupported Marshal instance variable")
        validateClasses value
        traverse_ (validateClasses . snd) attributes
    MArray values -> traverse_ validateClasses values
    MHash values -> traverse_ (\(key, value) -> validateClasses key >> validateClasses value) values
    _ -> Right ()

array :: MarshalValue -> Either ParseError [MarshalValue]
array value = case stripInstanceVariables value of MArray values -> Right values; _ -> bad "expected Marshal array"

field :: ByteString -> [(ByteString, MarshalValue)] -> Either ParseError MarshalValue
field key = maybeToRight (ParseError "missing native specification field") . lookup key

bad :: Text -> Either ParseError a
bad = Left . ParseError
