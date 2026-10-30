-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TupleSections #-}

-- | Native packaged Cargo manifests and publication metadata, without executing package code.
module Ecluse.Core.Registry.Cargo.Manifest (
    CargoManifest,
    CargoPublishMetadata,
    decodeManifest,
    manifestTextPaths,
    publicationMetadata,
    checkPublicationIndex,
    checkUploadAgreement,
    checkDependencyRegistries,
    parseRustRelease,
    publishMetadataValue,
    cratesIoIndex,
    usesUnstableDependencyField,
) where

import Control.Monad (foldM)
import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Char (isDigit)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Toml qualified
import Toml.Syntax qualified as Syntax

import Ecluse.Core.Registry (ParseError (..), parseRefusal)
import Ecluse.Core.Registry.Archive.Path (resolveArchivePath)
import Ecluse.Core.Registry.Cargo.Source (CargoSource, parseSource, sourceComparisonKey)
import Ecluse.Core.Registry.Cargo.Target (CargoTarget, parseTarget)
import Ecluse.Core.Security.Limits (Limits (..))
import Ecluse.Core.Version.Cargo (CargoKey, parseCargo)

-- | Identity is the exact spelling from the effective normalised Cargo.toml.
data CargoManifest = CargoManifest
    { cmName :: Text
    , cmVersion :: Text
    , cmPackage :: Table
    , cmRoot :: Table
    }
    deriving stock (Eq, Show)

-- | A complete native upload document. It contains no registry publication time or yank override.
newtype CargoPublishMetadata = CargoPublishMetadata Value
    deriving stock (Eq, Show)

type Table = Map Text Toml.Value

-- | Bound bytes and parsed nesting, reject duplicate TOML keys, and check exact native identity.
decodeManifest :: Limits -> Text -> Text -> ByteString -> Either ParseError CargoManifest
decodeManifest limits name version bytes = do
    when (maxMetadataBytes limits <= 0 || BS.length bytes > min 1048576 (maxMetadataBytes limits)) (parseRefusal "Cargo manifest exceeds its byte bound")
    text <- first (const (ParseError "Cargo manifest is not UTF-8")) (TE.decodeUtf8' bytes)
    boundSyntax (maxNestingDepth limits) text
    parsed <- first (const (ParseError "invalid Cargo TOML manifest")) (Toml.parse text)
    checkDepth (maxNestingDepth limits) 0 (Toml.Table' () (Toml.forgetTableAnns parsed))
    let root = tableValues (Toml.forgetTableAnns parsed)
    for_ (Map.lookup "cargo-features" root) $ \value -> do
        features <- strings value
        unless (null features) (parseRefusal "Cargo unstable manifest features are unsupported")
    packageValue <- aliased "package" "project" root >>= maybe (parseRefusal "Cargo manifest lacks package metadata") Right
    package <- table packageValue
    actualName <- required "name" package >>= string
    actualVersion <- required "version" package >>= string
    unless (actualName == name && actualVersion == version) (parseRefusal "Cargo manifest disagrees with the admitted native identity")
    when (Map.member "workspace" root || Map.member "workspace" package) (parseRefusal "Cargo packaged manifest retains workspace inheritance")
    for_ (Map.lookup "edition" package) $ \value -> do
        edition <- string value
        unless (edition `elem` ["2015", "2018", "2021", "2024"]) (parseRefusal "unsupported Cargo edition")
    pure (CargoManifest actualName actualVersion package root)

-- | Select Cargo's declared or default README and licence file without retaining other text payloads.
manifestTextPaths :: CargoManifest -> Set Text -> Either ParseError [Text]
manifestTextPaths manifest inventory = do
    readme <- readmePath manifest inventory
    licence <- optionalStringAlias "license-file" "license_file" (cmPackage manifest)
    paths <- traverse checkedPath (maybeToList readme <> maybeToList licence)
    for_ paths $ \path -> unless (Set.member path inventory) (parseRefusal "Cargo manifest declares an absent text file")
    pure (ordNub paths)

-- | Reconstruct the native publish document from normalised metadata and selected archive text.
publicationMetadata :: CargoManifest -> Map Text ByteString -> Either ParseError CargoPublishMetadata
publicationMetadata manifest content = do
    let package = cmPackage manifest
    (dependencies, features, rustVersion, links) <- resolverMetadata manifest
    authors <- optionalStrings "authors" package
    keywords <- optionalStrings "keywords" package
    categories <- optionalStrings "categories" package
    badges <- optionalTable "badges" (cmRoot manifest) >>= traverse (table >=> traverse string)
    readmeFile <- readmePath manifest (Map.keysSet content)
    readme <- traverse (textContent content) readmeFile
    licenceFile <- optionalStringAlias "license-file" "license_file" package
    for_ licenceFile $ \path -> void (textContent content path)
    fields <-
        traverse
            (\(source, target) -> (target,) <$> optionalString source package)
            [("description", "description"), ("documentation", "documentation"), ("homepage", "homepage"), ("license", "license"), ("repository", "repository")]
    pure . CargoPublishMetadata . object $
        [ "name" .= cmName manifest
        , "vers" .= cmVersion manifest
        , "deps" .= dependencies
        , "features" .= features
        , "authors" .= authors
        , "keywords" .= keywords
        , "categories" .= categories
        , "badges" .= badges
        , "readme" .= readme
        , "readme_file" .= readmeFile
        , "license_file" .= licenceFile
        , "rust_version" .= rustVersion
        , "links" .= links
        ]
            <> [Key.fromText key .= value | (key, value) <- fields]

resolverMetadata :: CargoManifest -> Either ParseError ([Value], Map Text [Text], Maybe Text, Maybe Text)
resolverMetadata manifest =
    (,,,)
        <$> manifestDependencies (cmRoot manifest)
        <*> (optionalTable "features" (cmRoot manifest) >>= traverse strings)
        <*> optionalStringAlias "rust-version" "rust_version" (cmPackage manifest)
        <*> optionalString "links" (cmPackage manifest)

-- | Compare resolver meaning against the effective archive manifest, relative to the selected logical registry.
checkUploadAgreement :: Limits -> Text -> CargoManifest -> Value -> Either ParseError ()
checkUploadAgreement limits registry manifest (Object metadata) = do
    (dependencies, features, rustVersion, links) <- resolverMetadata manifest
    currentRegistry <- parseSource registry
    expected <- traverse (uploadDependency currentRegistry) dependencies >>= foldM (insertDependency indexRepeat limits) Map.empty
    uploaded <- requiredJson "deps" metadata >>= array >>= traverse (uploadDependency currentRegistry)
    actual <- foldM (insertDependency indexRepeat limits) Map.empty uploaded
    unless (Map.keysSet expected == Map.keysSet actual) (parseRefusal "Cargo upload and manifest dependency inventories disagree")
    for_ (Map.toList expected) $ \(key, value) -> do
        agreed <- maybe (parseRefusal "Cargo upload omits a manifest dependency") Right (Map.lookup key actual)
        checkDependency value agreed
    uploadedFeatures <- requiredJson "features" metadata >>= jsonObject >>= traverse (array >=> traverse jsonString)
    expectedFeatures <- effectiveFeatures dependencies features
    actualFeatures <- effectiveFeatures uploaded (Map.fromList [(Key.toText key, values) | (key, values) <- KeyMap.toList uploadedFeatures])
    unless (expectedFeatures == actualFeatures) (parseRefusal "Cargo upload and manifest feature graphs disagree")
    actualLinks <- nullableString "links" metadata
    unless (links == actualLinks) (parseRefusal "Cargo upload and manifest links declarations disagree")
    expectedRust <- traverse parseRustRelease rustVersion
    actualRust <- nullableString "rust_version" metadata >>= traverse parseRustRelease
    unless (expectedRust == actualRust) (parseRefusal "Cargo upload and manifest Rust versions disagree")
checkUploadAgreement _ _ _ _ = parseRefusal "Cargo upload metadata is not an object"

{- | Refuse a dependency from a registry other than crates.io or one of this proxy's own. A null or
absent source is the publication registry, which the caller lists. Run after 'checkUploadAgreement'.
-}
checkDependencyRegistries :: [Text] -> Value -> Either ParseError ()
checkDependencyRegistries own (Object metadata) = do
    accepted <- Set.fromList . map sourceComparisonKey <$> traverse parseSource (cratesIoIndex : own)
    dependencies <- requiredJson "deps" metadata >>= array
    for_ dependencies $ \case
        Object fields -> do
            name <- requiredJson "name" fields >>= jsonString
            declared <- nullableString "registry" fields >>= traverse parseSource
            for_ declared $ \source ->
                unless (Set.member (sourceComparisonKey source) accepted) $
                    parseRefusal ("Cargo dependency '" <> name <> "' comes from a registry other than crates.io or this proxy: publish it through this proxy first, then depend on this proxy's copy")
        _ -> parseRefusal "Cargo upload dependency is not an object"
checkDependencyRegistries _ _ = parseRefusal "Cargo upload metadata is not an object"

uploadDependency :: CargoSource -> Value -> Either ParseError Value
uploadDependency registry (Object fields) = do
    name <- requiredJson "name" fields >>= jsonString
    alias <- nullableString "explicit_name_in_toml" fields
    kind <- fromMaybe "normal" <$> nullableString "kind" fields
    source <- maybe (Right registry) parseSource =<< nullableString "registry" fields
    pure . Object
        $ KeyMap.insert "explicit_name_in_toml" (toJSON (alias >>= \value -> value <$ guard (value /= name)))
            . KeyMap.insert "kind" (String kind)
            . KeyMap.insert "registry" (String (sourceComparisonKey source))
        $ fields
uploadDependency _ _ = parseRefusal "Cargo upload dependency is not an object"

effectiveFeatures :: [Value] -> Map Text [Text] -> Either ParseError (Map Text (Set Text))
effectiveFeatures dependencies features = do
    optionalDependencyAliases <- fmap catMaybes . forM dependencies $ \case
        Object fields -> do
            enabled <- requiredJson "optional" fields >>= jsonBool
            name <- requiredJson "name" fields >>= jsonString
            alias <- fromMaybe name <$> nullableString "explicit_name_in_toml" fields
            pure (alias <$ guard enabled)
        _ -> parseRefusal "Cargo publication dependency is not an object"
    let declared = Set.fromList <$> features
        suppressed = Set.fromList [alias | values <- Map.elems features, value <- values, Just alias <- [T.stripPrefix "dep:" value]]
        implicit = Map.fromList [(alias, Set.singleton ("dep:" <> alias)) | alias <- optionalDependencyAliases, Map.notMember alias features, Set.notMember alias suppressed]
    pure (Map.union declared implicit)

-- | Numeric minimum Rust releases permit one to three components, with trailing zeroes implied.
parseRustRelease :: Text -> Either ParseError CargoKey
parseRustRelease raw = do
    let components = T.splitOn "." raw
        complete = T.intercalate "." (components <> replicate (max 0 (3 - length components)) "0")
    unless (length components `elem` [1, 2, 3] && not (T.any (`elem` ['-', '+']) raw)) (parseRefusal "Cargo Rust version is invalid")
    maybe (parseRefusal "Cargo Rust version is invalid") Right (parseCargo complete)

-- | Cross-check source index facts and preserve its dependency requirements and merged feature representation.
checkPublicationIndex :: Limits -> CargoPublishMetadata -> Value -> Either ParseError CargoPublishMetadata
checkPublicationIndex limits (CargoPublishMetadata (Object metadata)) (Object index) = do
    for_ ["name", "vers"] $ \key ->
        unless (KeyMap.lookup key metadata == KeyMap.lookup key index) (parseRefusal "Cargo publication and source index identities disagree")
    nativeDeps <- requiredJson "deps" index >>= array
    expectedDeps <- requiredJson "deps" metadata >>= array
    projected <- traverse indexDependency nativeDeps
    unless (length expectedDeps == length projected) (parseRefusal "Cargo manifest and index dependency counts disagree")
    registry <- parseSource cratesIoIndex
    byIdentity <- traverse (uploadDependency registry) projected >>= foldM (insertDependency indexRepeat limits) Map.empty
    declared <- traverse (uploadDependency registry) expectedDeps >>= foldM (insertDependency "Cargo manifest repeats a dependency identity" limits) Map.empty
    unless (Map.keysSet declared == Map.keysSet byIdentity) (parseRefusal "Cargo manifest and index dependency identities disagree")
    for_ (Map.toList declared) $ \(key, expected) -> do
        actual <- maybe (parseRefusal "Cargo manifest and index dependency identities disagree") Right (Map.lookup key byIdentity)
        checkDependency expected actual
    sourceFeatures <- indexFeatures index
    authoredFeatures <- requiredJson "features" metadata >>= jsonObject
    for_ (KeyMap.toList authoredFeatures) $ \(key, value) -> do
        source <- maybe (parseRefusal "Cargo index omits a manifest feature") Right (KeyMap.lookup key sourceFeatures)
        checkFeatureValues value source
    authoredLinks <- nullableString "links" metadata
    indexedLinks <- nullableString "links" index
    unless (authoredLinks == indexedLinks) (parseRefusal "Cargo manifest and index links declarations disagree")
    for_ (KeyMap.lookup "rust_version" index) $ \value ->
        unless (KeyMap.lookup "rust_version" metadata == Just value) (parseRefusal "Cargo manifest and index Rust versions disagree")
    pure (CargoPublishMetadata (Object (KeyMap.insert "deps" (toJSON projected) (KeyMap.insert "features" (Object sourceFeatures) metadata))))
checkPublicationIndex _ _ _ = parseRefusal "Cargo source index entry is not an object"

-- | The native JSON body excludes the archive length framing and archive bytes.
publishMetadataValue :: CargoPublishMetadata -> Value
publishMetadataValue (CargoPublishMetadata value) = value

boundSyntax :: Int -> Text -> Either ParseError ()
boundSyntax cap text = go 0 0 (Syntax.Located Syntax.startPos text)
  where
    go depth dotted input = do
        when (cap <= 0 || depth > cap || dotted > cap) (parseRefusal "Cargo TOML nesting exceeds its bound")
        (located, rest) <- first (const (ParseError "invalid Cargo TOML token")) (Syntax.scanToken Syntax.ValueContext input)
        case Syntax.locThing located of
            Syntax.TokEOF -> Right ()
            Syntax.TokSquareO -> go (depth + 1) 0 rest
            Syntax.Tok2SquareO -> go (depth + 2) 0 rest
            Syntax.TokCurlyO -> go (depth + 1) 0 rest
            Syntax.TokSquareC -> go (max 0 (depth - 1)) 0 rest
            Syntax.Tok2SquareC -> go (max 0 (depth - 2)) 0 rest
            Syntax.TokCurlyC -> go (max 0 (depth - 1)) 0 rest
            Syntax.TokPeriod -> go depth (dotted + 1) rest
            Syntax.TokEquals -> go depth 0 rest
            Syntax.TokComma -> go depth 0 rest
            Syntax.TokNewline -> go depth 0 rest
            _ -> go depth dotted rest

checkDepth :: Int -> Int -> Toml.Value -> Either ParseError ()
checkDepth cap depth value = do
    when (cap <= 0 || depth > cap) (parseRefusal "Cargo TOML nesting exceeds its bound")
    case value of
        Toml.Table' _ nested -> traverse_ (checkDepth cap (depth + 1)) (tableValues nested)
        Toml.List' _ values -> traverse_ (checkDepth cap (depth + 1)) values
        _ -> Right ()

tableValues :: Toml.Table -> Table
tableValues (Toml.MkTable values) = snd <$> values

table :: Toml.Value -> Either ParseError Table
table (Toml.Table' _ value) = Right (tableValues value)
table _ = parseRefusal "Cargo manifest field is not a table"

string :: Toml.Value -> Either ParseError Text
string (Toml.Text' _ value) = Right value
string _ = parseRefusal "Cargo manifest field is not a string"

boolean :: Toml.Value -> Either ParseError Bool
boolean (Toml.Bool' _ value) = Right value
boolean _ = parseRefusal "Cargo manifest field is not a boolean"

strings :: Toml.Value -> Either ParseError [Text]
strings (Toml.List' _ values) = traverse string values
strings _ = parseRefusal "Cargo manifest field is not an array of strings"

required :: Text -> Table -> Either ParseError Toml.Value
required key = maybe (parseRefusal ("Cargo manifest lacks " <> key)) Right . Map.lookup key

optionalTable :: Text -> Table -> Either ParseError Table
optionalTable key values = maybe (Right Map.empty) table (Map.lookup key values)

optionalString :: Text -> Table -> Either ParseError (Maybe Text)
optionalString key = traverse string . Map.lookup key

optionalStrings :: Text -> Table -> Either ParseError [Text]
optionalStrings key values = maybe (Right []) strings (Map.lookup key values)

aliased :: Text -> Text -> Table -> Either ParseError (Maybe Toml.Value)
aliased primary legacy values = case (Map.lookup primary values, Map.lookup legacy values) of
    (Just _, Just _) -> parseRefusal "ambiguous Cargo manifest spelling aliases"
    (preferred, fallback) -> pure (preferred <|> fallback)

optionalStringAlias :: Text -> Text -> Table -> Either ParseError (Maybe Text)
optionalStringAlias primary legacy values = aliased primary legacy values >>= traverse string

readmePath :: CargoManifest -> Set Text -> Either ParseError (Maybe Text)
readmePath manifest inventory = case Map.lookup "readme" (cmPackage manifest) of
    Just (Toml.Bool' _ False) -> pure Nothing
    Just (Toml.Bool' _ True) -> pure (Just "README.md")
    Just value -> do
        path <- string value
        Just path <$ checkedPath path
    Nothing -> pure (find (`Set.member` inventory) ["README.md", "README.txt", "README"])

checkedPath :: Text -> Either ParseError Text
checkedPath = resolveArchivePath []

textContent :: Map Text ByteString -> Text -> Either ParseError Text
textContent content path = do
    canonical <- checkedPath path
    bytes <- maybe (parseRefusal "Cargo declared text file has no regular content") Right (Map.lookup canonical content)
    first (const (ParseError "Cargo publication text is not UTF-8")) (TE.decodeUtf8' bytes)

manifestDependencies :: Table -> Either ParseError [Value]
manifestDependencies root = do
    direct <- dependencyGroups Nothing root
    targets <- optionalTable "target" root
    targeted <- forM (Map.toList targets) $ \(target, value) -> table value >>= dependencyGroups (Just target)
    pure (direct <> concat targeted)

dependencyGroups :: Maybe Text -> Table -> Either ParseError [Value]
dependencyGroups target values =
    concat
        <$> traverse
            dependencyGroup
            [("dependencies", "dependencies", "normal"), ("dev-dependencies", "dev_dependencies", "dev"), ("build-dependencies", "build_dependencies", "build")]
  where
    dependencyGroup (key, legacy, kind) = do
        dependencies <- if key == legacy then optionalTable key values else aliased key legacy values >>= maybe (Right Map.empty) table
        traverse (uncurry (dependency target kind)) (Map.toList dependencies)

dependency :: Maybe Text -> Text -> Text -> Toml.Value -> Either ParseError Value
dependency target kind alias value = do
    details <- case value of
        Toml.Text' _ requirement -> pure (Map.singleton "version" (Toml.Text' () requirement))
        _ -> table value
    when (any (`Map.member` details) ["workspace", "path", "git", "branch", "rev", "tag", "registry"]) (parseRefusal "Cargo dependency retains an unresolved source declaration")
    requirement <- required "version" details >>= string
    when (T.null requirement) (parseRefusal "Cargo dependency has an empty version requirement")
    original <- fromMaybe alias <$> optionalString "package" details
    features <- optionalStrings "features" details
    isOptional <- maybe (Right False) boolean (Map.lookup "optional" details)
    defaultFeatures <- aliased "default-features" "default_features" details >>= maybe (Right True) boolean
    registry <- fromMaybe cratesIoIndex <$> optionalString "registry-index" details
    pure $
        object
            [ "name" .= original
            , "version_req" .= requirement
            , "features" .= features
            , "optional" .= isOptional
            , "default_features" .= defaultFeatures
            , "target" .= target
            , "kind" .= kind
            , "registry" .= registry
            , "explicit_name_in_toml" .= (if original == alias then Nothing else Just alias)
            ]

-- | The logical crates.io dependency source. This identity grants no transport authority.
cratesIoIndex :: Text
cratesIoIndex = "https://github.com/rust-lang/crates.io-index"

-- | Nightly @-Z bindeps@ and public-dependency fields, by presence. Mirrored index rows and first-party uploads both refuse them.
usesUnstableDependencyField :: KeyMap.KeyMap Value -> Bool
usesUnstableDependencyField fields = any (`KeyMap.member` fields) ["artifact", "bindep_target", "lib", "public"]

indexDependency :: Value -> Either ParseError Value
indexDependency (Object fields) = do
    when (usesUnstableDependencyField fields) (parseRefusal "unsupported Cargo index dependency capability")
    alias <- requiredJson "name" fields >>= jsonString
    original <- nullableString "package" fields
    requirement <- requiredJson "req" fields >>= jsonString
    features <- maybe (Right []) array (KeyMap.lookup "features" fields)
    traverse_ jsonString features
    isOptional <- maybe (Right False) jsonBool (KeyMap.lookup "optional" fields)
    defaults <- maybe (Right True) jsonBool (KeyMap.lookup "default_features" fields)
    target <- nullableString "target" fields
    kind <- fromMaybe "normal" <$> nullableString "kind" fields
    unless (kind `elem` ["normal", "dev", "build"]) (parseRefusal "unsupported Cargo dependency kind")
    registry <- fromMaybe cratesIoIndex <$> nullableString "registry" fields
    pure $
        object
            [ "name" .= fromMaybe alias original
            , "version_req" .= requirement
            , "features" .= features
            , "optional" .= isOptional
            , "default_features" .= defaults
            , "target" .= target
            , "kind" .= kind
            , "registry" .= registry
            , "explicit_name_in_toml" .= (alias <$ original)
            ]
indexDependency _ = parseRefusal "Cargo index dependency is not an object"

type DependencyKey = (Text, Maybe Text, Maybe CargoTarget, Text)

dependencyKey :: Limits -> Value -> Either ParseError DependencyKey
dependencyKey limits (Object fields) =
    (,,,)
        <$> (requiredJson "name" fields >>= jsonString)
        <*> nullableString "explicit_name_in_toml" fields
        <*> (nullableString "target" fields >>= traverse (parseTarget limits))
        <*> (requiredJson "kind" fields >>= jsonString)
dependencyKey _ _ = parseRefusal "Cargo publication dependency is not an object"

insertDependency :: Text -> Limits -> Map DependencyKey Value -> Value -> Either ParseError (Map DependencyKey Value)
insertDependency refusal limits values dependencyValue = do
    key <- dependencyKey limits dependencyValue
    when (Map.member key values) (parseRefusal refusal)
    pure (Map.insert key dependencyValue values)

-- First-party agreement refuses a repeat on either side with this text, which its client receives.
indexRepeat :: Text
indexRepeat = "Cargo index repeats a dependency identity"

checkDependency :: Value -> Value -> Either ParseError ()
checkDependency (Object expected) (Object actual) = do
    for_ ["optional", "default_features", "registry"] $ \key ->
        unless (KeyMap.lookup key expected == KeyMap.lookup key actual) (parseRefusal "Cargo manifest and index dependency facts disagree")
    expectedFeatures <- requiredJson "features" expected
    actualFeatures <- requiredJson "features" actual
    checkFeatureValues expectedFeatures actualFeatures
    expectedRequirement <- requiredJson "version_req" expected >>= jsonString
    actualRequirement <- requiredJson "version_req" actual >>= jsonString
    unless (normalRequirement expectedRequirement == normalRequirement actualRequirement) (parseRefusal "Cargo manifest and index dependency requirements disagree")
checkDependency _ _ = parseRefusal "Cargo publication dependency is not an object"

checkFeatureValues :: Value -> Value -> Either ParseError ()
checkFeatureValues expected actual = do
    expectedValues <- array expected >>= traverse jsonString
    actualValues <- array actual >>= traverse jsonString
    unless (Set.fromList expectedValues == Set.fromList actualValues) (parseRefusal "Cargo manifest and index feature declarations disagree")

normalRequirement :: Text -> Set Text
normalRequirement = Set.fromList . map comparator . T.splitOn ","
  where
    comparator value =
        let withoutBuild = fst (T.breakOn "+" (T.strip value))
            (operator, release) = T.span (`elem` ['=', '>', '<', '~', '^']) withoutBuild
            (core, prerelease) = T.breakOn "-" (T.stripStart release)
            parts = T.splitOn "." core
            wildcard part = part `elem` ["*", "x", "X"]
            (numbers, suffix) = break wildcard parts
            nativeWildcard = not (null numbers) && not (null suffix) && all wildcard suffix && length parts <= 3
            effectiveOperator
                | not (T.null operator) = operator
                | null suffix, Just (firstChar, _) <- T.uncons core, isDigit firstChar = "^"
                | otherwise = ""
            nativeCore
                | wildcard core = "*"
                | nativeWildcard = T.intercalate "." (numbers <> ["*" | T.null operator])
                | otherwise = core
         in effectiveOperator <> nativeCore <> prerelease

indexFeatures :: KeyMap.KeyMap Value -> Either ParseError (KeyMap.KeyMap Value)
indexFeatures index = do
    primary <- maybe (Right KeyMap.empty) jsonObject (KeyMap.lookup "features" index)
    extra <- maybe (Right KeyMap.empty) jsonObject (KeyMap.lookup "features2" index)
    traverse_ (array >=> traverse_ jsonString) primary
    traverse_ (array >=> traverse_ jsonString) extra
    pure (KeyMap.unionWith merge primary extra)
  where
    merge (Array firstValues) (Array nextValues) = Array (firstValues <> nextValues)
    merge firstValue _ = firstValue

requiredJson :: Key.Key -> KeyMap.KeyMap Value -> Either ParseError Value
requiredJson key = maybe (parseRefusal "Cargo index lacks a required publication field") Right . KeyMap.lookup key

nullableString :: Key.Key -> KeyMap.KeyMap Value -> Either ParseError (Maybe Text)
nullableString key values = case KeyMap.lookup key values of
    Nothing -> Right Nothing
    Just Null -> Right Nothing
    Just value -> Just <$> jsonString value

jsonString :: Value -> Either ParseError Text
jsonString (String value) = Right value
jsonString _ = parseRefusal "Cargo index publication field is not a string"

jsonBool :: Value -> Either ParseError Bool
jsonBool (Bool value) = Right value
jsonBool _ = parseRefusal "Cargo index publication field is not a boolean"

jsonObject :: Value -> Either ParseError (KeyMap.KeyMap Value)
jsonObject (Object value) = Right value
jsonObject _ = parseRefusal "Cargo index publication field is not an object"

array :: Value -> Either ParseError [Value]
array (Array values) = Right (toList values)
array _ = parseRefusal "Cargo index publication field is not an array"
