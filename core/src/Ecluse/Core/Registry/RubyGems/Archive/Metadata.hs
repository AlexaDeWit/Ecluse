-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native gem metadata projects inert YAML into checked identity and installation facts.
module Ecluse.Core.Registry.RubyGems.Archive.Metadata (
    GemMetadata (..),
    parseGemMetadata,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Text.Libyaml qualified as Y

import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.Archive.Path (checkPath)
import Ecluse.Core.Registry.RubyGems.Archive.Yaml
import Ecluse.Core.Registry.RubyGems.CompactSpec (GemSpecification (..))
import Ecluse.Core.Registry.RubyGems.Document
import Ecluse.Core.Registry.RubyGems.Project
import Ecluse.Core.Version (Version, mkVersion, renderVersion)
import Ecluse.Core.Version.Gem (parseGem)

-- | The complete metadata tree remains available beside checked publication fields.
data GemMetadata = GemMetadata
    { metadataSpecification :: GemSpecification
    , metadataFiles :: [Text]
    , metadataExtensions :: [Text]
    , metadataDocument :: YamlValue
    }
    deriving stock (Eq, Show)

-- | Require a supported native schema and canonical archive-relative file declarations.
parseGemMetadata :: YamlValue -> Either ParseError GemMetadata
parseGemMetadata document = do
    fields <- tagged "Gem::Specification" document
    name <- yamlField "name" fields >>= yamlText >>= projectName
    version <- yamlField "version" fields >>= nativeVersion
    platform <- yamlField "platform" fields >>= nativePlatform
    specificationVersion <- yamlField "specification_version" fields >>= yamlText
    unless (specificationVersion `elem` ["1", "2", "3", "4"]) (bad "unsupported gem metadata specification version")
    dependencies <- yamlField "dependencies" fields >>= yamlSequence >>= traverse nativeDependency
    ruby <- yamlField "required_ruby_version" fields >>= nativeRequirement
    gems <- yamlField "required_rubygems_version" fields >>= nativeRequirement
    metadata <- case Map.lookup "metadata" fields of
        Nothing -> Right Map.empty
        Just raw -> yamlMapping raw >>= traverse yamlText
    files <- yamlField "files" fields >>= paths
    extensions <- maybe (Right []) paths (Map.lookup "extensions" fields)
    let inventory = Set.fromList files
    unless (Set.size inventory == length files) (bad "duplicate gem metadata file")
    unless (all (`Set.member` inventory) extensions) (bad "gem extension is absent from metadata file inventory")
    let specification = GemSpecification name version platform (catMaybes dependencies) ruby gems metadata
    pure (GemMetadata specification files extensions document)

paths :: YamlValue -> Either ParseError [Text]
paths value =
    yamlSequence value
        >>= traverse
            ( \item -> do
                raw <- yamlText item
                parts <- checkPath raw
                unless (T.intercalate "/" parts == raw) (bad "non-canonical gem metadata path")
                pure raw
            )

tagged :: Text -> YamlValue -> Either ParseError (Map Text YamlValue)
tagged className value = do
    unless (yamlTag value == Y.UriTag (toString ("!ruby/object:" <> className))) (bad "incorrect native gem YAML tag")
    yamlMapping value

nativeVersion :: YamlValue -> Either ParseError Version
nativeVersion value = do
    fields <- tagged "Gem::Version" value
    unless (Map.keys fields == ["version"]) (bad "invalid Gem::Version YAML fields")
    raw <- yamlField "version" fields >>= yamlText
    unless (isJust (parseGem raw)) (bad "invalid gem metadata version")
    pure (mkVersion RubyGems raw)

nativeRequirement :: YamlValue -> Either ParseError GemRequirement
nativeRequirement value = do
    fields <- tagged "Gem::Requirement" value
    unless (Map.keys fields == ["requirements"]) (bad "invalid Gem::Requirement YAML fields")
    clauses <- yamlField "requirements" fields >>= yamlSequence >>= traverse clause
    maybeToRight (ParseError "empty gem metadata requirement") (nonEmpty clauses)
  where
    clause raw =
        yamlSequence raw >>= \case
            [operator, version] -> do
                op <- yamlText operator
                unless (op `elem` ["=", "!=", ">", "<", ">=", "<=", "~>"]) (bad "invalid gem metadata requirement operator")
                text <- renderVersion <$> nativeVersion version
                pure (op <> " " <> text)
            _ -> bad "invalid gem metadata requirement clause"

nativeDependency :: YamlValue -> Either ParseError (Maybe GemDependency)
nativeDependency value = do
    fields <- tagged "Gem::Dependency" value
    unless (all (`elem` ["name", "requirement", "type", "prerelease", "version_requirements"]) (Map.keys fields)) (bad "unsupported Gem::Dependency YAML field")
    name <- yamlField "name" fields >>= yamlText >>= projectName
    wanted <- yamlField "requirement" fields >>= nativeRequirement
    for_ (Map.lookup "version_requirements" fields) $ \other -> do
        old <- nativeRequirement other
        unless (sort (toList wanted) == sort (toList old)) (bad "conflicting gem metadata requirements")
    kind <- yamlField "type" fields >>= yamlText
    case kind of
        ":runtime" -> Right (Just (GemDependency name wanted))
        "runtime" -> Right (Just (GemDependency name wanted))
        ":development" -> Right Nothing
        "development" -> Right Nothing
        _ -> bad "unsupported gem metadata dependency type"

nativePlatform :: YamlValue -> Either ParseError GemPlatform
nativePlatform value = case value of
    YamlScalar _ text -> mkGemPlatform text
    _ -> do
        fields <- tagged "Gem::Platform" value
        unless (Map.keys fields == ["cpu", "os", "version"]) (bad "invalid Gem::Platform YAML fields")
        cpu <- yamlField "cpu" fields >>= optionalPart
        os <- yamlField "os" fields >>= yamlText
        version <- yamlField "version" fields >>= optionalPart
        mkGemPlatform (T.intercalate "-" (catMaybes [cpu, Just os, version]))
  where
    optionalPart raw = do
        text <- yamlText raw
        pure (if T.null text || yamlTag raw == Y.NullTag then Nothing else Just text)

bad :: Text -> Either ParseError a
bad = Left . ParseError
