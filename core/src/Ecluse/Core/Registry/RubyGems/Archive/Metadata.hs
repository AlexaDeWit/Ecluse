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
import Ecluse.Core.Registry (ParseError (..), parseRefusal)
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
    name <- yamlField "name" fields >>= yamlNativeText >>= projectName
    version <- yamlField "version" fields >>= nativeVersion
    platform <- yamlField "platform" fields >>= nativePlatform
    specificationVersion <- yamlField "specification_version" fields >>= yamlText
    unless (specificationVersion `elem` ["1", "2", "3", "4"]) (parseRefusal "unsupported gem metadata specification version")
    dependencies <- yamlField "dependencies" fields >>= yamlSequence >>= traverse nativeDependency
    ruby <- yamlField "required_ruby_version" fields >>= nativeRequirement
    gems <- yamlField "required_rubygems_version" fields >>= nativeRequirement
    metadata <- case Map.lookup "metadata" fields of
        Nothing -> Right Map.empty
        Just raw -> yamlMapping raw >>= traverse yamlText
    files <- yamlField "files" fields >>= paths
    extensions <- maybe (Right []) paths (Map.lookup "extensions" fields)
    let inventory = Set.fromList files
    unless (Set.size inventory == length files) (parseRefusal "duplicate gem metadata file")
    unless (all (`Set.member` inventory) extensions) (parseRefusal "gem extension is absent from metadata file inventory")
    let specification = GemSpecification name version platform (catMaybes dependencies) ruby gems metadata
    pure (GemMetadata specification files extensions document)

paths :: YamlValue -> Either ParseError [Text]
paths value =
    yamlSequence value
        >>= traverse
            ( \item -> do
                raw <- yamlText item
                parts <- checkPath raw
                unless (T.intercalate "/" parts == raw) (parseRefusal "non-canonical gem metadata path")
                pure raw
            )

tagged :: Text -> YamlValue -> Either ParseError (Map Text YamlValue)
tagged className value = do
    unless (yamlTag value == Y.UriTag (toString ("!ruby/object:" <> className))) (parseRefusal "incorrect native gem YAML tag")
    yamlMapping value

nativeVersion :: YamlValue -> Either ParseError Version
nativeVersion value = do
    fields <- tagged "Gem::Version" value
    unless (Map.keys fields == ["version"]) (parseRefusal "invalid Gem::Version YAML fields")
    raw <- yamlField "version" fields >>= yamlNativeText
    unless (isJust (parseGem raw)) (parseRefusal "invalid gem metadata version")
    pure (mkVersion RubyGems raw)

nativeRequirement :: YamlValue -> Either ParseError GemRequirement
nativeRequirement value = do
    fields <- tagged "Gem::Requirement" value
    unless (Map.keys fields == ["requirements"]) (parseRefusal "invalid Gem::Requirement YAML fields")
    clauses <- yamlField "requirements" fields >>= yamlSequence >>= traverse clause
    maybeToRight (ParseError "empty gem metadata requirement") (nonEmpty clauses)
  where
    clause raw =
        yamlSequence raw >>= \case
            [operator, version] -> do
                op <- yamlText operator
                unless (op `elem` ["=", "!=", ">", "<", ">=", "<=", "~>"]) (parseRefusal "invalid gem metadata requirement operator")
                text <- renderVersion <$> nativeVersion version
                pure (op <> " " <> text)
            _ -> parseRefusal "invalid gem metadata requirement clause"

nativeDependency :: YamlValue -> Either ParseError (Maybe GemDependency)
nativeDependency value = do
    fields <- tagged "Gem::Dependency" value
    unless (all (`elem` ["name", "requirement", "type", "prerelease", "version_requirements"]) (Map.keys fields)) (parseRefusal "unsupported Gem::Dependency YAML field")
    name <- yamlField "name" fields >>= yamlNativeText >>= projectName
    wanted <- yamlField "requirement" fields >>= nativeRequirement
    for_ (Map.lookup "version_requirements" fields) $ \other -> do
        old <- nativeRequirement other
        unless (sort (toList wanted) == sort (toList old)) (parseRefusal "conflicting gem metadata requirements")
    kind <- yamlField "type" fields >>= yamlText
    case kind of
        ":runtime" -> Right (Just (GemDependency name wanted))
        "runtime" -> Right (Just (GemDependency name wanted))
        ":development" -> Right Nothing
        "development" -> Right Nothing
        _ -> parseRefusal "unsupported gem metadata dependency type"

nativePlatform :: YamlValue -> Either ParseError GemPlatform
nativePlatform value = case value of
    YamlScalar{} -> yamlNativeText value >>= mkGemPlatform
    _ -> do
        fields <- tagged "Gem::Platform" value
        unless (Map.keys fields == ["cpu", "os", "version"]) (parseRefusal "invalid Gem::Platform YAML fields")
        cpu <- yamlField "cpu" fields >>= optionalPart
        os <- yamlField "os" fields >>= yamlNativeText
        version <- yamlField "version" fields >>= optionalPart
        mkGemPlatform (T.intercalate "-" (catMaybes [cpu, Just os, version]))
  where
    optionalPart raw = do
        text <- yamlNativeString raw
        pure (text >>= \part -> part <$ guard (not (T.null part)))
