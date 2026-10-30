-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Compact-index rows retain native requirements and reject ambiguous artifact metadata.
module Ecluse.Core.Registry.RubyGems.Wire (
    GemRowError (..),
    decodeInfoRow,
    infoRowIdentity,
    infoRowVersion,
    parseGemRequirement,
) where

import Control.Monad (foldM)
import Data.ByteString qualified as BS
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.Core.Package (HashAlg (SHA256), PackageName, mkHash)
import Ecluse.Core.Package.Entry (EntryKey)
import Ecluse.Core.Registry (ParseError (ParseError))
import Ecluse.Core.Registry.RubyGems.Document (GemDependency (..), GemRequirement, GemRow (..))
import Ecluse.Core.Registry.RubyGems.Project (GemCoordinate, mkCoordinate, mkGemPlatform, projectName, projectVersion)
import Ecluse.Core.Strict (strictElements)
import Ecluse.Core.Text.Iso8601 (readIso8601Utc)
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Core.Version.Gem (isGemRequirementOperator)

-- | Native syntax failure differs from an explicitly unsupported wire representation.
data GemRowError
    = -- | A required field or its grammar is invalid.
      GemRowMalformed Text
    | -- | The row describes a newer artifact format outside the supported client contract.
      GemRowUnsupported Text
    deriving stock (Eq, Show)

-- | Recover a claimed identity before decoding other fields, so malformed duplicates cannot hide.
infoRowIdentity :: PackageName -> ByteString -> Maybe GemCoordinate
infoRowIdentity name bytes = do
    text <- rightToMaybe (TE.decodeUtf8' (identityPrefix bytes))
    rightToMaybe (parseIdentity name text)

-- | Preserve the raw version claim even when the platform or later payload cannot decode.
infoRowVersion :: ByteString -> Maybe Version
infoRowVersion bytes = do
    raw <- rightToMaybe (TE.decodeUtf8' (BS.takeWhile (/= 45) (identityPrefix bytes)))
    rightToMaybe (projectVersion raw)

identityPrefix :: ByteString -> ByteString
identityPrefix = BS.takeWhile (/= 32)

-- | Decode one complete row without losing timestamp colons or requirement conjunctions.
decodeInfoRow :: PackageName -> EntryKey -> ByteString -> Either GemRowError GemRow
decodeInfoRow name key bytes = do
    text <- first (const (GemRowMalformed "invalid UTF-8 in RubyGems info row")) (TE.decodeUtf8' bytes)
    let (nativeIdentity, afterIdentity) = T.breakOn " " text
    when (T.null afterIdentity) (malformed "missing RubyGems row separator")
    coordinate <- first parseFailure (parseIdentity name nativeIdentity)
    let (dependencyText, afterPipe) = T.breakOn "|" (T.drop 1 afterIdentity)
    when (T.null afterPipe) (malformed "missing RubyGems metadata separator")
    extras <- parseFields (T.drop 1 afterPipe)
    when (Map.member "platform" extras) (Left (GemRowUnsupported "content-addressed RubyGems artifacts require newer clients"))
    checksumText <- maybeToRight (GemRowMalformed "missing RubyGems SHA256") (Map.lookup "checksum" extras)
    checksum <- first GemRowMalformed (mkHash SHA256 checksumText)
    dependencies <- if T.null dependencyText then Right [] else traverse parseDependency (T.splitOn "," dependencyText)
    when (Set.size (Set.fromList (map gdName dependencies)) /= length dependencies) (malformed "duplicate RubyGems dependency")
    requiredRuby <- maybe (Right (">= 0" :| [])) parseGemRequirement (Map.lookup "ruby" extras)
    requiredRubyGems <- maybe (Right (">= 0" :| [])) parseGemRequirement (Map.lookup "rubygems" extras)
    let !published = force (Map.lookup "created_at" extras >>= readIso8601Utc)
    pure GemRow{grEntryKey = key, grCoordinate = coordinate, grDependencies = strictElements dependencies, grRequiredRuby = requiredRuby, grRequiredRubyGems = requiredRubyGems, grChecksum = checksum, grPublishedAt = published, grExtras = extras, grSource = BS.copy bytes}
  where
    parseDependency raw = do
        let (dependencyName, requirement) = T.breakOn ":" raw
        when (T.null requirement) (malformed "missing RubyGems dependency requirement")
        parsedName <- first parseFailure (projectName dependencyName)
        GemDependency parsedName <$> parseGemRequirement (T.drop 1 requirement)

-- | Parse conjunctions while retaining each native operator and version spelling.
parseGemRequirement :: Text -> Either GemRowError GemRequirement
parseGemRequirement raw = do
    clauses <- maybeToRight (GemRowMalformed "empty RubyGems requirement") (nonEmpty (T.splitOn "&" raw))
    strictElements <$> traverse parseClause clauses
  where
    parseClause clause = do
        let stripped = T.strip clause
            (operator, rest) = T.span (`elem` ['=', '!', '<', '>', '~']) stripped
            version = T.strip rest
        unless (isGemRequirementOperator operator && not (T.null version) && isRight (projectVersion version)) (malformed "invalid RubyGems requirement")
        pure (T.copy stripped)

parseIdentity :: PackageName -> Text -> Either ParseError GemCoordinate
parseIdentity name nativeIdentity = do
    let (version, suffix) = T.breakOn "-" nativeIdentity
    platform <- mkGemPlatform (if T.null suffix then "ruby" else T.drop 1 suffix)
    mkCoordinate name (mkVersion RubyGems version) platform

parseFields :: Text -> Either GemRowError (Map Text Text)
parseFields raw = foldM insert Map.empty (T.splitOn "," raw)
  where
    insert fields field = do
        let (key, separated) = T.breakOn ":" field
        unless (not (T.null key) && T.all asciiField key && not (T.null separated) && not (Map.member key fields)) (malformed "invalid or duplicate RubyGems metadata field")
        pure (Map.insert (T.copy key) (T.copy (T.drop 1 separated)) fields)
    asciiField c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '_'

parseFailure :: ParseError -> GemRowError
parseFailure (ParseError detail) = GemRowMalformed detail

malformed :: Text -> Either GemRowError a
malformed = Left . GemRowMalformed
