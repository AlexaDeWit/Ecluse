-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Logical Cargo registry identity. Comparison grants no transport or credential authority.
module Ecluse.Core.Registry.Cargo.Source (
    CargoSource,
    parseSource,
    sourceComparisonKey,
) where

import Data.Char (isControl, isSpace)
import Data.IP (IPv6)
import Data.Text qualified as T
import Network.URI qualified as URI

import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Text (readDecimalText)

-- | The source kind and canonical URL remain separate, except Cargo's built-in crates.io substitution.
newtype CargoSource = CargoSource Text
    deriving stock (Eq, Ord, Show)

-- | Retain URI identity components and apply Cargo's comparison rules without forming a transport address.
parseSource :: Text -> Either ParseError CargoSource
parseSource source = do
    parsed <- maybe malformed Right (URI.parseURI (toString source))
    authority <- maybe malformed Right (URI.uriAuthority parsed)
    let scheme = T.toLower (T.dropEnd 1 (toText (URI.uriScheme parsed)))
        sparse = "sparse+" `T.isPrefixOf` scheme
        special = scheme `elem` ["http", "https", "ws", "wss", "ftp", "file"]
        rawHost = toText (URI.uriRegName authority)
        hostText = if special then T.toLower (toText (URI.unEscapeString (toString rawHost))) else rawHost
    hostname <- case T.stripPrefix "[" hostText >>= T.stripSuffix "]" of
        Just address -> maybe malformed (\value -> Right ("[" <> show (value :: IPv6) <> "]")) (readMaybe (toString address))
        Nothing -> do
            when (T.any (\char -> isControl char || isSpace char || char `elem` ['/', '\\', '?', '#', '@', ':', '[', ']', '<', '>', '^', '|', '%']) hostText && special) malformed
            pure hostText
    let github = not ("+" `T.isInfixOf` scheme) && hostname == "github.com"
        canonicalScheme = if github then "https" else scheme
    when ("+" `T.isInfixOf` scheme && not sparse) malformed
    when (T.null hostname && scheme /= "file") malformed
    writtenPort <- parsePort (toText (URI.uriPort authority))
    let nativePort = writtenPort >>= \value -> value <$ guard (Just value /= defaultPort scheme)
        canonicalPort = nativePort >>= \value -> value <$ guard (Just value /= defaultPort canonicalScheme)
        dotSegment segment = case T.toLower segment of
            "%2e" -> "."
            "%2e%2e" -> ".."
            ".%2e" -> ".."
            "%2e." -> ".."
            _ -> segment
        originalPath = toText (URI.uriPath parsed)
        path = if special && T.null originalPath then "/" else originalPath
        dotPath = T.intercalate "/" (map dotSegment (T.splitOn "/" path))
        withDots = parsed{URI.uriPath = toString dotPath}
    normalised <- maybe malformed Right (URI.parseURI (URI.normalizePathSegments (URI.uriToString id withDots "")))
    let normalPath = toText (URI.uriPath normalised)
        trimmed = if normalPath == "/" then normalPath else fromMaybe normalPath (T.stripSuffix "/" normalPath)
        casePath = if github then T.toLower trimmed else trimmed
        canonicalPath = if sparse then casePath else fromMaybe casePath (T.stripSuffix ".git" casePath)
        canonicalAuthority = authority{URI.uriRegName = toString hostname, URI.uriPort = maybe "" ((":" <>) . show) canonicalPort}
        canonical = parsed{URI.uriScheme = toString (canonicalScheme <> ":"), URI.uriAuthority = Just canonicalAuthority, URI.uriPath = toString canonicalPath}
        text = toText (URI.uriToString id canonical "")
        cratesIo = (not sparse && text == "https://github.com/rust-lang/crates.io-index") || (sparse && text == "sparse+https://index.crates.io/")
    pure (CargoSource (if cratesIo then "crates.io" else (if sparse then "sparse|" else "registry|") <> text))

-- | This key is for resolver comparison and must never form a request address.
sourceComparisonKey :: CargoSource -> Text
sourceComparisonKey (CargoSource key) = key

parsePort :: Text -> Either ParseError (Maybe Int)
parsePort "" = Right Nothing
parsePort ":" = Right Nothing
parsePort value = do
    number <- maybe malformed Right (T.stripPrefix ":" value >>= readDecimalText :: Maybe Integer)
    if number <= 65535 then pure (Just (fromInteger number)) else malformed

defaultPort :: Text -> Maybe Int
defaultPort scheme = case scheme of
    "http" -> Just 80
    "https" -> Just 443
    "ws" -> Just 80
    "wss" -> Just 443
    "ftp" -> Just 21
    _ -> Nothing

malformed :: Either ParseError a
malformed = Left (ParseError "Cargo registry source URL is malformed")
