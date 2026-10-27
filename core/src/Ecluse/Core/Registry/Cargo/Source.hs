-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Logical Cargo registry identity. Comparison grants no transport or credential authority.
module Ecluse.Core.Registry.Cargo.Source (
    CargoSource,
    parseSource,
    sourceComparisonKey,
) where

import Data.Char (isControl, isDigit, isSpace)
import Data.IP (IPv6)
import Data.Text qualified as T
import Network.URI qualified as URI

import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Text (readDecimalText, readHexText)

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
            if special then specialHost hostText else pure hostText
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
        canonicalPath = if sparse then casePath else gitPath casePath
        canonicalAuthority = authority{URI.uriRegName = toString hostname, URI.uriPort = maybe "" ((":" <>) . show) canonicalPort}
        canonical = parsed{URI.uriScheme = toString (canonicalScheme <> ":"), URI.uriAuthority = Just canonicalAuthority, URI.uriPath = toString canonicalPath}
        text = toText (URI.uriToString id canonical "")
        cratesIo = (not sparse && text == "https://github.com/rust-lang/crates.io-index") || (sparse && text == "sparse+https://index.crates.io/")
    pure (CargoSource (if cratesIo then "crates.io" else (if sparse then "sparse|" else "registry|") <> text))

-- | This key is for resolver comparison and must never form a request address.
sourceComparisonKey :: CargoSource -> Text
sourceComparisonKey (CargoSource key) = key

-- Cargo reinserts the shortened segment, so existing percent signs need another encoding step.
gitPath :: Text -> Text
gitPath path = case T.stripSuffix ".git" path of
    Nothing -> path
    Just shortened ->
        let (prefix, segment) = T.breakOnEnd "/" shortened
            parent = if prefix == "/" then prefix else T.dropEnd 1 prefix
            unchanged char = char > ' ' && char < '\DEL' && char `notElem` ['"', '#', '<', '>', '?', '^', '`', '{', '}', '/', '%', '\\']
         in if segment `elem` [".", ".."]
                then parent
                else prefix <> toText (URI.escapeURIString unchanged (toString segment))

specialHost :: Text -> Either ParseError Text
specialHost host =
    let withoutDot = fromMaybe host (T.stripSuffix "." host)
        lastPart = T.takeWhileEnd (/= '.') withoutDot
        numeric = (not (T.null lastPart) && T.all isDigit lastPart) || isJust (ipv4Digits lastPart)
     in if numeric then ipv4Host (T.splitOn "." withoutDot) else Right host

-- The SSRF recogniser requires four octets. Cargo also accepts shorter native URL forms.
ipv4Host :: [Text] -> Either ParseError Text
ipv4Host parts = do
    when (length parts > 4) malformed
    numbers <- traverse ipv4Number parts
    case reverse numbers of
        final : reversedPrefix -> do
            let prefix = reverse reversedPrefix
                finalWidth = 4 - length prefix
            when (any (> 255) prefix || final >= 256 ^ finalWidth) malformed
            let address = foldl' (\total byte -> total * 256 + byte) 0 prefix * 256 ^ finalWidth + final
            pure (T.intercalate "." [show (address `div` (256 ^ power) `mod` 256) | power <- [3, 2, 1, 0 :: Int]])
        _ -> malformed

ipv4Number :: Text -> Either ParseError Integer
ipv4Number part = do
    (base, digits) <- maybe malformed Right (ipv4Digits part)
    let width = case base of
            16 -> 8
            8 -> 11
            _ -> 10
    -- Bound significant digits before reading an integer. Leading zeroes remain valid.
    when (T.compareLength digits width == GT) malformed
    number <-
        if T.null digits
            then Right 0
            else maybe malformed Right $ case base of
                16 -> readHexText digits
                8 -> readMaybe ("0o" <> toString digits)
                _ -> readDecimalText digits
    if number <= toInteger (maxBound :: Word32) then Right number else malformed

ipv4Digits :: Text -> Maybe (Int, Text)
ipv4Digits part = do
    guard (not (T.null part))
    let (base, digits)
            | Just rest <- T.stripPrefix "0x" part <|> T.stripPrefix "0X" part = (16, rest)
            | T.compareLength part 2 /= LT, Just rest <- T.stripPrefix "0" part = (8, rest)
            | otherwise = (10, part)
        digit char = case base of
            16 -> isDigit char || char `elem` ['a' .. 'f'] || char `elem` ['A' .. 'F']
            8 -> char `elem` ['0' .. '7']
            _ -> isDigit char
    guard (T.all digit digits)
    pure (base, T.dropWhile (== '0') digits)

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
