-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The shared refusals every configuration key is decoded through: secret and unknown keys,
enumerations, ports, durations, the @http(s)@ scheme check, and the URL shapes.

A malformed key fails at load with the key named, never at its first use. The URL parsers run
'refuseCredentialMaterial' ahead of any refusal that quotes the value, because boot echoes every
resolved key as written.
-}
module Ecluse.Config.Parser (
    rejectSecretKeys,
    parseRegistryUrl,
    parseEnum,
    valueKind,
    rejectUnknownKeys,
    parseUrl,
    parseHttpUrl,
    HttpScheme (..),
    splitHttpScheme,
    parsePort,
    parseCodeArtifactDuration,
) where

import Data.Aeson (Value (..), parseJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.Text qualified as T

import Ecluse.Config.Types (Url, mkUrl)
import Ecluse.Core.Security (hostPortAddress, refuseCredentialMaterial)
import Ecluse.Core.Security.Egress (RegistryUrl, mkConfiguredRegistryUrl)

rejectSecretKeys :: KeyMap.KeyMap Value -> Parser ()
rejectSecretKeys o =
    case filter (`KeyMap.member` o) secretKeys of
        [] -> pure ()
        present ->
            fail
                ( "secret key(s) are not allowed in the config document (use environment variables): "
                    <> intercalate ", " (map (show . Key.toText) present)
                )
  where
    secretKeys :: [Key.Key]
    secretKeys = ["token", "authToken", "password", "secret", "credentialToken"]

-- mkConfiguredRegistryUrl runs first, because the refusal below it quotes the value. An authority
-- the egress gate cannot extract could only build a mount that refuses every fetch.
parseRegistryUrl :: String -> Value -> Parser RegistryUrl
parseRegistryUrl field = \case
    String t -> case mkConfiguredRegistryUrl t of
        Left reason -> fail (field <> ": " <> T.unpack reason)
        Right url
            | isNothing (hostPortAddress t) ->
                fail
                    ( field
                        <> ": registry URL must carry a host and, when a port is written, a decimal port in 1..65535 (got "
                        <> T.unpack t
                        <> ")"
                    )
            | otherwise -> pure url
    other -> fail (field <> " expected a string, but encountered " <> valueKind other)

parseEnum :: (Text -> Either Text a) -> String -> Value -> Parser a
parseEnum parser field = \case
    String t -> either (\e -> fail (field <> ": " <> T.unpack e)) pure (parser t)
    other -> fail (field <> " expected a string, but encountered " <> valueKind other)

valueKind :: Value -> String
valueKind = \case
    Object{} -> "an object"
    Array{} -> "an array"
    Number{} -> "a number"
    Bool{} -> "a boolean"
    Null -> "null"
    String{} -> "a string"

rejectUnknownKeys :: String -> [Key.Key] -> KeyMap.KeyMap Value -> Parser ()
rejectUnknownKeys context accepted o =
    let isUnknown k = k `notElem` accepted
     in case filter isUnknown (KeyMap.keys o) of
            [] -> pure ()
            unknown ->
                fail
                    ( "unexpected "
                        <> context
                        <> " key(s): "
                        <> intercalate ", " (map (show . Key.toText) unknown)
                    )

{- | An operator-configured URL Écluse hands to a cloud SDK rather than dialling itself, such as the
mirror-queue target. It keeps its provider's shape, and carries the shared credential refusal.
-}
parseUrl :: String -> Value -> Parser Url
parseUrl field = \case
    String t
        | Left reason <- refuseCredentialMaterial (T.pack field) (T.strip t) -> fail (T.unpack reason)
        | otherwise -> either (fail . T.unpack) pure (mkUrl (T.strip t))
    other -> fail (field <> " expected a string, but encountered " <> valueKind other)

{- | An @http(s)@ URL Écluse serves, rewrites against, or fetches from. Plain http stays legal for
loopback, the authority must be one the egress gate can extract, and a credential is refused.
-}
parseHttpUrl :: String -> Value -> Parser Url
parseHttpUrl field = \case
    String t -> httpUrlOf field (T.strip t)
    other -> fail (field <> " expected a string, but encountered " <> valueKind other)

-- | The scheme a configured @http(s)@ URL writes.
data HttpScheme = Http | Https
    deriving stock (Eq, Show)

{- | Split a URL into the scheme it writes and the text after the separator, or 'Nothing' for
neither @http@ nor @https@. It is the one scheme check the configuration layer shares.
-}
splitHttpScheme :: Text -> Maybe (HttpScheme, Text)
splitHttpScheme raw =
    ((Https,) <$> T.stripPrefix "https://" raw) <|> ((Http,) <$> T.stripPrefix "http://" raw)

-- The credential refusal runs first, because the two refusals under it quote the value.
httpUrlOf :: String -> Text -> Parser Url
httpUrlOf field trimmed
    | Left reason <- refuseCredentialMaterial (T.pack field) trimmed = fail (T.unpack reason)
    | isNothing (splitHttpScheme trimmed) =
        fail (field <> " must be an http:// or https:// URL (got " <> T.unpack trimmed <> ")")
    | isNothing (hostPortAddress trimmed) =
        fail
            ( field
                <> " must carry a host and, when a port is written, a decimal port in 1..65535 (got "
                <> T.unpack trimmed
                <> ")"
            )
    | otherwise = either (fail . T.unpack) pure (mkUrl trimmed)

-- | A listener port: 0..65535, where 0 asks the OS for an ephemeral port.
parsePort :: String -> Int -> Parser Int
parsePort field value
    | value >= 0 && value <= 65535 = pure value
    | otherwise = fail (field <> " must be a port in 0..65535 (0 = OS-assigned), got " <> show value)

{- | A CodeArtifact token duration in seconds, bounded to the 900..43200 the service accepts. An
out-of-range value would otherwise fail at the first mint, with the queue already accepting work.
-}
parseCodeArtifactDuration :: String -> Value -> Parser Natural
parseCodeArtifactDuration field v = do
    n <- case v of
        String t -> case readMaybe (T.unpack t) :: Maybe Natural of
            Just parsed -> pure parsed
            Nothing -> fail (field <> ": invalid duration: " <> T.unpack t)
        other -> parseJSON other
    if n >= 900 && n <= 43200
        then pure n
        else fail (field <> " must be a duration in seconds within 900..43200, got " <> show n)
