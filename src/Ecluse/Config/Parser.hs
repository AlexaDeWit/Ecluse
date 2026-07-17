-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Config.Parser (
    rejectSecretKeys,
    parseRegistryUrl,
    parseEnum,
    valueKind,
    rejectUnknownKeys,
    parseUrl,
    parseHttpUrl,
    parsePort,
    parseCodeArtifactDuration,
) where

import Data.Aeson (Value (..), parseJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, withText)
import Data.Text qualified as T

import Ecluse.Config.Types (Url, mkUrl)
import Ecluse.Core.Security (hostPortAddress)
import Ecluse.Core.Security.Egress (RegistryUrl, mkRegistryUrl)

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

-- A registry URL entry must be https (mkRegistryUrl) and must carry a dialable
-- authority: a host, and, when a port is written, a decimal port in 1..65535
-- (hostPortAddress, the same extraction the egress gate authorises by). The gate
-- treats an unextractable authority as refused, so an entry that fails here could
-- only ever produce a mount that refuses every fetch; failing the boot names the
-- offending value instead.
parseRegistryUrl :: Value -> Parser RegistryUrl
parseRegistryUrl = \case
    String t
        | isNothing (hostPortAddress t) ->
            fail
                ( "registry URL must carry a host and, when a port is written, a decimal port in 1..65535 (got "
                    <> T.unpack t
                    <> ")"
                )
        | otherwise -> either (fail . T.unpack) pure (mkRegistryUrl t)
    other -> fail ("parseRegistryUrl expected a string, but encountered a " <> valueKind other)

parseEnum :: (Text -> Either Text a) -> String -> Value -> Parser a
parseEnum parser field = \case
    String t -> either (\e -> fail (field <> ": " <> T.unpack e)) pure (parser t)
    other -> fail (field <> " expected a string, but encountered a " <> valueKind other)

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

parseUrl :: Value -> Parser Url
parseUrl = withText "Url" $ \t ->
    case mkUrl t of
        Right u -> pure u
        Left e -> fail (T.unpack e)

{- | An @http(s)@ URL Écluse itself serves or rewrites against (the public URL):
the scheme must be http or https (http stays legal for loopback development
deployments), and the authority must be dialable by the same extraction the egress
gate authorises ('hostPortAddress'), so a value that cannot name a real listener is
refused at load instead of surfacing as rewritten artifact URLs no client can fetch.
-}
parseHttpUrl :: String -> Value -> Parser Url
parseHttpUrl field = \case
    String t
        | not (any (`T.isPrefixOf` T.strip t) ["http://", "https://"]) ->
            fail (field <> " must be an http:// or https:// URL (got " <> T.unpack t <> ")")
        | isNothing (hostPortAddress (T.strip t)) ->
            fail
                ( field
                    <> " must carry a host and, when a port is written, a decimal port in 1..65535 (got "
                    <> T.unpack t
                    <> ")"
                )
        | otherwise -> either (fail . T.unpack) pure (mkUrl t)
    other -> fail (field <> " expected a string, but encountered a " <> valueKind other)

-- | A listener port: 0..65535, where 0 asks the OS for an ephemeral port.
parsePort :: String -> Int -> Parser Int
parsePort field value
    | value >= 0 && value <= 65535 = pure value
    | otherwise = fail (field <> " must be a port in 0..65535 (0 = OS-assigned), got " <> show value)

{- | A CodeArtifact authorisation-token duration in seconds, bounded to the range
the service accepts (900..43200); an out-of-range value would only fail later, at
the first mint, with the mirror queue already accepting work.
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
