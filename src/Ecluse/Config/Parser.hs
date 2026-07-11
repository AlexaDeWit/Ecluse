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
) where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, withText)
import Data.Text qualified as T

import Ecluse.Config.Types (Url, mkUrl)
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

parseRegistryUrl :: Value -> Parser RegistryUrl
parseRegistryUrl = \case
    String t -> either (fail . T.unpack) pure (mkRegistryUrl t)
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
    let isUnknown k = k `notElem` accepted && not ("aws" `T.isPrefixOf` Key.toText k)
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
