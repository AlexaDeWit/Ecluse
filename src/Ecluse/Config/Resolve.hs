-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

{- |
Hierarchical configuration resolution (Viper-style).
Unifies defaults, configuration files, and environment variables into a single
resolution tree with strict precedence: Defaults < File < Env.
-}
module Ecluse.Config.Resolve (
    deepMerge,
    buildEnvAst,
) where

import Data.Aeson (Value (..), eitherDecodeStrict)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap

import Data.Text qualified as T

{- | Right-biased deep merge of two Aeson Values.
Objects are merged recursively. Other types (Arrays, Strings, etc.) are
overwritten by the right side (the higher precedence value).
-}
deepMerge :: Value -> Value -> Value
deepMerge (Object l) (Object r) = Object $ KeyMap.unionWith deepMerge l r
deepMerge _ r = r

{- | Convert a list of environment variables into a nested JSON Object.
Filters for keys starting with @ECLUSE_@ and strips the prefix.
Double underscores (@__@) represent nested object paths.
Single underscores (@_@) are converted to camelCase for Aeson key matching.
For example, @ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM@ becomes
@{"mounts": {"npm": {"privateUpstream": ...}}}@.
Values that parse as valid JSON (like numbers or booleans) are decoded;
otherwise they remain as Strings.

Two kinds of variable never enter the AST: anything outside the @ECLUSE_@
prefix (the ambient SDK environment, @AWS_*@ included, is read directly at the
composition root; see "Ecluse.Config.Ambient"), and the reserved process-level
@ECLUSE_CONFIG@ (the config-document path override, consumed by
"Ecluse.Boot" before the document is even read, so it must not double as a
document key).
-}
buildEnvAst :: [(String, String)] -> Value
buildEnvAst env =
    foldl' deepMerge (Object KeyMap.empty) (map envVarValue configVars)
  where
    configVars = [(key, v) | (name, v) <- env, Just key <- [configEnvKey (T.pack name)]]

configEnvKey :: Text -> Maybe Text
configEnvKey name
    | name `elem` reservedProcessKeys = Nothing
    | otherwise = T.stripPrefix "ECLUSE_" name

{- | @ECLUSE_@-prefixed variables that address the boot process, not the config
document; they are consumed before resolution and never become document keys.
-}
reservedProcessKeys :: [Text]
reservedProcessKeys = ["ECLUSE_CONFIG"]

envVarValue :: (Text, String) -> Value
envVarValue (key, value) =
    nest (map toCamelCase (T.splitOn "__" key)) (parseEnvValue (T.pack value))

toCamelCase :: Text -> Key.Key
toCamelCase t =
    let words' = filter (not . T.null) (T.splitOn "_" t)
     in Key.fromText $ case words' of
            [] -> ""
            (w : ws) -> T.toLower w <> T.concat (map T.toTitle ws)

nest :: [Key.Key] -> Value -> Value
nest [] v = v
nest (p : ps) v = Object $ KeyMap.singleton p (nest ps v)

parseEnvValue :: Text -> Value
parseEnvValue txt = case eitherDecodeStrict (encodeUtf8 txt) of
    Right v -> v
    Left _ -> String txt
