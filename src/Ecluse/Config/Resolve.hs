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
-}
buildEnvAst :: [(String, String)] -> Value
buildEnvAst env = foldl' deepMerge (Object KeyMap.empty) (map mkValue ecluseVars)
  where
    ecluseVars =
        [ (k, v)
        | (key, v) <- env
        , Just k <- [T.stripPrefix "ECLUSE_" (T.pack key)]
        ]

    mkValue :: (Text, String) -> Value
    mkValue (k, v) =
        let parts = map toCamelCase (T.splitOn "__" k)
            val = parseEnvValue (T.pack v)
         in nest parts val

    toCamelCase :: Text -> Key.Key
    toCamelCase t =
        let words' = filter (not . T.null) (T.splitOn "_" t)
         in Key.fromText $ case words' of
                [] -> ""
                (w : ws) -> T.toLower w <> T.concat (map T.toTitle ws)

    nest :: [Key.Key] -> Value -> Value
    nest [] v = v
    nest (p : ps) v = Object $ KeyMap.singleton p (nest ps v)

    parseEnvValue txt = case eitherDecodeStrict (encodeUtf8 txt) of
        Right v -> v
        Left _ -> String txt
