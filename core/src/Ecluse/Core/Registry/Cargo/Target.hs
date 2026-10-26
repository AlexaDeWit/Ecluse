-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Bounded native Cargo platform declarations used only for resolver comparison.
module Ecluse.Core.Registry.Cargo.Target (
    CargoTarget,
    parseTarget,
) where

import Data.Char (isAlphaNum, isAsciiLower, isAsciiUpper, isDigit)
import Data.Text qualified as T

import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Security.Limits (Limits (maxNestingDepth))

-- | Named targets remain exact. Cfg declarations retain their ordered native expression structure.
data CargoTarget = NamedTarget Text | ConfiguredTarget Cfg
    deriving stock (Eq, Ord, Show)

data Cfg = NamedCfg Text | KeyCfg Text Text | AllCfg [Cfg] | AnyCfg [Cfg] | NotCfg Cfg
    deriving stock (Eq, Ord, Show)

-- | Parse Cargo's cfg grammar with the request's nesting limit, including final commas in all/any lists.
parseTarget :: Limits -> Text -> Either ParseError CargoTarget
parseTarget limits target = case T.stripPrefix "cfg(" target >>= T.stripSuffix ")" of
    Nothing -> do
        unless (T.all (\char -> isAlphaNum char || char `elem` ['_', '-', '.']) target) malformed
        pure (NamedTarget target)
    Just input -> do
        (expression, rest) <- parseExpr 1 input
        unless (T.null (spaces rest)) malformed
        pure (ConfiguredTarget expression)
  where
    parseExpr depth input = do
        when (maxNestingDepth limits <= 0 || depth > maxNestingDepth limits) (Left (ParseError "Cargo target nesting exceeds its bound"))
        (name, raw, rest) <- identifier input
        case (raw, name) of
            (False, "all") -> parseList AllCfg depth =<< symbol '(' rest
            (False, "any") -> parseList AnyCfg depth =<< symbol '(' rest
            (False, "not") -> do
                start <- symbol '(' rest
                (expression, after) <- parseExpr (depth + 1) start
                end <- symbol ')' after
                pure (NotCfg expression, end)
            _ -> case T.uncons (spaces rest) of
                Just ('=', after) -> do
                    start <- symbol '"' after
                    let (value, close) = T.breakOn "\"" start
                    end <- symbol '"' close
                    pure (KeyCfg name value, end)
                _ -> pure (NamedCfg name, rest)
    parseList constructor depth = loop []
      where
        loop held input = case T.uncons (spaces input) of
            Just (')', rest) -> pure (constructor (reverse held), rest)
            _ -> do
                (expression, rest) <- parseExpr (depth + 1) input
                case T.uncons (spaces rest) of
                    Just (',', after) -> loop (expression : held) after
                    Just (')', after) -> pure (constructor (reverse (expression : held)), after)
                    _ -> malformed

identifier :: Text -> Either ParseError (Text, Bool, Text)
identifier input = do
    let start = spaces input
        (raw, content) = maybe (False, start) (\value -> (True, value)) (T.stripPrefix "r#" start)
        (name, rest) = T.span (\char -> identStart char || isDigit char) content
    case T.uncons name of
        Just (char, _) | identStart char -> pure (name, raw, rest)
        _ -> malformed

identStart :: Char -> Bool
identStart char = char == '_' || isAsciiLower char || isAsciiUpper char

spaces :: Text -> Text
spaces = T.dropWhile (== ' ')

symbol :: Char -> Text -> Either ParseError Text
symbol expected input = case T.uncons (spaces input) of
    Just (actual, rest) | actual == expected -> Right rest
    _ -> malformed

malformed :: Either ParseError a
malformed = Left (ParseError "Cargo target declaration is malformed")
