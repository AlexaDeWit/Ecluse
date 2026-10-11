-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The PEP 440 grammar and canonical ordering keys for PyPI.
module Ecluse.Core.Version.Pep440 (
    Pep440Key (..),
    parsePep440,
    renderPep440,
    isPep440Stable,
) where

import Data.Char (isDigit)
import Data.List (dropWhileEnd)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.Read qualified as TR

import Ecluse.Core.Text (isAsciiAlphaNum)
import Ecluse.Core.Version.Token (VToken (VNum, VStr), classifyRun, numOr0, parseNumSeg, renderToken, withinVersionLength)

-- | Canonical key without trailing release zeros (@1.0 == 1.0.0@), with ranks for PEP 440's None-handling.
data Pep440Key = Pep440Key
    { p440Epoch :: Integer
    , p440Release :: [Integer]
    , p440Pre :: (Int, Int, Integer)
    -- ^ @(band, stage, n)@: bands 0/1/2 mean dev without pre or post, prerelease, and final or post.
    , p440Post :: (Int, Integer)
    -- ^ @(0, 0)@ when absent, so a final sorts below any post-release.
    , p440Dev :: (Int, Integer)
    -- ^ @(0, n)@ when present and @(1, 0)@ when absent, so a dev release sorts below its sibling.
    , p440Local :: [VToken]
    }
    deriving stock (Eq, Ord, Show, Generic)

instance NFData Pep440Key

-- | Canonicalise at most 1024 characters, refusing missing releases or trailing text. Accepted keys are fully evaluated and retain no parser state.
parsePep440 :: Text -> Maybe Pep440Key
parsePep440 raw = do
    guard (withinVersionLength raw)
    let lowered = T.toLower (T.strip raw)
        noV = fromMaybe lowered (T.stripPrefix "v" lowered)
        (mainPart, localRaw) = T.breakOn "+" noV
    guard (T.all isMainChar mainPart)
    (epoch, afterEpoch) <- parseEpoch mainPart
    (release, suffix) <- parseRelease afterEpoch
    suffixParts <- parsePep440Suffix suffix
    localToks <- parseLocal localRaw
    pure (force (assembleKey epoch release suffixParts localToks))
  where
    isMainChar c = isAsciiAlphaNum c || c == '.' || c == '!' || c == '-' || c == '_'

parseEpoch :: Text -> Maybe (Integer, Text)
parseEpoch mainPart = do
    let (epochText, afterEpoch) = case T.breakOn "!" mainPart of
            (e, rest)
                | T.null rest -> ("", mainPart)
                | otherwise -> (e, T.drop 1 rest)
    epoch <- if T.null epochText then pure 0 else parseNumSeg epochText
    pure (epoch, afterEpoch)

-- One trailing dot belongs to the release, including before a suffix. Two dots still fail.
parseRelease :: Text -> Maybe ([Integer], Text)
parseRelease raw = do
    (n, rest) <- rightToMaybe (TR.decimal raw)
    case T.uncons rest of
        Just ('.', afterDot) -> case T.uncons afterDot of
            Just (c, _) | isDigit c || c == '.' -> do
                (ns, suffix) <- parseRelease afterDot
                pure (n : ns, suffix)
            _ -> pure ([n], afterDot)
        _ -> pure ([n], rest)

parseLocal :: Text -> Maybe [VToken]
parseLocal lr
    | T.null lr = Just []
    | otherwise =
        let segs = T.split (`elem` ['.', '-', '_']) (T.drop 1 lr)
         in if all (\s -> not (T.null s) && T.all isAsciiAlphaNum s) segs
                then Just (map classifyRun segs)
                else Nothing

assembleKey ::
    Integer -> [Integer] -> (Maybe (Int, Integer), Maybe Integer, Maybe Integer) -> [VToken] -> Pep440Key
assembleKey epoch release (mPre, mPost, mDev) localToks =
    Pep440Key
        { p440Epoch = epoch
        , p440Release = canonicalRelease release
        , p440Pre = pre
        , p440Post = post
        , p440Dev = dev
        , p440Local = localToks
        }
  where
    pre = case mPre of
        Just (stage, n) -> (1, stage, n)
        Nothing
            | isJust mDev && isNothing mPost -> (0, 0, 0)
            | otherwise -> (2, 0, 0)
    post = case mPost of
        Nothing -> (0, 0)
        Just n -> (1, n)
    dev = case mDev of
        Nothing -> (1, 0)
        Just n -> (0, n)

-- A nonzero last segment means the parsed list already satisfies the key's invariant.
canonicalRelease :: [Integer] -> [Integer]
canonicalRelease release = case viaNonEmpty NE.last release of
    Just 0 -> dropWhileEnd (== 0) release
    _ -> release

parsePep440Suffix ::
    Text -> Maybe (Maybe (Int, Integer), Maybe Integer, Maybe Integer)
parsePep440Suffix s0 =
    let (pre, s1) = consumePre s0
        (post, s2) = consumePost s1
        (dev, s3) = consumeDev s2
     in if T.null s3 then Just (pre, post, dev) else Nothing

dropSep :: Text -> Text
dropSep s = case T.uncons s of
    Just (c, rest) | c == '.' || c == '-' || c == '_' -> rest
    _ -> s

consumePre :: Text -> (Maybe (Int, Integer), Text)
consumePre s =
    case asum (map (\(lbl, rk) -> (,) rk <$> T.stripPrefix lbl (dropSep s)) preLabels) of
        Nothing -> (Nothing, s)
        Just (rk, afterLabel) ->
            let (digits, rest) = T.span isDigit (dropSep afterLabel)
             in (Just (rk, numOr0 digits), rest)
  where
    preLabels =
        [ ("alpha", 0)
        , ("beta", 1)
        , ("preview", 2)
        , ("pre", 2)
        , ("rc", 2)
        , ("a", 0)
        , ("b", 1)
        , ("c", 2)
        ]

-- Try @rev@ before @r@ so its remaining text does not start with @ev@.
consumePost :: Text -> (Maybe Integer, Text)
consumePost s =
    case asum (map (\lbl -> T.stripPrefix lbl (dropSep s)) ["post", "rev", "r"]) of
        Just afterLabel ->
            let (digits, rest) = T.span isDigit (dropSep afterLabel)
             in (Just (numOr0 digits), rest)
        Nothing -> case T.stripPrefix "-" s of
            Just afterDash ->
                let (digits, rest) = T.span isDigit afterDash
                 in if T.null digits then (Nothing, s) else (Just (numOr0 digits), rest)
            Nothing -> (Nothing, s)

consumeDev :: Text -> (Maybe Integer, Text)
consumeDev s =
    case T.stripPrefix "dev" (dropSep s) of
        Just afterLabel ->
            let (digits, rest) = T.span isDigit (dropSep afterLabel)
             in (Just (numOr0 digits), rest)
        Nothing -> (Nothing, s)

-- | Render as @packaging.utils.canonicalize_version@, without trailing release zeros.
renderPep440 :: Pep440Key -> Text
renderPep440 k =
    epoch <> release <> pre <> post <> dev <> localSegment
  where
    epoch = case p440Epoch k of
        0 -> ""
        n -> show n <> "!"

    -- Stripping the trailing zeros can empty the release ("0.0"), and a version needs one segment.
    release = case p440Release k of
        [] -> "0"
        segs -> T.intercalate "." (map show segs)

    pre = case p440Pre k of
        (1, stage, n) -> stageLabel stage <> show n
        _ -> ""

    post = case p440Post k of
        (1, n) -> ".post" <> show n
        _ -> ""

    dev = case p440Dev k of
        (0, n) -> ".dev" <> show n
        _ -> ""

    localSegment = case p440Local k of
        [] -> ""
        toks -> "+" <> T.intercalate "." (map renderToken toks)

stageLabel :: Int -> Text
stageLabel = \case
    0 -> "a"
    1 -> "b"
    _ -> "rc"

-- | Whether a version has neither a prerelease nor a dev part. Post-releases are stable.
isPep440Stable :: Pep440Key -> Bool
isPep440Stable k = preBand /= 1 && devBand /= 0
  where
    (preBand, _, _) = p440Pre k
    (devBand, _) = p440Dev k
