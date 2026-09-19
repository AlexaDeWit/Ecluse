-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The PEP 440 grammar and ordering (PyPI).

'parsePep440' canonicalises a version (@1.0ALPHA1@, @1.0-1@, trailing zeros, ...) into a
'Pep440Key', the ordering tuple @(epoch, release, pre, post, dev, local)@ whose rank fields
make the derived 'Ord' reproduce the spec ordering. A version is stable iff it is neither a
pre-release (@a@\/@b@\/@rc@) nor a dev release. Post-releases stay stable.
-}
module Ecluse.Core.Version.Pep440 (
    Pep440Key (..),
    parsePep440,
    renderPep440,
    isPep440Stable,
) where

import Data.Char (isDigit)
import Data.List (dropWhileEnd, unsnoc)
import Data.Text qualified as T

import Ecluse.Core.Version.Token (VToken (VNum, VStr), classifyRun, isAsciiAlphaNum, numOr0, parseNumSeg, withinVersionLength)

{- | A parsed PEP 440 version as its canonical ordering key. The release carries no trailing
zeros (@1.0 == 1.0.0@), and the rank fields encode PEP 440's None-handling for the derived 'Ord'.
-}
data Pep440Key = Pep440Key
    { p440Epoch :: Integer
    , p440Release :: [Integer]
    , p440Pre :: (Int, Int, Integer)
    {- ^ @(band, stage, n)@. Band __0__ is a dev release with no prerelease and no post, which
    sorts before all prereleases (@1.0.dev1 < 1.0a1@), __1__ a prerelease, __2__ a final or post.
    -}
    , p440Post :: (Int, Integer)
    -- ^ @(0, 0)@ when absent, so a final sorts below any post-release.
    , p440Dev :: (Int, Integer)
    -- ^ @(0, n)@ when present and @(1, 0)@ when absent, so a dev release sorts below its sibling.
    , p440Local :: [VToken]
    }
    deriving stock (Eq, Ord, Show)

{- | Parse a PEP 440 version, canonicalising non-normalised spellings. Fails on anything that
is not one: no release, or unrecognised trailing text.
-}
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
    pure (assembleKey epoch release suffixParts localToks)
  where
    isMainChar c = isAsciiAlphaNum c || c == '.' || c == '!' || c == '-' || c == '_'

-- Split an optional @epoch!@ prefix off the main part: the epoch (0 when
-- absent) and the remainder.
parseEpoch :: Text -> Maybe (Integer, Text)
parseEpoch mainPart = do
    let (epochText, afterEpoch) = case T.breakOn "!" mainPart of
            (e, rest)
                | T.null rest -> ("", mainPart)
                | otherwise -> (e, T.drop 1 rest)
    epoch <- if T.null epochText then pure 0 else parseNumSeg epochText
    pure (epoch, afterEpoch)

-- Consume the leading dotted-numeric release: its segments (at least one) and
-- the unconsumed suffix.
parseRelease :: Text -> Maybe ([Integer], Text)
parseRelease afterEpoch = do
    let (releaseText, suffix) = T.span (\c -> isDigit c || c == '.') afterEpoch
        -- 'releaseText' greedily grabs the dot that separates the release from a suffix
        -- ("1.0.dev1" gives "1.0.", then ["1","0",""]), so drop one trailing empty segment.
        relSegs = dropTrailingEmpty (T.splitOn "." releaseText)
    guard (not (any T.null relSegs))
    release <- traverse parseNumSeg relSegs
    guard (not (null release))
    pure (release, suffix)
  where
    -- Drop at most one trailing empty segment, so a doubled trailing blank
    -- ("1.0..dev1") leaves one behind for the 'any T.null' guard above to reject.
    dropTrailingEmpty segs = case unsnoc segs of
        Just (initSegs, lastSeg) | T.null lastSeg -> initSegs
        _ -> segs

-- Parse the local segment (still carrying its leading @+@) into ordering
-- tokens. Empty input means no local segment.
parseLocal :: Text -> Maybe [VToken]
parseLocal lr
    | T.null lr = Just []
    | otherwise =
        let segs = T.split (`elem` ['.', '-', '_']) (T.drop 1 lr)
         in if all (\s -> not (T.null s) && T.all isAsciiAlphaNum s) segs
                then Just (map classifyRun segs)
                else Nothing

-- Assemble the canonical key: strip the release's trailing zeros and band the
-- suffix parts into the rank tuples documented on 'Pep440Key'.
assembleKey ::
    Integer -> [Integer] -> (Maybe (Int, Integer), Maybe Integer, Maybe Integer) -> [VToken] -> Pep440Key
assembleKey epoch release (mPre, mPost, mDev) localToks =
    Pep440Key
        { p440Epoch = epoch
        , p440Release = stripTrailingZeros release
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
    stripTrailingZeros = dropWhileEnd (== 0)

{- Consume a PEP 440 suffix into its prerelease, post and dev parts. Fails if any text
is left unconsumed, so trailing garbage never parses.
-}
parsePep440Suffix ::
    Text -> Maybe (Maybe (Int, Integer), Maybe Integer, Maybe Integer)
parsePep440Suffix s0 =
    let (pre, s1) = consumePre s0
        (post, s2) = consumePost s1
        (dev, s3) = consumeDev s2
     in if T.null s3 then Just (pre, post, dev) else Nothing

-- Drop one optional separator (@.@\/@-@\/@_@) from the front.
dropSep :: Text -> Text
dropSep s = case T.uncons s of
    Just (c, rest) | c == '.' || c == '-' || c == '_' -> rest
    _ -> s

{- Consume an optional prerelease label into @Just (stage, n)@, with stage
0\/1\/2 for a\/b\/rc. 'Nothing' if absent.
-}
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

{- Consume an optional post-release (@.postN@, @.revN@, @.rN@, or @-N@) into @Just n@. @post@
and @rev@ are tried before the single-letter @r@, so @revN@ never mis-splits as @r@ + @evN@. -}
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

-- Consume an optional dev-release (@.devN@) into @Just n@, or 'Nothing' if absent.
consumeDev :: Text -> (Maybe Integer, Text)
consumeDev s =
    case T.stripPrefix "dev" (dropSep s) of
        Just afterLabel ->
            let (digits, rest) = T.span isDigit (dropSep afterLabel)
             in (Just (numOr0 digits), rest)
        Nothing -> (Nothing, s)

{- | Render a parsed version as Python's @packaging.utils.canonicalize_version@ spells it,
keeping no trailing release zeros so @1.0@ and @1.0.0@ render alike.

>>> renderPep440 <$> parsePep440 "1.0.0"
Just "1"
>>> renderPep440 <$> parsePep440 "1!2.0ALPHA1-1.dev2+Ubuntu.7"
Just "1!2a1.post1.dev2+ubuntu.7"
-}
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

-- The canonical spelling of a prerelease stage, inverting 'consumePre''s rank.
stageLabel :: Int -> Text
stageLabel = \case
    0 -> "a"
    1 -> "b"
    _ -> "rc"

-- A local-segment token in canonical form. 'parseLocal' lowercased it on the way in.
renderToken :: VToken -> Text
renderToken = \case
    VNum n -> show n
    VStr s -> s

{- | Whether a PEP 440 version is stable: neither a pre-release nor a dev release. A
post-release /is/ stable, so @1.0.post1@ is stable and @1.0a1.dev2@ is not.
-}
isPep440Stable :: Pep440Key -> Bool
isPep440Stable k = preBand /= 1 && devBand /= 0
  where
    (preBand, _, _) = p440Pre k
    (devBand, _) = p440Dev k
