-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | PEP 440 behaviour against the frozen parser. VersionSpec retains the external oracle fixture.
module Ecluse.Core.Version.Pep440Spec (spec) where

import Data.Char (isDigit)
import Data.List (dropWhileEnd, unsnoc)
import Data.Text qualified as T
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Text (isAsciiAlphaNum)
import Ecluse.Core.Version (canonicalPep440, compareVersions, isStable, mkVersion, parseVersionKey, renderVersion)
import Ecluse.Core.Version.Token (VToken (VNum, VStr), classifyRun, numOr0, parseNumSeg, withinVersionLength)
import Ecluse.Test.Version (genPyPI)

-- | Preserve complete keys, refusals, rendering and ordering against the base parser.
spec :: Spec
spec = describe "PEP 440 against the frozen parser at 1a53a01" $ do
    it "keeps complete keys, refusals, rendering and stability for deterministic generated inputs" $
        take 10 [raw | raw <- pep440Cases, pep440Snapshot raw /= frozenSnapshot raw] `shouldBe` []
    modifyMaxSuccess (const 2000) $
        it "keeps ordering for generated valid and malformed pairs" $
            hedgehog $ do
                (a, b) <- forAll ((,) <$> Gen.choice [genPyPI, Gen.element pep440EdgeCases] <*> Gen.choice [genPyPI, Gen.element pep440EdgeCases])
                compareVersions (mkVersion PyPI a) (mkVersion PyPI b)
                    === (compare <$> frozenParsePep440 a <*> frozenParsePep440 b)
    it "keeps the existing canonical rendering examples" $ do
        renderVersion <$> canonicalPep440 "1.0.0" `shouldBe` Just "1"
        renderVersion <$> canonicalPep440 "1!2.0ALPHA1-1.dev2+Ubuntu.7"
            `shouldBe` Just "1!2a1.post1.dev2+ubuntu.7"
    it "keeps bare trailing dots and empty epochs accepted" $ do
        for_ ["1.", "1.0.", "!1", "v!1"] $ \raw ->
            parseVersionKey PyPI raw `shouldSatisfy` isRight
        for_ [".", "1..", "1..0", "1.0..dev1", "1!!1"] $ \raw ->
            parseVersionKey PyPI raw `shouldSatisfy` isLeft

-- The grammar is private. Its public Show instance exposes every key field without widening the API.
pep440Snapshot :: Text -> (Maybe Text, Maybe Text, Maybe Bool)
pep440Snapshot raw =
    ( show <$> key
    , renderVersion <$> canonicalPep440 raw
    , isStable <$> key
    )
  where
    key = rightToMaybe (parseVersionKey PyPI raw)

frozenSnapshot :: Text -> (Maybe Text, Maybe Text, Maybe Bool)
frozenSnapshot raw =
    ( (\key -> "PyPIKey (" <> show key <> ")") <$> parsed
    , frozenRenderPep440 <$> parsed
    , frozenIsPep440Stable <$> parsed
    )
  where
    parsed = frozenParsePep440 raw

pep440Cases :: [Text]
pep440Cases =
    [ prefix <> release <> suffix <> localPart
    | prefix <- ["", "v", "V", "1!", "01!", "!"]
    , release <- ["", "0", "0.0", "01", "1.0", "1.0.2", ".1", "1..0", "1.", "1.."]
    , suffix <- pep440SuffixCases
    , localPart <- ["", "+Ubuntu.07", "+1-a_b", "+", "+a..1"]
    ]
        <> pep440EdgeCases

pep440SuffixCases :: [Text]
pep440SuffixCases =
    [ separator <> label <> number
    | separator <- ["", ".", "-", "_"]
    , label <- ["a", "alpha", "b", "beta", "c", "rc", "pre", "preview", "post", "rev", "r", "dev"]
    , number <- ["", "01"]
    ]
        <> ["", "ALPHA1", "-1", ".rc_1.post-2.dev_3", ".post1.dev2", ".dev1.post2", "..dev1", "-", "_"]

pep440EdgeCases :: [Text]
pep440EdgeCases =
    [ lead <> T.replicate (total - T.length lead - T.length tailPart) "9" <> tailPart
    | total <- [1023, 1024, 1025]
    , lead <- ["", "1!", "v", "1.dev", "1+a"]
    , tailPart <- ["", ".", ".0", "+A"]
    ]
        <> [T.replicate 512 "1.", T.replicate 513 "1.", "1" <> T.replicate 511 ".0"]
        <> [front <> T.singleton c <> back | (front, back) <- [("", "1"), ("1", ".0"), ("1+", "a"), ("1a", "1"), ("1", "")], c <- characters]
        <> [space <> raw <> space | space <- [" ", "\t", "\r\n", "\xA0", "\x2003", "\x3000"], raw <- ["V01.0ALPHA1", "!1", "1..", "1.0+Ubuntu.7"]]
        <> ["", "v", "vv1", "!", "1!!1", "1!", "x!1", "1+", "1++a", "1+a+1", "1+a.", "1+a-", "1+_a", "1.0..dev1"]
  where
    characters = ['\0', '\x1B', '\x301', '\xE9', '\xDF', '\x130', '\x212A', '\x663', '\xB2', '\xFF11', '\x10400', '\x1D7CE']

-- Frozen from core/src/Ecluse/Core/Version/Pep440.hs at 1a53a01b0480ae61f486a6ec1eda0a27cb646181.
-- Only comments and the three entry point names differ from that parser.
data Pep440Key = Pep440Key
    { p440Epoch :: Integer
    , p440Release :: [Integer]
    , p440Pre :: (Int, Int, Integer)
    , p440Post :: (Int, Integer)
    , p440Dev :: (Int, Integer)
    , p440Local :: [VToken]
    }
    deriving stock (Eq, Ord, Show, Generic)

instance NFData Pep440Key

frozenParsePep440 :: Text -> Maybe Pep440Key
frozenParsePep440 raw = do
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

parseRelease :: Text -> Maybe ([Integer], Text)
parseRelease afterEpoch = do
    let (releaseText, suffix) = T.span (\c -> isDigit c || c == '.') afterEpoch

        relSegs = dropTrailingEmpty (T.splitOn "." releaseText)
    guard (not (any T.null relSegs))
    release <- traverse parseNumSeg relSegs
    guard (not (null release))
    pure (release, suffix)
  where
    dropTrailingEmpty segs = case unsnoc segs of
        Just (initSegs, lastSeg) | T.null lastSeg -> initSegs
        _ -> segs

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

frozenRenderPep440 :: Pep440Key -> Text
frozenRenderPep440 k =
    epoch <> release <> pre <> post <> dev <> localSegment
  where
    epoch = case p440Epoch k of
        0 -> ""
        n -> show n <> "!"

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

renderToken :: VToken -> Text
renderToken = \case
    VNum n -> show n
    VStr s -> s

frozenIsPep440Stable :: Pep440Key -> Bool
frozenIsPep440Stable k = preBand /= 1 && devBand /= 0
  where
    (preBand, _, _) = p440Pre k
    (devBand, _) = p440Dev k
