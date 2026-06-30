{- | The PEP 440 grammar and ordering (PyPI).

Parses a PEP 440 version into a 'Pep440Key' -- the canonical ordering tuple
@(epoch, release, pre, post, dev, local)@ -- canonicalising non-normalised
spellings (@1.0ALPHA1@, @1.0-1@, trailing zeros, …) along the way. Release has
trailing zeros stripped (@1.0 == 1.0.0@), and the rank tuples encode PEP 440's
None-handling so 'Ord' on 'Pep440Key' reproduces the spec ordering directly:

* @p440Pre@ is @(band, stage, n)@ where @band@ is __0__ for a dev release with
  no prerelease and no post (it sorts /before/ all prereleases, e.g.
  @1.0.dev1 < 1.0a1@), __1__ for an actual prerelease (with @stage@ a\/b\/rc and
  its number), and __2__ for a final or post release (sorts after prereleases).
* @p440Post@ is @(0,0)@ when absent, so a final sorts below any post-release.
* @p440Dev@ is @(0,n)@ when present and @(1,0)@ when absent, so a dev release
  sorts below its non-dev sibling.

A PEP 440 version is __stable__ iff it is neither a pre-release (@a@\/@b@\/@rc@)
nor a dev release; post-releases stay stable.
-}
module Ecluse.Core.Version.Pep440 (
    Pep440Key (..),
    parsePep440,
    parsePep440Suffix,
    consumePre,
    consumePost,
    consumeDev,
    dropSep,
    isPep440Stable,
) where

import Data.Char (isDigit)
import Data.List (dropWhileEnd, unsnoc)
import Data.Text qualified as T

import Ecluse.Core.Version.Token (VToken (..), isAsciiAlphaNum, maxVersionLength, numOr0, parseNumSeg)

{- | A parsed PEP 440 version as its canonical ordering key:
@(epoch, release, pre, post, dev, local)@. Release has trailing zeros stripped
(@1.0 == 1.0.0@). The rank tuples encode PEP 440's None-handling:

\* @p440Pre@ is @(band, stage, n)@ where @band@ is __0__ for a dev release with
  no prerelease and no post (it sorts /before/ all prereleases, e.g.
  @1.0.dev1 < 1.0a1@), __1__ for an actual prerelease (with @stage@ a\/b\/rc and
  its number), and __2__ for a final or post release (sorts after prereleases).
\* @p440Post@ is @(0,0)@ when absent, so a final sorts below any post-release.
\* @p440Dev@ is @(0,n)@ when present and @(1,0)@ when absent, so a dev release
  sorts below its non-dev sibling.
-}
data Pep440Key = Pep440Key
    { p440Epoch :: Integer
    , p440Release :: [Integer]
    , p440Pre :: (Int, Int, Integer)
    , p440Post :: (Int, Integer)
    , p440Dev :: (Int, Integer)
    , p440Local :: [VToken]
    }
    deriving stock (Eq, Ord, Show)

{- | Parse a PEP 440 version, canonicalising non-normalised spellings
(@1.0ALPHA1@, @1.0-1@, trailing zeros, …). Fails if the string is not a valid
PEP 440 version (e.g. no release, or unrecognised trailing text).
-}
parsePep440 :: Text -> Maybe Pep440Key
parsePep440 raw = do
    -- Bound the input length before any numeric parsing: a segment is read into an
    -- 'Integer' with 'readMaybe', which is quadratic in the digit count, so an
    -- unbounded run in hostile metadata would be an algorithmic-complexity DoS.
    guard (T.compareLength raw maxVersionLength /= GT)
    let lowered = T.toLower (T.strip raw)
        noV = fromMaybe lowered (T.stripPrefix "v" lowered)
        (mainPart, localRaw) = T.breakOn "+" noV
    guard (T.all isMainChar mainPart)
    let (epochText, afterEpoch) = case T.breakOn "!" mainPart of
            (e, rest)
                | T.null rest -> ("", mainPart)
                | otherwise -> (e, T.drop 1 rest)
    epoch <- if T.null epochText then pure 0 else parseNumSeg epochText
    let (releaseText, suffix) = T.span (\c -> isDigit c || c == '.') afterEpoch
        -- 'releaseText' greedily grabs the dot that separates the release from a
        -- suffix ("1.0.dev1" → "1.0." → ["1","0",""]), so drop *one* trailing
        -- empty segment -- but only one, and reject any remaining empty segment so
        -- interior/leading blanks ("1..0", ".1.0", "1.0..dev1") are not accepted.
        relSegs = dropTrailingEmpty (T.splitOn "." releaseText)
    guard (not (any T.null relSegs))
    release <- traverse parseNumSeg relSegs
    guard (not (null release))
    (mPre, mPost, mDev) <- parsePep440Suffix suffix
    localToks <- parseLocal localRaw
    let pre = case mPre of
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
    pure
        Pep440Key
            { p440Epoch = epoch
            , p440Release = stripTrailingZeros release
            , p440Pre = pre
            , p440Post = post
            , p440Dev = dev
            , p440Local = localToks
            }
  where
    isMainChar c = isAsciiAlphaNum c || c == '.' || c == '!' || c == '-' || c == '_'
    -- Drop at most one trailing empty segment (the release/suffix separator dot).
    -- Only the final segment is dropped, so a doubled trailing blank ("1.0..dev1")
    -- leaves an empty segment behind for the 'any T.null' guard above to reject.
    dropTrailingEmpty segs = case unsnoc segs of
        Just (initSegs, lastSeg) | T.null lastSeg -> initSegs
        _ -> segs
    stripTrailingZeros = dropWhileEnd (== 0)
    parseLocal lr
        | T.null lr = Just []
        | otherwise =
            let segs = T.split (`elem` ['.', '-', '_']) (T.drop 1 lr)
             in if all (\s -> not (T.null s) && T.all isAsciiAlphaNum s) segs
                    then Just (map localTok segs)
                    else Nothing
    localTok s = if T.all isDigit s then VNum (numOr0 s) else VStr s

{- | Consume a PEP 440 suffix into its prerelease\/post\/dev parts (each absent
or present), failing if any text is left unconsumed (so trailing garbage is
rejected). The banding into a sort key happens in 'parsePep440'.
-}
parsePep440Suffix ::
    Text -> Maybe (Maybe (Int, Integer), Maybe Integer, Maybe Integer)
parsePep440Suffix s0 =
    let (pre, s1) = consumePre s0
        (post, s2) = consumePost s1
        (dev, s3) = consumeDev s2
     in if T.null s3 then Just (pre, post, dev) else Nothing

-- | Drop one optional separator (@.@\/@-@\/@_@) from the front.
dropSep :: Text -> Text
dropSep s = case T.uncons s of
    Just (c, rest) | c == '.' || c == '-' || c == '_' -> rest
    _ -> s

{- | Consume an optional prerelease label into @Just (stage, n)@ (stage 0\/1\/2
for a\/b\/rc); 'Nothing' if absent.
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

{- | Consume an optional post-release (@.postN@, @.revN@, or @-N@) into @Just n@;
'Nothing' if absent.
-}
consumePost :: Text -> (Maybe Integer, Text)
consumePost s =
    case asum (map (\lbl -> T.stripPrefix lbl (dropSep s)) ["post", "rev"]) of
        Just afterLabel ->
            let (digits, rest) = T.span isDigit (dropSep afterLabel)
             in (Just (numOr0 digits), rest)
        Nothing -> case T.stripPrefix "-" s of
            Just afterDash ->
                let (digits, rest) = T.span isDigit afterDash
                 in if T.null digits then (Nothing, s) else (Just (numOr0 digits), rest)
            Nothing -> (Nothing, s)

-- | Consume an optional dev-release (@.devN@) into @Just n@; 'Nothing' if absent.
consumeDev :: Text -> (Maybe Integer, Text)
consumeDev s =
    case T.stripPrefix "dev" (dropSep s) of
        Just afterLabel ->
            let (digits, rest) = T.span isDigit (dropSep afterLabel)
             in (Just (numOr0 digits), rest)
        Nothing -> (Nothing, s)

{- | Whether a PEP 440 version is stable: neither a pre-release (@a@\/@b@\/@rc@)
nor a dev release. Post-releases /are/ stable. So @1.0@ and @1.0.post1@ are
stable; @1.0a1@, @1.0rc1@, @1.0.dev1@ and @1.0a1.dev2@ are not.
-}
isPep440Stable :: Pep440Key -> Bool
isPep440Stable k = noPre k && noDev k
  where
    -- Final/post: no prerelease band (1) and no dev band (0). The field
    -- semantics are documented on 'Pep440Key'; post-releases stay stable.
    noPre key = case p440Pre key of (band, _, _) -> band /= 1
    noDev key = case p440Dev key of (band, _) -> band /= 0
