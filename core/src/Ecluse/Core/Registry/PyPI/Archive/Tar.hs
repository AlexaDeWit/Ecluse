-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Stream PAX-aware tar members and resolve metadata links inside the bounded archive index.
module Ecluse.Core.Registry.PyPI.Archive.Tar (TarDecode, extractTar) where

import Codec.Archive.Tar qualified as Tar
import Conduit
import Control.Monad (foldM)
import Control.Monad.Except (throwError)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.PyPI.Archive.Path (checkPath, metadataPath)
import Ecluse.Core.Registry.PyPI.Project (DistributionKind (Sdist))
import Ecluse.Core.Security.Limits (Limits (..))
import Ecluse.Core.Version (Version)

-- | Decoder refusals remain values while compression libraries retain their IO boundary.
type TarDecode = ExceptT ParseError IO

data Member = Regular | Directory | Link Text

data TarState = TarState
    { tsCount :: !Int
    , tsCharge :: !Int
    , tsMembers :: Map Text Member
    , tsGlobal :: Map ByteString ByteString
    , tsLocal :: Map ByteString ByteString
    , tsPending :: !Bool
    , tsMetadata :: Maybe Text
    , tsCaptured :: Maybe ByteString
    }

emptyState :: TarState
emptyState = TarState 0 0 Map.empty Map.empty Map.empty False Nothing Nothing

-- | A metadata link can require one more local decoding pass, never another download or retained unrelated payload.
extractTar :: Limits -> PackageName -> Version -> ConduitT () ByteString TarDecode () -> IO (Either ParseError ByteString)
extractTar limits package version source = do
    firstPass <- runPass Nothing
    case firstPass >>= resolveMetadata limits of
        Left fault -> pure (Left fault)
        Right (Left bytes) -> pure (Right bytes)
        Right (Right target) -> do
            secondPass <- runPass (Just target)
            pure $ secondPass >>= maybe (bad "metadata link target has no regular-file content") Right . tsCaptured
  where
    runPass target = runExceptT . runConduit $ source .| boundInflated limits .| walkTar limits package version target emptyState

boundInflated :: Limits -> ConduitT ByteString ByteString TarDecode ()
boundInflated limits = go 0
  where
    go seen =
        await >>= \case
            Nothing -> pure ()
            Just chunk -> do
                let next = seen + toInteger (BS.length chunk)
                when (next > toInteger (maxMirrorArtifactBytes limits)) (refuse "archive inflated bytes exceed their bound")
                yield chunk
                go next

walkTar :: Limits -> PackageName -> Version -> Maybe Text -> TarState -> ConduitT ByteString o TarDecode TarState
walkTar limits package version target tarState = do
    header <- takeBytes 512
    when (BS.length header /= 512) (refuse "tar archive has no complete trailer")
    if BS.all (== 0) header
        then do
            trailer <- takeBytes 512
            unless (BS.length trailer == 512 && BS.all (== 0) trailer) (refuse "invalid tar trailer")
            awaitForever $ \chunk -> unless (BS.all (== 0) chunk) (refuse "tar archive has trailing nonzero bytes")
            when (tsPending tarState) (refuse "orphan extended tar header")
            pure tarState
        else do
            when (tsCount tarState >= maxArtifactCount limits) (refuse "archive entry count exceeds its bound")
            entry <- checked (readHeader header)
            next <- case Tar.entryContent entry of
                Tar.OtherEntryType code _ size | code `elem` ['x', 'g', 'L', 'K'] -> extendedEntry limits code size tarState
                content -> ordinaryEntry limits package version target header content tarState
            walkTar limits package version target next{tsCount = tsCount tarState + 1}

readHeader :: ByteString -> Either ParseError Tar.Entry
readHeader header = case Tar.read (BSL.fromStrict header) of
    Tar.Next entry _ -> Right entry
    _ -> bad "malformed tar header"

takeBytes :: Int -> ConduitT ByteString o TarDecode ByteString
takeBytes size = takeCE size .| foldC

skipPayload :: Integer -> ConduitT ByteString o TarDecode ()
skipPayload = consumePayload Nothing

consumePayload :: Maybe (IORef [ByteString]) -> Integer -> ConduitT ByteString o TarDecode ()
consumePayload output = go
  where
    go remaining
        | remaining == 0 = pure ()
        | otherwise =
            await >>= \case
                Nothing -> refuse "truncated tar payload"
                Just chunk -> do
                    let count = fromInteger (min remaining (toInteger (BS.length chunk)))
                        (wanted, rest) = BS.splitAt count chunk
                    for_ output $ \held -> liftIO (modifyIORef' held (wanted :))
                    unless (BS.null rest) (leftover rest)
                    go (remaining - toInteger count)

readPayload :: Int -> Integer -> ConduitT ByteString o TarDecode ByteString
readPayload cap size = do
    when (size < 0 || size > toInteger cap) (refuse "tar metadata or extended header exceeds its bound")
    held <- liftIO (newIORef [])
    consumePayload (Just held) size
    chunks <- liftIO (readIORef held)
    pure (BS.concat (reverse chunks))

padding :: Integer -> Integer
padding size = (512 - size `mod` 512) `mod` 512

extendedEntry :: Limits -> Char -> Int64 -> TarState -> ConduitT ByteString o TarDecode TarState
extendedEntry limits code rawSize tarState = do
    let size = toInteger rawSize
    payload <- readPayload (min 16384 (maxMetadataBytes limits)) size
    skipPayload (padding size)
    values <-
        checked $
            if code `elem` ['x', 'g']
                then parsePax payload
                else do
                    let value = BS.takeWhile (/= 0) payload
                    pure (Map.singleton (if code == 'L' then "path" else "linkpath") value)
    if code == 'g'
        then pure tarState{tsGlobal = Map.filter (not . BS.null) (Map.union values (tsGlobal tarState))}
        else do
            unless (Map.null (Map.intersection values (tsLocal tarState))) (refuse "ambiguous extended tar headers")
            pure tarState{tsLocal = Map.union values (tsLocal tarState), tsPending = True}

ordinaryEntry :: Limits -> PackageName -> Version -> Maybe Text -> ByteString -> Tar.EntryContent -> TarState -> ConduitT ByteString o TarDecode TarState
ordinaryEntry limits package version target header content tarState = do
    let attrs = Map.filter (not . BS.null) (Map.union (tsLocal tarState) (tsGlobal tarState))
    path <- checked (utf8 (Map.findWithDefault (headerPath header) "path" attrs))
    parts <- checked (checkPath path)
    when (length parts > maxNestingDepth limits) (refuse "tar path depth exceeds its bound")
    let canonical = T.copy (T.intercalate "/" parts)
    selected <- checked (metadataPath package version Sdist parts)
    when (Map.member canonical (tsMembers tarState)) (refuse "duplicate archive entry")
    when (selected && isJust (tsMetadata tarState)) (refuse "ambiguous archive Core Metadata")
    member <- checked (memberOf canonical attrs header content)
    size <- checked (payloadSize attrs header content)
    when (size > toInteger (maxMirrorArtifactBytes limits)) (refuse "tar entry size exceeds its bound")
    let charge = BS.length (TE.encodeUtf8 canonical) + 192 + case member of Link link -> BS.length (TE.encodeUtf8 link); _ -> 0
    when (charge > maxMetadataBytes limits - tsCharge tarState) (refuse "archive path inventory exceeds its bound")
    captured <- case member of
        Regular | maybe selected (== canonical) target -> Just . BS.copy <$> readPayload (maxMetadataBytes limits) size
        _ -> Nothing <$ skipPayload size
    skipPayload (padding size)
    pure
        tarState
            { tsCharge = tsCharge tarState + charge
            , tsMembers = Map.insert canonical member (tsMembers tarState)
            , tsLocal = Map.empty
            , tsPending = False
            , tsMetadata = if selected then Just canonical else tsMetadata tarState
            , tsCaptured = captured <|> tsCaptured tarState
            }

memberOf :: Text -> Map ByteString ByteString -> ByteString -> Tar.EntryContent -> Either ParseError Member
memberOf path attrs header = \case
    Tar.NormalFile{} -> Right Regular
    Tar.Directory -> Right Directory
    Tar.SymbolicLink{} -> Link <$> linkTarget True
    Tar.HardLink{} -> Link <$> linkTarget False
    _ -> bad "unsupported tar device or framing extension"
  where
    linkTarget relative = do
        value <- utf8 (Map.findWithDefault (BS.takeWhile (/= 0) (BS.take 100 (BS.drop 157 header))) "linkpath" attrs)
        normaliseLink (if relative then reverse (drop 1 (reverse (T.splitOn "/" path))) else []) value

normaliseLink :: [Text] -> Text -> Either ParseError Text
normaliseLink base target = do
    when (T.null target || T.isPrefixOf "/" target || T.length target > 4096 || T.any (\c -> c < ' ' || c == '\\' || c == ':' || c == '\DEL') target) (bad "unsafe tar link target")
    parts <- reverse <$> foldM step (reverse base) (T.splitOn "/" target)
    void (checkPath (T.intercalate "/" parts))
    pure (T.copy (T.intercalate "/" parts))
  where
    step held "" = Right held
    step held "." = Right held
    step [] ".." = bad "tar link escapes its archive"
    step (_ : rest) ".." = Right rest
    step held part = Right (part : held)

payloadSize :: Map ByteString ByteString -> ByteString -> Tar.EntryContent -> Either ParseError Integer
payloadSize attrs header content = do
    original <- case content of
        Tar.NormalFile _ size -> Right (toInteger size)
        Tar.OtherEntryType _ _ size -> Right (toInteger size)
        _ -> if zeroSize (BS.take 12 (BS.drop 124 header)) then Right 0 else bad "non-file tar entry carries a payload"
    size <- maybe (Right original) decimal (Map.lookup "size" attrs)
    when (size < 0) (bad "negative tar payload size")
    case content of
        Tar.NormalFile{} -> Right size
        _ | size == 0 -> Right 0
        _ -> bad "non-file tar entry carries a payload"

zeroSize :: ByteString -> Bool
zeroSize bytes = BS.all (`elem` [0, 32, 48]) bytes || bytes == BS.cons 128 (BS.replicate 11 0)

headerPath :: ByteString -> ByteString
headerPath header = if BS.null prefix then suffix else prefix <> "/" <> suffix
  where
    prefix = BS.takeWhile (/= 0) (BS.take 155 (BS.drop 345 header))
    suffix = BS.takeWhile (/= 0) (BS.take 100 header)

parsePax :: ByteString -> Either ParseError (Map ByteString ByteString)
parsePax = go Map.empty
  where
    go fields bytes
        | BS.null bytes = Right fields
        | otherwise = do
            let (digits, separated) = BS.break (== 32) bytes
            size <- decimal digits
            when (size <= toInteger (BS.length digits + 3) || size > toInteger (BS.length bytes) || BS.null separated) (bad "truncated pax record")
            let record = BS.take (fromInteger size - BS.length digits - 1) (BS.drop 1 separated)
                (key, value) = BS.break (== 61) record
                payload = BS.take (BS.length value - 2) (BS.drop 1 value)
            unless (BS.isSuffixOf "\n" record && not (BS.null key) && not (BS.null value) && not (BS.any (\c -> c <= 32 || c == 127) key)) (bad "malformed pax record")
            when (any (`BS.isPrefixOf` key) ["GNU.sparse", "SCHILY.realsize", "SCHILY.filetype"]) (bad "unsupported pax framing extension")
            when (key == "hdrcharset" && payload /= "UTF-8" && not (BS.null payload)) (bad "unsupported pax header charset")
            next <-
                if key `elem` ["path", "linkpath", "size"]
                    then do
                        when (Map.member key fields) (bad "duplicate pax framing attribute")
                        pure (Map.insert key payload fields)
                    else Right fields
            go next (BS.drop (fromInteger size) bytes)

decimal :: ByteString -> Either ParseError Integer
decimal bytes
    | BS.null bytes || BS.length bytes > 20 || not (BS.all (\c -> c >= 48 && c <= 57) bytes) = bad "malformed pax decimal"
    | otherwise = Right (BS.foldl' (\n c -> n * 10 + fromIntegral c - 48) 0 bytes)

resolveMetadata :: Limits -> TarState -> Either ParseError (Either ByteString Text)
resolveMetadata limits tarState = do
    for_ (Map.toList (tsMembers tarState)) $ \(path, member) -> do
        for_ (prefixes path) $ \prefix -> case Map.lookup prefix (tsMembers tarState) of
            Just Link{} -> bad "archive member traverses a link"
            _ -> Right ()
        case member of
            Link _ -> void (resolveMember limits (tsMembers tarState) Set.empty path)
            _ -> Right ()
    path <- maybe (bad "archive has no Core Metadata") Right (tsMetadata tarState)
    target <- resolveMember limits (tsMembers tarState) Set.empty path
    case Map.lookup target (tsMembers tarState) of
        Just Regular -> pure (maybe (Right target) Left (tsCaptured tarState))
        _ -> bad "Core Metadata link does not name a regular file"

resolveMember :: Limits -> Map Text Member -> Set Text -> Text -> Either ParseError Text
resolveMember limits members seen path
    | Set.member path seen = bad "cyclic tar link"
    | Set.size seen >= maxNestingDepth limits = bad "tar link chain exceeds its bound"
    | otherwise = case Map.lookup path members of
        Nothing -> case Map.lookupGE (path <> "/") members of
            Just (child, _) | (path <> "/") `T.isPrefixOf` child -> Right path
            _ -> bad "tar link target is absent"
        Just (Link target) -> resolveMember limits members (Set.insert path seen) target
        Just _ -> Right path

prefixes :: Text -> [Text]
prefixes path = map (T.intercalate "/") (drop 1 (scanl (\held part -> held <> [part]) [] (reverse (drop 1 (reverse (T.splitOn "/" path))))))

utf8 :: ByteString -> Either ParseError Text
utf8 = first (const (ParseError "archive path is not UTF-8")) . TE.decodeUtf8'

checked :: Either ParseError a -> ConduitT i o TarDecode a
checked = either (lift . throwError) pure

refuse :: Text -> ConduitT i o TarDecode a
refuse = lift . throwError . ParseError

bad :: Text -> Either ParseError a
bad = Left . ParseError
