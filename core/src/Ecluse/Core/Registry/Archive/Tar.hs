-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE RankNTypes #-}

-- | Complete PAX-aware tar traversal with bounded inventory and selected payload retention.
module Ecluse.Core.Registry.Archive.Tar (
    TarDecode,
    TarMember (..),
    TarScan,
    scanTar,
    tarMembers,
    tarContents,
    resolveTarMember,
    sourceReader,
) where

import Codec.Archive.Tar qualified as Tar
import Conduit
import Control.Monad.Except (throwError)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

import Ecluse.Core.Registry (ParseError (..), parseRefusal)
import Ecluse.Core.Registry.Archive.Path (checkPath, resolveArchivePath)
import Ecluse.Core.Security.Limits (Limits (..))

-- | Decoder refusals remain values while compression libraries retain their IO boundary.
type TarDecode = ExceptT ParseError IO

-- | Archive member kinds retain links without following host filesystem paths.
data TarMember = Regular | Directory | Link Text
    deriving stock (Eq, Show)

-- | A complete bounded inventory and only the selected regular-file payloads.
data TarScan = TarState
    { tsCount :: !Int
    , tsCharge :: !Int
    , tsMembers :: Map Text TarMember
    , tsGlobal :: Map ByteString ByteString
    , tsLocal :: Map ByteString ByteString
    , tsPending :: !Bool
    , tsCaptured :: Map Text ByteString
    }

emptyState :: TarScan
emptyState = TarState 0 0 Map.empty Map.empty Map.empty False Map.empty

-- | Consume the whole archive and retain only explicitly selected regular files.
scanTar :: Limits -> (Int -> IO ()) -> ([Text] -> Either ParseError Bool) -> (forall a. (ConduitT () ByteString TarDecode () -> IO a) -> IO a) -> IO (Either ParseError TarScan)
scanTar limits charge select withSource = do
    result <- withSource $ \source -> runExceptT . runConduit $ source .| boundInflated limits .| walkTar limits charge select emptyState
    pure $ result >>= \scan -> scan <$ validateMembers limits scan

-- | Inventory paths are canonical archive-relative names.
tarMembers :: TarScan -> Map Text TarMember
tarMembers = tsMembers

-- | Missing payloads were not selected or were not regular files.
tarContents :: TarScan -> Map Text ByteString
tarContents = tsCaptured

-- | Resolve an inventory link with the same depth bound used during traversal.
resolveTarMember :: Limits -> TarScan -> Text -> Either ParseError Text
resolveTarMember limits scan = resolveMember limits (tsMembers scan) Set.empty

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

walkTar :: Limits -> (Int -> IO ()) -> ([Text] -> Either ParseError Bool) -> TarScan -> ConduitT ByteString o TarDecode TarScan
walkTar limits charge select tarState = do
    liftIO (charge (tarWorkspace tarState))
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
                Tar.OtherEntryType code _ size | code `elem` ['x', 'g', 'L', 'K'] -> extendedEntry limits charge code size tarState
                content -> ordinaryEntry limits charge select header content tarState
            walkTar limits charge select next{tsCount = tsCount tarState + 1}

readHeader :: ByteString -> Either ParseError Tar.Entry
readHeader header = case Tar.read (BSL.fromStrict header) of
    Tar.Next entry _ -> Right entry
    _ -> parseRefusal "malformed tar header"

takeBytes :: Int -> ConduitT ByteString o TarDecode ByteString
takeBytes size = takeCE size .| foldC

skipPayload :: Integer -> ConduitT ByteString o TarDecode ()
skipPayload = consumePayload (const (pure ())) Nothing

consumePayload :: (Int -> IO ()) -> Maybe (IORef [ByteString]) -> Integer -> ConduitT ByteString o TarDecode ()
consumePayload charge output = go 0 0
  where
    go seen chunks remaining
        | remaining == 0 = pure ()
        | otherwise =
            await >>= \case
                Nothing -> refuse "truncated tar payload"
                Just chunk -> do
                    let count = fromInteger (min remaining (toInteger (BS.length chunk)))
                        (wanted, rest) = BS.splitAt count chunk
                        seen' = seen + count
                        chunks' = chunks + 1
                    for_ output $ \held -> liftIO $ do
                        charge (3 * seen' + 64 * chunks')
                        modifyIORef' held (wanted :)
                    unless (BS.null rest) (leftover rest)
                    go seen' chunks' (remaining - toInteger count)

readPayload :: (Int -> IO ()) -> Int -> Integer -> ConduitT ByteString o TarDecode ByteString
readPayload charge cap size = do
    when (size < 0 || size > toInteger cap) (refuse "tar metadata or extended header exceeds its bound")
    held <- liftIO (newIORef [])
    consumePayload charge (Just held) size
    chunks <- liftIO (readIORef held)
    pure (BS.concat (reverse chunks))

tarWorkspace :: TarScan -> Int
tarWorkspace tarState = 65536 + 2 * tsCharge tarState + attributesWeight (tsGlobal tarState) + attributesWeight (tsLocal tarState) + sum (map BS.length (Map.elems (tsCaptured tarState)))
  where
    attributesWeight = Map.foldlWithKey' (\total key value -> total + 192 + BS.length key + BS.length value) 0

padding :: Integer -> Integer
padding size = (512 - size `mod` 512) `mod` 512

extendedEntry :: Limits -> (Int -> IO ()) -> Char -> Int64 -> TarScan -> ConduitT ByteString o TarDecode TarScan
extendedEntry limits charge code rawSize tarState = do
    let size = toInteger rawSize
    payload <- readPayload (\wanted -> charge (tarWorkspace tarState + wanted)) (min 16384 (maxMetadataBytes limits)) size
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

ordinaryEntry :: Limits -> (Int -> IO ()) -> ([Text] -> Either ParseError Bool) -> ByteString -> Tar.EntryContent -> TarScan -> ConduitT ByteString o TarDecode TarScan
ordinaryEntry limits charge select header content tarState = do
    let attrs = Map.filter (not . BS.null) (Map.union (tsLocal tarState) (tsGlobal tarState))
    path <- checked (utf8 (Map.findWithDefault (headerPath header) "path" attrs))
    parts <- checked (checkPath path)
    when (length parts > maxNestingDepth limits) (refuse "tar path depth exceeds its bound")
    let canonical = T.copy (T.intercalate "/" parts)
    selected <- checked (select parts)
    when (Map.member canonical (tsMembers tarState)) (refuse "duplicate archive entry")
    member <- checked (memberOf canonical attrs header content)
    size <- checked (payloadSize attrs header content)
    when (size > toInteger (maxMirrorArtifactBytes limits)) (refuse "tar entry size exceeds its bound")
    let pathCost = BS.length (TE.encodeUtf8 canonical) + 192 + case member of Link link -> BS.length (TE.encodeUtf8 link); _ -> 0
    when (pathCost > maxMetadataBytes limits - tsCharge tarState) (refuse "archive path inventory exceeds its bound")
    liftIO (charge (tarWorkspace tarState + 2 * pathCost))
    captured <- case member of
        Regular | selected -> Just . BS.copy <$> readPayload (\wanted -> charge (tarWorkspace tarState + 2 * pathCost + wanted)) (maxMetadataBytes limits - sum (map BS.length (Map.elems (tsCaptured tarState)))) size
        _ -> Nothing <$ skipPayload size
    skipPayload (padding size)
    let retained = maybe (tsCaptured tarState) (\bytes -> Map.insert canonical bytes (tsCaptured tarState)) captured
    when (sum (map BS.length (Map.elems retained)) > maxMetadataBytes limits) (refuse "selected archive content exceeds its bound")
    pure
        tarState
            { tsCharge = tsCharge tarState + pathCost
            , tsMembers = Map.insert canonical member (tsMembers tarState)
            , tsLocal = Map.empty
            , tsPending = False
            , tsCaptured = retained
            }

memberOf :: Text -> Map ByteString ByteString -> ByteString -> Tar.EntryContent -> Either ParseError TarMember
memberOf path attrs header = \case
    Tar.NormalFile{} -> Right Regular
    Tar.Directory -> Right Directory
    Tar.SymbolicLink{} -> Link <$> linkTarget True
    Tar.HardLink{} -> Link <$> linkTarget False
    _ -> parseRefusal "unsupported tar device or framing extension"
  where
    linkTarget relative = do
        value <- utf8 (Map.findWithDefault (BS.takeWhile (/= 0) (BS.take 100 (BS.drop 157 header))) "linkpath" attrs)
        resolveArchivePath (if relative then reverse (drop 1 (reverse (T.splitOn "/" path))) else []) value

payloadSize :: Map ByteString ByteString -> ByteString -> Tar.EntryContent -> Either ParseError Integer
payloadSize attrs header content = do
    original <- case content of
        Tar.NormalFile _ size -> Right (toInteger size)
        Tar.OtherEntryType _ _ size -> Right (toInteger size)
        _ -> if zeroSize (BS.take 12 (BS.drop 124 header)) then Right 0 else parseRefusal "non-file tar entry carries a payload"
    size <- maybe (Right original) decimal (Map.lookup "size" attrs)
    when (size < 0) (parseRefusal "negative tar payload size")
    case content of
        Tar.NormalFile{} -> Right size
        _ | size == 0 -> Right 0
        _ -> parseRefusal "non-file tar entry carries a payload"

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
            when (size <= toInteger (BS.length digits + 3) || size > toInteger (BS.length bytes) || BS.null separated) (parseRefusal "truncated pax record")
            let record = BS.take (fromInteger size - BS.length digits - 1) (BS.drop 1 separated)
                (key, value) = BS.break (== 61) record
                payload = BS.take (BS.length value - 2) (BS.drop 1 value)
            unless (BS.isSuffixOf "\n" record && not (BS.null key) && not (BS.null value) && not (BS.any (\c -> c <= 32 || c == 127) key)) (parseRefusal "malformed pax record")
            when (any (`BS.isPrefixOf` key) ["GNU.sparse", "SCHILY.realsize", "SCHILY.filetype"]) (parseRefusal "unsupported pax framing extension")
            when (key == "hdrcharset" && payload /= "UTF-8" && not (BS.null payload)) (parseRefusal "unsupported pax header charset")
            next <-
                if key `elem` ["path", "linkpath", "size"]
                    then do
                        when (Map.member key fields) (parseRefusal "duplicate pax framing attribute")
                        pure (Map.insert key (BS.copy payload) fields)
                    else Right fields
            go next (BS.drop (fromInteger size) bytes)

decimal :: ByteString -> Either ParseError Integer
decimal bytes
    | BS.null bytes || BS.length bytes > 20 || not (BS.all (\c -> c >= 48 && c <= 57) bytes) = parseRefusal "malformed pax decimal"
    | otherwise = Right (BS.foldl' (\n c -> n * 10 + fromIntegral c - 48) 0 bytes)

validateMembers :: Limits -> TarScan -> Either ParseError ()
validateMembers limits tarState =
    for_ (Map.toList (tsMembers tarState)) $ \(path, member) -> do
        for_ (prefixes path) $ \prefix -> case Map.lookup prefix (tsMembers tarState) of
            Just Link{} -> parseRefusal "archive member traverses a link"
            _ -> Right ()
        case member of
            Link _ -> void (resolveTarMember limits tarState path)
            _ -> Right ()

resolveMember :: Limits -> Map Text TarMember -> Set Text -> Text -> Either ParseError Text
resolveMember limits members seen path
    | Set.member path seen = parseRefusal "cyclic tar link"
    | Set.size seen >= maxNestingDepth limits = parseRefusal "tar link chain exceeds its bound"
    | otherwise = case Map.lookup path members of
        Nothing -> case Map.lookupGE (path <> "/") members of
            Just (child, _) | (path <> "/") `T.isPrefixOf` child -> Right path
            _ -> parseRefusal "tar link target is absent"
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

-- | Convert a scoped decoder pull action without hiding synchronous failures or cancellation.
sourceReader :: IO (Either ParseError ByteString) -> ConduitT () ByteString TarDecode ()
sourceReader readChunk = do
    result <- liftIO readChunk
    case result of
        Left fault -> lift (throwError fault)
        Right bytes -> unless (BS.null bytes) (yield bytes >> sourceReader readChunk)
