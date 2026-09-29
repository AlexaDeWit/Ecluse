-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Generated registry bodies for differential reader tests. Most are well formed, and each can
carry escapes of every code unit, lone surrogates, invalid and overlong UTF-8, raw control bytes
from 0x00, deep nesting, long and malformed numbers, exponents past 'Int', duplicate keys, the
separators json-stream's lexer skips, and truncation or stray bytes.
-}
module Ecluse.Test.Registry.JsonBytes (
    genJsonBytes,
    genPackumentBytes,
    genServablePackumentBytes,
    genSimpleIndexBytes,
    damaged,
    genChunks,
    releaseKeys,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Ecluse.Test.Package (validSha1, validSha512Sri)

-- | Any JSON value, with member names drawn from the given pool as often as not.
genJsonBytes :: [ByteString] -> Gen ByteString
genJsonBytes names = render <$> Gen.frequency [(8, genValue names 4), (1, genDeep names)]

-- | An npm packument: its name, versions, times and tags in any order, and other members besides.
genPackumentBytes :: Gen ByteString
genPackumentBytes = render <$> objectOf (Gen.list (Range.linear 0 7) member)
  where
    member =
        Gen.frequency
            [ (2, (,) (quoted "name") <$> Gen.frequency [(4, genString), (1, genValue releaseFields 1)])
            , (5, (,) (quoted "versions") <$> container (objectOf (Gen.list (Range.linear 0 5) release)))
            , (3, (,) (quoted "time") <$> container (objectOf (Gen.list (Range.linear 0 5) ((,) <$> versionKey <*> timestamp))))
            , (3, (,) (quoted "dist-tags") <$> container (objectOf (Gen.list (Range.linear 0 3) ((,) <$> genKey ["latest", "next"] <*> Gen.frequency [(4, quoted . Builder.byteString <$> Gen.element releaseKeys), (1, genValue [] 1)]))))
            , (1, (,) <$> genKey ["description", "readme", "users"] <*> genValue releaseFields 3)
            ]
    container object' = Gen.frequency [(8, object'), (1, pure "null"), (1, genValue [] 2)]
    release = (,) <$> versionKey <*> Gen.frequency [(8, objectOf (Gen.list (Range.linear 0 12) field)), (1, genValue releaseFields 2)]
    field = do
        name <- Gen.element releaseFields
        (,) (quoted (Builder.byteString name)) <$> Gen.frequency [(3, fieldValue name), (1, genValue releaseFields 4)]
    fieldValue name = case name of
        "dist" -> objectOf (Gen.list (Range.linear 0 6) ((,) <$> genKey ["tarball", "shasum", "integrity", "signatures", "attestations", "fileCount", "unpackedSize"] <*> genValue ["keyid", "sig", "url", "provenance", "predicateType"] 3))
        "_npmUser" -> Gen.frequency [(1, genString), (2, objectOf (Gen.list (Range.linear 0 3) ((,) <$> genKey ["name", "email", "url"] <*> genScalar)))]
        "dependencies" -> objectOf (Gen.list (Range.linear 0 4) ((,) <$> genKey ["dep", "other", "tarball"] <*> genScalar))
        _ -> genValue releaseFields 3
    timestamp = Gen.frequency [(4, pure "\"2026-05-14T19:25:26.000Z\""), (1, genScalar)]
    versionKey = Gen.frequency [(4, quoted . Builder.byteString <$> Gen.element releaseKeys), (1, genKey ["created", "modified"]), (1, genString)]

{- | An npm packument for @thing@ whose releases carry what installation reads among hostile members,
tarball URLs with escapes, queries and fragments, and times and tags, so a listing from it serves.
-}
genServablePackumentBytes :: Gen ByteString
genServablePackumentBytes = do
    versions <- objectOf (Gen.list (Range.linear 1 6) release)
    time <- objectOf (Gen.list (Range.linear 0 4) ((,) <$> versionKey <*> Gen.frequency [(4, pure "\"2026-05-14T19:25:26.000Z\""), (1, genScalar)]))
    tags <- objectOf (Gen.list (Range.linear 0 2) ((,) <$> genKey ["latest", "next"] <*> versionKey))
    others <- Gen.list (Range.linear 0 3) ((,) <$> genKey ["description", "readme", "users", "author"] <*> genValue releaseFields 3)
    members <- Gen.shuffle ([(quoted "name", quoted "thing"), (quoted "versions", versions), (quoted "time", time), (quoted "dist-tags", tags)] <> others)
    render <$> objectOf (pure members)
  where
    versionKey = quoted . Builder.byteString <$> Gen.element releaseKeys
    release = do
        key <- Gen.element releaseKeys
        file <- Gen.frequency [(4, pure ("thing-" <> Builder.byteString key <> ".tgz")), (3, mconcat <$> Gen.list (Range.linear 1 4) (Gen.element filePieces))]
        extraDist <- Gen.list (Range.linear 0 2) ((,) <$> genKey ["signatures", "fileCount", "unpackedSize"] <*> genValue [] 2)
        dist <- objectOf (Gen.shuffle ([(quoted "tarball", quoted ("https://registry.npmjs.org/thing/-/" <> file)), (quoted "integrity", quoted (Builder.byteString (encodeUtf8 validSha512Sri))), (quoted "shasum", quoted (Builder.byteString (encodeUtf8 validSha1)))] <> extraDist))
        extra <- Gen.list (Range.linear 0 4) ((,) <$> genKey ["description", "author", "dependencies", "gitHead", "readme", "deprecated", "bin", "dist"] <*> genValue releaseFields 3)
        fields <- Gen.shuffle ([(quoted "name", quoted "thing"), (quoted "version", quoted (Builder.byteString key)), (quoted "dist", dist)] <> extra)
        (,) (quoted (Builder.byteString key)) <$> objectOf (pure fields)
    filePieces = ["thing-1.0.0.tgz", "\\u0041", "%2F", "?q=1", "#frag", "\\/", "\\u002f", "\\n", "\\\"", "\xe9", "..", "\\u00e9"]

-- | A PyPI Simple index for the project @thing@, with files for several releases and other members.
genSimpleIndexBytes :: Gen ByteString
genSimpleIndexBytes = render <$> objectOf (Gen.list (Range.linear 0 7) member)
  where
    member =
        Gen.frequency
            [ (2, (,) (quoted "name") <$> Gen.frequency [(4, pure "\"thing\""), (1, genValue [] 1)])
            , (2, (,) (quoted "meta") <$> Gen.frequency [(4, objectOf (Gen.list (Range.linear 0 3) ((,) <$> genKey ["api-version", "_last-serial", "tracks"] <*> Gen.frequency [(3, pure "\"1.1\""), (1, genValue [] 2)]))), (1, genValue [] 2)])
            , (5, (,) (quoted "files") <$> Gen.frequency [(8, arrayOf (Gen.list (Range.linear 0 6) file)), (1, pure "null"), (1, genValue [] 2)])
            , (2, (,) (quoted "versions") <$> Gen.frequency [(4, arrayOf (Gen.list (Range.linear 0 4) (Gen.frequency [(4, quoted . Builder.byteString <$> Gen.element releaseKeys), (1, genScalar)]))), (1, genValue [] 2)])
            , (1, (,) <$> genKey ["project-status", "alternate-locations"] <*> genValue ["status", "reason"] 2)
            , (1, (,) <$> genKey ["other"] <*> genValue [] 3)
            ]
    file = Gen.frequency [(8, objectOf (Gen.list (Range.linear 0 9) fileMember)), (1, genValue [] 2)]
    fileMember =
        Gen.frequency
            [ (3, (,) (quoted "filename") <$> Gen.frequency [(5, quoted <$> filename), (1, genScalar)])
            , (2, (,) (quoted "hashes") <$> Gen.frequency [(4, objectOf (Gen.list (Range.linear 0 3) ((,) <$> genKey ["sha256", "md5", "custom"] <*> genScalar))), (1, genValue [] 2)])
            , (4, (,) <$> genKey ["url", "requires-python", "size", "upload-time", "yanked", "provenance", "core-metadata", "data-dist-info-metadata"] <*> Gen.frequency [(4, genScalar), (1, genValue [] 2)])
            ]
    filename = do
        version <- Gen.element releaseKeys
        project <- Gen.element ["thing", "Thing", "other"]
        suffix <- Gen.element [".tar.gz", "-py3-none-any.whl", ".zip", "-cp312-cp312-manylinux_2_17_x86_64.whl", ".exe"]
        pure (project <> "-" <> Builder.byteString version <> suffix)

-- | The release keys generated bodies use, so a selected read finds its release as often as not.
releaseKeys :: [ByteString]
releaseKeys = ["1.0.0", "2.0.0-beta.1", "1.0.0rc1", "3.1"]

releaseFields :: [ByteString]
releaseFields =
    [ "name"
    , "version"
    , "dist"
    , "deprecated"
    , "hasInstallScript"
    , "scripts"
    , "license"
    , "_npmUser"
    , "dependencies"
    , "devDependencies"
    , "peerDependenciesMeta"
    , "directories"
    , "devEngines"
    , "publishConfig"
    , "workspaces"
    , "exports"
    , "bin"
    , "engines"
    , "os"
    , "gitHead"
    , "description"
    , "author"
    , "url"
    , "tarball"
    ]

-- | The body as generated, or truncated, with a stray byte, or with trailing bytes.
damaged :: ByteString -> Gen ByteString
damaged body =
    Gen.frequency
        [ (12, pure body)
        , (2, (`BS.take` body) <$> Gen.int (Range.linear 0 (BS.length body)))
        , (1, (\position byte -> BS.take position body <> BS.singleton byte <> BS.drop position body) <$> Gen.int (Range.linear 0 (BS.length body)) <*> Gen.word8 Range.linearBounded)
        , (1, (\position byte -> BS.take position body <> BS.singleton byte <> BS.drop (position + 1) body) <$> Gen.int (Range.linear 0 (BS.length body)) <*> Gen.element (BS.unpack "{}[]\",:\\ 0e-"))
        , (1, (body <>) <$> Gen.element [" ", "garbage", "}", "{\"name\":\"late\"}"])
        ]

-- | The body as it might arrive: one chunk, or pieces of any size down to a byte.
genChunks :: ByteString -> Gen [ByteString]
genChunks body = Gen.frequency [(1, pure [body]), (4, pieces body)]
  where
    pieces rest
        | BS.null rest = pure []
        | otherwise = do
            size <- Gen.frequency [(4, Gen.int (Range.linear 1 8)), (2, Gen.int (Range.linear 9 64)), (1, Gen.int (Range.linear 65 4096))]
            (BS.take size rest :) <$> pieces (BS.drop size rest)

render :: Builder.Builder -> ByteString
render = toStrict . Builder.toLazyByteString

genValue :: [ByteString] -> Int -> Gen Builder.Builder
genValue names depth =
    Gen.frequency ([(5, genScalar)] <> [(2, objectOf (Gen.list (Range.linear 0 5) ((,) <$> genKey names <*> genValue names (depth - 1)))) | depth > 0] <> [(2, arrayOf (Gen.list (Range.linear 0 5) (genValue names (depth - 1)))) | depth > 0])

-- Deep chains of arrays and objects, past any structural budget a test reads with.
genDeep :: [ByteString] -> Gen Builder.Builder
genDeep names = do
    levels <- Gen.int (Range.linear 1 90)
    opens <- Gen.list (Range.singleton levels) (Gen.element [True, False])
    leaf <- genScalar
    key <- genKey names
    let open isArray = if isArray then "[" else "{" <> key <> ":"
        close isArray = if isArray then "]" else "}"
    pure (foldMap open opens <> leaf <> foldMap close (reverse opens))

genScalar :: Gen Builder.Builder
genScalar = Gen.frequency [(4, genString), (3, genNumber), (1, genLiteral)]

objectOf :: Gen [(Builder.Builder, Builder.Builder)] -> Gen Builder.Builder
objectOf members = do
    pairs <- members
    colons <- traverse (const colon) pairs
    wrapped "{" "}" [key <> separator <> value | ((key, value), separator) <- zip pairs colons]

arrayOf :: Gen [Builder.Builder] -> Gen Builder.Builder
arrayOf = (>>= wrapped "[" "]")

wrapped :: Builder.Builder -> Builder.Builder -> [Builder.Builder] -> Gen Builder.Builder
wrapped open close items = do
    separators <- traverse (const comma) items
    let joined = mconcat (zipWith (\index (item, separator) -> (if index == (0 :: Int) then "" else separator) <> item) [0 ..] (zip items separators))
    space <- Gen.element ["", " ", "\n\t "]
    pure (open <> space <> joined <> space <> close)

-- json-stream's lexer reads commas and colons as whitespace, so missing and doubled ones reach the parser.
comma :: Gen Builder.Builder
comma = Gen.frequency [(24, pure ","), (2, pure " , "), (1, pure ""), (1, pure ",,"), (1, pure ":")]

colon :: Gen Builder.Builder
colon = Gen.frequency [(24, pure ":"), (2, pure " : "), (1, pure ""), (1, pure ",")]

genKey :: [ByteString] -> Gen Builder.Builder
genKey names
    | null names = genString
    | otherwise = Gen.frequency [(6, quoted . Builder.byteString <$> Gen.element names), (2, genString)]

quoted :: Builder.Builder -> Builder.Builder
quoted text = "\"" <> text <> "\""

genString :: Gen Builder.Builder
genString = quoted . mconcat <$> Gen.list (Range.linear 0 6) piece
  where
    piece =
        Gen.frequency
            [ (10, Builder.byteString . encodeUtf8 <$> Gen.text (Range.linear 0 10) Gen.alphaNum)
            , (3, Gen.element ["\\n", "\\\"", "\\\\", "\\/", "\\b", "\\f", "\\r", "\\t"])
            , (2, (\code -> "\\u" <> Builder.word16HexFixed code) <$> Gen.word16 Range.constantBounded)
            , (1, pure "\\ud83d\\ude00")
            , (1, Gen.element ["\\ud800", "\\udc00", "\\ud800x", "\\ud800\\u0041", "\\uDBFF\\uDFFF"])
            , (2, Builder.byteString . encodeUtf8 <$> Gen.text (Range.linear 1 3) Gen.unicode)
            , (1, Builder.byteString <$> Gen.element ["\xff", "\xc0\x80", "\xe0\x80\xaf", "\xe0\x9f\xbf", "\xf0\x80\x80\xaf", "\xf0\x8f\xbf\xbf", "\xe2\x82", "\xed\xa0\x80", "\xf4\x90\x80\x80", "\x80"])
            , (1, Builder.word8 <$> Gen.word8 (Range.constant 0 31))
            , (1, Gen.element ["\\x", "\\u12", "\\uZZZZ", "\\"])
            ]

genNumber :: Gen Builder.Builder
genNumber =
    Gen.frequency
        [ (6, Builder.intDec <$> Gen.int (Range.linearFrom 0 (-100000) 100000))
        , (2, (\whole fraction -> Builder.intDec whole <> "." <> Builder.intDec fraction) <$> Gen.int (Range.linearFrom 0 (-1000) 1000) <*> Gen.int (Range.linear 0 99999))
        , (2, (\whole power -> Builder.intDec whole <> "e" <> Builder.intDec power) <$> Gen.int (Range.linear (-9) 9) <*> Gen.int (Range.linearFrom 0 (-100000) 100000))
        , (1, (\power -> "1e" <> Builder.string7 power) <$> Gen.list (Range.linear 19 25) Gen.digit)
        , (1, Builder.string7 <$> Gen.list (Range.linear 19 400) Gen.digit)
        , (1, Gen.element ["-", "1.2.3", "--1", "1e", "+1", ".", "-0", "01", "1E+2", "0.000"])
        ]

genLiteral :: Gen Builder.Builder
genLiteral = Gen.frequency [(8, Gen.element ["true", "false", "null"]), (1, Gen.element ["tru", "nul", "truex", "nulll"])]
