-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The frozen decoder digest remains independent of the maintained implementation.
module Data.JsonStream.UnescapeSpec (spec) where

import Crypto.Hash (Context, Digest, SHA256, hashFinalize, hashInit, hashUpdate)
import Data.Aeson ((.:), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.JsonStream.Lexer qualified as Lexer
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (UnicodeException (DecodeError))
import Numeric (showHex)
import Test.Hspec (Spec, describe, it, shouldBe)

import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Support (expectRight)

data Reference = Reference Int Int Int Text

instance Aeson.FromJSON Reference where
    parseJSON = Aeson.withObject "frozen decoder reference" $ \object ->
        Reference <$> object .: "cases" <*> object .: "accepted" <*> object .: "rejected" <*> object .: "outputSha256"

data OracleCase = OracleCase Text ByteString Bool Bool

data Totals = Totals !Int !Int !Int !(Context SHA256)

-- | Check payload acceptance, decoded bytes, and complete error values through the public facade.
spec :: Spec
spec = describe "string payload decoder" $ do
    it "matches the frozen decoder across all generated inputs" $ do
        Reference cases accepted rejected expected <- readFileBS referencePath >>= expectRight . Aeson.eitherDecodeStrict
        let Totals actual right left context = foldl' recordCase (Totals 0 0 0 hashInit) oracleCases
        shouldBe (actual, right, left) (cases, accepted, rejected)
        shouldBe (show (hashFinalize context :: Digest SHA256) :: Text) expected

    it "preserves raw ASCII payload bytes, including controls and quotes" $
        for_ ["", "plain ASCII", "\"", "\n", "\0"] $ \bytes ->
            shouldBe (Lexer.decodeString bytes) (Right (TE.decodeLatin1 bytes))

    it "decodes escapes and paired surrogates" $ do
        shouldBe (Lexer.decodeString validEscapes) (Right (T.pack ['"', '\\', '/', '\b', '\f', '\n', '\r', '\t']))
        shouldBe (Lexer.decodeString "\\ud800\\udc00") (Right "\x10000")
        shouldBe (Lexer.decodeString "\\uDBFF\\uDFFF") (Right "\x10ffff")
        shouldBe (Lexer.decodeString "\\ud801\\udc37") (Right "\x10437")

    it "preserves the error constructor, description, and absent offending byte" $
        for_ malformedExamples $ \bytes ->
            shouldBe (Lexer.decodeString bytes) (Left (DecodeError invalidUtf8Description Nothing))

recordCase :: Totals -> OracleCase -> Totals
recordCase (Totals count accepted rejected context) input =
    let bytes = caseBytes input
        result = Lexer.decodeString bytes
        encoded = LBS.toStrict (Aeson.encode (oracleRecord input bytes result)) <> lineEnding
        updated = hashUpdate context encoded
     in case result of
            Right _ -> Totals (count + 1) (accepted + 1) rejected updated
            Left _ -> Totals (count + 1) accepted (rejected + 1) updated

caseBytes :: OracleCase -> ByteString
caseBytes (OracleCase _ bytes sliced _)
    | sliced = BS.take (BS.length bytes) (BS.drop (BS.length sentinelPrefix) (sentinelPrefix <> bytes <> sentinelSuffix))
    | otherwise = bytes

oracleRecord :: OracleCase -> ByteString -> Either UnicodeException Text -> Aeson.Value
oracleRecord (OracleCase name _ _ comparators) bytes result =
    Aeson.object $
        ["name" .= name, "input" .= payload bytes, "result" .= outcome result]
            <> if comparators
                then
                    [ "textUtf8" .= outcome (TE.decodeUtf8' bytes)
                    , "aesonQuoted" .= (Aeson.eitherDecodeStrict (quoteBytes <> bytes <> quoteBytes) :: Either String Text)
                    ]
                else []

payload :: ByteString -> Aeson.Value
payload bytes = Aeson.object ["length" .= BS.length bytes, "sha256" .= hexSha256Of bytes]

outcome :: Either UnicodeException Text -> Aeson.Value
outcome (Right text) = Aeson.object ["right" .= payload (TE.encodeUtf8 text), "characters" .= T.length text]
outcome (Left (DecodeError description byte)) = Aeson.object ["left" .= ("DecodeError" :: Text), "description" .= description, "byte" .= byte]
outcome (Left other) = Aeson.object ["unexpectedUnicodeError" .= (show other :: Text)]

-- The order and auxiliary comparison fields belong to the pre-port frozen digest.
oracleCases :: [OracleCase]
oracleCases =
    concatMap directedCases directed
        <> concatMap byteCases allBytes
        <> [plain ("lone-surrogate-" <> show cp) (unicodeEscape cp) | cp <- [surrogateStart .. surrogateEnd]]
        <> concatMap unicodeCases unicodeBoundaries
        <> [OracleCase name bytes True False | (name, bytes) <- longPayloads]
  where
    directedCases (name, bytes) =
        [OracleCase name bytes False True, OracleCase (name <> "-slice") bytes True False]
    byteCases leadingByte =
        [plain ("byte-" <> show leadingByte) (BS.singleton leadingByte), plain ("escape-byte-" <> show leadingByte) (BS.pack [backslashByte, leadingByte])]
            <> [plain ("pair-" <> show leadingByte <> "-" <> show followingByte) (BS.pack [leadingByte, followingByte]) | followingByte <- allBytes]

plain :: Text -> ByteString -> OracleCase
plain name bytes = OracleCase name bytes False False

unicodeCases :: (Int, Char) -> [OracleCase]
unicodeCases (cp, scalar) =
    [plain ("unicode-" <> show cp) encoded]
        <> [plain ("utf8-truncated-" <> show cp <> "-" <> show size) (BS.take size encoded) | size <- positions]
        <> [ plain
                ("utf8-mutated-" <> show cp <> "-" <> show position <> "-" <> show byte)
                (BS.take position encoded <> BS.singleton byte <> BS.drop (position + 1) encoded)
           | position <- positions
           , byte <- mutationBytes
           ]
  where
    encoded = TE.encodeUtf8 (T.singleton scalar)
    positions = [0 .. BS.length encoded - 1]

unicodeEscape :: Int -> ByteString
unicodeEscape cp = "\\u" <> TE.encodeUtf8 (T.justifyRight unicodeHexWidth '0' (T.pack (showHex cp "")))

directed :: [(Text, ByteString)]
directed =
    [ ("empty", "")
    , ("ascii", "plain ASCII")
    , ("raw-quote", "\"")
    , ("raw-newline", "\n")
    , ("raw-null", BS.singleton 0)
    , ("valid-escapes", validEscapes)
    , ("trailing-backslash", "tail\\")
    , ("unknown-escape", "\\x")
    , ("unicode-empty", "\\u")
    , ("unicode-short", "\\u12")
    , ("unicode-nonhex", "\\u0X12")
    , ("unicode-lower", "\\uabcd")
    , ("unicode-upper", "\\uABCD")
    , ("escaped-null", "\\u0000")
    , ("pair-min", "\\ud800\\udc00")
    , ("pair-max", "\\uDBFF\\uDFFF")
    , ("pair-known", "\\ud801\\udc37")
    , ("high-high", "\\ud800\\ud800")
    , ("low-high", "\\udc00\\ud800")
    , ("high-ascii", "\\ud800\\u0041")
    , ("high-raw", "\\ud800a")
    , ("high-truncated-low", "\\ud800\\udc")
    , ("utf8-surrogate", BS.pack [0xed, 0xa0, 0x80])
    , ("utf8-overlong-two", BS.pack [0xc0, 0x80])
    , ("utf8-overlong-three", BS.pack [0xe0, 0x80, 0x80])
    , ("utf8-overlong-four", BS.pack [0xf0, 0x80, 0x80, 0x80])
    , ("utf8-too-large", BS.pack [0xf4, 0x90, 0x80, 0x80])
    , ("utf8-five-byte", BS.pack [0xf8, 0x88, 0x80, 0x80, 0x80])
    ]

longPayloads :: [(Text, ByteString)]
longPayloads =
    [ ("long-plain", BS.replicate longBytes asciiByte)
    , ("long-escaped", BS.concat (replicate longEscapeCount "\\u0061\\n"))
    , ("long-utf8", BS.concat (replicate longEscapeCount (BS.pack [0xf0, 0x90, 0x90, 0xb7])))
    , ("long-invalid-tail", BS.replicate longBytes asciiByte <> BS.singleton invalidByte)
    , ("long-lone-surrogate-tail", BS.replicate longBytes asciiByte <> "\\ud800")
    , ("long-pair", BS.concat (replicate longEscapeCount "\\ud801\\udc37"))
    ]

malformedExamples :: [ByteString]
malformedExamples =
    [ "\\"
    , "\\x"
    , "\\u12"
    , "\\u0X12"
    , "\\ud800"
    , "\\udc00"
    , "\\ud800\\u0041"
    , BS.pack [0xed, 0xa0, 0x80]
    , BS.pack [0xf4, 0x90, 0x80, 0x80]
    ]

unicodeBoundaries :: [(Int, Char)]
unicodeBoundaries =
    [ (0, '\x0')
    , (1, '\x1')
    , (31, '\x1f')
    , (32, '\x20')
    , (127, '\x7f')
    , (128, '\x80')
    , (2047, '\x7ff')
    , (2048, '\x800')
    , (55295, '\xd7ff')
    , (57344, '\xe000')
    , (65535, '\xffff')
    , (65536, '\x10000')
    , (1114111, '\x10ffff')
    ]

allBytes, mutationBytes :: [Word8]
allBytes = [minBound .. maxBound]
mutationBytes = [0, 32, 92, 127, 128, 191, 192, 223, 224, 239, 240, 244, 255]

sentinelPrefix, sentinelSuffix, validEscapes, lineEnding, quoteBytes :: ByteString
sentinelPrefix = BS.pack [255, 92, 0]
sentinelSuffix = BS.pack [128, 255, 92]
validEscapes = BS.pack [92, 34, 92, 92, 92, 47, 92, 98, 92, 102, 92, 110, 92, 114, 92, 116]
lineEnding = "\n"
quoteBytes = "\""

surrogateStart, surrogateEnd, unicodeHexWidth, longBytes, longEscapeCount :: Int
surrogateStart = 55296
surrogateEnd = 57343
unicodeHexWidth = 4
longBytes = 131072
longEscapeCount = 32768

backslashByte, asciiByte, invalidByte :: Word8
backslashByte = 92
asciiByte = 97
invalidByte = 255

invalidUtf8Description :: String
invalidUtf8Description = "Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream"

referencePath :: FilePath
referencePath = "core/test/unit/fixtures/json-stream/unescape-oracle.json"
