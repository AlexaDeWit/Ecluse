-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared PyPI declaration cases, filenames, PEP 691 entries, and the simple index that
carries them, for configuration, projection, routing, and performance checks.
-}
module Ecluse.Test.Registry.PyPI (
    pypiEntryVerdicts,
    simpleFile,
    filesNamed,
    withFileKeys,
    yankedForms,
    simpleIndex,
    simpleIndexWith,
    separatorHeavySdist,
    alicePair,
) where

import Data.Aeson (Value (Bool, Null, Number, String), object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.Types (Pair)
import Data.Text qualified as T

import Ecluse.Core.Credential (ClientCredential (ClientCredential), mkSecret)
import Ecluse.Test.Json (withKeys)
import Ecluse.Test.Package (validSha256)

{- | The Basic pair every PyPI credential example builds on: a named user with a password.
It renders as @Basic YWxpY2U6aHVudGVyMg==@.
-}
alicePair :: Maybe ClientCredential
alicePair = Just (ClientCredential (Just "alice") (mkSecret "hunter2"))

-- | A PEP 691 entry on the declared files host with a SHA-256 digest.
simpleFile :: Text -> Value
simpleFile filename =
    object
        [ "filename" .= filename
        , "url" .= ("https://files.pythonhosted.org/packages/a0/" <> filename)
        , "hashes" .= object ["sha256" .= validSha256]
        , "requires-python" .= (">=3.10" :: Text)
        , "upload-time" .= ("2026-05-14T19:25:26Z" :: Text)
        , "provenance" .= ("https://pypi.org/integrity/x/provenance" :: Text)
        ]

-- | One 'simpleFile' entry per filename, in the order given.
filesNamed :: [Text] -> [Value]
filesNamed = map simpleFile

-- | A file entry with the given keys added or overridden, so an example names only its own axis.
withFileKeys :: [(Key, Value)] -> Value -> Value
withFileKeys = withKeys

{- | Each wire form of a file entry's @yanked@ member, as the keys that carry it, with whether it
withdraws the file.
-}
yankedForms :: [(String, [(Key, Value)], Bool)]
yankedForms =
    [ ("true", [("yanked", Bool True)], True)
    , ("a string", [("yanked", String "broken sdist")], True)
    , ("an empty string", [("yanked", String "")], True)
    , ("false", [("yanked", Bool False)], False)
    , ("null", [("yanked", Null)], False)
    , ("an absent member", [], False)
    , ("a number", [("yanked", Number 1)], False)
    , ("an array", [("yanked", toJSON [String "broken sdist"])], False)
    , ("an object", [("yanked", object ["reason" .= String "x"])], False)
    ]

-- | A PEP 691 simple index: the project name and its file entries, nothing else.
simpleIndex :: Text -> [Value] -> Value
simpleIndex name = simpleIndexWith name []

{- | 'simpleIndex' carrying site-specific top-level fields, such as a @meta@ block or the
PEP 700 @tracks@ array, applied between the name and the files.
-}
simpleIndexWith :: Text -> [Pair] -> [Value] -> Value
simpleIndexWith name extra files = object (["name" .= name] <> extra <> ["files" .= files])

-- | A malformed sdist with distinct suffixes for allocation and scaling measurements.
separatorHeavySdist :: Text -> Int -> Text -> Text
separatorHeavySdist project count suffix = project <> "-1" <> T.replicate count "_a" <> "_" <> suffix <> ".tar.gz"

-- | Accepted and refused declarations shared by the parser and configuration suites.
pypiEntryVerdicts :: [(Text, Bool)]
pypiEntryVerdicts =
    [ ("acme", True)
    , ("Acme_Tools", True)
    , ("ACME", True)
    , ("acme._-tools", True)
    , (T.replicate 100 "a", True)
    , (T.replicate 101 "a", False)
    , ("-acme", False)
    , ("_acme", False)
    , (".acme", False)
    , ("acme-", False)
    , ("acme_", False)
    , ("acme.", False)
    , ("acme-*", True)
    , ("acme_*", True)
    , ("acme.*", True)
    , ("ACME-*", True)
    , ("acme*", False)
    , ("-*", False)
    , ("*acme", False)
    , ("*", False)
    , ("@acme", False)
    , ("acme/tools", False)
    , ("acme tools", False)
    , (",", False)
    , (".", False)
    ]
