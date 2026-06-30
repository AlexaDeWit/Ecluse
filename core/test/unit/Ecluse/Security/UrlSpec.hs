module Ecluse.Security.UrlSpec (spec) where

import Data.Text qualified as T
import Hedgehog (annotateShow, forAll)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (
    PackageName,
    mkPackageName,
    mkScope,
 )
import Ecluse.Core.Security (
    UrlError (..),
    upstreamUrlFor,
 )

-- | An unscoped npm package identity.
unscoped :: Text -> PackageName
unscoped = mkPackageName Npm Nothing

-- | A scoped npm package identity (scope, base name).
scoped :: Text -> Text -> PackageName
scoped scope = mkPackageName Npm (Just (mkScope scope))

spec :: Spec
spec = do
    upstreamUrlSpec
    propertiesSpec

upstreamUrlSpec :: Spec
upstreamUrlSpec = describe "upstreamUrlFor" $ do
    let base = "https://registry.npmjs.org"

    describe "builds a URL for a legitimate package" $ do
        it "joins the base URL and an unscoped name" $
            upstreamUrlFor base (unscoped "is-odd")
                `shouldBe` Right "https://registry.npmjs.org/is-odd"
        it "renders a scoped name in npm wire form (@scope%2Fname) under the base" $
            -- The scope separator is the structural '%2F' this builder writes -- the
            -- same wire form the data plane uses -- not a literal '/' that a segment
            -- splitter downstream could re-split.
            upstreamUrlFor base (scoped "babel" "code-frame")
                `shouldBe` Right "https://registry.npmjs.org/@babel%2Fcode-frame"
        it "accepts a name with interior dots (not over-rejected)" $
            upstreamUrlFor base (unscoped "lodash.merge")
                `shouldBe` Right "https://registry.npmjs.org/lodash.merge"
        it "tolerates a single trailing slash on the base without doubling it" $
            upstreamUrlFor "https://registry.npmjs.org/" (unscoped "is-odd")
                `shouldBe` Right "https://registry.npmjs.org/is-odd"
        it "emits exactly one %2F scope separator for a scoped name (no double-encoding)" $
            -- The structural '%2F' the scoped path carries is the separator this
            -- builder writes, never an encoding of a component's content: a
            -- legitimate '@scope/name' yields a single '%2F', the '@' is verbatim,
            -- and the hyphen in the base name is left literal.
            upstreamUrlFor base (scoped "babel" "code-frame")
                `shouldBe` Right "https://registry.npmjs.org/@babel%2Fcode-frame"

    describe "percent-encodes an accepted component so a once-decoded escape cannot reach the upstream raw" $ do
        it "re-encodes a literal '%' in an unscoped name (the %2e%2e%2f vector)" $
            -- 'foo%2e%2e%2fbar' passes the denylist (no literal '/'), so the
            -- defence in depth is to encode the '%' -- the upstream must receive
            -- '%25', never a live '%2e%2e%2f' a decode-and-normalise CDN resolves
            -- to traversal.
            upstreamUrlFor base (unscoped "foo%2e%2e%2fbar")
                `shouldBe` Right "https://registry.npmjs.org/foo%252e%252e%252fbar"
        it "re-encodes a literal '%' hidden in the base name of a scoped package" $
            upstreamUrlFor base (scoped "babel" "code%2e%2eframe")
                `shouldBe` Right "https://registry.npmjs.org/@babel%2Fcode%252e%252eframe"
        it "encodes an accepted '?' (and the '=' after it) so it cannot inject an upstream query" $
            -- '?' becomes '%3F' and the reserved '=' '%3D', so the whole component
            -- is opaque path, never a query the upstream would parse.
            upstreamUrlFor base (unscoped "pkg?inject=1")
                `shouldBe` Right "https://registry.npmjs.org/pkg%3Finject%3D1"
        it "encodes an accepted '#' so it cannot inject an upstream fragment" $
            upstreamUrlFor base (unscoped "pkg#frag")
                `shouldBe` Right "https://registry.npmjs.org/pkg%23frag"

    describe "refuses to build a URL from a hostile identifier" $ do
        it "rejects a traversal segment in the base name" $
            upstreamUrlFor base (unscoped "..") `shouldBe` Left (UnsafeComponent "..")
        it "rejects a current-directory base name" $
            upstreamUrlFor base (unscoped ".") `shouldBe` Left (UnsafeComponent ".")
        it "rejects an embedded slash (would smuggle a path)" $
            upstreamUrlFor base (unscoped "foo/bar") `shouldBe` Left (UnsafeComponent "foo/bar")
        it "rejects an encoded-slash artdefact decoded to a real slash" $
            -- "@scope%2f..%2f" decodes to "scope/../"; recovered as a base name
            -- carrying '/' and '..', which must not be interpolated.
            upstreamUrlFor base (unscoped "scope/../") `shouldBe` Left (UnsafeComponent "scope/../")
        it "rejects an embedded backslash" $
            upstreamUrlFor base (unscoped "foo\\bar") `shouldBe` Left (UnsafeComponent "foo\\bar")
        it "rejects a CRLF-injecting name" $
            upstreamUrlFor base (unscoped "foo\r\nHost: evil")
                `shouldBe` Left (UnsafeComponent "foo\r\nHost: evil")
        it "rejects a control character in the name" $
            upstreamUrlFor base (unscoped "foo\0bar") `shouldBe` Left (UnsafeComponent "foo\0bar")
        it "rejects a traversal in the scope of a scoped name" $
            upstreamUrlFor base (scoped ".." "pkg") `shouldBe` Left (UnsafeComponent "..")
        it "rejects a slash hidden in the base name of a scoped package" $
            upstreamUrlFor base (scoped "babel" "code/frame")
                `shouldBe` Left (UnsafeComponent "code/frame")

    describe "handles an '@'-leading name with no scope separator" $ do
        it "accepts a bare '@'-prefixed name as a single component, percent-encoding the stray '@'" $
            -- A rendered name starting with '@' but carrying no '/' is treated as
            -- one component (the single-component fallback). With no structural
            -- scope frame, the '@' is component content, so it is percent-encoded
            -- ('%40') rather than emitted as a sigil.
            upstreamUrlFor base (unscoped "@foo")
                `shouldBe` Right "https://registry.npmjs.org/%40foo"
        it "still rejects a traversal hidden after an '@' scope prefix" $
            -- "@..\/b" splits to ["..", "b"]; the ".." component is caught.
            upstreamUrlFor base (unscoped "@../b") `shouldBe` Left (UnsafeComponent "..")

    describe "refuses an empty base URL" $
        it "rejects an empty configured base URL" $
            upstreamUrlFor "" (unscoped "is-odd") `shouldBe` Left EmptyBaseUrl

propertiesSpec :: Spec
propertiesSpec = describe "properties" $ do
    it "an accepted upstream URL never contains a traversal/separator/injection artefact" $
        hedgehog $ do
            name <- forAll genName
            case upstreamUrlFor "https://registry.npmjs.org" name of
                Left _ -> H.success -- refused names are fine
                Right url -> do
                    -- An accepted URL is base ++ "/" ++ (one optional structural
                    -- "@…%2F…" scope frame over percent-encoded components), so
                    -- beyond the scheme its path must carry no "/../" or "/./"
                    -- smuggle, no backslash, no control character, and no live
                    -- escape, query, fragment, or space a component could inject.
                    let path = T.drop (T.length "https://registry.npmjs.org") url
                    annotateShow url
                    H.assert (not ("/../" `T.isInfixOf` path))
                    H.assert (not ("/./" `T.isInfixOf` path))
                    H.assert (not ("\\" `T.isInfixOf` path))
                    H.assert (T.all (\c -> c /= '\n' && c /= '\r' && c /= '\0') path)
                    -- No injectable delimiter survives unescaped: a query, fragment,
                    -- semicolon, or space a component carried is percent-encoded, so
                    -- none appears literally in the path.
                    H.assert (T.all (\c -> c `notElem` ['?', '#', ';', ' ']) path)
                    -- Every '%' is a well-formed '%XX' escape this builder wrote
                    -- (the '%2F' separator, or an encodeComponent escape) -- a raw
                    -- '%' a component carried is itself re-encoded to '%25', so no
                    -- live escape (the once-decoded '%2e%2e%2f' vector) leaks.
                    H.assert (allEscapesWellFormed path)

{- | Whether every @\'%\'@ in the text begins a well-formed @%XX@ escape (two hex
digits). A raw @\'%\'@ that a component carried is re-encoded to @%25@, so a path
this builder produced has only well-formed escapes -- the assertion that no live,
once-decoded escape (the @%2e%2e%2f@ vector) survives into the upstream URL.
-}
allEscapesWellFormed :: Text -> Bool
allEscapesWellFormed = go . toString
  where
    go ('%' : a : b : rest) = isHexDigit a && isHexDigit b && go rest
    go ('%' : _) = False -- a '%' not followed by two hex digits is a raw, unescaped '%'
    go (_ : rest) = go rest
    go [] = True
    isHexDigit c = c `elem` (['0' .. '9'] <> ['a' .. 'f'] <> ['A' .. 'F'])

{- | A package-name generator mixing benign names with hostile components
(traversal, slashes, control chars), exercising both arms of 'upstreamUrlFor'.
-}
genName :: H.Gen PackageName
genName = Gen.choice [unscoped <$> raw, scoped <$> rawScope <*> raw]
  where
    raw = Gen.frequency [(6, benign), (4, Gen.element hostile)]
    rawScope = Gen.frequency [(6, benign), (2, Gen.element hostile)]
    benign = Gen.text (Range.linear 1 8) (Gen.frequency [(8, Gen.alphaNum), (2, Gen.element ['.', '-', '_'])])
    hostile = ["..", ".", "a/b", "a\\b", "a\tb", "a\0b", "x/../y", "foo\r\nbar", ""]
