-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Security.LimitsSpec (spec) where

import Data.Aeson (Value (Array, Bool, Null, Number, Object, String), eitherDecodeStrict)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Hedgehog (annotateShow, forAll, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (
    PackageDetails (..),
    PackageInfo (..),
    PackageName,
    renderPackageName,
 )
import Ecluse.Core.Registry.Npm.Project (Projection (NameMismatch, Projected), parsePackageInfoFromValue)
import Ecluse.Core.Security (
    LimitError (..),
    Limits (..),
    boundedRead,
    checkNestingDepth,
    checkVersionCount,
    checkVersionCountOf,
    defaultLimits,
 )
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Test.Package (sampleDetails, unscopedNpm)

-- | A minimal per-version snapshot. Only the name and version are meaningful here.
details :: PackageName -> Version -> PackageDetails
details name version = (sampleDetails name version){pkgLicenses = ["MIT"]}

{- | Drive 'boundedRead' with a 'State'-monad chunk producer. It pops one chunk per call and
yields an empty 'ByteString', the @BodyReader@ EOF signal, once the list runs out.
-}
runBounded :: Limits -> [ByteString] -> Either LimitError ByteString
runBounded limits = evalState (boundedRead limits next)
  where
    next :: State [ByteString] ByteString
    next =
        get >>= \case
            [] -> pure BS.empty
            (c : cs) -> put cs >> pure c

spec :: Spec
spec = do
    boundedReadSpec
    versionCountSpec
    nestingDepthSpec
    realPackumentSpec
    propertiesSpec

boundedReadSpec :: Spec
boundedReadSpec = describe "boundedRead" $ do
    let limits = defaultLimits{maxBodyBytes = 10}

    it "returns the whole body when within the byte budget" $
        runBounded limits ["hello", "12345"] `shouldBe` Right "hello12345"

    it "returns an empty body for an immediately-EOF reader" $
        runBounded limits [] `shouldBe` Right ""

    it "aborts fail-closed past the byte budget (never a partial body)" $
        -- 11 bytes against a 10-byte cap: a 'Left', not the first 10 bytes.
        runBounded limits ["hello", "world!"] `shouldBe` Left (BodyTooLarge 10)

    it "reports the configured ceiling in the error" $
        runBounded (defaultLimits{maxBodyBytes = 4}) ["abcde"]
            `shouldBe` Left (BodyTooLarge 4)

    it "accepts a body exactly at the budget" $
        runBounded limits ["1234567890"] `shouldBe` Right "1234567890"

    it "rejects any non-empty body under a zero budget" $
        runBounded (defaultLimits{maxBodyBytes = 0}) ["x"] `shouldBe` Left (BodyTooLarge 0)

    it "accepts an empty body even under a zero budget" $
        runBounded (defaultLimits{maxBodyBytes = 0}) [] `shouldBe` Right ""

    it "treats an empty chunk as EOF (the BodyReader contract), stopping early" $
        -- An empty 'ByteString' is the reader's end signal, so nothing reads the chunk after
        -- it. This pins the @http-client@ @BodyReader@ semantics 'boundedRead' relies on.
        runBounded limits ["ab", "", "cd"] `shouldBe` Right "ab"

    it "passes a small body under the generous default budget" $
        -- Exercises 'defaultLimits' (the 12 MiB cap) directly.
        runBounded defaultLimits ["small", "body"] `shouldBe` Right "smallbody"

    it "stops reading once the budget is breached (does not drain the reader)" $ do
        -- An IORef-backed reader (the real monad is IO) lets us observe that
        -- 'boundedRead' stops pulling chunks after it decides to abort.
        ref <- newIORef (["aaaa", "bbbb", "cccc", "dddd"] :: [ByteString])
        let next = atomicModifyIORef' ref $ \case
                [] -> ([], BS.empty)
                (c : cs) -> (cs, c)
        result <- boundedRead (defaultLimits{maxBodyBytes = 6}) next
        result `shouldBe` Left (BodyTooLarge 6)
        -- "aaaa" (4) fits. "bbbb" breaches at 8 > 6 and aborts, so nothing pulls
        -- "cccc" or "dddd": two chunks remain unread.
        remaining <- readIORef ref
        remaining `shouldBe` ["cccc", "dddd"]

versionCountSpec :: Spec
versionCountSpec = describe "checkVersionCount" $ do
    let limits = defaultLimits{maxVersionCount = 3}

    it "passes a packument within the version budget (returns it unchanged)" $
        checkVersionCount limits (packumentWith 3) `shouldBe` Right (packumentWith 3)

    it "rejects a packument with too many versions, fail-closed" $
        checkVersionCount limits (packumentWith 4)
            `shouldBe` Left (TooManyVersions 4 3)

    it "passes an empty packument" $
        checkVersionCount limits (packumentWith 0) `shouldBe` Right (packumentWith 0)

    it "rejects a pathological huge-version-count document" $
        case checkVersionCount limits (packumentWith 5000) of
            Left (TooManyVersions seen cap) -> (seen, cap) `shouldBe` (5000, 3)
            other -> expectationFailure ("expected TooManyVersions, got " <> show other)

    it "passes a realistic packument under the default version budget" $
        -- Exercises 'defaultLimits' directly (not via a record override).
        checkVersionCount defaultLimits (packumentWith 25) `shouldBe` Right (packumentWith 25)

    it "applies the same ceiling through checkVersionCountOf, on a bare count" $ do
        checkVersionCountOf limits 3 `shouldBe` Right ()
        checkVersionCountOf limits 4 `shouldBe` Left (TooManyVersions 4 3)

nestingDepthSpec :: Spec
nestingDepthSpec = describe "checkNestingDepth" $ do
    let limits = defaultLimits{maxNestingDepth = 3}

    it "passes a scalar (depth 1)" $
        checkNestingDepth limits (Number 1) `shouldBe` Right (Number 1)

    it "passes a document exactly at the depth budget" $
        -- {"a": {"b": 1}} is depth 3: object → object → scalar.
        let v = nestObject 3 in checkNestingDepth limits v `shouldBe` Right v

    it "rejects a document one level too deep, fail-closed" $
        checkNestingDepth limits (nestObject 4) `shouldBe` Left (TooDeeplyNested 3)

    it "rejects a deeply-nested array payload" $
        checkNestingDepth limits (nestArray 50) `shouldBe` Left (TooDeeplyNested 3)

    it "counts an empty container as a leaf (depth 1)" $
        checkNestingDepth (defaultLimits{maxNestingDepth = 1}) (Array V.empty)
            `shouldBe` Right (Array V.empty)

    it "passes a realistic shallow document under the default budget" $
        let doc = Object (KeyMap.fromList [("name", String "thing"), ("nested", nestObject 2)])
         in checkNestingDepth defaultLimits doc `shouldBe` Right doc

    it "accepts all JSON scalar kinds as leaves" $
        -- Object/Array carry every scalar constructor (String/Number/Bool/Null),
        -- so the walk visits each leaf arm of the depth check.
        let doc =
                Object
                    ( KeyMap.fromList
                        [ ("s", String "x")
                        , ("n", Number 1)
                        , ("b", Bool True)
                        , ("z", Null)
                        , ("xs", Array (V.fromList [Bool False, Null, Number 2]))
                        ]
                    )
         in checkNestingDepth defaultLimits doc `shouldBe` Right doc

{- | The default 'Limits' (12 MiB body, 100k versions, depth 64) must never refuse a legitimate
trusted package. This case drives the serve-path sequence (bounded read, decode, depth check,
projection, version count) over a real untrimmed @express@ packument: about 805 KB, 288 versions,
JSON depth 7. The live smoke tier ("Ecluse.RegistryProtocolSpec") covers larger packuments against
current data.
-}
realPackumentSpec :: Spec
realPackumentSpec = describe "default Limits admit a real large trusted packument (no false positive)" $ do
    it "express: bounded read, decode, depth, projection, and version count all clear the defaults" $ do
        body <- readFileBS "core/test/unit/fixtures/npm/express.full.json"
        -- 1. Body size: the bounded read returns the whole body (within maxBodyBytes).
        bounded <- case runBounded defaultLimits [body] of
            Left err -> expectationFailure ("real packument refused by the body bound: " <> show err) >> pure ""
            Right b -> pure b
        bounded `shouldBe` body
        -- 2. Decode to a Value, then 3. depth-check it (within maxNestingDepth).
        value <- case eitherDecodeStrict bounded of
            Left e -> expectationFailure ("real packument did not decode: " <> e) >> pure (Object mempty)
            Right v -> pure v
        depthChecked <- case checkNestingDepth defaultLimits value of
            Left err -> expectationFailure ("real packument refused by the nesting bound: " <> show err) >> pure (Object mempty)
            Right v -> pure v
        -- 4. Project to the typed view (it is a well-formed packument), then
        -- 5. version-count check it (within maxVersionCount).
        info <- case parsePackageInfoFromValue (unscopedNpm "express") depthChecked of
            Left err -> expectationFailure ("real packument did not project: " <> show err) >> pure emptyInfo
            Right (Projected i) -> pure i
            Right (NameMismatch reported) ->
                expectationFailure ("real packument self-reported an unexpected name: " <> toString reported) >> pure emptyInfo
        case checkVersionCount defaultLimits info of
            Left err -> expectationFailure ("real packument refused by the version bound: " <> show err)
            Right admitted -> do
                renderPackageName (infoName admitted) `shouldBe` "express"
                -- A genuinely large version set, well under the 100k ceiling: proof the
                -- count bound clears a real package, not a toy one.
                Map.size (infoVersions admitted) `shouldSatisfy` (> 200)
                Map.size (infoVersions admitted) `shouldSatisfy` (<= maxVersionCount defaultLimits)

-- An empty 'PackageInfo' placeholder so a failed projection keeps the example total.
-- 'expectationFailure' already failed the example before anything forces this.
emptyInfo :: PackageInfo
emptyInfo =
    PackageInfo
        { infoName = unscopedNpm "unused"
        , infoVersions = Map.empty
        , infoDistTags = Map.empty
        , infoInvalidEntries = []
        }

propertiesSpec :: Spec
propertiesSpec = describe "properties" $ do
    it "boundedRead reconstructs the body iff it fits the budget" $
        hedgehog $ do
            -- Chunks are non-empty: a faithful @BodyReader@ emits an empty 'ByteString' only at
            -- EOF, so 'BS.concat' of the whole list is a faithful oracle for the body.
            chunks <- forAll (Gen.list (Range.linear 0 8) (Gen.bytes (Range.linear 1 6)))
            cap <- forAll (Gen.int (Range.linear 0 40))
            let total = BS.concat chunks
                result = runBounded (defaultLimits{maxBodyBytes = cap}) chunks
            annotateShow (BS.length total, cap)
            -- Non-vacuity: the generator must reach both the within- and
            -- over-budget arms often.
            H.cover 5 "within budget" (BS.length total <= cap)
            H.cover 5 "over budget" (BS.length total > cap)
            if BS.length total <= cap
                then result === Right total -- exact bytes, never truncated
                else result === Left (BodyTooLarge cap)

-- | A scalar wrapped in @n-1@ nested single-key objects, giving total depth @n@.
nestObject :: Int -> Value
nestObject n
    | n <= 1 = Number 1
    | otherwise = Object (KeyMap.singleton "a" (nestObject (n - 1)))

-- | A scalar wrapped in @n-1@ nested single-element arrays, giving total depth @n@.
nestArray :: Int -> Value
nestArray n
    | n <= 1 = Number 1
    | otherwise = Array (V.singleton (nestArray (n - 1)))

packumentWith :: Int -> PackageInfo
packumentWith n =
    let name = unscopedNpm "thing"
        ver i = "0.0." <> show i
     in PackageInfo
            { infoName = name
            , infoVersions =
                Map.fromList
                    [ (ver i, details name (mkVersion Npm (ver i)))
                    | i <- [1 .. n]
                    ]
            , infoDistTags = Map.empty
            , infoInvalidEntries = []
            }
