-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The stored publication state round-trips, and every unreadable form is a distinct fault.
module Ecluse.Core.Osv.Publication.EnvelopeSpec (spec) where

import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian, picosecondsToDiffTime)
import Test.Hspec

import Ecluse.Core.Osv.Publication
import Ecluse.Core.Osv.Publication.Envelope (decodeEnvelope, encodeEnvelope, maxEnvelopeBytes)
import Ecluse.Core.Osv.Schema (EpssStatus (EnrichmentAvailable, EnrichmentUnavailable), osvSchemaEpoch)
import Ecluse.Test.Snapshot (digestOf)

spec :: Spec
spec = describe "the publication metadata envelope" $ do
    nonce <- runIO newPublicationNonce
    for_ ["npm", "pypi"] $ \ecosystem -> do
        let full = stateFor nonce ecosystem
        for_
            [ ("with a current EPSS marker", full)
            , ("through an outage, with the last EPSS marker kept", full{psEpssStatus = EnrichmentUnavailable, psEpssCreditBytes = 77, psAdjustment = EpssOutageCredit})
            , ("with no EPSS marker ever", full{psEpssStatus = EnrichmentUnavailable, psLastEpss = Nothing, psAdjustment = InitialBaseline})
            , ("with unknown source timestamps", full{psOsv = (psOsv full){smLastModified = Nothing}, psLastEpss = (\marker -> marker{smLastModified = Nothing}) <$> psLastEpss full})
            ]
            $ \(label, published) ->
                it ("round-trips a " <> toString ecosystem <> " state " <> label) $ do
                    pairs <- either (fail . show) pure (encodeEnvelope published)
                    decodeEnvelope pairs `shouldBe` Right published

        it ("reads keys in any case for " <> toString ecosystem) $ do
            pairs <- either (fail . show) pure (encodeEnvelope full)
            decodeEnvelope [(T.toUpper key, value) | (key, value) <- pairs] `shouldBe` Right full

    let full = stateFor nonce "npm"
    it "names every key an object written before guarded publication lacks" $
        case decodeEnvelope [("owner", "operator")] of
            Left (EnvelopeMissing keys) -> toList keys `shouldBe` envelopeKeys
            other -> expectationFailure ("expected every key reported missing, got " <> show other)

    it "reports an unknown version before any missing key" $
        decodeEnvelope [("publication-version", "2")] `shouldBe` Left (EnvelopeUnsupported "2")

    for_
        [ ("osv-digest", "sha1-wire:abc")
        , ("osv-source", "sha256-wire:" <> T.replicate 64 "a")
        , ("osv-last-modified", "yesterday")
        , ("publication-nonce", "not-a-nonce")
        , ("baseline-bytes", "-1")
        , ("baseline-bytes", "99999999999999999999")
        , ("baseline-adjustment", "sideways")
        , ("epss-status", "maybe")
        , ("epss-digest", "none")
        ]
        $ \(key, value) ->
            it ("refuses " <> toString key <> " = " <> toString value) $ do
                pairs <- either (fail . show) pure (encodeEnvelope full)
                decodeEnvelope (replaceValue key value pairs) `shouldBe` Left (EnvelopeMalformed key)

    it "refuses available enrichment that carries no EPSS marker" $ do
        pairs <- either (fail . show) pure (encodeEnvelope full{psEpssStatus = EnrichmentUnavailable, psLastEpss = Nothing})
        decodeEnvelope (replaceValue "epss-status" "available" pairs) `shouldBe` Left (EnvelopeMalformed "epss-status")

    it "keeps the longest state it writes under the store's limit" $ do
        let longest = full{psRelevantRows = maxCount, psScoredRows = maxCount, psBaselineBytes = maxCount, psEpssCreditBytes = maxCount, psOsv = (psOsv full){smLastModified = Just preciseStamp}}
        pairs <- either (fail . show) pure (encodeEnvelope longest)
        sum [T.length key + T.length value | (key, value) <- pairs] `shouldSatisfy` (< maxEnvelopeBytes)

    it "refuses a stored envelope past the limit" $ do
        pairs <- either (fail . show) pure (encodeEnvelope full)
        decodeEnvelope (("padding", T.replicate maxEnvelopeBytes "x") : pairs) `shouldSatisfy` either isTooLarge (const False)

    it "refuses to write a value outside printable ASCII" $
        encodeEnvelope full{psEcosystem = "npm\233"} `shouldBe` Left (EnvelopeMalformed "ecosystem")
  where
    isTooLarge = \case
        EnvelopeTooLarge _ -> True
        _ -> False

envelopeKeys :: [Text]
envelopeKeys =
    [ "publication-version"
    , "ecosystem"
    , "schema-epoch"
    , "osv-source"
    , "osv-last-modified"
    , "osv-digest"
    , "epss-status"
    , "epss-source"
    , "epss-last-modified"
    , "epss-digest"
    , "relevant-rows"
    , "scored-rows"
    , "baseline-bytes"
    , "epss-credit-bytes"
    , "publication-nonce"
    , "baseline-adjustment"
    ]

stateFor :: PublicationNonce -> Text -> PublishedState
stateFor nonce ecosystem =
    PublishedState
        { psEcosystem = ecosystem
        , psSchemaEpoch = osvSchemaEpoch
        , psOsv = SourceMarker (sourceIdentityOf "https://osv.example.test/all.zip") (Just stamp) (sourceDigestOf (digestOf "osv"))
        , psEpssStatus = EnrichmentAvailable
        , psLastEpss = Just (SourceMarker (sourceIdentityOf "https://epss.example.test/scores.csv.gz") (Just stamp) (sourceDigestOf (digestOf "epss")))
        , psRelevantRows = 1234
        , psScoredRows = 567
        , psBaselineBytes = 89012345
        , psEpssCreditBytes = 0
        , psNonce = nonce
        , psAdjustment = OrdinaryBaseline
        }
  where
    stamp = UTCTime (fromGregorian 2026 10 16) 3600

-- A twelve-digit fraction, the finest a stamp is written with.
preciseStamp :: UTCTime
preciseStamp = UTCTime (fromGregorian 2026 10 16) (picosecondsToDiffTime 86399999999999999)

maxCount :: Integer
maxCount = toInteger (maxBound :: Int64)

replaceValue :: Text -> Text -> [(Text, Text)] -> [(Text, Text)]
replaceValue key value pairs = (key, value) : filter ((/= key) . fst) pairs
