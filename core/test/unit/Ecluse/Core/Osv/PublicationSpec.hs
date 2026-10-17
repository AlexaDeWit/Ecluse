-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The publication decision over ordering, equality, missing evidence, and the durable size baseline.
module Ecluse.Core.Osv.PublicationSpec (spec) where

import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), addUTCTime, fromGregorian)
import Test.Hspec

import Ecluse.Core.Osv.Provenance (ProvenanceSource (EpssFeed, OsvExport))
import Ecluse.Core.Osv.Publication
import Ecluse.Core.Osv.Schema (EpssStatus (EnrichmentAvailable, EnrichmentUnavailable), osvSchemaEpoch)
import Ecluse.Test.Snapshot (digestOf)

spec :: Spec
spec = do
    fixture <- runIO newFixture
    for_ ["npm", "pypi"] $ \ecosystem -> describe ("publication guard for " <> toString ecosystem) $ do
        let fx = fixture{fxEcosystem = ecosystem}
        firstPublicationSpec fx
        orderingSpec fx
        epssOrderingSpec fx
        predecessorSpec fx
        shrinkSpec fx
        epssCreditSpec fx
        rebaselineSpec fx
    capabilitySpec fixture

firstPublicationSpec :: Fixture -> Spec
firstPublicationSpec fx = describe "an empty store" $ do
    it "creates the object only if it is still absent, and starts the baseline at the artifact's size" $ do
        let request = requestFor fx (osvAt Nothing "osv-1") (Just (epssAt (Just (hour 1)) "epss-1")) 5000
        plan <- expectPlan (planWith fx request StoreAbsent)
        (plPrecondition plan, plOsvOrder plan, plEpssOrder plan) `shouldBe` (CreateOnly, FreshScope, Just FreshScope)
        let stored = plState plan
        (psBaselineBytes stored, psEpssCreditBytes stored, psAdjustment stored) `shouldBe` (5000, 0, InitialBaseline)
        (psEcosystem stored, psSchemaEpoch stored, psEpssStatus stored) `shouldBe` (fxEcosystem fx, osvSchemaEpoch, EnrichmentAvailable)
        (smLastModified (psOsv stored), psNonce stored) `shouldBe` (Nothing, prNonce request)

    it "records an unavailable feed with no EPSS marker" $ do
        stored <- plState <$> expectPlan (planWith fx (requestFor fx (osvAt (Just (hour 1)) "osv-1") Nothing 5000) StoreAbsent)
        (psEpssStatus stored, psLastEpss stored) `shouldBe` (EnrichmentUnavailable, Nothing)

orderingSpec :: Fixture -> Spec
orderingSpec fx = describe "ordering the OSV export" $ do
    for_
        [ ("a later Last-Modified moves forward", osvAt (Just (hour 11)) "osv-b", Right OrderedForward)
        , ("an earlier Last-Modified is refused", osvAt (Just (hour 9)) "osv-b", Left (OlderSnapshot (hour 9) (hour 10)))
        , ("the same Last-Modified and bytes refresh", osvAt (Just (hour 10)) "osv-a", Right EqualSnapshot)
        , ("the same Last-Modified with other bytes is refused", osvAt (Just (hour 10)) "osv-b", Left (AmbiguousSnapshot (hour 10)))
        , ("identical bytes without a Last-Modified refresh", osvAt Nothing "osv-a", Right IdenticalBytes)
        , ("other bytes without a Last-Modified are refused", osvAt Nothing "osv-b", Left UnorderedSnapshot)
        , ("a changed source identity starts a fresh scope, even with an older date", otherOsvAt (Just (hour 1)) "osv-b", Right FreshScope)
        ]
        $ \(label, candidate, expected) ->
            it label $
                osvOutcome (planAgainst fx (osvAt (Just (hour 10)) "osv-a") candidate) `shouldBe` first Just expected

    it "refuses other bytes when only the stored artifact lacks a Last-Modified" $
        osvOutcome (planAgainst fx (osvAt Nothing "osv-a") (osvAt (Just (hour 11)) "osv-b")) `shouldBe` Left (Just UnorderedSnapshot)

    it "keeps the stored Last-Modified through an identical refresh, so a later change is still ordered against it" $ do
        refreshed <- expectPlan (planAgainst fx (osvAt (Just (hour 10)) "osv-a") (osvAt Nothing "osv-a"))
        smLastModified (psOsv (plState refreshed)) `shouldBe` Just (hour 10)
        for_ [(osvAt (Just (hour 9)) "osv-b", OlderSnapshot (hour 9) (hour 10)), (osvAt Nothing "osv-b", UnorderedSnapshot)] $ \(changed, refusal) ->
            osvOutcome (planWith fx (requestFor fx changed Nothing 5000) (present 5000 (plState refreshed))) `shouldBe` Left (Just refusal)

    it "keeps the newer run's artifact whichever of two runs finishes last" $ do
        let older = requestFor fx (osvAt (Just (hour 9)) "osv-old") Nothing 5000
            newer = requestFor fx (osvAt (Just (hour 10)) "osv-new") Nothing 5000
        newerFirst <- expectPlan (planWith fx newer StoreAbsent)
        osvOutcome (planWith fx older (present 5000 (plState newerFirst))) `shouldBe` Left (Just (OlderSnapshot (hour 9) (hour 10)))
        olderFirst <- expectPlan (planWith fx older StoreAbsent)
        osvOutcome (planWith fx newer (present 5000 (plState olderFirst))) `shouldBe` Right OrderedForward

epssOrderingSpec :: Fixture -> Spec
epssOrderingSpec fx = describe "ordering the EPSS feed on its own evidence" $ do
    let stored = baseState fx (osvAt (Just (hour 10)) "osv-a") (Just (epssAt (Just (hour 10)) "epss-a"))
    it "refuses older EPSS data even beside a newer OSV export" $
        planWith fx (requestFor fx (osvAt (Just (hour 11)) "osv-b") (Just (epssAt (Just (hour 9)) "epss-b")) 5000) (present 5000 stored)
            `shouldBe` Left (SourceOutOfOrder EpssFeed (OlderSnapshot (hour 9) (hour 10)))

    it "publishes through an optional outage and keeps the last successful EPSS marker" $ do
        plan <- expectPlan (planWith fx (requestFor fx (osvAt (Just (hour 11)) "osv-b") Nothing 5000) (present 5000 stored))
        plEpssOrder plan `shouldBe` Nothing
        (psEpssStatus (plState plan), psLastEpss (plState plan)) `shouldBe` (EnrichmentUnavailable, Just (epssAt (Just (hour 10)) "epss-a"))

    it "orders a returning feed against the marker the outage kept" $ do
        during <- plState <$> expectPlan (planWith fx (requestFor fx (osvAt (Just (hour 11)) "osv-b") Nothing 5000) (present 5000 stored))
        planWith fx (requestFor fx (osvAt (Just (hour 12)) "osv-c") (Just (epssAt (Just (hour 9)) "epss-old")) 5000) (present 5000 during)
            `shouldBe` Left (SourceOutOfOrder EpssFeed (OlderSnapshot (hour 9) (hour 10)))
        recovered <- expectPlan (planWith fx (requestFor fx (osvAt (Just (hour 12)) "osv-c") (Just (epssAt (Just (hour 12)) "epss-new")) 5000) (present 5000 during))
        plEpssOrder recovered `shouldBe` Just OrderedForward
        (psEpssStatus (plState recovered), psLastEpss (plState recovered)) `shouldBe` (EnrichmentAvailable, Just (epssAt (Just (hour 12)) "epss-new"))

predecessorSpec :: Fixture -> Spec
predecessorSpec fx = describe "a stored object it cannot order against" $ do
    let request = requestFor fx (osvAt (Just (hour 2)) "osv-2") Nothing 5000
        stored = baseState fx (osvAt (Just (hour 1)) "osv-1") Nothing
    it "refuses an object without publication metadata, naming what is missing" $ do
        let missing = EnvelopeMissing ("publication-version" :| ["osv-source"])
        planWith fx request (StorePresent revision 5000 (Left missing)) `shouldBe` Left (PredecessorUnreadable missing)
        renderPublicationRefusal (PredecessorUnreadable missing) `shouldSatisfy` T.isInfixOf "publication-version, osv-source"

    for_ [("another ecosystem", stored{psEcosystem = "cargo"}), ("another schema epoch", stored{psSchemaEpoch = osvSchemaEpoch - 1})] $ \(label, foreignState) ->
        it ("refuses metadata that names " <> label) $
            planWith fx request (present 5000 foreignState) `shouldBe` Left (PredecessorOutOfScope (psEcosystem foreignState) (psSchemaEpoch foreignState))

shrinkSpec :: Fixture -> Spec
shrinkSpec fx = describe "the durable size baseline" $ do
    let stored = (baseState fx (osvAt (Just (hour 10)) "osv-a") Nothing){psBaselineBytes = 1000}
    for_ [(980, Nothing), (1000, Nothing), (979, Just (ShrinkReading 979 980 1000 0))] $ \(bytes, refusal) ->
        it ("measures " <> show bytes <> " bytes against a 1000-byte baseline at 2%") $
            leftToMaybe (planWith fx (requestFor fx (osvAt (Just (hour 11)) "osv-b") Nothing bytes) (present 1000 stored))
                `shouldBe` (ShrinkPastBaseline <$> refusal)

    it "never lowers the baseline after small shrinks, so they cannot add up" $ do
        let steps = walk fx stored [990, 985, 981, 979]
        map (fmap (psBaselineBytes . plState)) steps
            `shouldBe` [Right 1000, Right 1000, Right 1000, Left (ShrinkPastBaseline (ShrinkReading 979 980 1000 0))]

    it "raises the baseline when the artifact grows, and still accepts a small withdrawal after it" $
        map (fmap (psBaselineBytes . plState)) (walk fx stored [1200, 1180]) `shouldBe` [Right 1200, Right 1200]

    it "rounds the minimum up, so a fraction of a byte never passes" $
        planWith fx (requestFor fx (osvAt (Just (hour 11)) "osv-b") Nothing 980) (present 1001 stored{psBaselineBytes = 1001})
            `shouldBe` Left (ShrinkPastBaseline (ShrinkReading 980 981 1001 0))

epssCreditSpec :: Fixture -> Spec
epssCreditSpec fx = describe "the EPSS outage credit" $ do
    let scored = (baseState fx (osvAt (Just (hour 10)) "osv-a") (Just (epssAt (Just (hour 10)) "epss-a"))){psBaselineBytes = 1000, psScoredRows = 10}
        outage stored bytes = planWith fx (requestFor fx (osvAt (Just (hour 11)) "osv-b") Nothing bytes) (present 1000 stored)
        recovery = requestFor fx (osvAt (Just (hour 12)) "osv-c") (Just (epssAt (Just (hour 12)) "epss-b"))
    it "grants the credit once, capped at 5% of the previous size, on the fall from available to unavailable" $ do
        accepted <- plState <$> expectPlan (outage scored 931)
        (psEpssCreditBytes accepted, psAdjustment accepted, psBaselineBytes accepted) `shouldBe` (50, EpssOutageCredit, 1000)
        outage scored 930 `shouldBe` Left (ShrinkPastBaseline (ShrinkReading 930 931 1000 50))

    for_ [(800, 40, 941), (1200, 60, 922)] $ \(previousBytes, credit, minimumBytes) ->
        it ("caps the credit at 5% of the stored object's " <> show previousBytes <> " bytes, not of its 1000-byte baseline") $ do
            let lost bytes = planWith fx (requestFor fx (osvAt (Just (hour 11)) "osv-b") Nothing bytes) (StorePresent revision previousBytes (Right scored))
            psEpssCreditBytes . plState <$> lost minimumBytes `shouldBe` Right credit
            lost (minimumBytes - 1) `shouldBe` Left (ShrinkPastBaseline (ShrinkReading (minimumBytes - 1) minimumBytes 1000 credit))

    it "sizes the credit at 12 bytes per scored range when that is under the cap" $ do
        accepted <- plState <$> expectPlan (outage scored{psScoredRows = 2} 957)
        psEpssCreditBytes accepted `shouldBe` 24
        outage scored{psScoredRows = 2} 956 `shouldBe` Left (ShrinkPastBaseline (ShrinkReading 956 957 1000 24))

    it "carries the credit through the outage without granting more, so the same artifact passes again" $ do
        during <- plState <$> expectPlan (outage scored 931)
        let again bytes = planWith fx (requestFor fx (osvAt (Just (hour 12)) "osv-c") Nothing bytes) (present 931 during)
        psEpssCreditBytes . plState <$> again 931 `shouldBe` Right 50
        again 930 `shouldBe` Left (ShrinkPastBaseline (ShrinkReading 930 931 1000 50))

    it "grants nothing to an outage that follows an unavailable feed" $
        outage scored{psEpssStatus = EnrichmentUnavailable} 979 `shouldBe` Left (ShrinkPastBaseline (ShrinkReading 979 980 1000 0))

    it "clears the credit when the feed returns, against the unlowered baseline" $ do
        during <- plState <$> expectPlan (outage scored 931)
        recovered <- plState <$> expectPlan (planWith fx (recovery 980) (present 931 during))
        (psEpssCreditBytes recovered, psBaselineBytes recovered) `shouldBe` (0, 1000)
        planWith fx (recovery 979) (present 931 during) `shouldBe` Left (ShrinkPastBaseline (ShrinkReading 979 980 1000 0))

    it "does not walk the baseline down over repeated outages and recoveries" $ do
        firstOutage <- plState <$> expectPlan (outage scored 931)
        recovered <- plState <$> expectPlan (planWith fx (recovery 1000) (present 931 firstOutage))
        secondOutage <- plState <$> expectPlan (planWith fx (requestFor fx (osvAt (Just (hour 13)) "osv-d") Nothing 931) (present 1000 recovered))
        (psBaselineBytes secondOutage, psEpssCreditBytes secondOutage) `shouldBe` (1000, 50)

    it "grants no credit when the cap is zero" $ do
        noCredit <- policyOf 2 0
        planPublication noCredit DurableBaseline (requestFor fx (osvAt (Just (hour 11)) "osv-b") Nothing 931) (present 1000 scored)
            `shouldBe` Left (ShrinkPastBaseline (ShrinkReading 931 980 1000 0))

    it "grants no credit for fewer scores in a feed that arrived" $
        planWith fx (requestFor fx (osvAt (Just (hour 11)) "osv-b") (Just (epssAt (Just (hour 11)) "epss-b")) 931) (present 1000 scored)
            `shouldBe` Left (ShrinkPastBaseline (ShrinkReading 931 980 1000 0))

rebaselineSpec :: Fixture -> Spec
rebaselineSpec fx = describe "an operator rebaseline" $ do
    let stored = (baseState fx (osvAt (Just (hour 10)) "osv-a") Nothing){psBaselineBytes = 1000}
        approved expected allowance at bytes = do
            approval <- Rebaseline (ObjectRevision expected) <$> percentOf allowance
            pure (requestFor fx (osvAt (Just (hour at)) "osv-b") Nothing bytes){prRebaseline = Just approval}
    for_ [("\"etag-1\"", "quoted"), ("etag-1", "unquoted")] $ \(expected, label) ->
        it ("restarts the baseline under its own allowance when the " <> label <> " ETag matches") $ do
            request <- approved expected 50 11 600
            accepted <- plState <$> expectPlan (planWith fx request (present 1000 stored))
            (psBaselineBytes accepted, psEpssCreditBytes accepted, psAdjustment accepted) `shouldBe` (600, 0, OperatorRebaseline)

    it "still holds the artifact to the rebaseline's own allowance" $ do
        request <- approved "\"etag-1\"" 10 11 600
        planWith fx request (present 1000 stored) `shouldBe` Left (ShrinkPastBaseline (ShrinkReading 600 900 1000 0))

    it "binds to the revision it names, so a retry cannot carry it to another predecessor" $ do
        request <- approved "\"etag-0\"" 50 11 600
        planWith fx request (present 1000 stored) `shouldBe` Left (RebaselineMismatch (ObjectRevision "\"etag-0\"") (Just revision))
        planWith fx request StoreAbsent `shouldBe` Left (RebaselineMismatch (ObjectRevision "\"etag-0\"") Nothing)

    it "cannot bypass the ordering check or the missing-metadata refusal" $ do
        request <- approved "\"etag-1\"" 100 9 600
        planWith fx request (present 1000 stored) `shouldBe` Left (SourceOutOfOrder OsvExport (OlderSnapshot (hour 9) (hour 10)))
        let missing = EnvelopeMissing ("baseline-bytes" :| [])
        planWith fx request (StorePresent revision 1000 (Left missing)) `shouldBe` Left (PredecessorUnreadable missing)

capabilitySpec :: Fixture -> Spec
capabilitySpec fx = describe "the store's baseline capability" $ do
    let stored = (baseState fx (osvAt (Just (hour 10)) "osv-a") Nothing){psBaselineBytes = 2000}
        request = requestFor fx (osvAt (Just (hour 11)) "osv-b") Nothing
    it "measures a previous-object store from the stored object's own size" $ do
        psBaselineBytes . plState <$> planPublication (fxPolicy fx) PreviousObjectBaseline (request 980) (present 1000 stored) `shouldBe` Right 980
        planPublication (fxPolicy fx) PreviousObjectBaseline (request 979) (present 1000 stored) `shouldBe` Left (ShrinkPastBaseline (ShrinkReading 979 980 1000 0))

    it "checks no size where the store keeps no baseline, and names each capability" $ do
        isRight (planPublication (fxPolicy fx) NoBaseline (request 1) (present 1000 stored)) `shouldBe` True
        map renderBaselineCapability [DurableBaseline, PreviousObjectBaseline, NoBaseline] `shouldBe` ["durable baseline", "previous-object baseline", "no baseline"]

-- What every case shares: the shipped 2% and 5%, one nonce, and the ecosystem under test.
data Fixture = Fixture
    { fxPolicy :: ShrinkPolicy
    , fxNonce :: PublicationNonce
    , fxEcosystem :: Text
    }

newFixture :: IO Fixture
newFixture = Fixture <$> policyOf 2 5 <*> newPublicationNonce <*> pure "npm"

policyOf :: Rational -> Rational -> IO ShrinkPolicy
policyOf shrink cap = ShrinkPolicy <$> percentOf shrink <*> percentOf cap

percentOf :: Rational -> IO Percent
percentOf = maybe (fail "a fixture percentage is out of range") pure . mkPercent

hour :: Integer -> UTCTime
hour offset = addUTCTime (fromInteger (offset * 3600)) (UTCTime (fromGregorian 2026 10 1) 0)

osvAt, otherOsvAt, epssAt :: Maybe UTCTime -> ByteString -> SourceMarker
osvAt = markerFor "https://osv.example.test/all.zip"
otherOsvAt = markerFor "https://mirror.example.test/all.zip"
epssAt = markerFor "https://epss.example.test/scores.csv.gz"

markerFor :: Text -> Maybe UTCTime -> ByteString -> SourceMarker
markerFor url stamp body = SourceMarker (sourceIdentityOf url) stamp (sourceDigestOf (digestOf body))

revision :: ObjectRevision
revision = ObjectRevision "\"etag-1\""

present :: Integer -> PublishedState -> StoreObservation
present bytes stored = StorePresent revision bytes (Right stored)

-- A stored state each case adjusts. Scored rows follow the feed, as a compile records them.
baseState :: Fixture -> SourceMarker -> Maybe SourceMarker -> PublishedState
baseState fx osv epss =
    PublishedState
        { psEcosystem = fxEcosystem fx
        , psSchemaEpoch = osvSchemaEpoch
        , psOsv = osv
        , psEpssStatus = maybe EnrichmentUnavailable (const EnrichmentAvailable) epss
        , psLastEpss = epss
        , psRelevantRows = 100
        , psScoredRows = 0
        , psBaselineBytes = 5000
        , psEpssCreditBytes = 0
        , psNonce = fxNonce fx
        , psAdjustment = OrdinaryBaseline
        }

requestFor :: Fixture -> SourceMarker -> Maybe SourceMarker -> Integer -> PublicationRequest
requestFor fx osv epss bytes =
    PublicationRequest (CandidateFacts (fxEcosystem fx) osv epss 100 (maybe 0 (const 10) epss)) bytes (fxNonce fx) Nothing

planWith :: Fixture -> PublicationRequest -> StoreObservation -> Either PublicationRefusal PublicationPlan
planWith fx = planPublication (fxPolicy fx) DurableBaseline

planAgainst :: Fixture -> SourceMarker -> SourceMarker -> Either PublicationRefusal PublicationPlan
planAgainst fx stored candidate = planWith fx (requestFor fx candidate Nothing 5000) (present 5000 (baseState fx stored Nothing))

-- Publish each size in turn over the last accepted state, each under a later OSV snapshot.
walk :: Fixture -> PublishedState -> [Integer] -> [Either PublicationRefusal PublicationPlan]
walk fx = go 11
  where
    go _ _ [] = []
    go at stored (bytes : rest) =
        let outcome = planWith fx (requestFor fx (osvAt (Just (hour at)) ("osv-" <> show at)) Nothing bytes) (present bytes stored)
         in outcome : go (at + 1) (either (const stored) plState outcome) rest

expectPlan :: Either PublicationRefusal PublicationPlan -> IO PublicationPlan
expectPlan = either (fail . toString . renderPublicationRefusal) pure

-- The OSV half of an outcome: its refusal, or the order it was accepted in.
osvOutcome :: Either PublicationRefusal PublicationPlan -> Either (Maybe OrderRefusal) SourceOrder
osvOutcome = bimap refusalOf plOsvOrder
  where
    refusalOf = \case
        SourceOutOfOrder OsvExport order -> Just order
        _ -> Nothing
