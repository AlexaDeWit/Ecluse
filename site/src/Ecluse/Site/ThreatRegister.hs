-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The threat-register fragment: Écluse's OWASP Threat Dragon model rendered as
an overview table plus one detail section per threat.

The model file is the single source of truth, and every site build regenerates the
fragment from it, so the register cannot drift from the model. Threat Dragon nests
a threat under the diagram cell it threatens, so the walk carries that element's
name down onto each threat it holds.
-}
module Ecluse.Site.ThreatRegister (
    -- * The model
    Threat (..),
    decodeThreats,
    sortThreats,

    -- * Rendering
    anchorThreats,
    renderThreatRegister,
) where

import Data.Aeson (FromJSON, Value, eitherDecodeStrict, parseJSON, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser)
import Data.Text qualified as T

import Ecluse.Site.Markdown (
    Alignment (AlignLeft, AlignRight),
    attributedHeading,
    bold,
    escapeCell,
    heading,
    htmlSpan,
    link,
    slugify,
    table,
 )

{- | One threat, flattened out of the diagram cell it hangs off. Threat Dragon
writes every field but the element, and an entry missing one still renders.
-}
data Threat = Threat
    { threatNumber :: Maybe Int
    -- ^ The model's own numbering, which also forms the detail section's anchor.
    , threatTitle :: Text
    , threatCategory :: Maybe Text
    -- ^ The STRIDE category, Threat Dragon's @type@ field.
    , threatSeverity :: Maybe Text
    , threatStatus :: Maybe Text
    , threatDescription :: Maybe Text
    -- ^ Markdown prose, emitted into the detail section unparsed.
    , threatMitigation :: Maybe Text
    -- ^ Markdown prose, emitted into the detail section unparsed.
    , threatElement :: Text
    -- ^ The diagram element the threat hangs off.
    }
    deriving stock (Eq, Show)

{- | Decode every threat in a Threat Dragon v2 model, in model order. A missing
diagram, cell, or optional field is tolerated. Malformed JSON is not.
-}
decodeThreats :: ByteString -> Either Text [Threat]
decodeThreats = bimap toText modelThreats . eitherDecodeStrict

-- | Order the register by the model's numbering. An unnumbered threat sorts first.
sortThreats :: [Threat] -> [Threat]
sortThreats = sortOn (fromMaybe 0 . threatNumber)

newtype ThreatDragonModel = ThreatDragonModel {modelThreats :: [Threat]}

instance FromJSON ThreatDragonModel where
    parseJSON = withObject "Threat Dragon model" $ \o -> do
        detail <- o .: "detail"
        diagrams <- detail .:? "diagrams" .!= []
        ThreatDragonModel . concat <$> traverse diagramThreats diagrams

diagramThreats :: Value -> Parser [Threat]
diagramThreats = withObject "diagram" $ \o -> do
    cells <- o .:? "cells" .!= []
    concat <$> traverse cellThreats cells

cellThreats :: Value -> Parser [Threat]
cellThreats = withObject "diagram cell" $ \o -> do
    payload <- o .:? "data"
    maybe (pure []) elementThreats payload

elementThreats :: Value -> Parser [Threat]
elementThreats = withObject "cell data" $ \o -> do
    element <- o .:? "name" .!= unnamedElement
    entries <- o .:? "threats" .!= []
    traverse (threatOf element) entries

threatOf :: Text -> Value -> Parser Threat
threatOf element = withObject "threat" $ \o ->
    Threat
        <$> o .:? "number"
        <*> o .:? "title" .!= ""
        <*> o .:? "type"
        <*> o .:? "severity"
        <*> o .:? "status"
        <*> o .:? "description"
        <*> o .:? "mitigation"
        <*> pure element

unnamedElement :: Text
unnamedElement = "(unnamed)"

{- | Pair each threat with the anchor its detail section carries. The model can
number two threats alike, and a repeated anchor takes an occurrence suffix.
-}
anchorThreats :: [Threat] -> [(Text, Threat)]
anchorThreats = go []
  where
    go _ [] = []
    go used (threat : rest) =
        let anchor = firstFreeAnchor used ("threat-" <> threatNumberText threat)
         in (anchor, threat) : go (anchor : used) rest

-- The candidate stream is endless and the used list is finite, so one candidate
-- is always free.
firstFreeAnchor :: [Text] -> Text -> Text
firstFreeAnchor used base = fromMaybe base (find (`notElem` used) candidates)
  where
    candidates = base : [base <> "-" <> show n | n <- [2 :: Int ..]]

{- | Render the register: the overview table, then the detail sections its threat
numbers link down to.
-}
renderThreatRegister :: [Threat] -> Text
renderThreatRegister threats =
    T.unlines (overviewTable anchored <> ["", heading 2 "Threat detail"] <> concatMap detailSection anchored)
  where
    anchored = anchorThreats (sortThreats threats)

overviewTable :: [(Text, Threat)] -> [Text]
overviewTable threats = table columns (map overviewRow threats)
  where
    columns =
        [ (AlignRight, "#")
        , (AlignLeft, "Severity")
        , (AlignLeft, "Status")
        , (AlignLeft, "Category")
        , (AlignLeft, "Threat")
        , (AlignLeft, "Element")
        ]

overviewRow :: (Text, Threat) -> [Text]
overviewRow (anchor, threat) =
    [ link (threatNumberText threat) ("#" <> anchor)
    , severityBadge threat
    , statusBadge threat
    , escapeCell (fromMaybe absentField (threatCategory threat))
    , escapeCell (threatTitle threat)
    , emphasised (escapeCell (threatElement threat))
    ]

detailSection :: (Text, Threat) -> [Text]
detailSection (anchor, threat) =
    [ ""
    , attributedHeading 3 anchor ["threat"] (threatNumberText threat <> ". " <> threatTitle threat)
    , ""
    , T.unwords
        [ severityBadge threat
        , statusBadge threat
        , fromMaybe absentField (threatCategory threat)
        , separator
        , emphasised (threatElement threat)
        ]
    ]
        <> labelledProse "Threat." (threatDescription threat)
        <> labelledProse "Mitigation." (threatMitigation threat)

-- The description and mitigation fields carry Markdown, so they pass through
-- unparsed behind a bold label.
labelledProse :: Text -> Maybe Text -> [Text]
labelledProse label body = case body of
    Just prose | not (T.null prose) -> ["", bold label <> " " <> prose]
    _ -> []

severityBadge :: Threat -> Text
severityBadge threat = badge "severity" (threatSeverity threat) (fromMaybe "?" (threatSeverity threat))

statusBadge :: Threat -> Text
statusBadge threat = badge "status" (threatStatus threat) (statusLabel (threatStatus threat))

-- Threat Dragon stores "not applicable" as NA, and the register spells it out. A
-- status the model omits reads as the question mark a missing severity uses.
statusLabel :: Maybe Text -> Text
statusLabel = \case
    Just "NA" -> "N/A"
    Just status -> status
    Nothing -> "?"

-- The kind class carries the styling, so a value the model omits contributes no
-- class rather than a dangling "severity-" token.
badge :: Text -> Maybe Text -> Text -> Text
badge kind value = htmlSpan ("badge" : kindClass)
  where
    kindClass = case slugify <$> value of
        Just slug | not (T.null slug) -> [kind <> "-" <> slug]
        _ -> []

threatNumberText :: Threat -> Text
threatNumberText = maybe "?" show . threatNumber

emphasised :: Text -> Text
emphasised text = "*" <> text <> "*"

-- The entity keeps the source ASCII, per docs/style.md section 13.
separator :: Text
separator = "&middot;"

absentField :: Text
absentField = "-"
