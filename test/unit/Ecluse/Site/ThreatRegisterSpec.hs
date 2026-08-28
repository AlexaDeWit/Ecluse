-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Site.ThreatRegisterSpec (spec) where

import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Site.ThreatRegister (
    Threat (
        Threat,
        threatCategory,
        threatDescription,
        threatElement,
        threatMitigation,
        threatNumber,
        threatSeverity,
        threatStatus,
        threatTitle
    ),
    anchorThreats,
    decodeThreats,
    renderThreatRegister,
    sortThreats,
 )

spec :: Spec
spec = do
    describe "decodeThreats" $ do
        it "flattens a threat out of the cell it hangs off" $
            decodeThreats (modelOf [cellOf "Écluse proxy" [fullEntry]])
                `shouldBe` Right
                    [ Threat
                        { threatNumber = Just 7
                        , threatTitle = "A standing privilege"
                        , threatCategory = Just "Elevation of privilege"
                        , threatSeverity = Just "High"
                        , threatStatus = Just "Mitigated"
                        , threatDescription = Just "The threat."
                        , threatMitigation = Just "The control."
                        , threatElement = "Écluse proxy"
                        }
                    ]
        it "keeps every optional field absent when the entry omits it" $
            decodeThreats (modelOf [cellOf "An element" [object []]])
                `shouldBe` Right [bare{threatElement = "An element"}]
        it "names an element the model leaves unnamed" $
            decodeThreats (modelOf [object ["data" .= object ["threats" .= [object []]]]])
                `shouldBe` Right [bare]
        it "reads through a cell that carries no threats" $
            decodeThreats (modelOf [object ["data" .= object ["name" .= ("Idle" :: Text)]]])
                `shouldBe` Right []
        it "reads through a cell that carries no data" $
            decodeThreats (modelOf [object ["shape" .= ("actor" :: Text)]])
                `shouldBe` Right []
        it "rejects malformed JSON" $
            decodeThreats "{" `shouldSatisfy` isLeft

    describe "sortThreats" $ do
        it "orders the register by the model's numbering" $
            map threatNumber (sortThreats (map numbered [3, 1, 2]))
                `shouldBe` [Just 1, Just 2, Just 3]
        it "sorts an unnumbered threat first" $
            map threatNumber (sortThreats [numbered 2, bare])
                `shouldBe` [Nothing, Just 2]

    describe "anchorThreats" $ do
        it "anchors a threat on its number" $
            map fst (anchorThreats (map numbered [1, 2]))
                `shouldBe` ["threat-1", "threat-2"]
        it "suffixes a repeated number, counting from two" $
            map fst (anchorThreats (map numbered [14, 14, 14]))
                `shouldBe` ["threat-14", "threat-14-2", "threat-14-3"]
        it "anchors an unnumbered threat on the question mark it renders as" $
            map fst (anchorThreats [bare]) `shouldBe` ["threat-?"]

    describe "renderThreatRegister" $ do
        it "links an overview row down to its own detail section" $
            render [numbered 7] `carries` "| [7](#threat-7) |"
        it "anchors the detail heading and marks it with the threat class" $
            render [numbered 7] `carries` "### 7. Threat 7 {#threat-7 .threat}"
        it "orders the overview rows by threat number" $
            overviewNumbers (render (map numbered [3, 1, 2])) `shouldBe` ["1", "2", "3"]
        it "gives two threats sharing a number their own anchors" $ do
            let rendered = render [numbered 14, numbered 14]
            rendered `carries` "| [14](#threat-14) |"
            rendered `carries` "| [14](#threat-14-2) |"
            rendered `carries` "{#threat-14-2 .threat}"
        it "gives two threats sharing a title their own anchors" $ do
            let rendered = render [(numbered 4){threatTitle = "Same"}, (numbered 5){threatTitle = "Same"}]
            rendered `carries` "### 4. Same {#threat-4 .threat}"
            rendered `carries` "### 5. Same {#threat-5 .threat}"
        it "renders the element in emphasis and the category as written" $
            render [numbered 7] `carries` "| Tampering | Threat 7 | *An element* |"
        it "escapes a pipe in a title that would end the cell" $
            render [(numbered 7){threatTitle = "a | b"}] `carries` "| a \\| b |"
        it "labels the description and the mitigation" $ do
            render [numbered 7] `carries` "**Threat.** The threat."
            render [numbered 7] `carries` "**Mitigation.** The control."
        it "omits a label the model leaves empty" $
            render [(numbered 7){threatMitigation = Just ""}]
                `shouldSatisfy` (not . T.isInfixOf "**Mitigation.**")
        it "heads the detail sections" $
            render [numbered 7] `carries` "## Threat detail"

    describe "badge classes" $ do
        it "carries the severity" $
            for_ [("Critical", "critical"), ("High", "high"), ("Medium", "medium"), ("Low", "low")] $
                \(value, slug) ->
                    render [(numbered 7){threatSeverity = Just value}]
                        `carries` ("<span class=\"badge severity-" <> slug <> "\">" <> value <> "</span>")
        it "carries the status" $
            for_ [("Open", "open"), ("Mitigated", "mitigated"), ("Accepted", "accepted")] $
                \(value, slug) ->
                    render [(numbered 7){threatStatus = Just value}]
                        `carries` ("<span class=\"badge status-" <> slug <> "\">" <> value <> "</span>")
        it "spells NA out as N/A" $
            render [(numbered 7){threatStatus = Just "NA"}]
                `carries` "<span class=\"badge status-na\">N/A</span>"
        it "renders a missing severity as a question mark under no kind class" $
            render [bare] `carries` "<span class=\"badge\">?</span>"
        it "renders a missing category as a dash" $
            render [bare] `carries` "| - |"

render :: [Threat] -> Text
render = renderThreatRegister

carries :: Text -> Text -> Expectation
carries rendered fragment = (fragment `T.isInfixOf` rendered) `shouldBe` True

-- The overview rows in rendered order, read back from each row's first cell.
overviewNumbers :: Text -> [Text]
overviewNumbers rendered =
    [ T.takeWhile (/= ']') number
    | line <- T.lines rendered
    , Just number <- [T.stripPrefix "| [" line]
    ]

bare :: Threat
bare =
    Threat
        { threatNumber = Nothing
        , threatTitle = ""
        , threatCategory = Nothing
        , threatSeverity = Nothing
        , threatStatus = Nothing
        , threatDescription = Nothing
        , threatMitigation = Nothing
        , threatElement = "(unnamed)"
        }

numbered :: Int -> Threat
numbered number =
    Threat
        { threatNumber = Just number
        , threatTitle = "Threat " <> show number
        , threatCategory = Just "Tampering"
        , threatSeverity = Just "High"
        , threatStatus = Just "Mitigated"
        , threatDescription = Just "The threat."
        , threatMitigation = Just "The control."
        , threatElement = "An element"
        }

modelOf :: [Value] -> ByteString
modelOf cells = BL.toStrict (encode (object ["detail" .= object ["diagrams" .= [object ["cells" .= cells]]]]))

cellOf :: Text -> [Value] -> Value
cellOf name threats = object ["data" .= object ["name" .= name, "threats" .= threats]]

fullEntry :: Value
fullEntry =
    object
        [ "number" .= (7 :: Int)
        , "title" .= ("A standing privilege" :: Text)
        , "type" .= ("Elevation of privilege" :: Text)
        , "severity" .= ("High" :: Text)
        , "status" .= ("Mitigated" :: Text)
        , "description" .= ("The threat." :: Text)
        , "mitigation" .= ("The control." :: Text)
        ]
