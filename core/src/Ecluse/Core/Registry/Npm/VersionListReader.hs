-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native npm inventory reads retain compact witnesses for the shared release decoder.
module Ecluse.Core.Registry.Npm.VersionListReader (npmVersionListWalk) where

import Data.Aeson (Value (Null, Number, Object, String))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.JsonStream.Lexer (Cursor, Element (..))

import Ecluse.Core.Registry.Json.Intern (SipKey (SipKey), nameBytes, nameText, newInternTable)
import Ecluse.Core.Registry.Json.Shape (Mode (Keep), Shape (..), Trees (Trees), namedMembers, readShape)
import Ecluse.Core.Registry.Json.Walk (Step, Walk (finish), eachMember, emit, isString, pureStep, readString, skipFrom, tooDeep, withElement)
import Ecluse.Core.Registry.Npm.Project (projectVersionListEntry)
import Ecluse.Core.Registry.VersionList (VersionListItem (..))
import Ecluse.Core.Security (LimitError, Limits, maxNestingDepth)

-- | Count every source entry, including unusable releases, without retaining its installation metadata.
npmVersionListWalk :: Limits -> (s -> VersionListItem -> Either LimitError s) -> s -> Cursor -> Step s
npmVersionListWalk limits step initial tokens = withElement tokens $ \element rest -> case element of
    ObjectBegin -> send initial VersionListObject $ \begun ->
        if depth <= 0
            then tooDeep element rest
            else eachMember topField (\acc _ -> send acc VersionListObject finish) begun rest
    _ -> skipFrom element rest (const (finish initial))
  where
    depth = maxNestingDepth limits
    send = emit (pureStep step)
    topField acc key after continue
        | nameBytes key == "versions" = withElement after $ \element rest ->
            if depth - 1 <= 0
                then tooDeep element rest
                else case element of
                    ObjectBegin -> send acc VersionListContainer $ \begun -> eachMember release continue begun rest
                    JValue Null -> send acc VersionListContainer (`continue` rest)
                    _ -> skipFrom element rest (\afterValue -> send acc VersionListInvalidContainer (`continue` afterValue))
        | otherwise = withElement after $ \element rest -> skipFrom element rest (continue acc)
    release acc key after continue = withElement after $ \element rest ->
        readWitnessObject (depth - 2) (releaseField depth) element rest $ \raw afterValue ->
            send acc (projectVersionListEntry (nameText key) (Just raw)) (`continue` afterValue)

type WitnessReader s = Element -> Cursor -> (Value -> Cursor -> Step s) -> Step s

releaseField :: Int -> ByteString -> Maybe (Key.Key, WitnessReader s)
releaseField depth = \case
    "name" -> Just ("name", stringWitness (depth - 3))
    "version" -> Just ("version", stringWitness (depth - 3))
    "dist" -> Just ("dist", readWitnessObject (depth - 3) (distField (depth - 4)))
    "scripts" -> Just ("scripts", scriptsWitness (depth - 3))
    "deprecated" -> Just ("deprecated", ignoredWitness (depth - 3))
    "hasInstallScript" -> Just ("hasInstallScript", shapedWitness (Scalar (depth - 3)))
    "license" -> Just ("license", shapedWitness (personShape ["type", "url"] (depth - 3)))
    "_npmUser" -> Just ("_npmUser", shapedWitness (personShape ["name", "email", "url"] (depth - 3)))
    _ -> Nothing

distField :: Int -> ByteString -> Maybe (Key.Key, WitnessReader s)
distField budget = \case
    "tarball" -> Just ("tarball", stringWitness budget)
    "shasum" -> Just ("shasum", stringWitness budget)
    "integrity" -> Just ("integrity", stringWitness budget)
    _ -> Nothing

readWitnessObject :: Int -> (ByteString -> Maybe (Key.Key, WitnessReader s)) -> WitnessReader s
readWitnessObject budget field element rest next
    | budget <= 0 = tooDeep element rest
    | ObjectBegin <- element = eachMember member (next . Object) mempty rest
    | otherwise = skipFrom element rest (next Null)
  where
    member fields name after continue = withElement after $ \value afterKey -> case field (nameBytes name) of
        Nothing -> skipFrom value afterKey (continue fields)
        Just (key, readValue) -> readValue value afterKey $ \witness afterValue ->
            continue (if KeyMap.member key fields then fields else KeyMap.insert key witness fields) afterValue

stringWitness :: Int -> WitnessReader s
stringWitness budget element rest next
    | budget <= 0 = tooDeep element rest
    | isString element = readString element rest (\_ -> next (String ""))
    | JValue Null <- element = next Null rest
    | otherwise = skipFrom element rest (next (Number 0))

ignoredWitness :: Int -> WitnessReader s
ignoredWitness budget element rest next
    | budget <= 0 = tooDeep element rest
    | otherwise = skipFrom element rest (next Null)

scriptsWitness :: Int -> WitnessReader s
scriptsWitness budget element rest next
    | budget <= 0 = tooDeep element rest
    | ObjectBegin <- element = eachMember member (\valid -> next (if valid then Object mempty else Number 0)) True rest
    | JValue Null <- element = next Null rest
    | otherwise = skipFrom element rest (next (Number 0))
  where
    -- Every duplicate value participates. A later valid value cannot repair an invalid one.
    member valid _ after continue = withElement after $ \value afterKey ->
        if budget - 1 <= 0
            then tooDeep value afterKey
            else
                if isString value
                    then readString value afterKey (\_ -> continue valid)
                    else skipFrom value afterKey (continue False)

personShape :: [Text] -> Int -> Shape
personShape keys budget = StringOr budget (ObjectWith budget (namedMembers [(key, Scalar (budget - 1)) | key <- keys]) (Scalar budget))

-- Keep never inserts source keys or strings, so this empty table does not hash upstream data.
shapedWitness :: Shape -> WitnessReader s
shapedWitness shape element rest next =
    readShape Trees shape Keep (newInternTable (SipKey 0 0) []) element rest (\value _ -> next value)
