-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Cargo ownership declarations use native collision keys and separator-delimited prefixes.
module Ecluse.Core.Registry.Cargo.FirstParty (
    CargoFirstParty,
    projectFirstPartyEntry,
    cargoFirstPartyName,
    cargoFirstPartyNames,
) where

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (ParseError)
import Ecluse.Core.Registry.Cargo.Project (cargoCollisionKey, projectName)
import Ecluse.Core.Registry.FirstParty (FirstPartyEntry, FirstPartyGrammar (..), FirstPartyNames (..), ownsFirstPartyName, parseFirstPartyEntry)

-- | The tag that keeps Cargo entries apart from other ecosystems'. It has no values.
data CargoNames

-- | An exact crate name or a native collision prefix declared by the registry operator.
type CargoFirstParty = FirstPartyEntry CargoNames

-- | Prefixes end in a hyphen or underscore, and names compare by their collision key.
cargoFirstPartyGrammar :: FirstPartyGrammar CargoNames
cargoFirstPartyGrammar =
    FirstPartyGrammar
        { fpLabel = "Cargo"
        , fpSeparator = \c -> c == '-' || c == '_'
        , fpProjectName = projectName
        , fpOwnershipKey = cargoCollisionKey
        }

-- | Parse an exact crate name or a prefix with a terminal separator followed by @*@.
projectFirstPartyEntry :: Text -> Either ParseError CargoFirstParty
projectFirstPartyEntry = parseFirstPartyEntry cargoFirstPartyGrammar

-- | Match native ownership aliases, excluding foreign ecosystems and npm scopes.
cargoFirstPartyName :: NonEmpty CargoFirstParty -> PackageName -> Bool
cargoFirstPartyName = ownsFirstPartyName cargoFirstPartyGrammar

-- | Names and prefixes the mount's @firstParty@ entries declare.
cargoFirstPartyNames :: FirstPartyNames CargoFirstParty
cargoFirstPartyNames =
    FirstPartyNames
        { namesEntry = projectFirstPartyEntry
        , namesOwn = cargoFirstPartyName
        , namesEntryNoun = "Cargo entry"
        , namesListNoun = "crate or prefix"
        }
