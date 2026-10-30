-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | RubyGems ownership declarations use exact names and separator-delimited prefixes.
module Ecluse.Core.Registry.RubyGems.FirstParty (
    RubyGemsFirstParty,
    projectFirstPartyEntry,
    rubyGemsFirstPartyName,
) where

import Ecluse.Core.Package (PackageName, renderPackageName)
import Ecluse.Core.Registry (ParseError)
import Ecluse.Core.Registry.FirstParty (FirstPartyEntry, FirstPartyGrammar (..), ownsFirstPartyName, parseFirstPartyEntry)
import Ecluse.Core.Registry.RubyGems.Project (projectName, unscopedRubyGems)

-- | The tag that keeps RubyGems entries apart from another ecosystem's.
data RubyGemsNames

-- | An exact name or a prefix declared by the registry operator.
type RubyGemsFirstParty = FirstPartyEntry RubyGemsNames

-- Exact native spelling, a @-@, @_@ or @.@ before a prefix's @*@, and no foreign or scoped name.
rubyGemsFirstPartyGrammar :: FirstPartyGrammar RubyGemsNames
rubyGemsFirstPartyGrammar =
    FirstPartyGrammar
        { fpLabel = "RubyGems"
        , fpSeparator = (`elem` ['-', '_', '.'])
        , fpProjectName = projectName
        , fpOwnershipKey = \name -> renderPackageName name <$ guard (unscopedRubyGems name)
        }

-- | Parse an exact gem name or a prefix with a terminal separator followed by @*@.
projectFirstPartyEntry :: Text -> Either ParseError RubyGemsFirstParty
projectFirstPartyEntry = parseFirstPartyEntry rubyGemsFirstPartyGrammar

-- | Match exact native identities, excluding foreign ecosystems and npm scopes.
rubyGemsFirstPartyName :: NonEmpty RubyGemsFirstParty -> PackageName -> Bool
rubyGemsFirstPartyName = ownsFirstPartyName rubyGemsFirstPartyGrammar
