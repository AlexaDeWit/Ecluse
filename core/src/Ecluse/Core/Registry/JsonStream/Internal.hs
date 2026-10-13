-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Member selection shared by production combinators and independent reference readers.
module Ecluse.Core.Registry.JsonStream.Internal (Members (..)) where

import Data.Aeson (Value)
import Data.Aeson.Key qualified as Key
import Data.JsonStream.Parser qualified as J

-- | Object member selection. Named members share each stored key across retained objects.
data Members
    = NamedMembers (HashMap Text (Key.Key, J.Parser Value))
    | EveryMember (J.Parser Value)
