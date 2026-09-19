-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The read-only half of the CodeArtifact maintenance leaf: the calls that only observe, and
the evidence one observed version carries. A 'ReadPlane' holds no deletion, no cursor write, and
no publication, so holding one confers no authority over the repository, and an observation is
evidence a later decision reads rather than a permission it acts on. The requests and verdicts
the whole leaf shares live in "Ecluse.Runtime.Maintenance.CodeArtifact.Decide".
-}
module Ecluse.Runtime.Maintenance.CodeArtifact.Read (
    -- * The calls that only observe
    ReadPlane (..),

    -- * Where an observation was made
    identityOfStore,

    -- * One listing page
    versionsOfPage,
) where

import Ecluse.Runtime.Maintenance.CodeArtifact.Read.Internal (
    ReadPlane (..),
    identityOfStore,
    versionsOfPage,
 )
