-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The advisory-artifact identity the sync writes and every reader carries.

"Ecluse.Core.Cve" opens the artifact this marker names and "Ecluse.Runtime.Cve.Sync" installs
it. The rule vocabulary, the serve pipeline and the sweep only carry it, so it lives apart
from the module that opens SQLite.
-}
module Ecluse.Core.Cve.Types (
    DbEtag (..),
) where

{- | An artifact version marker: S3's ETag, opaque text compared for equality only.
Two objects with equal ETags carry equal bytes, so an unchanged ETag means nothing to do.
-}
newtype DbEtag = DbEtag Text
    deriving stock (Eq, Show)
