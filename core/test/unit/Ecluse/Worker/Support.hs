-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The one import the worker unit specs take, over the fixture values
("Ecluse.Worker.Support.Fixtures") and the effectful doubles
("Ecluse.Worker.Support.Runtime").
-}
module Ecluse.Worker.Support (
    module Ecluse.Worker.Support.Fixtures,
    module Ecluse.Worker.Support.Runtime,
) where

import Ecluse.Worker.Support.Fixtures
import Ecluse.Worker.Support.Runtime
