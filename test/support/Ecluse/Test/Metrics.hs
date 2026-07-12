-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The metric vocabulary __projections__ the metrics tests assert against.

The @ecluse.*@ metric catalogue ('Ecluse.Core.Telemetry.Metrics') keeps its bounded-label
discipline correct-by-construction: the 'MetricName' and 'LabelKey' enums are closed sums,
so no unbounded identifier can be made into a label. These three projections exist so a
test can __enumerate__ that vocabulary and pin the invariants over it: that every metric
name renders to its @ecluse.*@ (or OTel @http.*@) wire name, that the label-key set is the
closed bounded set, and that no high-cardinality identifier is among the keys.

'allMetricNames' and 'allLabelKeys' are the Generic-derived 'Universe' enumerations of the
two enums; 'highCardinalityKeys' is the deny-list a test intersects against the label keys
to prove none of @package@ \/ @version@ \/ @scope@ \/ @message@ is a label.
-}
module Ecluse.Test.Metrics (
    allMetricNames,
    allLabelKeys,
    highCardinalityKeys,
) where

-- relude's prelude exports a Bounded/Enum-based `universe`; hide it so the
-- Generic-derived `Data.Universe.Class.universe` is the one in scope here.
import Prelude hiding (universe)

import Data.Universe.Class (universe)

import Ecluse.Core.Telemetry.Metrics (LabelKey, MetricName)

-- | Every metric in the catalogue (the Generic-derived 'Universe' enumeration).
allMetricNames :: [MetricName]
allMetricNames = universe

-- | Every label key in the closed set.
allLabelKeys :: [LabelKey]
allLabelKeys = universe

{- | The high-cardinality identifiers that must __never__ be metric labels: they live on
spans and the structured log line instead. The label-domain guard asserts none of these is
a 'LabelKey' wire name; there is, by construction, no @Label@ that produces one.
-}
highCardinalityKeys :: [Text]
highCardinalityKeys = ["package", "version", "scope", "message"]
