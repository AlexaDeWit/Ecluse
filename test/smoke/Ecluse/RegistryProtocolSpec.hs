module Ecluse.RegistryProtocolSpec (spec) where

import Test.Hspec

{- | Smoke tests make __live__ calls to public registries (npm, PyPI) to confirm
our JSON decoding and protocol handling match reality.

They depend on uncontrolled external services, so they are __allowed to fail by
design__ and never gate a merge (the CI @gate@ does not depend on them). A
failure is a prompt to investigate — protocol drift, or just flakiness — not an
automatic blocker.

Real cases are added alongside the registry client and JSON codecs; this
placeholder keeps the suite wired up.
-}
spec :: Spec
spec =
    describe "live registry protocol (npm / PyPI)" $
        it "decodes a real packument from the public npm registry" pending
