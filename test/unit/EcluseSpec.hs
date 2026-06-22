module EcluseSpec (spec) where

import Test.Hspec
import UnliftIO (timeout)

import Ecluse (run)

{- | The umbrella module is the composition root the @ecluse@ executable calls
into. It lives in the library (not @app/Main.hs@) so it can be exercised here
rather than only through the binary — and so it stays linked into the unit suite,
where the coverage completeness guard ('scripts/coverage.sh') can see it. 'run'
assembles the composition root and starts the blocking server, so under a short
timeout it keeps serving rather than returning — the liveness check that it wires
up and starts without throwing.
-}
spec :: Spec
spec =
    describe "run" $
        it "assembles the composition root and starts serving (blocks) without throwing" $
            timeout 100000 run `shouldReturn` Nothing
