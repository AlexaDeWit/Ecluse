module EcluseSpec (spec) where

import Test.Hspec

import Ecluse (run)

{- | The umbrella module is the composition root the @ecluse@ executable calls
into. It lives in the library (not @app/Main.hs@) so it can be exercised here
rather than only through the binary — and so it stays linked into the unit suite,
where the coverage completeness guard ('scripts/coverage.sh') can see it. 'run'
is a placeholder today, so this is a liveness check; expand it as the server and
worker layers land.
-}
spec :: Spec
spec =
    describe "run" $
        it "starts without throwing" $
            run `shouldReturn` ()
