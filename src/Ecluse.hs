{- | The library entry point.

'run' is the composition root the @ecluse@ executable calls into (see
"Main"); keeping it here, rather than in @app/Main.hs@, keeps all logic in the
library where it can be tested. It is a placeholder until the server and worker
layers land (see @docs\/architecture.md@).
-}
module Ecluse (run) where

-- | Start Écluse. Currently a placeholder that announces startup.
run :: IO ()
run = putTextLn "écluse starting..."
