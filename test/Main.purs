module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Node.FS (FS)
import Test.Spec.Discovery (discover)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (PROCESS, run)

main :: forall e.
  Eff
    ( fs :: FS
    , avar :: AVAR
    , console :: CONSOLE
    , process :: PROCESS
    | e
    )
    Unit
main = discover "Test\\..+Spec" >>= run [consoleReporter]