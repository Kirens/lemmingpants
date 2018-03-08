module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import Halogen.Aff as HA
import Network.HTTP.Affjax (AJAX)

main :: forall e. Eff (HA.HalogenEffects (ajax :: AJAX, console :: CONSOLE, dom :: DOM | e)) Unit
main = do
  log "Hello sailor!"
