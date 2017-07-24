module Main where

import Control.Monad.Eff (Eff)
import Einfluss.Server (startApp)
import Einfluss.Types (AppEff)
import Node.HTTP (Server)

--------------------------------------------------------------------------------

main :: ∀ eff. Eff (AppEff eff) Server
main = startApp
