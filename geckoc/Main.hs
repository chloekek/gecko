module Main
  ( main
  ) where

import Gecko.C.Expression (Expression (GetGlobal))
import Gecko.C.PhaseI (execPhaseI)
import Gecko.C.PhaseII (execPhaseII)
import Gecko.Position (Position (..))

import qualified Gecko.C.PhaseI as C.PhaseI
import qualified Gecko.C.PhaseII as C.PhaseII

main :: IO ()
main = do
  let π = Position "<example>" 1 1

  kinds <- either (fail . show) pure $
    execPhaseI $ do
      C.PhaseI.recordTypeDefinition π "i32" (GetGlobal π "type")
      C.PhaseI.recordTypeDefinition π"pax" (GetGlobal π"type")
      pure ()
  print kinds

  typeDefinitions <- either (fail . show) pure $
    flip execPhaseII kinds $ do
      C.PhaseII.recordWrapperDefinition π "pax" [] (GetGlobal π "i32")
      pure ()
  print typeDefinitions
