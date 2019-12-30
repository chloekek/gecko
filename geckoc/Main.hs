module Main
  ( main
  ) where

import Gecko.C.Expression (Expression (GetGlobal))
import Gecko.C.PhaseI (execPhaseI)
import Gecko.C.PhaseII (execPhaseII)
import Gecko.Position (Position (..))

import qualified Data.Text.IO as Text
import qualified Gecko.As.Parse as As.Parse
import qualified Gecko.C.PhaseI as C.PhaseI
import qualified Gecko.C.PhaseII as C.PhaseII

main :: IO ()
main = do
  source1 <- Text.readFile "std/list.gkas"
  either putStr print $ As.Parse.parseUnit "std/list.gkas" source1

  source2 <- Text.readFile "std/list/nil.gkas"
  either putStr print $ As.Parse.parseUnit "std/list/nil.gkas" source2

  source3 <- Text.readFile "std/list/cons.gkas"
  either putStr print $ As.Parse.parseUnit "std/list/cons.gkas" source3

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
