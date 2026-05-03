module Main (main) where

import Graphics.Gloss     (Display (..), play, black)
import System.Environment (getArgs)
import Lib                (envi0, Envi (..), Coord (..))
import Niveaux            (chargerObstacles)
import GlossUI
import GlossCoords        (windowW, windowH)

main :: IO ()
main = do
  args   <- getArgs
  obsIni <- case args of
               (f:_) -> chargerObstacles f
               []    -> return []
  let env0    = envi0 { obstacles = obsIni }
  spr        <- loadSprites
  let world0  = GlossEnvi env0 spr '\0' False False 0 (Coord 15 18) Nothing
      display = InWindow "Spacegame" (windowW, windowH) (100, 100)
  play display black 60 world0 renderGloss handleEvent stepGloss
