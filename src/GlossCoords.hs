module GlossCoords
  ( cellSize
  , hudH
  , windowW
  , windowH
  , toGlossXY
  ) where

import Lib (Coord (..))

cellSize :: Float
cellSize = 32

hudH :: Float
hudH = 60

windowW :: Int
windowW = 960

windowH :: Int
windowH = 700

-- Grille 30x20, cellule 32px, HUD 60px en bas.
-- Gloss : origine au centre, y croissant vers le haut.
-- Jeu   : origine en haut-gauche, y croissant vers le bas.

toGlossXY :: Coord -> (Float, Float)
toGlossXY (Coord c l) =
  ( (fromIntegral c - 14.5) * cellSize
  , (9.5 - fromIntegral l)  * cellSize + hudH / 2
  )
