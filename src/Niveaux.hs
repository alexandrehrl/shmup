module Niveaux (chargerObstacles) where

import Lib (Obstacle (..), Coord (..))

-- Charge un fichier ASCII où '#' représente un obstacle.
-- Chaque ligne du fichier correspond à une ligne de l'écran de jeu.
-- Retourne la liste des obstacles correspondant aux '#' trouvés.
chargerObstacles :: FilePath -> IO [Obstacle]
chargerObstacles path = do
  contenu <- readFile path
  let ls = lines contenu
  return [ Obstacle (Coord c l)
         | (l, ligne) <- zip [0..] ls
         , (c, car)   <- zip [0..] ligne
         , car == '#' ]
