module Main (main) where

import Test.Hspec
import Test.QuickCheck

import Data.Maybe (isJust)
import Lib
import GlossCoords (toGlossXY)

-- =============================================================================
-- Arbitrary instances pour QuickCheck
-- =============================================================================

instance Arbitrary Direction where
  arbitrary = elements [H, B, G, D, N]

instance Arbitrary Coord where
  arbitrary = do
    c <- choose (0, 29)
    l <- choose (0, 19)
    return (Coord c l)

-- =============================================================================
-- Environnement minimal pour les tests unitaires
-- =============================================================================

envi0Test :: Envi
envi0Test = envi0

-- =============================================================================
-- Propriétés QuickCheck
-- =============================================================================

-- Après un déplacement, la colonne du joueur reste dans [0, largeur-1]
prop_depJ_col_inBounds :: Direction -> Bool
prop_depJ_col_inBounds dir =
  let (_, env') = runJeu (depJ dir) envi0Test
      c = col (jouCoord (joueuse env'))
      w = largeur (ecran env')
  in  c >= 0 && c < w

-- Après un déplacement, la ligne du joueur reste dans [0, hauteur-1]
prop_depJ_lig_inBounds :: Direction -> Bool
prop_depJ_lig_inBounds dir =
  let (_, env') = runJeu (depJ dir) envi0Test
      l = lig (jouCoord (joueuse env'))
      h = hauteur (ecran env')
  in  l >= 0 && l < h

-- Après scrollObstacles, aucun obstacle ne dépasse le bas de l'écran
prop_scroll_inBounds :: Property
prop_scroll_inBounds =
  let obs   = [Obstacle (Coord 5 10), Obstacle (Coord 3 18), Obstacle (Coord 7 19)]
      env   = envi0Test { obstacles = obs }
      (_, env') = runJeu scrollObstacles env
      h     = hauteur (ecran env')
  in  property (all (\o -> lig (obsCoord o) < h) (obstacles env'))

-- =============================================================================
-- Tests HSpec
-- =============================================================================

main :: IO ()
main = hspec $ do

  describe "charToDir" $ do
    it "convertit 'z' en Haut"   $ charToDir 'z' `shouldBe` H
    it "convertit 's' en Bas"    $ charToDir 's' `shouldBe` B
    it "convertit 'q' en Gauche" $ charToDir 'q' `shouldBe` G
    it "convertit 'd' en Droite" $ charToDir 'd' `shouldBe` D
    it "convertit tout autre caractère en Neutre" $ do
      charToDir 'a' `shouldBe` N
      charToDir ' ' `shouldBe` N
      charToDir 'x' `shouldBe` N

  describe "depJ" $ do
    it "ne sort pas par la gauche" $ do
      let env = envi0Test { joueuse = Joueuse (Coord 0 10) 3 }
          (_, env') = runJeu (depJ G) env
      col (jouCoord (joueuse env')) `shouldBe` 0

    it "ne sort pas par le haut" $ do
      let env = envi0Test { joueuse = Joueuse (Coord 15 0) 3 }
          (_, env') = runJeu (depJ H) env
      lig (jouCoord (joueuse env')) `shouldBe` 0

    it "ne sort pas par la droite" $ do
      let env = envi0Test { joueuse = Joueuse (Coord 29 10) 3 }
          (_, env') = runJeu (depJ D) env
          w = largeur (ecran env')
      col (jouCoord (joueuse env')) `shouldBe` (w - 1)

    it "ne sort pas par le bas" $ do
      let env = envi0Test { joueuse = Joueuse (Coord 15 19) 3 }
          (_, env') = runJeu (depJ B) env
          h = hauteur (ecran env')
      lig (jouCoord (joueuse env')) `shouldBe` (h - 1)

    it "QuickCheck : colonne toujours dans les bornes" $
      property prop_depJ_col_inBounds

    it "QuickCheck : ligne toujours dans les bornes" $
      property prop_depJ_lig_inBounds

  describe "scrollObstacles" $ do
    it "descend chaque obstacle d'une ligne" $ do
      let env = envi0Test { obstacles = [Obstacle (Coord 5 3)] }
          (_, env') = runJeu scrollObstacles env
      map (lig . obsCoord) (obstacles env') `shouldBe` [4]

    it "supprime les obstacles qui atteignent le bas" $ do
      let h   = hauteur (ecran envi0Test)
          env = envi0Test { obstacles = [Obstacle (Coord 5 (h - 1))] }
          (_, env') = runJeu scrollObstacles env
      obstacles env' `shouldBe` []

    it "QuickCheck : aucun obstacle hors écran après scroll" $
      property prop_scroll_inBounds

  describe "impactProj" $ do
    it "détecte un ennemi sur la case" $ do
      let grille = construireGrille envi0Test
                     { ennemis = [Ennemi (Coord 5 5) 2 Direct 0] }
      isJust (impactProj (Coord 5 5) grille) `shouldBe` True

    it "retourne Nothing sur une case vide" $ do
      let grille = construireGrille envi0Test { ennemis = [] }
      impactProj (Coord 0 0) grille `shouldBe` Nothing

    it "retourne Nothing sur une case joueur (pas une cible)" $ do
      let env    = envi0Test { joueuse = Joueuse (Coord 5 5) 3, ennemis = [] }
          grille = construireGrille env
      impactProj (Coord 5 5) grille `shouldBe` Nothing

  describe "valeurBonus" $ do
    it "retourne la valeur d'un BonusScore présent" $ do
      let bns     = [Bonus (Coord 3 3) BonusScore]
          bMap    = construireMapBonus envi0Test { bonus = bns }
      valeurBonus (Coord 3 3) bMap effetsBonus `shouldBe` Just (BonusScore, 200)

    it "retourne la valeur d'un ExtraVie présent" $ do
      let bns     = [Bonus (Coord 2 4) ExtraVie]
          bMap    = construireMapBonus envi0Test { bonus = bns }
      valeurBonus (Coord 2 4) bMap effetsBonus `shouldBe` Just (ExtraVie, 1)

    it "retourne Nothing si aucun bonus à cette coordonnée" $ do
      let bMap = construireMapBonus envi0Test { bonus = [] }
      valeurBonus (Coord 0 0) bMap effetsBonus `shouldBe` Nothing

  describe "configPourNiveau" $ do
    it "niveau 1 : boss PV = 5" $
      cfgPVBoss (configPourNiveau 1) `shouldBe` 5

    it "niveau 3 : boss PV = 11" $
      cfgPVBoss (configPourNiveau 3) `shouldBe` 11

    it "niveau 5 : boss PV = 17" $
      cfgPVBoss (configPourNiveau 5) `shouldBe` 17

    it "fréquence ennemi niveau 1 = 8" $
      cfgFreqEn (configPourNiveau 1) `shouldBe` 8

    it "fréquence ennemi diminue avec le niveau" $
      cfgFreqEn (configPourNiveau 5) < cfgFreqEn (configPourNiveau 1) `shouldBe` True

  describe "GlossCoords" $ do
    describe "toGlossXY" $ do
      it "coin haut-gauche (0,0) -> (-464, 334)" $
        toGlossXY (Coord 0 0) `shouldBe` (-464.0, 334.0)
      it "coin bas-droite (29,19) -> (464, -274)" $
        toGlossXY (Coord 29 19) `shouldBe` (464.0, -274.0)
      it "colonne 14, ligne 9 -> (-16, 46)" $
        toGlossXY (Coord 14 9) `shouldBe` (-16.0, 46.0)
