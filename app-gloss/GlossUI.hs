module GlossUI
  ( Sprites (..)
  , GlossEnvi (..)
  , loadSprites
  , renderGloss
  , handleEvent
  , stepGloss
  ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game (Event (..), Key (..), SpecialKey (..), KeyState (..))
import Graphics.Gloss.Juicy (loadJuicyPNG)

import Lib
import GlossCoords

-- Types

data Sprites = Sprites
  { sprPlayer :: Picture
  , sprDirect :: Picture
  , sprZigzag :: Picture
  , sprTireur :: Picture
  , sprBoss :: Picture
  , sprObs :: Picture
  , sprLaserJ :: Picture
  , sprLaserE :: Picture
  , sprBonus :: Picture
  , sprHeart :: Picture
  , sprBackground :: Picture
  }

data GlossEnvi = GlossEnvi
  { gEnvi :: Envi
  , gSprites :: Sprites
  , gDir :: Char
  , gFire :: Bool
  , gFireHeld :: Bool
  , gAccum :: Float
  , gPrevPlayerCoord :: Coord
  , gTransition :: Maybe Float
  }

-- Sprites generes par Gloss (utilises si PNG absent)

-- Polygone regulier a n cotes de rayon r
ngon :: Int -> Float -> Picture
ngon n r = Polygon
  [ ( r * cos (2 * pi * fromIntegral i / fromIntegral n)
    , r * sin (2 * pi * fromIntegral i / fromIntegral n) )
  | i <- [0 .. n - 1]
  ]

-- Etoile a n branches, rayon exterieur r1, rayon interieur r2, pointe vers le haut
starPicture :: Int -> Float -> Float -> Picture
starPicture n r1 r2 = Polygon
  [ let angle = 2 * pi * fromIntegral i / fromIntegral (2 * n) + pi / 2
        r = if even i then r1 else r2
    in (r * cos angle, r * sin angle)
  | i <- [0 .. 2 * n - 1]
  ]

makeSprites :: Sprites
makeSprites = Sprites
  { sprPlayer = Color cyan $ Polygon [(0, 14), (-11, -8), (-5, -5), (0, -9), (5, -5), (11, -8)]
  , sprDirect = Color red $ Polygon [(0, -14), (-10, 8), (10, 8)]
  , sprZigzag = Color blue $ Polygon [(0, 13), (11, 0), (0, -13), (-11, 0)]
  , sprTireur = Color orange $ ngon 8 12
  , sprBoss = Pictures
      [ Color green $ Polygon [(0, 20), (-18, 0), (0, -20), (18, 0)]
      , Color (dark green) $ Polygon [(-7, 7), (7, 7), (7, -7), (-7, -7)]
      ]
  , sprObs = Color (greyN 0.6) $ Polygon
      [(5, 13), (12, 7), (10, -3), (5, -13), (-4, -11), (-13, -5), (-9, 5), (-4, 13)]
  , sprLaserJ = Pictures
      [ Color white $ rectangleSolid 5 28
      , Color (light cyan) $ rectangleSolid 10 20
      , Color cyan $ Polygon [(0, 14), (6, 4), (3, 4), (3, -14), (-3, -14), (-3, 4), (-6, 4)]
      ]
  , sprLaserE = Pictures
      [ Color (dark red) $ rectangleSolid 5 22
      , Color red $ rectangleSolid 3 20
      ]
  , sprBonus = Color yellow $ starPicture 5 11 5
  , sprHeart = Pictures
      ( [ Color (dark red) $ Translate (5*(fromIntegral c-3)) (5*(fromIntegral r-2.5)) (rectangleSolid 6 6)
        | (c,r) <- heartPixels ]
      ++ [ Color red $ Translate (5*(fromIntegral c-3)) (5*(fromIntegral r-2.5)) (rectangleSolid 4.5 4.5)
         | (c,r) <- heartPixels ])
  , sprBackground = Color black (rectangleSolid (fromIntegral windowW) (fromIntegral windowH))
  }

loadOrGen :: Float -> Picture -> FilePath -> IO Picture
loadOrGen sc fallback path = do
  mp <- loadJuicyPNG path
  return $ maybe fallback (Scale sc sc) mp

loadSprites :: IO Sprites
loadSprites = do
  let def = makeSprites
  Sprites
    <$> loadOrGen 1.00 (sprPlayer def) "assets/player.png"
    <*> loadOrGen 1.00 (sprDirect def) "assets/enemy_direct.png"
    <*> loadOrGen 1.00 (sprZigzag def) "assets/enemy_zigzag.png"
    <*> loadOrGen 1.00 (sprTireur def) "assets/enemy_tireur.png"
    <*> loadOrGen 1.00 (sprBoss def) "assets/boss.png"
    <*> loadOrGen 1.00 (sprObs def) "assets/obstacle.png"
    <*> loadOrGen 0.50 (sprLaserJ def) "assets/laser_joueur.png"
    <*> loadOrGen 0.50 (sprLaserE def) "assets/laser_ennemi.png"
    <*> loadOrGen 0.65 (sprBonus def) "assets/bonus.png"
    <*> loadOrGen 0.09 (sprHeart def) "assets/heart.png"
    <*> loadOrGen 0.75 (sprBackground def) "assets/background.png"

-- Dessin

drawAt :: Picture -> Coord -> Picture
drawAt pic coord =
  let (x, y) = toGlossXY coord
  in Translate x y pic

drawLerp :: Float -> Picture -> Coord -> Coord -> Picture
drawLerp t pic prev curr =
  let (px, py) = toGlossXY prev
      (cx, cy) = toGlossXY curr
  in Translate (px + t*(cx-px)) (py + t*(cy-py)) pic

drawLerpDown :: Float -> Picture -> Coord -> Picture
drawLerpDown t pic c@(Coord c0 r0) = drawLerp t pic (Coord c0 (r0-1)) c

drawLerpUp :: Float -> Picture -> Coord -> Picture
drawLerpUp t pic c@(Coord c0 r0) = drawLerp t pic (Coord c0 (r0+1)) c

renderObs :: Float -> Sprites -> Obstacle -> Picture
renderObs t spr o = drawLerpDown t (sprObs spr) (obsCoord o)

renderEnnemi :: Sprites -> Ennemi -> Picture
renderEnnemi spr e = drawAt pic (enCoord e)
  where
    pic = case enType e of
      Direct -> sprDirect spr
      Zigzag -> sprZigzag spr
      Tireur -> sprTireur spr

renderProj :: Float -> Sprites -> Projectile -> Picture
renderProj t spr p
  | projJoueur p = drawLerpUp t (sprLaserJ spr) (projCoord p)
  | otherwise = drawLerpDown t (sprLaserE spr) (projCoord p)

renderBonusEnt :: Sprites -> Bonus -> Picture
renderBonusEnt spr b = drawAt (sprBonus spr) (bonusCoord b)

renderBossEnt :: Sprites -> Boss -> Picture
renderBossEnt spr b = drawAt (Scale 2 2 (sprBoss spr)) (bossCoord b)

renderJoueur :: Float -> Sprites -> Coord -> Joueuse -> Picture
renderJoueur t spr prevCoord j = drawLerp t (sprPlayer spr) prevCoord (jouCoord j)

-- HUD et overlay

hudY :: Float
hudY = -(fromIntegral (windowH `div` 2) - hudH / 2)

hudTextScale :: Float
hudTextScale = 0.20

-- Dessine un texte avec une petite ombre et un effet "gras"
drawNiceText :: Float -> Color -> String -> Picture
drawNiceText sc col txt = Pictures
  [ -- Ombre portee
    Translate 2 (-2) $ Color (makeColor 0 0 0 0.8) baseText
  , Translate 3 (-3) $ Color (makeColor 0 0 0 0.8) baseText
    -- Faux Gras (decalage de 1px)
  , Translate (-1) 0 $ Color col baseText
  , Translate 1    0 $ Color col baseText
  , Translate 0 (-1) $ Color col baseText
  , Translate 0    1 $ Color col baseText
    -- Texte normal
  , Color col baseText
  ]
  where baseText = Scale sc sc (Text txt)

heartPixels :: [(Int, Int)]
heartPixels =
  [ (1,5),(2,5),(4,5),(5,5)
  , (0,4),(1,4),(2,4),(3,4),(4,4),(5,4),(6,4)
  , (0,3),(1,3),(2,3),(3,3),(4,3),(5,3),(6,3)
  , (1,2),(2,2),(3,2),(4,2),(5,2)
  , (2,1),(3,1),(4,1)
  , (3,0)
  ]

renderPV :: Picture -> Int -> Float -> Picture
renderPV heartSpr n y = Pictures
  [ Translate (startX + fromIntegral i * spacing) y heartSpr
  | i <- [0 .. n - 1]
  ]
  where
    spacing = 40
    startX = -445

renderHUD :: Sprites -> Envi -> Picture
renderHUD spr env = Pictures
  [ Color (makeColor 0 0 0 0.75) (Translate 0 hudY (rectangleSolid (fromIntegral windowW) hudH))
  , renderPV (sprHeart spr) (pvs (joueuse env)) hudY
  , seg (-230) scrStr
  , seg 30 nivStr
  , seg 260 bssStr
  ]
  where
    scrStr = "Score: " ++ show (score env) ++ " / " ++ show (scoreRequis (niveau env))
    nivStr = "Niv: " ++ show (niveau env)
    bssStr = case boss env of
               Just b -> "BOSS " ++ show (bossPV b) ++ " PV"
               Nothing -> ""
    seg x txt = Translate x hudY $ drawNiceText hudTextScale white txt

renderOverlay :: Statut -> Picture
renderOverlay EnCours = Blank
renderOverlay Perdu = Translate (-200) 0 (drawNiceText 0.4 red "GAME OVER")
renderOverlay Gagne = Translate (-220) 0 (drawNiceText 0.4 yellow "VICTOIRE !")

renderTransitionOverlay :: Int -> Float -> Picture
renderTransitionOverlay niv timeLeft = Pictures
  [ Color (makeColor 0 0 0 0.65) (rectangleSolid (fromIntegral windowW) (fromIntegral windowH))
  , Translate (-155) 40 $ drawNiceText 0.5 white ("NIVEAU " ++ show niv)
  , Translate (-55) (-50) $ drawNiceText 0.8 yellow countStr
  ]
  where
    countStr
      | timeLeft > 3 = "3"
      | timeLeft > 2 = "2"
      | timeLeft > 1 = "1"
      | otherwise = "GO!"

-- Rendu complet

renderGloss :: GlossEnvi -> Picture
renderGloss w = Pictures
  [ sprBackground spr
  , Pictures (map (renderObs t spr) (obstacles env))
  , Pictures (map (renderEnnemi spr) (ennemis env))
  , Pictures (map (renderProj t spr) (projectiles env))
  , Pictures (map (renderBonusEnt spr) (bonus env))
  , maybe Blank (renderBossEnt spr) (boss env)
  , renderJoueur t spr (gPrevPlayerCoord w) (joueuse env)
  , renderHUD spr env
  , renderOverlay (statut env)
  , case gTransition w of
      Nothing -> Blank
      Just tl -> renderTransitionOverlay (niveau env) tl
  ]
  where
    env = gEnvi w
    spr = gSprites w
    t = min 1.0 (gAccum w / tickInterval)

-- Entrees clavier

handleEvent :: Event -> GlossEnvi -> GlossEnvi
handleEvent (EventKey (Char c) Down _ _) w
  | c `elem` "zsqd" = w { gDir = c }
handleEvent (EventKey (Char c) Up _ _) w
  | c `elem` "zsqd" && gDir w == c = w { gDir = '\0' }
handleEvent (EventKey (Char ' ') Down _ _) w = w { gFire = True, gFireHeld = True }
handleEvent (EventKey (Char ' ') Up _ _) w = w { gFireHeld = False }
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) w = w { gFire = True, gFireHeld = True }
handleEvent (EventKey (SpecialKey KeySpace) Up _ _) w = w { gFireHeld = False }
handleEvent _ w = w

-- Step (8x/s)

tickInterval :: Float
tickInterval = 1.0 / 8.0

startNextLevel :: GlossEnvi -> GlossEnvi
startNextLevel w =
  let env = gEnvi w
      env' = env { niveau = niveau env + 1
                 , joueuse = Joueuse (Coord 15 18) (pvs (joueuse env))
                 , ennemis = []
                 , obstacles = []
                 , projectiles = []
                 , bonus = []
                 , boss = Nothing
                 }
  in w { gEnvi = env'
       , gTransition = Just 4.0
       , gDir = '\0'
       , gFire = False
       , gFireHeld = False
       , gPrevPlayerCoord = Coord 15 18
       }

normalStep :: GlossEnvi -> Float -> GlossEnvi
normalStep w accum' =
  let env = gEnvi w
      prevCoord = jouCoord (joueuse env)
      shouldFire = gFire w || gFireHeld w
      c = if shouldFire then ' ' else gDir w
      env' = snd (runJeu (tour c) env)
      w1 = w { gEnvi = env'
             , gFire = False
             , gAccum = accum' - tickInterval
             , gPrevPlayerCoord = prevCoord
             }
  in if score env' >= scoreRequis (niveau env') && statut env' == EnCours
     then startNextLevel w1
     else w1

stepGloss :: Float -> GlossEnvi -> GlossEnvi
stepGloss dt w =
  case gTransition w of
    Just tl ->
      let tl' = tl - dt
      in if tl' <= 0
         then w { gTransition = Nothing, gAccum = 0, gFire = False }
         else w { gTransition = Just tl' }
    Nothing ->
      if statut (gEnvi w) /= EnCours
      then w
      else
        let accum' = gAccum w + dt
        in if accum' < tickInterval
           then w { gAccum = accum' }
           else normalStep w accum'