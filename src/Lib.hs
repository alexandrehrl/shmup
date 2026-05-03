module Lib
  ( -- Types
    Ecran (..)
  , Coord (..)
  , Direction (..)
  , Obstacle (..)
  , TypeEnnemi (..)
  , Ennemi (..)
  , Projectile (..)
  , TypeBonus (..)
  , Bonus (..)
  , Boss (..)
  , ConfigNiveau (..)
  , Joueuse (..)
  , Statut (..)
  , Cellule (..)
  , Grille
  , MapEffets
  , Envi (..)
    -- Monade Etat
  , Etat (..)
  , EtatJeu
  , get
  , put
  , modify
    -- Config niveaux
  , configPourNiveau
  , scoreRequis
    -- Maps et monade Maybe
  , effetsBonus
  , construireGrille
  , construireMapBonus
  , valeurBonus
  , impactProj
  , impactJoueur
    -- Logique de jeu
  , charToDir
  , depJ
  , tirer
  , avancerProjectiles
  , bougerEnnemis
  , bougerBoss
  , scrollObstacles
  , spawnObstacle
  , spawnEnnemi
  , spawnBonus
  , spawnBoss
  , tirerEnnemis
  , tirerBoss
  , traiterCollisionsProj
  , traiterCollisionsBoss
  , traiterCollisionsJoueur
  , collecterBonus
  , majStatut
  , tour
  , affiche
    -- Moteur
  , envi0
  , runJeu
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe, isJust)
import System.Random (StdGen, mkStdGen, randomR)

-- Section 1 : Types de base

data Ecran = Ecran
  { largeur :: Int
  , hauteur :: Int
  }

data Coord = Coord
  { col :: Int
  , lig :: Int
  } deriving (Eq, Ord, Show)

data Direction = H | B | G | D | N
  deriving (Eq, Show)

data Obstacle = Obstacle
  { obsCoord :: Coord
  } deriving (Eq, Show)

data TypeEnnemi = Direct | Zigzag | Tireur
  deriving (Eq, Ord, Show)

data Ennemi = Ennemi
  { enCoord :: Coord
  , enPV :: Int
  , enType :: TypeEnnemi
  , enTick :: Int
  } deriving (Eq, Show)

data Projectile = Projectile
  { projCoord :: Coord
  , projJoueur :: Bool
  } deriving (Eq, Show)

data TypeBonus = ExtraVie | BonusScore
  deriving (Eq, Ord, Show)

data Bonus = Bonus
  { bonusCoord :: Coord
  , bonusType :: TypeBonus
  } deriving (Eq, Show)

data Boss = Boss
  { bossCoord :: Coord
  , bossPV :: Int
  , bossTick :: Int
  } deriving (Eq, Show)

data ConfigNiveau = ConfigNiveau
  { cfgFreqObs :: Int
  , cfgFreqEn :: Int
  , cfgPVBoss :: Int
  , cfgScoreBoss :: Int
  } deriving (Eq, Show)

data Joueuse = Joueuse
  { jouCoord :: Coord
  , pvs :: Int
  } deriving (Eq, Show)

data Statut = EnCours | Perdu | Gagne
  deriving (Eq, Show)

data Cellule
  = CellObs
  | CellEnnemi Int TypeEnnemi
  | CellBoss Int
  | CellProjJoueur
  | CellProjEnnemi
  | CellBonus TypeBonus
  | CellJoueur
  deriving (Eq, Show)

type Grille = Map Coord [Cellule]
type MapEffets = Map TypeBonus Int

data Envi = Envi
  { ecran :: Ecran
  , obstacles :: [Obstacle]
  , ennemis :: [Ennemi]
  , projectiles :: [Projectile]
  , bonus :: [Bonus]
  , boss :: Maybe Boss
  , joueuse :: Joueuse
  , statut :: Statut
  , score :: Int
  , niveau :: Int
  , tours :: Int
  , gen :: StdGen
  }

-- Section 2 : Monade Etat

newtype Etat s a = Etat { runEtat :: s -> (a, s) }

instance Functor (Etat s) where
  fmap f (Etat g) = Etat $ \s ->
    let (a, s') = g s
    in (f a, s')

instance Applicative (Etat s) where
  pure a = Etat $ \s -> (a, s)
  Etat ef <*> Etat ex = Etat $ \s ->
    let (f, s') = ef s
        (a, s'') = ex s'
    in (f a, s'')

instance Monad (Etat s) where
  return = pure
  Etat ex >>= f = Etat $ \s ->
    let (a, s') = ex s
        Etat eg = f a
    in eg s'

get :: Etat s s
get = Etat $ \s -> (s, s)

put :: s -> Etat s ()
put s = Etat $ \_ -> ((), s)

modify :: (s -> s) -> Etat s ()
modify f = Etat $ \s -> ((), f s)

type EtatJeu a = Etat Envi a

-- Section 3 : Config niveaux

scoreRequis :: Int -> Int
scoreRequis n = 500 * (n * (n + 1)) `div` 2

configPourNiveau :: Int -> ConfigNiveau
configPourNiveau n = ConfigNiveau
  { cfgFreqObs = max 1 (12 - n)
  , cfgFreqEn = max 1 (9 - n)
  , cfgPVBoss = 3 * n + 2
  , cfgScoreBoss = max 100 (scoreRequis n - 300)
  }

-- Section 4 : Grille et monade Maybe

effetsBonus :: MapEffets
effetsBonus = Map.fromList
  [ (ExtraVie, 1)
  , (BonusScore, 100)
  ]

construireGrille :: Envi -> Grille
construireGrille env =
  let obsE = [(obsCoord o, [CellObs]) | o <- obstacles env]
      enE = [(enCoord e, [CellEnnemi (enPV e) (enType e)]) | e <- ennemis env]
      bossE = case boss env of
                Nothing -> []
                Just b -> [(bossCoord b, [CellBoss (bossPV b)])]
      projE = [(projCoord p, [if projJoueur p then CellProjJoueur else CellProjEnnemi]) | p <- projectiles env]
      bonE = [(bonusCoord b, [CellBonus (bonusType b)]) | b <- bonus env]
      jouE = [(jouCoord (joueuse env), [CellJoueur])]
  in Map.fromListWith (++) (obsE ++ enE ++ bossE ++ projE ++ bonE ++ jouE)

construireMapBonus :: Envi -> Map Coord Bonus
construireMapBonus env =
  Map.fromList [(bonusCoord b, b) | b <- bonus env]

valeurBonus :: Coord -> Map Coord Bonus -> MapEffets -> Maybe (TypeBonus, Int)
valeurBonus c bonusMap effetMap = do
  b <- Map.lookup c bonusMap
  valeur <- Map.lookup (bonusType b) effetMap
  return (bonusType b, valeur)

impactProj :: Coord -> Grille -> Maybe [Cellule]
impactProj c grille = do
  cellules <- Map.lookup c grille
  let cibles = filter estCible cellules
  if null cibles then Nothing else Just cibles
  where
    estCible CellObs = True
    estCible (CellEnnemi _ _) = True
    estCible (CellBoss _) = True
    estCible _ = False

impactJoueur :: Coord -> Grille -> Maybe [Cellule]
impactJoueur c grille = do
  cellules <- Map.lookup c grille
  let dangers = filter estDanger cellules
  if null dangers then Nothing else Just dangers
  where
    estDanger CellObs = True
    estDanger (CellEnnemi _ _) = True
    estDanger CellProjEnnemi = True
    estDanger _ = False

-- Section 5 : Opérations de jeu

charToDir :: Char -> Direction
charToDir 'z' = H
charToDir 's' = B
charToDir 'q' = G
charToDir 'd' = D
charToDir _ = N

depJ :: Direction -> EtatJeu ()
depJ dir = modify $ \env ->
  let Joueuse (Coord c l) pv = joueuse env
      w = largeur (ecran env)
      h = hauteur (ecran env)
      (c', l') = case dir of
        H -> (c, max 0 (l - 1))
        B -> (c, min (h - 1) (l + 1))
        G -> (max 0 (c - 1), l)
        D -> (min (w - 1) (c + 1), l)
        N -> (c, l)
  in env { joueuse = Joueuse (Coord c' l') pv }

tirer :: EtatJeu ()
tirer = modify $ \env ->
  let Joueuse (Coord c l) _ = joueuse env
  in if l > 0
     then env { projectiles = Projectile (Coord c (l - 1)) True : projectiles env }
     else env

avancerProjectiles :: EtatJeu ()
avancerProjectiles = modify $ \env ->
  let h = hauteur (ecran env)
      avance p
        | projJoueur p =
            let Coord c l = projCoord p
            in if l - 1 >= 0 then Just p { projCoord = Coord c (l - 1) } else Nothing
        | otherwise =
            let Coord c l = projCoord p
            in if l + 1 < h then Just p { projCoord = Coord c (l + 1) } else Nothing
  in env { projectiles = mapMaybe avance (projectiles env) }

bougerEnnemis :: EtatJeu ()
bougerEnnemis = modify $ \env ->
  let w = largeur (ecran env)
      h = hauteur (ecran env)
      bouger e =
        let Coord c l = enCoord e
            tick' = enTick e + 1
        in case enType e of
             Direct -> e { enCoord = Coord c (l + 1), enTick = tick' }
             Zigzag ->
               let dc = if even (enTick e) then 1 else -1
                   c' = min (w - 1) (max 0 (c + dc))
               in e { enCoord = Coord c' (l + 1), enTick = tick' }
             Tireur ->
               let dc = if (enTick e `div` 3) `mod` 2 == 0 then 1 else -1
                   c' = min (w - 1) (max 0 (c + dc))
                   dl = if enTick e `mod` 3 == 0 then 1 else 0
               in e { enCoord = Coord c' (l + dl), enTick = tick' }
      survivant e = case enType e of
        Tireur -> let dl = if enTick e `mod` 3 == 0 then 1 else 0 in lig (enCoord e) + dl < h
        _ -> lig (enCoord e) + 1 < h
  in env { ennemis = map bouger (filter survivant (ennemis env)) }

bougerBoss :: EtatJeu ()
bougerBoss = modify $ \env ->
  case boss env of
    Nothing -> env
    Just b ->
      let w = largeur (ecran env)
          tick' = bossTick b + 1
          phase = tick' `mod` (2 * (w - 1))
          c' = if phase < w then phase else 2 * (w - 1) - phase
      in env { boss = Just b { bossCoord = Coord c' 1, bossTick = tick' } }

scrollObstacles :: EtatJeu ()
scrollObstacles = modify $ \env ->
  let h = hauteur (ecran env)
      obs' = [ Obstacle (Coord c (l + 1)) | Obstacle (Coord c l) <- obstacles env, l + 1 < h ]
  in env { obstacles = obs' }

spawnObstacle :: EtatJeu ()
spawnObstacle = modify $ \env ->
  let cfg = configPourNiveau (niveau env)
      n = cfgFreqObs cfg
      (r, gen1) = randomR (0 :: Int, n - 1) (gen env)
      (c, gen2) = randomR (0, largeur (ecran env) - 1) gen1
  in if r == 0
     then env { obstacles = Obstacle (Coord c 0) : obstacles env, gen = gen2 }
     else env { gen = gen1 }

spawnEnnemi :: EtatJeu ()
spawnEnnemi = modify $ \env ->
  let cfg = configPourNiveau (niveau env)
      n = cfgFreqEn cfg
      (r, gen1) = randomR (0 :: Int, n - 1) (gen env)
      (c, gen2) = randomR (0, largeur (ecran env) - 1) gen1
      (t, gen3) = randomR (0 :: Int, 2) gen2
      typeEn = case niveau env of
                 1 -> Direct
                 2 -> if t == 0 then Direct else Zigzag
                 _ -> case t :: Int of { 0 -> Direct; 1 -> Zigzag; _ -> Tireur }
      pvEn = case typeEn of { Tireur -> 3; _ -> 2 }
  in if r == 0
     then env { ennemis = Ennemi (Coord c 0) pvEn typeEn 0 : ennemis env, gen = gen3 }
     else env { gen = gen1 }

spawnBonus :: EtatJeu ()
spawnBonus = modify $ \env ->
  let w = largeur (ecran env)
      h = hauteur (ecran env)
      (r, gen1) = randomR (0 :: Int, 9) (gen env)
      (c, gen2) = randomR (0, w - 1) gen1
      (l, gen3) = randomR (2, h - 2) gen2
      (t, gen4) = randomR (0 :: Int, 1) gen3
      tb = if t == 0 then ExtraVie else BonusScore
  in if r == 0
     then env { bonus = Bonus (Coord c l) tb : bonus env, gen = gen4 }
     else env { gen = gen1 }

spawnBoss :: EtatJeu ()
spawnBoss = modify $ \env ->
  let cfg = configPourNiveau (niveau env)
  in if boss env == Nothing && statut env == EnCours && score env >= cfgScoreBoss cfg
     then let w = largeur (ecran env) in env { boss = Just (Boss (Coord (w `div` 2) 1) (cfgPVBoss cfg) 0) }
     else env

tirerEnnemis :: EtatJeu ()
tirerEnnemis = modify $ \env ->
  let h = hauteur (ecran env)
      (r, gen1) = randomR (0 :: Int, 3) (gen env)
      tir e =
        let Coord c l = enCoord e
        in if l + 1 < h then Just (Projectile (Coord c (l + 1)) False) else Nothing
      projTireurs = mapMaybe tir [ e | e <- ennemis env, enType e == Tireur ]
      projZigzags = if r == 0 then mapMaybe tir [ e | e <- ennemis env, enType e == Zigzag ] else []
  in env { projectiles = projTireurs ++ projZigzags ++ projectiles env, gen = gen1 }

tirerBoss :: EtatJeu ()
tirerBoss = modify $ \env ->
  case boss env of
    Nothing -> env
    Just b ->
      let Coord c l = bossCoord b
          w = largeur (ecran env)
          h = hauteur (ecran env)
          niv = niveau env
          tirs
            | niv == 1 && bossTick b `mod` 4 == 0 && l + 1 < h = [ Projectile (Coord c (l + 1)) False ]
            | niv == 2 && bossTick b `mod` 4 == 0 && l + 1 < h =
                [ Projectile (Coord (max 0 (c - 1)) (l + 1)) False
                , Projectile (Coord c (l + 1)) False
                , Projectile (Coord (min (w - 1) (c + 1)) (l + 1)) False
                ]
            | niv == 3 && even (bossTick b) && l + 1 < h =
                [ Projectile (Coord (max 0 (c - 1)) (l + 1)) False
                , Projectile (Coord c (l + 1)) False
                , Projectile (Coord (min (w - 1) (c + 1)) (l + 1)) False
                ]
            | niv >= 4 && even (bossTick b) && l + 1 < h =
                [ Projectile (Coord (max 0 (c - 2)) (l + 1)) False
                , Projectile (Coord (max 0 (c - 1)) (l + 1)) False
                , Projectile (Coord c (l + 1)) False
                , Projectile (Coord (min (w - 1) (c + 1)) (l + 1)) False
                , Projectile (Coord (min (w - 1) (c + 2)) (l + 1)) False
                ]
            | otherwise = []
      in env { projectiles = tirs ++ projectiles env }

traiterCollisionsProj :: EtatJeu ()
traiterCollisionsProj = do
  env <- get
  let grille = construireGrille env
      projsJ = filter projJoueur (projectiles env)
      projsE = filter (not . projJoueur) (projectiles env)
      estCibleOE x = case x of { CellObs -> True; CellEnnemi _ _ -> True; _ -> False }
      impactOE c = case Map.lookup c grille of
                     Nothing -> Nothing
                     Just cs -> let t = filter estCibleOE cs in if null t then Nothing else Just t
      hitsPos = [ projCoord p | p <- projsJ, isJust (impactOE (projCoord p)) ]
      crossPairs = [ (projCoord p, Coord c (l + 1)) | p <- projsJ, let Coord c l = projCoord p, isJust (impactOE (Coord c (l + 1))) ]
      crossLasers = map fst crossPairs
      crossTargets = map snd crossPairs
      hitLasers = hitsPos ++ crossLasers
      projsJ' = filter (\p -> projCoord p `notElem` hitLasers) projsJ
      traiterEn e
        | enCoord e `elem` hitsPos || enCoord e `elem` crossTargets =
            if enPV e - 1 <= 0 then Nothing else Just e { enPV = enPV e - 1 }
        | otherwise = Just e
      enApres = mapMaybe traiterEn (ennemis env)
      nbTues = length [ e | e <- ennemis env, enCoord e `elem` hitsPos || enCoord e `elem` crossTargets, enPV e <= 1 ]
      obsApres = [ o | o <- obstacles env, obsCoord o `notElem` hitsPos, obsCoord o `notElem` crossTargets ]
  put env
    { projectiles = projsJ' ++ projsE
    , ennemis = enApres
    , obstacles = obsApres
    , score = score env + 100 * nbTues
    }

traiterCollisionsBoss :: EtatJeu ()
traiterCollisionsBoss = do
  env <- get
  case boss env of
    Nothing -> return ()
    Just b -> do
      let bc = bossCoord b
          projsJ = filter projJoueur (projectiles env)
          hits = [ projCoord p | p <- projsJ, projCoord p == bc ]
          nbHits = length hits
          proj' = [ p | p <- projectiles env, not (projJoueur p) || projCoord p /= bc ]
      if nbHits == 0
        then return ()
        else if bossPV b - nbHits <= 0
          then modify $ \e -> e { boss = Nothing, projectiles = proj', score = score env + 500 }
          else modify $ \e -> e { boss = Just b { bossPV = bossPV b - nbHits }, projectiles = proj' }

traiterCollisionsJoueur :: EtatJeu ()
traiterCollisionsJoueur = do
  env <- get
  let grille = construireGrille env
      Joueuse jc pv = joueuse env
      mDangers = impactJoueur jc grille
      enContact = length [ e | e <- ennemis env, enCoord e == jc ]
      prjContact = length [ p | p <- projectiles env, not (projJoueur p), projCoord p == jc ]
      obsContact = min 1 (length [ o | o <- obstacles env, obsCoord o == jc ])
      degats = case mDangers of { Nothing -> 0; Just _ -> enContact + prjContact + obsContact }
      pv' = max 0 (pv - degats)
      stat' = if pv' <= 0 then Perdu else statut env
      en' = [ e | e <- ennemis env, enCoord e /= jc ]
      proj' = [ p | p <- projectiles env, projJoueur p || projCoord p /= jc ]
  put env
    { joueuse = Joueuse jc pv'
    , statut = stat'
    , ennemis = en'
    , projectiles = proj'
    }

collecterBonus :: EtatJeu ()
collecterBonus = do
  env <- get
  let Joueuse jc pv = joueuse env
      bonusMap = construireMapBonus env
      mCollecte = valeurBonus jc bonusMap effetsBonus
  case mCollecte of
    Nothing -> return ()
    Just (tb, valeur) ->
      let bonus' = [ b | b <- bonus env, bonusCoord b /= jc ]
          (pv', score') = case tb of
            ExtraVie -> (min (pv + valeur) 5, score env)
            BonusScore -> (pv, score env + valeur)
      in modify $ \e -> e { bonus = bonus', joueuse = Joueuse jc pv', score = score' }

majStatut :: EtatJeu ()
majStatut = return ()

-- Section 6 : Affichage

affiche :: EtatJeu String
affiche = do
  env <- get
  let w = largeur (ecran env)
      h = hauteur (ecran env)
      Joueuse _ pv = joueuse env
      grille = construireGrille env
      cell c l = case Map.lookup (Coord c l) grille of
                   Nothing -> ' '
                   Just cells -> rendreCellule cells
      rendreCellule cells
        | CellJoueur `elem` cells = '^'
        | any estBoss cells = 'B'
        | any estEnnemi cells = enemyChar cells
        | CellObs `elem` cells = '#'
        | CellProjJoueur `elem` cells = '|'
        | CellProjEnnemi `elem` cells = '.'
        | any estBonus cells = '$'
        | otherwise = ' '
      estBoss (CellBoss _) = True
      estBoss _ = False
      estEnnemi (CellEnnemi _ _) = True
      estEnnemi _ = False
      estBonus (CellBonus _) = True
      estBonus _ = False
      enemyChar cells = case [ t | CellEnnemi _ t <- cells ] of
                          (Zigzag:_) -> 'W'
                          (Tireur:_) -> 'T'
                          _ -> 'V'
      bord = "+" ++ replicate w '-' ++ "+"
      ligne l = "|" ++ [ cell c l | c <- [0 .. w - 1] ] ++ "|"
      corps = unlines [ ligne l | l <- [0 .. h - 1] ]
      bossInfo = case boss env of { Nothing -> ""; Just b -> " BOSS PV:" ++ show (bossPV b) }
      infoBar = case statut env of
        Perdu -> "*** GAME OVER *** (Score: " ++ show (score env) ++ ")"
        Gagne -> "*** VICTOIRE ! *** (Score: " ++ show (score env) ++ ")"
        EnCours -> "PV:" ++ show pv ++ " Score:" ++ show (score env) ++ "/" ++ show (scoreRequis (niveau env))
                ++ " Niv:" ++ show (niveau env) ++ bossInfo ++ " [ZQSD=move ESPACE=tir]"
  return $ bord ++ "\n" ++ corps ++ bord ++ "\n" ++ infoBar

-- Section 7 : Tour complet

tour :: Char -> EtatJeu String
tour c = do
  env <- get
  case statut env of
    EnCours -> do
      depJ (charToDir c)
      if c == ' ' then tirer else return ()
      avancerProjectiles
      scrollObstacles
      bougerEnnemis
      traiterCollisionsBoss
      bougerBoss
      spawnObstacle
      spawnEnnemi
      spawnBonus
      spawnBoss
      tirerEnnemis
      tirerBoss
      traiterCollisionsProj
      traiterCollisionsJoueur
      collecterBonus
      majStatut
      modify $ \e -> e { tours = tours e + 1 }
      affiche
    _ -> affiche

-- Section 8 : Moteur

envi0 :: Envi
envi0 = Envi
  { ecran = Ecran 30 20
  , obstacles = []
  , ennemis = []
  , projectiles = []
  , bonus = []
  , boss = Nothing
  , joueuse = Joueuse (Coord 15 18) 3
  , statut = EnCours
  , score = 0
  , niveau = 1
  , tours = 0
  , gen = mkStdGen 42
  }

runJeu :: EtatJeu a -> Envi -> (a, Envi)
runJeu = runEtat