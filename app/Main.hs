{-# LANGUAGE CPP #-}
module Main (main) where

import Lib
import Niveaux (chargerObstacles)
#ifdef mingw32_HOST_OS
import System.IO (hSetBuffering, hSetEcho, BufferMode (..), stdin)
#else
import System.IO (hSetBuffering, hSetEcho, hReady, BufferMode (..), stdin)
#endif
import System.Environment (getArgs)
import Control.Concurrent (threadDelay)

#ifdef mingw32_HOST_OS
import Foreign.C.Types (CInt (..))

foreign import ccall unsafe "_kbhit" c_kbhit :: IO CInt
foreign import ccall unsafe "_getch" c_getch :: IO CInt
#endif

tickUs :: Int
tickUs = 120000 -- 120 ms par tick ≈ 8 FPS

-- Lit un caractère sans attendre Entrée ; '\0' si aucune touche disponible.
lireEntree :: IO Char
#ifdef mingw32_HOST_OS
lireEntree = do
  ready <- c_kbhit
  if ready /= 0
    then do
      n <- c_getch
      -- Les touches spéciales (flèches…) produisent deux octets : 0 ou 0xe0 suivi du code.
      -- On consomme le second octet et on renvoie '\0' (tick neutre).
      if n == 0 || n == 0xe0
        then c_getch >> return '\0'
        else return (toEnum (fromIntegral n))
    else return '\0'
#else
lireEntree = do
  ready <- hReady stdin
  if ready then getChar else return '\0'
#endif

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  args <- getArgs
  obsInitiaux <- case args of
    (fichier:_) -> chargerObstacles fichier
    [] -> return []
  let env0 = envi0 { obstacles = obsInitiaux }
      (aff0, env0') = runJeu affiche env0
  clearScreen
  putStr aff0
  boucle env0'

boucle :: Envi -> IO ()
boucle env = do
  threadDelay tickUs
  c <- lireEntree
  let (aff, env') = runJeu (tour c) env
  clearScreen
  putStr aff
  case statut env' of
    EnCours -> boucle env'
    _ -> do
      putStrLn ""
      putStrLn "Appuyez sur Entrée pour quitter."
      _ <- getLine
      return ()

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"