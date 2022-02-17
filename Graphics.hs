module Graphics where

import MapHandling as MH
import Object as O

-- ! Is object needed here?

{- MapInit width height

   PRECONS: 
   RETURNS: 
   EXAMPLE: 
   VARIANT: 
   SIDE EFFECTS: 
-}
mapInit :: Int -> Int -> MH.Map
mapInit = MH.newMap

{- printMap xPos yPos radius

   PRECONS: 
   RETURNS: 
   EXAMPLE: 
   VARIANT: 
   SIDE EFFECTS: 
-}
printMap :: MH.Map -> Int -> Int -> Int -> IO ()
printMap = MH.printSection 

{- printFullMap

   PRECONS: 
   RETURNS: 
   EXAMPLE: 
   VARIANT: 
   SIDE EFFECTS: 
-}
printFullMap :: MH.Map -> IO ()
printFullMap = MH.printMap

{- splash

   PRECONS: 
   RETURNS: 
   EXAMPLE: 
   VARIANT: 
   SIDE EFFECTS: 
-}
splash :: IO ()
splash = do
  putStrLn "                ___________.__                              "
  putStrLn "               \\__    ___/|  |__   ____                     "
  putStrLn "                 |    |   |  |  \\_/ __ \\                    "
  putStrLn "                 |    |   |   Y  \\  ___/                    "
  putStrLn "                 |____|   |___|  /\\___  >                   "
  putStrLn "                               \\/     \\/                    "
  putStrLn ".____                                    .___         _____ "
  putStrLn "|    |    ____   ____   ____   ____    __| _/   _____/ ____\\"
  putStrLn "|    |  _/ __ \\ / ___\\_/ __ \\ /    \\  / __ |   /  _ \   \__\\ "
  putStrLn "|    |__\\  ___// /_/  >  ___/|   |  \\/ /_/ |  (  <_> )  |   "
  putStrLn "|_______ \\___  >___  / \\___  >___|  /\\____ |   \\____/|__|   "
  putStrLn "        \\/   \\/_____/      \\/     \\/      \\/                "
  putStrLn "  __________.__                 __                          "
  putStrLn "  \\____    /|  | _____    ____ |  | __ ___________  ____    "
  putStrLn "    /     / |  | \\__  \\ _/ ___\\|  |/ // __ \\_  __ \\/    \\   "
  putStrLn "   /     /_ |  |__/ __ \\  \\___|    <\\  ___/|  | \\/   |  \\  "
  putStrLn "  /_______ \\|____(____  /\\___  >__|_ \\___  >__|  |___|  /  "
  putStrLn "          \\/          \\/     \\/     \\/    \\/           \\/   "