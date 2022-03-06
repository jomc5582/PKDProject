module Graphics where

import MapHandling as MH

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
  putStrLn ""
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
  putStrLn ""

ruleSplash :: IO ()
ruleSplash = do
   putStrLn ""
   putStrLn " ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
   putStrLn ""
   putStrLn " Welcome to The Legend of Zlackern! "
   putStrLn ""
   putStrLn " You are Zlackern, and your goal is to make it to your computer - you obviously"
   putStrLn " have code that you need to finish because of your slacking"
   putStrLn ""
   putStrLn " In this game you move using cardinal directions " 
   putStrLn ""
   putStrLn " For example, if you want to move Northwest on the map, enter 'move NW' "
   putStrLn ""
   putStrLn " This is you: Z "
   putStrLn " These are enemies (watch out!): E "
   putStrLn " This is your goal: C "
   putStrLn ""
   putStrLn " On your adventure you have the chance to dig for treasure, which looks like this: X "
   putStrLn ""
   putStrLn " There are also trees (T) and boulders (O) around that you can interact with"
   putStrLn ""
   putStrLn " ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

winSplash :: IO ()
winSplash = do
   splash
   putStrLn " ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
   putStrLn ""
   putStrLn " Congratulations, you won! " 
   putStrLn ""
   putStrLn " You survived your adventure and made it to your computer "
   putStrLn ""
   putStrLn " We hope you enjoyed your journey here!  " 
   putStrLn ""
   putStrLn " ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
   putStrLn ""

--if the player chooses n, add string "Thanks for playing!" and then trigger the quit function