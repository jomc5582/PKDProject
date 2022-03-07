module Graphics where

import MapHandling as MH ( Map, printMap, printSection )

{- printMap xPos yPos radius
   Prints a part of the map like PrintSection from MapHandling.hs
   PRECONS: Any valid Map and coordinates x and y within the map boundries.
   RETURNS: -
   EXAMPLE: (The printed representations are used instead of the list form of the maps)
                         " _ _ _ _ _ "
                         " _ P _ _ _ "          " P _ _ "
            printSection " _ _ _ _ _ " 2 2 1 -> " _ _ _ "
                         " _ _ _ _ _ "          " _ _ _ "
                         " _ _ _ _ _ "
   VARIANT: -
   SIDE EFFECTS: Prints to the screen using IO monad
-}
printMap :: MH.Map -> Int -> Int -> Int -> IO ()
printMap = MH.printSection 

{- printFullMap map
   Prints the full map like PrintMap from MapHandling.hs
   PRECONS: Any valid map.
   RETURNS: -
   EXAMPLE: printMap (newMap 5 5) =
            " _ _ _ _ _ "
            " _ _ _ _ _ "
            " _ _ _ _ _ "
            " _ _ _ _ _ "
            " _ _ _ _ _ "
   VARIANT: -
   SIDE EFFECTS: Prints to the screen using IO monad
-}
printFullMap :: MH.Map -> IO ()
printFullMap = MH.printMap

{- splash
   Prints a stylized splash to the screen.
   PRECONS: -
   RETURNS: -
   EXAMPLE: -
   VARIANT: -
   SIDE EFFECTS: Prints to the screen using IO monad.
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

{- ruleSplash
   Prints a stylized splash to the screen.
   PRECONS: -
   RETURNS: -
   EXAMPLE: -
   VARIANT: -
   SIDE EFFECTS: Prints to the screen using IO monad.
-}
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

{- bossSplash
   Prints a stylized splash to the screen.
   PRECONS: -
   RETURNS: -
   EXAMPLE: -
   VARIANT: -
   SIDE EFFECTS: Prints to the screen using IO monad.
-}

bossSplash :: IO ()
bossSplash = do
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
   putStrLn " This is your goal: C "
   putStrLn " These are enemies (watch out!): E "
   putStrLn ""
   putStrLn " This is a boss - a stronger enemy: B "
   putStrLn " When a boss is almost dead and in its final form, it looks like this: F "
   putStrLn ""
   putStrLn " On your adventure you have the chance to dig for treasure, which looks like this: X "
   putStrLn ""
   putStrLn " There are also trees (T) and boulders (O) around that you can interact with"
   putStrLn ""
   putStrLn " ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

{- winSplash
   Prints a stylized splash to the screen.
   PRECONS: -
   RETURNS: -
   EXAMPLE: -
   VARIANT: -
   SIDE EFFECTS: Prints to the screen using IO monad.
-}
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

{- menuSplash
   Prints a stylized splash to the screen.
   PRECONS: -
   RETURNS: -
   EXAMPLE: -
   VARIANT: -
   SIDE EFFECTS: Prints to the screen using IO monad.
-}
menuSplash :: IO ()
menuSplash = do 
   putStrLn ""
   putStrLn " ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
   putStrLn ""
   putStrLn " - LEVELS - " 
   putStrLn ""
   putStrLn " Level 1 - Exploration / puzzle "
   putStrLn ""
   putStrLn " Level 2 - Boss "
   putStrLn ""
   putStrLn " Level 3 - NOT DONE"
   putStrLn ""
   putStrLn " Please enter the number of the level you wish to play. "
   putStrLn ""
   putStrLn " ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
   putStrLn ""

