import System.IO ()
import MapHandling as MH ( Map, generateMap )
import Object as O
    ( bossPhaseOne,
      bossPhaseThree,
      bossPhaseTwo,
      dig,
      enemyHits,
      getEnemies,
      getPlayerCoord,
      getWin,
      hit,
      move,
      moveEnemies,
      pushDir,
      shake,
      visionRange,
      Direction(..),
      Position )
import Graphics as G
    ( printMap, splash, ruleSplash, bossSplash, winSplash, menuSplash )

{- main
   Prints vital information and starts the game initialization for the user.
   THIS IS FROM WHERE THE GAME IS RUN.
   PRECONS: -
   RETURNS: -
   EXAMPLE: -
   VARIANT: -
   SIDE EFFECTS: prints to the console.
-}
main :: IO ()
main = do
  G.splash
  pause
  initalize

{- pause 
   pauses the game and acts as a breaker so that the user may read certain information.
   PRECONS: -
   RETURNS: -
   EXAMPLE: -
   VARIANT: -
   SIDE EFFECTS: prints to the console and reads input from the user terminal.
-}
pause :: IO ()
pause = do
  putStrLn ""
  putStrLn "Press Enter to continue..."
  wait <- getLine
  putStrLn ""

{- initalize 
   Initalizes the maps and starts the game.
   PRECONS: -
   RETURNS: -
   EXAMPLE: -
   VARIANT: -
   SIDE EFFECTS: reads from files.
-}
initalize :: IO ()
initalize = do
  level1 <- readFile "level1.txt"
  let level1rows = lines level1
  level2 <- readFile "level2.txt"
  let level2rows = lines level2
  level3 <- readFile "level3.txt"
  let level3rows = lines level3

  let levels = level1rows : level2rows : level3rows : []
  
  playAgainLoop levels

{- playAgainLoop [level]
   The game menu where the player may choose a map to play.
   PRECONS: Any valid mapstate consiting of a (map, score) tuple.
   RETURNS: -
   EXAMPLE: -
   VARIANT: -
   SIDE EFFECTS: prints to the console and reads input from the user terminal.
-}
playAgainLoop :: [[String]] -> IO ()
playAgainLoop levels = do
  G.menuSplash
  putStrLn "Enter a number 1 to 3 to play respective level..."
  x <- getLine
  let level = levels !! (read (take 1 x) - 1)

  G.ruleSplash
  if x == "2" then G.bossSplash else putStrLn ""
  pause
  
  loop ((generateMap level, length level), 0) 1 -- REQUIRES THE FIRST INTEGER VALUE TO BE THE SAME AS THE AMOUNT OF ROWS IN THE "level1.txt" FILE. THE SECOND ONE IS SCORE, STARTS AT 0

  putStrLn "Do you want to play again? (y/n)"
  input <- getLine

  if input == "n" then putStrLn "Thanks for playing!" else playAgainLoop levels

{- loop (map, score)
   The main game loop for any loaded map.
   PRECONS: Any valid mapstate consiting of a (map, score) tuple.
   RETURNS: -
   EXAMPLE: -
   VARIANT: input == "quit" gives termination of the recursion.
   SIDE EFFECTS: prints to the console and reads user input.
-}
loop :: (Map, Int) -> Int -> IO ()
loop mapState@(map, score) turn = do
  putStrLn ""
  putStrLn scoreLine
  putStrLn ""
  -- Printing the map
  G.printMap map (playerX + 1) playerY visionRange
  
  putStrLn "What does the player wish to do? Eg. 'push SE', 'move W', 'hit N' or 'dig'"
  input <- getLine
  
  if getWin map then winSplash else (if input == "quit" then putStrLn "Quitting..." else loop (newState input turn) (turn +1)) 
  where playerX   = fst (O.getPlayerCoord 0 map)
        playerY   = snd (O.getPlayerCoord 0 map)
        newState  = update mapState 
        scoreLine = "Score: " ++ show score

{- update (map, points) input
   processes the turns for all enteties on the map in order of player first,
   enemies second and the boss last.
   PRECONS: Any valid map
   RETURNS: The updated map ready for the next turn of the game.
   EXAMPLE: (The printed representations are used instead of the list form of the maps)
                     " _ _ _ _ _ "                       " _ E _ Z _ "
                     " E _ _ Z _ "                       " _ _ _ _ _ "
            update ( " _ _ _ _ _ " , 10) "move N" 5 -> ( " _ _ _ E _ " , 10)
                     " _ _ E E _ "                       " _ _ E _ _ "
                     " _ _ _ _ _ "                       " _ _ _ _ _ "
   VARIANT: -
   SIDE EFFECTS: -
-}
update :: (Map, Int) -> String -> Int -> (Map, Int)
update mapState input turn = bossTurn (enemyTurn (playerMap, playerScore) playerMap) turn
  where playerTurn  = playerInput input mapState
        playerMap   = fst playerTurn
        playerScore = snd playerTurn

{- playerInput input map
   The input is processed and interpeted on the map if it is one of the predefined.
   PRECONS: any inputed string and a valid map.
   RETURNS: The map with the players wanted input performed.
   EXAMPLE: (The printed representations are used instead of the list form of the maps)
                                    " _ _ _ _ _ "            " _ _ _ N _ "
                                    " _ _ _ Z _ "            " _ _ _ _ _ "
            playerInput "move N"  ( " _ _ _ _ _ " , 10) -> ( " _ _ _ _ _ " , 10)
                                    " _ _ _ _ _ "            " _ _ _ _ _ "
                                    " _ _ _ _ _ "            " _ _ _ _ _ "
   VARIANT: -
   SIDE EFFECTS: -
-}
playerInput :: String -> (Map, Int) -> (Map, Int)
playerInput _ (([],   h), p) = (([],   h), p)
playerInput input map@((m:ap, h), p)
  | take 4 input == "move"  = (move    (playerCoord (m:ap, h)) (translateDir (drop 5 input)) (m:ap, h), p)
  | take 4 input == "push"  = (pushDir (translateDir (drop 5 input)) (playerCoord (m:ap, h)) (m:ap, h), p)
  | take 3 input == "dig"   = (dig     (playerCoord (m:ap, h))                               (m:ap, h), p + 100)
  | take 5 input == "shake" = (shake   (playerCoord (m:ap, h)) (translateDir (drop 6 input)) (m:ap, h), p + 100)
  | take 3 input == "hit"   = (hit     (playerCoord (m:ap, h)) (translateDir (drop 4 input)) (m:ap, h), p + 10)
  | otherwise               = (                                                              (m:ap, h), p)
  where x = fst (playerCoord (m:ap, h))
        y = snd (playerCoord (m:ap, h))

{- translateDir dirStr
   translates an inputed string into the direction value of it.
   PRECONS: Any string
   RETURNS: The direction value of the string if it is the respective string.
   EXAMPLE: translateDir "N" = N
   VARIANT: -
   SIDE EFFECTS: -
-}
translateDir :: String -> O.Direction
translateDir dir
  | dir == "N"  = N
  | dir == "NE" = NE
  | dir == "E"  = E
  | dir == "SE" = SE
  | dir == "S"  = S
  | dir == "SW" = SW
  | dir == "W"  = W
  | dir == "NW" = NW
  | otherwise   = O.Void

{- playerCoord map
   Returns the coords of the player on the map acording to the getPlayerCoord funciton in objects.
   PRECONS: A valid map with only one player.
   RETURNS: The position of the player on the map
   EXAMPLE: (The printed representations are used instead of the list form of the maps)
                        " _ _ _ _ _ "
                        " _ _ _ Z _ "
            playerCoord " _ _ _ _ _ " = (3, 1)
                        " _ _ _ _ _ "
                        " _ _ _ _ _ "
   VARIANT: -
   SIDE EFFECTS: -
-}
playerCoord :: Map -> Position
playerCoord = O.getPlayerCoord 0

{- enemyTurn map mapAux
   processes the turn for any and all enemies.
   PRECONS: Any valid map tupled with an int.
   RETURNS: The map with all the enemies turns processed.
   EXAMPLE: (The printed representations are used instead of the list form of the maps)
                        " _ _ _ _ _ "      " _ _ _ _ _ "
                        " _ Z _ _ E "      " _ Z _ E _ "
            enemyTurn   " _ _ _ _ _ " ->   " _ _ _ _ _ "
                        " _ _ _ _ _ "      " _ E _ _ _ "
                        " _ E E _ _ "      " _ _ E _ _ "
   VARIANT: -
   SIDE EFFECTS: -
-}
enemyTurn :: (Map, Int) -> Map -> (Map, Int)
enemyTurn (map, p) mapAux = (enemyMove, p + enemyHits (getEnemies map 0) enemyMove)
  where enemyMove         = O.moveEnemies 0 map mapAux

{- bossTurn (map, points) -> turn -> (map, points)
   processes the turn for the boss if it exsists on the map, in any of it's stages.
   PRECONS: Any valid map tupled with an int.
   RETURNS: The map edited with the bosses actions, eg either summoning more enemies or running from the player.
   EXAMPLE: (The printed representations are used instead of the list form of the maps)
                       " _ _ _ _ _ "      " _ _ _ _ _ "
                       " _ Z _ _ _ "      " _ Z E _ _ "
            bossTurn   " _ _ B _ _ " 4 -> " _ E _ _ _ "
                       " _ _ _ _ _ "      " _ _ _ B _ "
                       " _ _ _ _ _ "      " _ _ _ _ _ "
   VARIANT: -
   SIDE EFFECTS: -
-}
bossTurn :: (Map, Int) -> Int -> (Map, Int)
bossTurn (map, p) turn = (bossPhaseThree (bossPhaseTwo (bossPhaseOne map turn) turn) turn, p)
