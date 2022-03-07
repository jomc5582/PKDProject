import System.IO
import MapHandling as MH
import Object as O
import Graphics as G

{- main
   PRECONS: 
   RETURNS: 
   EXAMPLE: 
   VARIANT: 
   SIDE EFFECTS: 
-}
main :: IO ()
main = do
  G.splash
  pause
  G.ruleSplash
  pause
  initalize

pause :: IO ()
pause = do
  putStrLn ""
  putStrLn "Press Enter to continue..."
  wait <- getLine
  putStrLn ""

{- init
   PRECONS: 
   RETURNS: 
   EXAMPLE: 
   VARIANT: 
   SIDE EFFECTS: 
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

{- loop map
   PRECONS: 
   RETURNS: 
   EXAMPLE: 
   VARIANT: 
   SIDE EFFECTS: 
-}
playAgainLoop :: [[String]] -> IO ()
playAgainLoop levels = do
  G.menuSplash
  putStrLn "Enter a number 1 to 3 to play respective level..."
  x <- getLine
  let level = levels !! (read (take 1 x) - 1)

  loop ((generateMap level, length level), 0) 1 -- REQUIRES THE FIRST INTEGER VALUE TO BE THE SAME AS THE AMOUNT OF ROWS IN THE "level1.txt" FILE. THE SECOND ONE IS SCORE, STARTS AT 0

  putStrLn "Do you want to play again? (y/n)"
  input <- getLine

  if input == "n" then putStrLn "Thanks for playing!" else playAgainLoop levels

{- loop (map, score)
   PRECONS: 
   RETURNS: 
   EXAMPLE: 
   VARIANT: 
   SIDE EFFECTS: 
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

{- update map input
   PRECONS: 
   RETURNS: 
   EXAMPLE: 
   VARIANT: 
   SIDE EFFECTS: 
-}
update :: (Map, Int) -> String -> Int -> (Map, Int)
update mapState input turn = bossTurn (enemyTurn (playerMap, playerScore) playerMap) turn
  where playerTurn  = playerInput input mapState
        playerMap   = fst playerTurn
        playerScore = snd playerTurn

{- playerInput input map
   PRECONS: 
   RETURNS: 
   EXAMPLE: 
   VARIANT: 
   SIDE EFFECTS: 
-}
playerInput :: String -> (Map, Int) -> (Map, Int)
playerInput _     (([],   h), p) = (([],   h), p)
playerInput input map@((m:ap, h), p)
  | take 4 input == "move"  = (move    (playerCoord (m:ap, h)) (translateDir (drop 5 input)) (m:ap, h), p)
  | take 4 input == "push"  = (pushDir (translateDir (drop 5 input)) (playerCoord (m:ap, h)) (m:ap, h), p)
  | take 3 input == "dig"   = (dig     (playerCoord (m:ap, h))                               (m:ap, h), p + 100)
  | take 5 input == "shake" = (shake   (playerCoord (m:ap, h)) (translateDir (drop 6 input)) (m:ap, h), p + 100)
  | take 3 input == "hit"   = (hit     (playerCoord (m:ap, h)) (translateDir (drop 4 input)) (m:ap, h), p + 10)
  | otherwise               = (                                                              (m:ap, h), p)
  where x = fst (playerCoord (m:ap, h))
        y = snd (playerCoord (m:ap, h))

{- translateDir
   PRECONS: 
   RETURNS: 
   EXAMPLE: 
   VARIANT: 
   SIDE EFFECTS: 
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

{- 
   PRECONS: 
   RETURNS: 
   EXAMPLE: 
   VARIANT: 
   SIDE EFFECTS: 
-}
playerCoord :: Map -> Position
playerCoord = O.getPlayerCoord 0

{- enemyTurn map mapAux
   PRECONS: 
   RETURNS: 
   EXAMPLE: 
   VARIANT: 
   SIDE EFFECTS: 
-}
enemyTurn :: (Map, Int) -> Map -> (Map, Int)
enemyTurn (map, p) mapAux = (enemyMove, p + enemyHits (getEnemies map 0) enemyMove)
  where enemyMove         = O.moveEnemies 0 map mapAux

{- 
   PRECONS: 
   RETURNS: 
   EXAMPLE: 
   VARIANT: 
   SIDE EFFECTS: 
-}
bossTurn :: (Map, Int) -> Int -> (Map, Int)
bossTurn (map, p) turn = (bossPhaseThree (bossPhaseTwo (bossPhaseOne map turn) turn) turn, p)

{- 
   PRECONS: 
   RETURNS: 
   EXAMPLE: 
   VARIANT: 
   SIDE EFFECTS: 
-}
