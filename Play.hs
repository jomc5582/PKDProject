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
  G.rulesplash
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
  mapFile <- readFile "Map.txt"
  let rows = lines mapFile 
  --MH.printMap (move (playerCoord ((generateMap rows), 20)) N ((generateMap rows), 20))
  loop (((generateMap rows), 20), 0) -- REQUIRES THE FIRST INTEGER VALUE TO BE THE SAME AS THE AMOUNT OF ROWS IN THE "Map.txt" FILE. THE SECOND ONE IS SCORE, STARTS AT 0
  --loop ((editMap (newMap 5 5) 2 2 (('X', False), Temp ('Z', True))), 0)
{- loop map
   PRECONS: 
   RETURNS: 
   EXAMPLE: 
   VARIANT: 
   SIDE EFFECTS: 
-}
loop :: (Map, Int) -> IO ()
loop mapState@(map, score) = do
  putStrLn ""
  putStrLn scoreLine
  putStrLn ""
  -- Printing the map
  G.printMap map (playerX + 1) playerY visionRange
  
  putStrLn "What does the player wish to do?"
  input <- getLine 
  putStrLn ""
  
  --let  -- (move (playerCoord map) (translateDir (drop 5 input)) map) -- ERROR IS IN THIS ONE
  
  if input == "quit" then putStrLn "Quitting..." else loop (newState input) -- (fst (playerInput input map), score + snd (playerInput input map))
  where playerX   = fst (O.getPlayerCoord 0 map)
        playerY   = snd (O.getPlayerCoord 0 map)
        newState  = update mapState
        scoreLine = "Score: " ++ (show score)
  
{- update map input
   PRECONS: 
   RETURNS: 
   EXAMPLE: 
   VARIANT: 
   SIDE EFFECTS: 
-}
update :: (Map, Int) -> String -> (Map, Int)
update mapState input = playerInput input mapState
   {-
   map1 <- playerInput input mapState
   --map2 <- update Enemies
   --timer?
   return map1
   -}

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
--  | take 5 input == "shake" = 
--  | take 
  | otherwise              = (                                                              (m:ap, h), p)
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