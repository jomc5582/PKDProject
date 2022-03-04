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
   G.rulesplash
   initalize

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

{- loop map
   PRECONS: 
   RETURNS: 
   EXAMPLE: 
   VARIANT: 
   SIDE EFFECTS: 
-}
loop :: (MH.Map, Int) -> IO ()
loop mapState = do
  
  putStrLn ""
  -- Printing the map
  G.printMap map playerX playerY visionRange
  putStrLn (show playerX)
  putStrLn (show playerY)
  putStrLn ""

  -- ! BREAKER
  putStrLn "What does the player wish to do?"
  input <- getLine 

  putStrLn input
  
  putStrLn ""
  if input == "quit" then putStrLn "Quitting..." else loop (playerInput input map)
  where playerX = fst (O.getPlayerCoord 0 map)
        playerY = snd (O.getPlayerCoord 0 map)
        map     = fst mapState
        score   = snd mapState

  {-
{- update map
   PRECONS: 
   RETURNS: 
   EXAMPLE: 
   VARIANT: 
   SIDE EFFECTS: 
-}
update :: MH.Map -> IO ()
update mapState = do 
   input <- getLine
   --playerInput input map
   -- update Enemies
   -- timer
   return mapState
-}

{- playerInput input map
   PRECONS: 
   RETURNS: 
   EXAMPLE: 
   VARIANT: 
   SIDE EFFECTS: 
-}
playerInput :: String -> Map -> (Map, Int)
playerInput input map@(m:ap, h)
  | take 4 input == "move"  = (move (playerCoord map) (translateDir (drop 5 input)) map, 0)
  | take 3 input == "dig"   = dig (playerCoord map) map
--  | take 5 input == "shake" = 
  | otherwise               = (map, 0)

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
playerCoord = O.getPlayerCoord 0