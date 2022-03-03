import Prelude hiding (init)
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
   init

{- Make the init function intiialize the loop with the starting
   correct values for starting the game. For example starting the game
   with a player on a populated map. Maybe figure out a neat way of 
   initializing the map.
                                                                      -}
{- init
   PRECONS: 
   RETURNS: 
   EXAMPLE: 
   VARIANT: 
   SIDE EFFECTS: 
-}
init :: IO ()
init = loop (MH.newMap 10 10)

{- Make the loop go through all update functions for respective list
   and make it check and update the lists for enteties like monsters
   and interactables and of course the player.
                                                                      -}
{- loop map
   PRECONS: 
   RETURNS: 
   EXAMPLE: 
   VARIANT: 
   SIDE EFFECTS: 
-}
loop :: MH.Map -> IO ()
loop mapState = do
  putStrLn ""
  loop (update mapState)
  
{- Make a list of all the items on the map ex: ("Player", Position)
   and make the function imperatively loop through all entries on the
   map and update their state. Example "MoveAI ("Monster", Position)"
   If it is easier, make multiple update functions "updateMonster",
   "updatePlayer", "updateInteractables" and give separate lists of
   respective type.
                                                                      -}
{- update map
   PRECONS: 
   RETURNS: 
   EXAMPLE: 
   VARIANT: 
   SIDE EFFECTS: 
-}
update :: MH.Map -> IO (MH.Map)
update mapState = do 
   input <- getLine
   playerInput input map
   -- update Enemies
   -- timer
   return mapState

{- playerInput input map
   PRECONS: 
   RETURNS: 
   EXAMPLE: 
   VARIANT: 
   SIDE EFFECTS: 
-}
playerInput = undefined
{-
--playerInput :: IO ()
playerInput input map
  | take 4 input == "move"  then move  else 
  | take 3 input == "dig"   then dig   else 
  | take 5 input == "shake" then shake else
  | otherwise = playerInput

-}