import MapHandling as MH
import Object as O
import Graphics as G

main :: IO ()
main = do
   G.splash 
   G.rulesplash


{- Make the init function intiialize the loop with the starting
   correct values for starting the game. For example starting the game
   with a player on a populated map. Maybe figure out a neat way of 
   initializing the map.
                                                                      -}
init :: IO ()
init = loop (MH.newMap 10 10)

{- Make the loop go through all update functions for respective list
   and make it check and update the lists for enteties like monsters
   and interactables and of course the player.
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
update :: MH.Map -> MH.Map
update mapState = mapState
