module Object where

import MapHandling as MH

type Position  = (Int, Int)

{- Direction
   Represents a direction or lack of direction (Void)
-}
data Direction = N | NE | E | SE | S | SW | W | NW | Void deriving Eq

{- movePos (x0, y0) (x1, y1) map
   Moves the temporary tile from the first position to the second. 
   This overwrites the value at the second position and leaves the 
   first tile with a Void temp value.
   PRECONS: Two non negative coordinates that are within the bounds
            of the map.
   RETURNS: The map with the specified tile moved.
   EXAMPLE: (The printed representations are used instead of the list form of the maps)
                                 " _ _ _ _ _ "    " _ _ _ _ _ "
                                 " _ P _ _ _ "    " _ _ _ _ _ "
              move (1, 1) (3, 2) " _ _ _ _ _ " -> " _ _ _ P _ "
                                 " _ _ _ _ _ "    " _ _ _ _ _ "
                                 " _ _ _ _ _ "    " _ _ _ _ _ "
   VARIANT: -
   SIDE EFFECTS: -
-}
movePos :: Position -> Position -> Map -> Map
movePos (x0, y0) (x1, y1) map = MH.editMapTemp (MH.editMapTemp map x1 y1 (snd oldPos)) x0 y0 MH.Void
  where oldPos = MH.readMap map x0 y0

{- move (x0, y0) dir map
   Checks if the tile being moved into is alvaliable if so it moves the temp value from the specified tile
   one step in the specied direction, does not move if Void.
   PRECONS: A valid coordinate to the tile and a valid direction where the tile in the direction is within
            the maps bounds.
   RETURNS: The map with the specified tile moved .
   EXAMPLE: (The printed representations are used instead of the list form of the maps)
                             " _ _ _ _ _ "    " _ _ _ _ _ "
                             " _ P _ _ _ "    " _ _ P _ _ "
               move (1, 1) E " _ _ _ _ _ " -> " _ _ _ _ _ "
                             " _ _ _ _ _ "    " _ _ _ _ _ "
                             " _ _ _ _ _ "    " _ _ _ _ _ "
                             
                             " _ _ _ _ _ "    " _ P _ _ _ "
                             " _ P _ _ _ "    " _ _ _ _ _ "
               move (1, 1) N " _ _ _ _ _ " -> " _ _ _ _ _ "
                             " _ _ _ _ _ "    " _ _ _ _ _ "
                             " _ _ _ _ _ "    " _ _ _ _ _ "
   VARIANT: -
   SIDE EFFECTS: -
-}
move :: Position -> Direction -> Map -> Map
move pos dir map
  | collides ((fst pos + (fst value)), (snd pos + (snd value))) map = map
  | otherwise                                                           = moveAux pos dir map
  where value = directionalValue dir

{- moveAux (x0, y0) dir map
   Auxillary function, run from move
   PRECONS: A valid coordinate to the tile and a valid direction where the tile in the direction is within
            the maps bounds.
   RETURNS: The map with the specified tile moved. 
   EXAMPLE: (The printed representations are used instead of the list form of the maps)
                                " _ _ _ _ _ "    " _ _ _ _ _ "
                                " _ P _ _ _ "    " _ _ P _ _ "
               moveAux (1, 1) E " _ _ _ _ _ " -> " _ _ _ _ _ "
                                " _ _ _ _ _ "    " _ _ _ _ _ "
                                " _ _ _ _ _ "    " _ _ _ _ _ "
                             
                                " _ _ _ _ _ "    " _ P _ _ _ "
                                " _ P _ _ _ "    " _ _ _ _ _ "
               moveAux (1, 1) N " _ _ _ _ _ " -> " _ _ _ _ _ "
                                " _ _ _ _ _ "    " _ _ _ _ _ "
                                " _ _ _ _ _ "    " _ _ _ _ _ "
   VARIANT: -
   SIDE EFFECTS: -
-}
moveAux :: Position -> Direction -> Map -> Map
moveAux (_, _)  Object.Void map = map
moveAux (x0, y0) dir map        = MH.editMapTemp (MH.editMapTemp map x1 y1 (snd (MH.readMap map x0 y0))) x0 y0 MH.Void
  where numDir = directionalValue dir
        x1     = x0 + fst numDir
        y1     = y0 + snd numDir

{- directionalValue dir
   Returns the coordinate values of an entered direction. Void returns (0, 0)
   PRECONS: Any valid direction
   RETURNS: the pair of coordinates representing the change in coordinates from moving said direction.
   EXAMPLE: directionalValue N    == (-1, 0)
            directionalValue Void == (0, 0)
   VARIANT: -
   SIDE EFFECTS: - 
-}
directionalValue :: Direction -> Position
directionalValue dir
  | dir == N  = ( 0, -1)
  | dir == NE = ( 1, -1)
  | dir == E  = ( 1,  0)
  | dir == SE = ( 1,  1)
  | dir == S  = ( 0,  1)
  | dir == SW = (-1,  1)
  | dir == W  = (-1,  0)
  | dir == NW = (-1, -1)
  | otherwise = ( 0,  0)

{- collides (x, y) map
   Returns wether the specified position on the map is occupied and would collide with entering values.
   PRECONS: A valid non negative coordinate within the maps bounds.
   RETURNS: True if it is occupied / solid, False if it is not.
   EXAMPLE: (The printed representations are used instead of the list form of the maps)
                           " _ _ _ _ _ "
                           " _ _ _ _ _ "
           collides (3, 3) " _ _ _ _ _ " = True
                           " _ _ _ P _ "
                           " _ _ _ _ _ "

                           " _ _ _ _ _ "
                           " _ _ _ _ _ "
           collides (0, 0) " _ _ _ _ _ " = False
                           " _ _ _ P _ "
                           " _ _ _ _ _ "
   VARIANT: -
   SIDE EFFECTS: -
-}
collides :: Position -> Map -> Bool
collides (x, y) map = MH.getCollision (MH.readMap map x y)

{- getType pos map
   gets the tile type of a specified tile. Gives temp values priority
   PRECONS: A valid coordinate on the map.
   RETURNS: the tiles value.
   EXAMPLE: (The printed representations are used instead of the list form of the maps)
                           " _ _ _ _ _ "
                           " _ _ _ _ _ "
           getType (3, 3)  " _ _ _ _ _ " = ('P', True)
                           " _ _ _ P _ "
                           " _ _ _ _ _ "
   VARIANT: 
   SIDE EFFECTS: 
-}
getType :: Position -> Map -> MH.Base
getType (x, y) map
  | MH.hasTemp tile = MH.tempToBase (snd tile)
  | otherwise       = fst tile
  where tile        = MH.readMap map x y

{- directionFrom pos1 pos2
   Gets the direction of position one relative position two.
   PRECONS: Two valid positions wihtin the map boundries.
   RETURNS: The direction from position one to position two.
   EXAMPLE: directionFrom (1, 1) (1, 2) = direction S
   VARIANT: -
   SIDE EFFECTS: -
-} 
directionFrom :: Position -> Position -> Direction
directionFrom (x1, y1) (x2, y2)
  | x1 < x2   = if y1 < y2 then SW else if y1 == y2 then W           else NW
  | x1 == x2  = if y1 < y2 then S  else if y1 == y2 then Object.Void else N
  | x1 > x2   = if y1 < y2 then SE else if y1 == y2 then E           else NE
  | otherwise = Object.Void

{- clearTile pos map
   clears a specified tile on the map.
   PRECONS: A valid coordinate within the boundries of the map.
   RETURNS: The map with the tile specified changed to (('_', False), Void)
   EXAMPLE: 
                            " _ _ _ _ _ "   " _ _ _ _ _ "
                            " _ _ _ _ _ "   " _ _ _ _ _ "
           clearTile (3, 3) " _ _ _ _ _ " = " _ _ _ _ _ "
                            " _ _ _ P _ "   " _ _ _ _ _ "
                            " _ _ _ _ _ "   " _ _ _ _ _ "
   VARIANT: -
   SIDE EFFECTS: -
-}
clearTile :: Position -> Map -> Map
clearTile (x, y) map = editMap map x y (('_', False), MH.Void)

{- player
   The representation of a player.
   PRECONS: -
   RETURNS: The value of the player. Temp ('P', True)
   EXAMPLE: player = Temp ('P', True)
   VARIANT: -
   SIDE EFFECTS: -
-}
player :: MH.Temporary
player = Temp ('Z', True)

{- enemy
   The representation of an enemy.
   PRECONS: -
   RETURNS: the value of an enemy. Temp ('E', True)
   EXAMPLE: Temp ('E', True)
   VARIANT: -
   SIDE EFFECTS: -
-}
enemy :: MH.Temporary
enemy = Temp ('E', True)

{- boulder
   The representation of a boulder in the game.
   PRECONS: -
   RETURNS: The value of a boulder. Temp ('O', True)
   EXAMPLE: boulder = Temp ('O', True)
   VARIANT: -
   SIDE EFFECTS: -
-}
boulder :: MH.Temporary
boulder = Temp ('O', True)

{- treasure
   The representaion of treasure in the game.
   PRECONS: -
   RETURNS: The value of treasure. ('X', False)
   EXAMPLE: treasure = ('X', False)
   VARIANT: -
   SIDE EFFECTS: -
-}
treasure :: MH.Base
treasure = ('X', False)

{- treasure
   The representaion of treasure in the game.
   PRECONS: -
   RETURNS: The value of treasure. ('X', False)
   EXAMPLE: treasure = ('X', False)
   VARIANT: -
   SIDE EFFECTS: -
-}
goal :: MH.Base
goal = ('C', False)

{- push pos playerPos map
   Checks if a tile is occupying the space and if it is of a type that is allowed to be pushed.
   PRECONS: A valid position within the maps boundries.
   RETURNS: The map with updated positions.
   EXAMPLE: -
   VARIANT: -
   SIDE EFFECTS: -
-}
pushDir :: Direction -> Position -> Map -> Map
pushDir dir (x, y) map = move (dx + x, dy + y) dir map --push (dx + x, dy + y) (x, y) map
  where value = directionalValue dir
        dx = fst value
        dy = snd value

{- push pos playerPos map
   Checks if a tile is occupying the space and if it is of a type that is allowed to be pushed.
   PRECONS: A valid position within the maps boundries.
   RETURNS: The map with updated positions.
   EXAMPLE: -
   VARIANT: -
   SIDE EFFECTS: -
-}
push :: Position -> Position -> Map -> Map
push pos playerPos map
  | getType pos map == ('O', True) = pushAux pos playerPos map
  | getType pos map == ('E', True) = pushAux pos playerPos map
  | getType pos map == ('P', True) = pushAux pos playerPos map
  | otherwise                      = map

{- pushAux pos playerPos map
   Auxillary function, meant to be ran from push
   PRECONS: A valid position within the maps boundries.
   RETURNS: The map with updated positions 
   EXAMPLE: -
   VARIANT: -
   SIDE EFFECTS: - 
-}
pushAux :: Position -> Position -> Map -> Map
pushAux pos playerPos = move pos (directionFrom playerPos pos)

{- dig pos map
   PRECONS: A valid position within the maps boundries.
   RETURNS: The changed (or unchanged) map tupled with a score value.
   EXAMPLE: 
   VARIANT: -
   SIDE EFFECTS: -
-}
dig :: Position -> Map -> Map
dig pos map = if getType pos map == ('X', False) then clearTile pos map else map

{- shake pos map
   PRECONS: A valid position within the maps boundries.
   RETURNS: The changed (or unchanged) map tupled with a score value.
   EXAMPLE: 
   VARIANT: -
   SIDE EFFECTS: -
-}
shake :: Position -> Direction -> Map -> (Map, Int)
shake (x, y) dir map = if getType (x + dx, y + dy) map == ('T', True) then (editMapTemp map (x + dx) (y + dy) MH.Void, 100)
                                                                      else (map, 0)
  where dy = snd (directionalValue dir)
        dx = fst (directionalValue dir)
{- 
   PRECONS: 
   RETURNS: 
   EXAMPLE: 
   VARIANT: 
   SIDE EFFECTS: 
-}
getPlayerCoord :: Int -> Map -> Position
getPlayerCoord _ ([], _)   = (-1, -1)
getPlayerCoord y (m:ap, h) 
  | x == -1   = getPlayerCoord (y + 1) (ap, h) 
  | otherwise = (x `div` 2, y)
  where
  checkRow :: MapRow -> Int -> Int
  checkRow []     _ = -1
  checkRow (r:ow) w
    | snd r == Temp ('Z', True) = w
    | otherwise                 = checkRow ow (w + 1)
  x = checkRow m 0

{- 
   PRECONS: 
   RETURNS: 
   EXAMPLE: 
   VARIANT: 
   SIDE EFFECTS: 
-}
visionRange :: Int
visionRange = 3

{- 
   PRECONS: 
   RETURNS: 
   EXAMPLE: 
   VARIANT: 
   SIDE EFFECTS: 
-}
