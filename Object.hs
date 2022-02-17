module Object where

import MapHandling as MH

type Position  = (Int, Int)

{- Direction
   Represents a direction or lack of direction (Void)
-}
data Direction = N | NE | E | SE | S | SW | W | NW | Void deriving Eq

-- TODO REMAKE WITH TRY CATCH STRUCTURE
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

-- TODO REMAKE WITH TRY CATCH STRUCTURE
{- move (x0, y0) dir map
   Moves the temp value from the specified tile one step in the specied direction, does not move if Void.
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
move (_, _)  Object.Void map = map
move (x0, y0) dir map        = MH.editMapTemp (MH.editMapTemp map x1 y1 (snd (MH.readMap map x0 y0))) x0 y0 MH.Void
  where numDir = directionalValue dir
        y1     = y0 + fst numDir
        x1     = x0 + snd numDir

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
  | dir == N  = (-1,  0)
  | dir == NE = (-1,  1)
  | dir == E  = ( 0,  1)
  | dir == SE = ( 1,  1)
  | dir == S  = ( 1,  0)
  | dir == SW = ( 1, -1)
  | dir == W  = ( 0, -1)
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


{- 

   PRECONS: 
   RETURNS: 
   EXAMPLE: 
   VARIANT: 
   SIDE EFFECTS: 
-}