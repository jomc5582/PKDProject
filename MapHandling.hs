module MapHandling where

{- -- TODO
MAYBE MAKE INTO ARRAY INSTEAD OF LIST
MUTABLE QUALITY WOULD MAKE IT MORE 
USEFUL FOR ACTUAL GAME DEVELOPMENT
-}
--          (Visual, occupied)
type TileBit = (Char, Bool)
type Tile    = (TileBit, TempTile)
type MapRow  = [Tile]
type MapPart = [MapRow]
--         (map, height)
type Map = (MapPart, Int)

{- TempTile Temp TileBit | Void
   Represents a temporary (Movable) tile where if there is none is left as Void.
-}
data TempTile = Temp TileBit | Void deriving Eq

{- newMap w h
   Generates an empty map of entered width w and entered height h.
   PRECONS: Any positive integers w and h
   RETURNS: The representation of an empty map of the specified size
   EXAMPLE: newMap 5 5 = [[((' ', False), Void), (('_', False), Void), ((' ', False), Void), (('_', False), Void), ((' ', False), Void), (('_', False), Void), ((' ', False), Void), (('_', False), Void), ((' ', False), Void), (('_', False), Void), ((' ', False), Void)],[((' ', False), Void), (('_', False), Void), ((' ', False), Void), (('_', False), Void), ((' ', False), Void), (('_', False), Void), ((' ', False), Void), (('_', False), Void), ((' ', False), Void), (('_', False), Void), ((' ', False), Void)],[((' ', False), Void), (('_', False), Void), ((' ', False), Void), (('_', False), Void), ((' ', False), Void), (('_', False), Void), ((' ', False), Void), (('_', False), Void), ((' ', False), Void), (('_', False), Void), ((' ', False), Void)],[((' ', False), Void), (('_', False), Void), ((' ', False), Void), (('_', False), Void), ((' ', False), Void), (('_', False), Void), ((' ', False), Void), (('_', False), Void), ((' ', False), Void), (('_', False), Void), ((' ', False), Void)],[((' ', False), Void), (('_', False), Void), ((' ', False), Void), (('_', False), Void), ((' ', False), Void), (('_', False), Void), ((' ', False), Void), (('_', False), Void), ((' ', False), Void), (('_', False), Void), ((' ', False), Void)]]
            Which when printed is representet as a grid of underscore signs '_' and spaces ' '.
            " _ _ _ _ _ "
            " _ _ _ _ _ "
            " _ _ _ _ _ "  -- Empty grid representation
            " _ _ _ _ _ "
            " _ _ _ _ _ "
   VARIANT: size w and or size h
   SIDE EFFECTS: -
-}
newMap :: Int -> Int -> Map
newMap w h            = (newMapHeight (newMapWidth w) h, h)
  where newMapWidth :: Int -> MapRow
        newMapWidth w
          | w <= 0    = [none]
          | otherwise = none : empty : newMapWidth (w - 1)
        newMapHeight :: MapRow -> Int -> MapPart
        newMapHeight w h
          | h <= 0    = []
          | otherwise = w : newMapHeight w (h - 1)
        empty = (('_', False), Void)
        none  = ((' ', False), Void)

{- printMap map
   Prints the entered map row by row.
   PRECONS: Any valid map.
   RETURNS: -
   EXAMPLE: printMap (newMap 5 5) =
            " _ _ _ _ _ "
            " _ _ _ _ _ "
            " _ _ _ _ _ "
            " _ _ _ _ _ "
            " _ _ _ _ _ "
   VARIANT: length of list containing the map
   SIDE EFFECTS: Prints to the screen using IO monad
-}
printMap :: Map -> IO ()
printMap ([], _) = putStrLn ""
printMap (r:ows, h)
  | null ows     = do print (rowToString r)
  | otherwise    = do
     print (rowToString r)
     printMap (ows, h)

{- rowToString row
   Turns any MapRow value into a String value for ease of printing.
   PRECONS: Any valid MapRow data.
   RETURNS: The string of chars from the MapRow value where
            TempTiles if they exists take precedence over the base tile value.
   EXAMPLE: rowToString [((' ', False), Void), (('_', False), ('P', True)), ((' ', False), Void), (('_', False), Void)]
                        = " P _"
   VARIANT: length MapRow
   SIDE EFFECTS: -
-}
rowToString :: MapRow -> String
rowToString [] = []
rowToString ((x, Void):xs)   = fst x : rowToString xs
rowToString ((x, Temp y):xs) = fst y : rowToString xs

{- printSection map x y radius
   Prints a section of the map within the radius of cardinal (Taxi-cab)
   coordinates of the point provided.
   PRECONS: Any Valid non negative coordinates within the maps bounds.
   RETURNS: -
   EXAMPLE: (The printed representations are used instead of the list form of the maps)
                         " _ _ _ _ _ "
                         " _ P _ _ _ "          " P _ _ "
            printSection " _ _ _ _ _ " 2 2 1 -> " _ _ _ "
                         " _ _ _ _ _ "          " _ _ _ "
                         " _ _ _ _ _ "
   VARIANT: length map
   SIDE EFFECTS: Prints the specified section of the map to the terminal row by row.
-}
printSection :: Map -> Int -> Int -> Int -> IO ()
printSection map = printSectionAux map 0

{- printSectionAux map acc x y radius
   Auxillary function, meant to be ran from printSection
   PRECONS: Any Valid non negative coordinates within the maps bounds and an accumulator value at 0.
   RETURNS: -
   EXAMPLE: (The printed representations are used instead of the list form of the maps)
                         " _ _ _ _ _ "
                         " _ P _ _ _ "            " P _ _ "
            printSection " _ _ _ _ _ " 0 2 2 1 -> " _ _ _ "
                         " _ _ _ _ _ "            " _ _ _ "
                         " _ _ _ _ _ "
   VARIANT: length map
   SIDE EFFECTS: Prints the specified section of the map to the terminal row by row.
-}
printSectionAux :: Map -> Int -> Int -> Int -> Int -> IO ()
printSectionAux ([], h)   i x y r = putStrLn ""
printSectionAux (m:ap, h) i x y r
  | i <= y - r - 1 = printSectionAux (ap, h) (i+1) x y r
  | i >= y + r + 1 = putStrLn ""
  | otherwise      = do
     print (' ' : take (4*r+1) (drop (2*x-2*r-1) (rowToString m)) ++ " ")
     printSectionAux (ap, h) (i+1) x y r

{- editMap Map x y tile
   Edits the map at the specified position to the specified tile.
   PRECONS: A valid non negative coordinate within the maps bounds.
   RETURNS: The edited map.
   EXAMPLE: editMap (newMap 5 5) 2 2 (('O', False), Temp ('P', True)) ->
                         " _ _ _ _ _ "
                         " _ _ _ _ _ "
                         " _ _ P _ _ "
                         " _ _ _ _ _ "
                         " _ _ _ _ _ "
   VARIANT: length map
   SIDE EFFECTS: -
-}
editMap :: Map -> Int -> Int -> Tile -> Map
editMap ([], h) _ _ _       = ([], h)
editMap (r:ows, h) x y tile = editMapAux (r:ows, h) ([], h) 0 (2 * x + 1) y tile

{- editMapAux oldMap newMap acc x y tile
   Auxillary function meant to be ran from editMap
   PRECONS: A valid non negative coordinate within the maps bounds
            and an accumulator at ([], h) and an accumulator at 0
            where h is the height of the final map.
   RETURNS: The edited map.
   EXAMPLE: editMapAux (newMap 5 5) ([], 5) 0 2 2 (('O', False), Temp ('P', True)) ->
                         " _ _ _ _ _ "
                         " _ _ _ _ _ "
                         " _ _ P _ _ "
                         " _ _ _ _ _ "
                         " _ _ _ _ _ "
   VARIANT: length map
   SIDE EFFECTS: -
-}
editMapAux :: Map -> Map -> Int -> Int -> Int -> Tile -> Map
editMapAux ([], _)   new y x0 y0 tile = new
editMapAux (o:ld, h) new y x0 y0 tile
  | y == y0    = (newMap ++ [editMapWidth o tile x0] ++ ld , h)
  | otherwise  = editMapAux (ld, h) (newMap ++ [o], h) (y + 1) x0 y0 tile
  where newMap = fst new
        editMapWidth :: MapRow -> Tile -> Int -> MapRow
        editMapWidth []  tile _ = []
        editMapWidth map tile w = take w map ++ tile : drop (w + 1) map

--editMapWidth (m:ap) obj 0 = obj : ap
--editMapWidth (m:ap) obj w = m : editMapWidth ap obj (w - 1)

{- editMapTemp map x y TempTile
   Edits the temp value of the tile att he specifed location.
   PRECONS: A valid non negative coordinate within the maps bounds.
   RETURNS: The map with the temptile value edited at the specified location.
   EXAMPLE: editMapTemp (newMap 5 5) 2 2 Temp ('P', True) ->
                         " _ _ _ _ _ "
                         " _ _ _ _ _ "
                         " _ _ P _ _ "
                         " _ _ _ _ _ "
                         " _ _ _ _ _ "
   VARIANT: length map
   SIDE EFFECTS: -
-}
editMapTemp :: Map -> Int -> Int -> TempTile -> Map
editMapTemp ([], h) _ _ _       = ([], h)
editMapTemp (r:ows, h) x y temp = editMapTempAux (r:ows, h) ([], h) 0 (2 * x + 1) y temp

{- editMapTempAux oldMap newMap aux x y temp
   Auxillary function meant to be ran from editMapTemp.
   PRECONS: A valid non negative coordinate within the maps bounds and an accumulator 
   ([], h) where h is the height of the final map and an accumulator y which is initialized 0.
   RETURNS: The map with the temptile value edited at the specified location.
   EXAMPLE: editMapTempAux (newMap 5 5) ([], h) 0 2 2 Temp ('P', True) ->
                         " _ _ _ _ _ "
                         " _ _ _ _ _ "
                         " _ _ P _ _ "
                         " _ _ _ _ _ "
                         " _ _ _ _ _ "
   VARIANT: length map
   SIDE EFFECTS: -
-}
editMapTempAux :: Map -> Map -> Int -> Int -> Int -> TempTile -> Map
editMapTempAux ([], _)   new y x0 y0 temp = new
editMapTempAux (o:ld, h) new y x0 y0 temp
  | y == y0    = (newMap ++ [editMapWidthTemp o temp x0] ++ ld , h)
  | otherwise  = editMapTempAux (ld, h) (newMap ++ [o], h) (y + 1) x0 y0 temp
  where newMap = fst new
        editMapWidthTemp :: MapRow -> TempTile -> Int -> MapRow
        editMapWidthTemp []  temp _ = []
        editMapWidthTemp map temp w = take w map ++ changeTemp map temp w : drop (w + 1) map
        changeTemp :: MapRow -> TempTile -> Int -> Tile
        changeTemp map temp i       = (fst (map !! max 0 (i-1)), temp)
{-
-- TODO EDIT TO "TAKE / DROP FUNCTION"? 
-- ! Maybe remake editMapTemp all together?
editMapWidthTemp :: MapRow -> TempTile -> Int -> MapRow
editMapWidthTemp []     temp _      = []
editMapWidthTemp ((m, _):ap) temp 0 = (m, temp) : ap
editMapWidthTemp (m:ap) temp w      = m : editMapWidthTemp ap temp (w - 1)
-}

{- readMap map x y
   Returns the tile value at the location 
   PRECONS: A valid non negative coordinate within the maps bounds.
   RETURNS: The tile at the location of (x, y) on the map.
   EXAMPLE: readMap (newMap 5 5) 3 3 = (('_', False), Void) 
   VARIANT: -
   SIDE EFFECTS: - 
-}
readMap :: Map -> Int -> Int -> Tile
readMap ([], h)  x y = ((' ', False), Void)
readMap (map, h) x y = map !! max 0 y !! max 0 (2*x+1)

{- hasTemp tile
   Returns True if the specified tile has a temp value which would take precedence
   over the regular value.
   PRECONS: Any valid Tile.
   RETURNS: True if the tile has a none Void temp value, false otherwise
   EXAMPLE: hasTemp (('_', False), ('P', True)) = True
            hasTemp (('A', True),  Void)        = False
   VARIANT: -
   SIDE EFFECTS: -
-}
hasTemp :: Tile -> Bool
hasTemp (_, Void) = False
hasTemp (_, _)    = True

{- getCollision tile
   Returns True if the tile is occupied / would collide with something were to move into the tile.
   PRECONS: Any valid Tile.
   RETURNS: True if the tile is occupied, either by the temp value or the base tile. Where the temp
            tile takes precedence.
   EXAMPLE: getCollision (('_', False), ('P', True)) = True
            getCollision (('A', True),  Void)        = True
            getCollision (('P', True), ('_', False)) = False
   VARIANT: -
   SIDE EFFECTS: -
-}
getCollision :: Tile -> Bool 
getCollision ((tile, col), Void)  = col
getCollision (_, Temp (temp, tempCol)) = tempCol


