module MapHandling where

-- Tre i rad
-- Sänka skäpp

{-
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

data TempTile = Temp TileBit | Void deriving Eq

{- newMap w h
   PRECONS: 
   
   EXAMPLE: 
   VARIANT: 
-- TODO REMAKE USING TEXAS RANGES?
-}
newMap :: Int -> Int -> Map
newMap w h            = (newMapHeight (newMapWidth w) h, h)
  where newMapWidth :: Int -> MapRow
        newMapWidth w
          | w <= 0    = [empty]
          | otherwise = empty : empty : newMapWidth (w - 1)
        newMapHeight :: MapRow -> Int -> MapPart
        newMapHeight w h
          | h <= 0    = []
          | otherwise = w : newMapHeight w (h - 1)
        empty = ((' ', False), Void)

{- printMap map rows

-}
printMap :: Map -> IO ()
printMap ([], _) = putStrLn ""
printMap (r:ows, h)
  | null ows     = do print (rowToString r)
  | otherwise    = do
     print (rowToString r)
     printMap (ows, h)

{- rowToString row

-}
rowToString :: MapRow -> String 
rowToString [] = []
rowToString ((x, Void):xs)   = (fst x) : rowToString xs
rowToString ((x, Temp y):xs) = (fst y) : rowToString xs

{- printSection map x y radius

-}
printSection :: Map -> Int -> Int -> Int -> IO ()
printSection map = printSectionAux map 0

{- printSectionAux map acc x y radius

-}
printSectionAux :: Map -> Int -> Int -> Int -> Int -> IO ()
printSectionAux ([], h)   i x y r = putStrLn ""
printSectionAux (m:ap, h) i x y r
  | i <= y - r - 1 = printSectionAux (ap, h) (i+1) x y r
  | i >= y + r + 1 = putStrLn ""
  | otherwise      = do
     print (' ' : take (4*r+1) (drop (2*x-2*r-1) (rowToString m)) ++ " ")
     printSectionAux (ap, h) (i+1) x y r

{- editMap Map x y c

-}
editMap :: Map -> Int -> Int -> Tile -> Map
editMap ([], h) _ _ _      = ([], h)
editMap (r:ows, h) x y obj = editMapAux (r:ows, h) ([], h) 0 (2 * x + 1) y obj

editMapAux :: Map -> Map -> Int -> Int -> Int -> Tile -> Map
editMapAux ([], _)   new y x0 y0 obj = new
editMapAux (o:ld, h) new y x0 y0 obj
  | y == y0    = (newMap ++ [editMapWidth o obj x0] ++ ld , h)
  | otherwise  = editMapAux (ld, h) (newMap ++ [o], h) (y + 1) x0 y0 obj
  where newMap = fst new

-- TODO EDIT TO "TAKE / DROP FUNCTION"?
editMapWidth :: MapRow -> Tile -> Int -> MapRow
editMapWidth []     obj _ = []
editMapWidth (m:ap) obj 0 = obj : ap
editMapWidth (m:ap) obj w = m : editMapWidth ap obj (w - 1)

{- readMap map x y

-}
readMap :: Map -> Int -> Int -> Tile
readMap ([], h)  x y = ((' ', False), Void)
readMap (map, h) x y = map !! max 0 y !! max 0 (2*x+1)
