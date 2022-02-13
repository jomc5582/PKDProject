module MapHandling where

{-
MAYBE MAKE INTO ARRAY INSTEAD OF LIST
MUTABLE QUALITY WOULD MAKE IT MORE 
USEFUL FOR ACTUAL GAME DEVELOPMENT
-}
type MapRow = String
type MapPart = [String]
--         (map, height)
type Map = (MapPart, Int)

{- newMap w h
   PRECONS: 
   
   EXAMPLE: 
   VARIANT: 

-}
newMap :: Int -> Int -> Map
newMap w h            = (newMapHeight (newMapWidth w) h, h)
  where newMapWidth :: Int -> String
        newMapWidth w
          | w <= 0    = [' ']
          | otherwise = ' ' : '_' : newMapWidth (w - 1)
        newMapHeight :: String -> Int -> MapPart
        newMapHeight w h
          | h <= 0    = []
          | otherwise = w : newMapHeight w (h - 1)

{- printMap map rows

-}
printMap :: Map -> IO ()
printMap ([], _) = print ""
printMap (r:ows, h)
  | null ows     = print r
  | otherwise    = do
     print r
     printMap (ows, h)

editMap :: Map -> Int -> Int -> Char -> Map
editMap ([], h) _ _ _      = ([], h)
editMap (r:ows, h) x y obj = editMapAux (r:ows, h) ([], h) 0 (2 * x) y obj

editMapAux :: Map -> Map -> Int -> Int -> Int -> Char -> Map
editMapAux ([], _)   new y x0 y0 obj = new
editMapAux (o:ld, h) new y x0 y0 obj
  | y == y0    = (editMapWidth o obj x0 : newMap, h)
  | otherwise  = editMapAux (ld, h) (o:newMap, h) (y - 1) x0 y0 obj
  where newMap = fst new

editMapWidth :: MapRow -> Char -> Int -> MapRow
editMapWidth []     obj _ = []
editMapWidth (m:ap) obj 0 = obj : ap
editMapWidth (m:ap) obj w = m : editMapWidth ap obj (w - 1)

--addMapRow

