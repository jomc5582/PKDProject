module Object where

import MapHandling as MH

type Position  = (Int, Int)

{- Direction

-}
data Direction = N | NE | E | SE | S | SW | W | NW | Void deriving Eq

{- move

-}
movePos :: Position -> Position -> Map -> Map
movePos (x0, y0) (x1, y1) map = MH.editMap (MH.editMap map x1 y1 (MH.readMap map x0 y0)) x0 y0 (('_', False), MH.Void)

{- move 

-}
move :: Position -> Direction -> Map -> Map 
move (x0, y0) dir map = MH.editMap (MH.editMap map x1 y1 (MH.readMap map x0 y0)) x0 y0 (('_', False), MH.Void)
  where numDir = directionalValue dir
        y1     = y0 + (fst numDir)
        x1     = x0 + (snd numDir)

{- directionalValue dir

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

--collide