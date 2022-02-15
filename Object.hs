module Object where

import MapHandling as MH

type Position = (Int, Int)

move :: Position -> Position -> Map -> Map
move (x0, y0) (x1, y1) map = MH.editMap (MH.editMap map x1 y1 (MH.readMap map x0 y0)) x0 y0 ('_', False)

--collide