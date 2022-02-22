import MapHandling as MH
import Object as O
import Graphics as G

init = undefined

loop :: MH.Map -> IO ()
loop mapState = do
  putStrLn ""
  loop newMapState
  where newMapState = update mapState
  


update :: MH.Map -> MH.Map
update mapState = undefined
