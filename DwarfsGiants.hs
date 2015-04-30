import Control.Monad
import Data.Array

main :: IO ()
main =
  do  n:rs <- liftM lines getContents
      let rel :: String -> (Int,Int)
          rel str = let [x,y] = map read (words str) in (x,y)
          bnds = (1,9999)
          graph = accumArray (flip (:)) [] bnds . map rel $ take (read n) rs
          table = listArray bnds . map chain $ range bnds
          chain x = 1 + maximum (0:map (table ! ) (graph ! x))
      print $ maximum (elems table)
