import System.IO
import Data.List
import Data.Ord
main=hSetBuffering stdout NoBuffering>>getLine>>=(\[k,l,s,t]->let
  z=cycle[0,1,1,1,0,-1,-1,-1]
  loop(x,y)=putStrLn(words"N NE E SE S SW W NW"!!d)>>loop(m d)
   where (m,d)=(\i->(x+z!!i,y+z!!(i+6)),minimumBy(comparing$(\(u,v)->abs(k-u)+abs(l-v)).m)[0..7])
 in loop(s,t)).map read.words
