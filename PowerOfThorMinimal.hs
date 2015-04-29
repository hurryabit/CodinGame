import System.IO
import Data.List
import Data.Ord
data D=N|NE|E|SE|S|SW|W|NW deriving(Show,Enum)
main=hSetBuffering stdout NoBuffering>>getLine>>=(\[k,l,s,t]->let
  z=cycle[0,1,1,1,0,-1,-1,-1]
  loop(x,y)=print d>>loop(m d)
   where (m,d)=((\i->(x+z!!i,y+z!!(i+6))).fromEnum,minimumBy(comparing$(\(u,v)->abs(k-u)+abs(l-v)).m)[N ..])
 in loop(s,t)).map read.words
