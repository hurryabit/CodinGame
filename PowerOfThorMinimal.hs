import System.IO
main=hSetBuffering stdout NoBuffering>>getLine>>=(\[k,l,s,t]->let
  z=cycle[0,1,1,1,0,-1,-1,-1]
  loop(x,y)=putStrLn(words"N NE E SE S SW W NW"!!d)>>loop q
   where (_,q,d)=minimum[(abs(k-u)+abs(l-v),p,i)|i<-[0..7],let p@(u,v)=(x+z!!i,y+z!!(i+6))]
 in loop(s,t)).map read.words
