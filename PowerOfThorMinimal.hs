import System.IO
main=hSetBuffering stdout NoBuffering>>getLine>>=(\[k,l,s,t]->let
  z=[0,1,1,1,0,-1,-1,-1]
  loop(x,y)=putStrLn(words"N NE E SE S SW W NW"!!d)>>loop p
   where (_,p,d)=minimum[(abs(k-u)+abs(l-v),(u,v),i)|i<-[0..7],let u=x+z!!i;v=y+z!!mod(i+6)8]
 in loop(s,t)).map read.words
