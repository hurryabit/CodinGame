main=getLine>>=(\[a,b,s,t]->let[a,b]?x|x>0=a|1>0=b;x=s-a;y=t-b;u=abs x;v=abs y;d=["NS"?y,"WE"?x]in mapM putStrLn$replicate(min u v)d++repeat[d?(v-u)]).map read.words


 -- main=getLine>>=(\[a,b,s,t]->
 --  let
 --   z=[0,1,1,1,0,-1,-1,-1]
 --   l(x,y)=
 --    let(_,p,d)=minimum[(abs u+abs v,(u,v),words"N NE E SE S SW W NW"!!i)|i<-[0..7],let u=x+z!!i;v=y+z!!mod(i+6)8]
 --    in putStrLn d>>l p
 --  in l(s-a,t-b)).map read.words
