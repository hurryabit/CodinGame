import Control.Applicative
import Control.Monad
import Data.Array
import Data.List
import Data.List.Split
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Maybe

type Point = (Double,Double)

dist :: Point -> Point -> Double
dist (lat1,lon1) (lat2,lon2) =
  let x = (lon2 - lon1) * cos ((lat1 + lat2) / 2)
      y = lat2 - lat1
  in  6371.0 * sqrt (x*x + y*y)

type Ident = String

data Stop = Stop
  { ident    :: Ident
  , name     :: String
  , position :: Point
  }
  deriving (Show)

parseStop :: String -> Stop
parseStop line =
  let ident:name:_:lat:lon:_ = splitWhen (','==) line
      radians = ((/ 180.0) . (* pi)) . read
  in  Stop
        { ident    = ident
        , name     = init (tail name)
        , position = (radians lat,radians lon)
        }

parseEdge :: String -> (Ident,Ident)
parseEdge line =
  let src:tgt:_ = words line
  in  (src,tgt)

main :: IO ()
main = do
  src:tgt:n:rest1 <- liftM lines getContents
  let (stops,m:rest2) = splitAt (read n) rest1
      edges = take (read m) rest2
  case solve src tgt (map parseStop stops) (map parseEdge edges) of
    []  -> putStrLn "IMPOSSIBLE"
    pth -> mapM_ (putStrLn . name) pth

infinity :: Double
infinity = 1/0

solve :: Ident -> Ident -> [Stop] -> [(Ident,Ident)] -> [Stop]
solve src tgt stops edges =
  let v2sArray  = listArray (1,length stops) stops
      v2s v     = v2sArray ! v
      i2vMap    = Map.fromList [ (ident s,v) | (v,s) <- assocs v2sArray ]
      i2v i     = fromJust $ i `Map.lookup` i2vMap
      graph     = accumArray (flip (:)) [] (bounds v2sArray) $
                    [ (i2v i1,i2v i2) | (i1,i2) <- edges ]
      v2p       = position . v2s
  in  map v2s $ astar graph (\v1 v2 -> dist (v2p v1) (v2p v2)) (i2v src) (i2v tgt)

type Vertex = Int

type Endo a = a -> a

astar :: Array Vertex [Vertex] -> (Vertex -> Vertex -> Double) -> Vertex -> Vertex -> [Vertex]
astar graph dist src tgt = retrace tgt []
  where
    process :: Endo (Map.Map Double (Vertex,Vertex),IntMap.IntMap Vertex)
    process (open,closed)
      | cur == tgt                 = (Map.empty                      , closed')
      | cur `IntMap.member` closed = (open'                          , closed )
      | otherwise                  = (foldr queue open' (graph ! cur), closed')
      where
        Just ((est,(cur,pre)),open') = Map.minViewWithKey open
        closed' = IntMap.insert cur pre closed
        queue :: Vertex -> Endo (Map.Map Double (Vertex,Vertex))
        queue suc = Map.insert est' (suc,cur)
          where
            est' = est - dist cur tgt + dist cur suc + dist suc tgt
    open0    = Map.singleton (dist src tgt) (src,src)
    closed0  = IntMap.empty
    traces   = snd $ until (Map.null . fst) process (open0,closed0)
    retrace cur pth =
      case IntMap.lookup cur traces of
        Nothing        -> pth
        Just nxt
          | nxt == cur -> cur:pth
          | otherwise  -> retrace nxt (cur:pth)
