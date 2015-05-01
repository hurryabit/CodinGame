import Control.Monad
import Data.Array
import Data.List
import Data.Maybe

type Point = (Int,Int) -- (y,x)

type Grid = Array Point Bool

main = do
  width  <- readLn
  height <- readLn
  grid   <- fmap (listArray ((0,0),(height-1,width-1)) . map ('0' ==) . concat) $ replicateM height getLine
  let triples = apuInit grid
  forM_ triples $ \(node,right,bottom) ->
    putStrLn . unwords . map show . concatMap (\(y,x) -> [x,y]) $ node : map (fromMaybe (-1,-1)) [right,bottom]

apuInit :: Grid -> [(Point,Maybe Point,Maybe Point)]
apuInit grid =
  let nodes =
        [ point | (point,True) <- assocs grid]
      rightNeighbors =
        zipWith checkRightNeighbor nodes $ map Just (drop 1 nodes) ++ [Nothing]
      bottomNeighbors =
        map (bottomNeighbor grid) nodes
  in  zip3 nodes rightNeighbors bottomNeighbors

checkRightNeighbor :: Point -> Maybe Point -> Maybe Point
checkRightNeighbor (y1,x1) mpoint = do
  point <- mpoint
  guard $ y1 == fst point
  return point

bottomNeighbor :: Grid -> Point -> Maybe Point
bottomNeighbor grid (y,x) = find (grid !) [ (y',x) | y' <- [y+1 .. ymax] ]
  where (_,(ymax,_)) = bounds grid
