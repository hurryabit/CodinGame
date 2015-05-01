import System.IO
import Control.Monad
import Data.List
import Data.Ord

data Dir = N | NE | E | SE | S | SW | W | NW
  deriving (Eq, Ord, Show, Enum, Bounded)

type Point = (Int,Int)

move :: Point -> Dir -> Point
move (x,y) dir = (x+dx,y+dy)
  where
    (dx,dy) =
      case dir of
        N  -> ( 0,-1)
        NE -> ( 1,-1)
        E  -> ( 1, 0)
        SE -> ( 1, 1)
        S  -> ( 0, 1)
        SW -> (-1, 1)
        W  -> (-1, 0)
        NW -> (-1,-1)

dist :: Point -> Point -> Int
dist (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- DO NOT REMOVE
  [xl,yl,xt,yt] <- fmap (map read . words) getLine
  loop (xl,yl) (xt,yt)

loop :: Point -> Point -> IO ()
loop light thor = do
  _ <- getLine
  let dir = minimumBy (comparing $ dist light . move thor) [minBound .. maxBound]
  print dir
  let thor' = move thor dir
  unless (light == thor') $ loop light thor'
