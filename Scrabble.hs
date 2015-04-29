import Control.Monad
import Data.Array

type Histogram = Array Char Int

histogram :: String -> Histogram
histogram  = accumArray (+) 0 ('a','z') . attach 1

attach :: Int -> String -> [(Char,Int)]
attach = map . flip (,)

pointsTable :: Array Char Int
pointsTable = array ('a','z') $ concatMap (uncurry attach)
  [ ( 1, "eaionrtlsu")
  , ( 2, "dg")
  , ( 3, "bcmp")
  , ( 4, "fhvwy")
  , ( 5, "k")
  , ( 8, "jx")
  , (10, "qz")
  ]

below :: Histogram -> Histogram -> Bool
below h1 h2 = and $ zipWith (<=) (elems h1) (elems h2)

points :: Histogram -> Int
points = sum . zipWith (*) (elems pointsTable) . elems

main :: IO ()
main = do
  n    <- readLn
  wrds <- replicateM n getLine
  dice <- liftM histogram getLine
  let (_,_,sol) = maximum [ (points hist,idx,wrd) | (idx,wrd) <- zip [0,-1..] wrds, let hist = histogram wrd, hist `below` dice ]
  putStrLn sol
