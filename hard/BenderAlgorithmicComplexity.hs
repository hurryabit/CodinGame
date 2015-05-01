import Control.Monad
import Data.List
import Data.Ord

data Sample = Sample
  { size   :: Int
  , points :: [(Double,Double)]
  }

main :: IO ()
main = do
  s <- readLn
  ps <- replicateM s $ liftM ((\[x,y] -> (x,y)) . map read . words) getLine
  let sample = Sample s ps
      matches = [ (sample `match` complexity,complexity) | complexity <- complexities ]
      (_,bestMatch) = maximumBy (comparing fst) matches
  putStrLn $ name bestMatch

data Complexity = Complexity
  { name :: String
  , fun  :: Double -> Double
  }

complexities :: [Complexity]
complexities =
  [ Complexity "O(1)"         $ \n -> 1
  , Complexity "O(log n)"     $ \n -> log n
  , Complexity "O(n)"         $ \n -> n
  , Complexity "O(n log n)"   $ \n -> n * log n
  , Complexity "O(n^2)"       $ \n -> n^2
  , Complexity "O(n^2 log n)" $ \n -> n^2 * log n
  , Complexity "O(n^3)"       $ \n -> n^3
  , Complexity "O(2^n)"       $ \n -> 2**n
  ]

match :: Sample -> Complexity -> Double
match sample (Complexity { fun = f }) =
  let consts = map (\(x,y) -> y / f x) (points sample)
      mean   = sum consts / fromIntegral (size sample)
      (lower,upper) = (0.9 * mean,1.1 * mean)
      count p = length . filter p
      inRange = count (\c -> lower <= c && c <= upper) consts
  in  fromIntegral inRange / fromIntegral (size sample)
