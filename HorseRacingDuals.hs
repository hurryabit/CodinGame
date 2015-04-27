import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- readLn
  horses <- replicateM n $ fmap read getLine :: IO [Int]
  let sorted = sort horses
      diff   = minimum $ zipWith (-) (tail sorted) sorted
  print diff
