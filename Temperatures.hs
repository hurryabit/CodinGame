import Data.List
import Data.Ord

main :: IO ()
main = do
  _ <- getLine
  temps <- fmap (map read . words) getLine
  print $ if null temps then 0 else minimumBy cmp temps
  where
    cmp :: Int -> Int -> Ordering
    cmp x y
      | ax == ay  = y `compare` x
      | otherwise = ax `compare` ay
      where
        ax = abs x
        ay = abs y
