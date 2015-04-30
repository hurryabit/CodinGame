main :: IO ()
main =
  do  getLine
      vs <- fmap (map read . words) getLine :: IO [Int]
      let p = minimum $ 0:zipWith (-) vs (scanl1 max vs)
      print p
