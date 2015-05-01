import Data.List
import Data.Ord

overlap :: String -> String -> String
overlap xs ys
  | xs `isInfixOf` ys = ys
  | ys `isInfixOf` xs = xs
  | otherwise         =
      let ts = reverse (tails xs)
          is = inits ys
          (o,_) = last . filter (uncurry (==)) $ zip ts is
      in  xs ++ drop (length o) ys

main :: IO ()
main =
  do  n:ls <- fmap lines getContents
      let gs = take (read n) ls
      print $ minimum . map (length . foldl1 overlap) $ permutations gs
