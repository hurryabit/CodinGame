import Data.List

conway :: Int -> [[Int]]
conway r = iterate (concatMap (\g -> [length g,head g]) . group) [r]

main :: IO ()
main = do
  r <- readLn
  l <- readLn
  putStrLn . intercalate " " . map show $ conway r !! (l-1)
