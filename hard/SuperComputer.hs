import Data.List
import Data.Ord

main :: IO ()
main = do
  n:rest <- fmap lines getContents
  let jobs :: [(Int,Int)]
      jobs = map ((\[j,d] -> (j,j+d-1)) . map read . words) $ take (read n) rest
      doit (e,c) (j,k)
        | j > e     = (k,c+1)
        | otherwise = (e,c)
      (_,c) = foldl doit (0,0) $ sortBy (comparing snd) jobs
  print c
