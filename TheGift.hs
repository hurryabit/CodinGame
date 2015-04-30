import Control.Monad
import Data.List

main :: IO ()
main =
  do  n  <- readLn
      c  <- readLn
      bs <- liftM sort $ replicateM n readLn
      let ps = zipWith3 (\b c' k -> min b (c' `div` k)) bs cs [n,n-1 ..]
          cs = scanl (-) c ps
          c0 = last cs
      if c0 > 0
        then putStrLn "IMPOSSIBLE"
        else mapM_ print ps
