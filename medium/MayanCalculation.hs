import Control.Monad
import Data.Array
import Data.List

look :: (Ix i, Eq e) => Array i e -> e -> i
look tab elt = maybe (error "symbol not found") fst . find ((elt ==) . snd) $ assocs tab

getMayanNum :: Int -> Array Int [String] -> IO Int
getMayanNum height alph =
  do  s  <- readLn
      ls <- replicateM s getLine
      return $ foldl (\n d -> 20*n+d) 0 . map (look alph) . blocks height $ ls

main :: IO ()
main =
  do  [width,height] <- liftM (map read . words) getLine
      alph <- liftM (listArray (0,19) . transpose . map (blocks width)) $ replicateM height getLine
      [n1,n2] <- replicateM 2 $ getMayanNum height alph
      op <- liftM (maybe (error "op not found") id . flip lookup [("+",(+)),("*",(*)),("-",(-)),("/",div)]) getLine
      let res = n1 `op` n2
      putStr $ unlines . concatMap (alph !) $ digits res

digits :: Int -> [Int]
digits 0 = [0]
digits n = reverse $ unfoldr (\k -> guard (k>0) >> return (uncurry (flip (,)) $ k `divMod` 20)) n

blocks :: Int -> [a] -> [[a]]
blocks n = unfoldr $ \xs -> guard (not $ null xs) >> return (splitAt n xs)
