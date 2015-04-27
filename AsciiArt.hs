import Control.Monad
import Data.Array
import Data.Char
import Data.List

main :: IO ()
main = do
  width  <- readLn
  height <- readLn
  text   <- getLine
  table  <- fmap (listArray ('A',unknown) . blocks width . transpose) $ replicateM height getLine
  let look char
        | isAlpha char = table ! toUpper char
        | otherwise    = table ! unknown
  mapM_ putStrLn $ transpose . concatMap look $ text

unknown :: Char
unknown = succ 'Z'

blocks :: Int -> [a] -> [[a]]
blocks n = unfoldr $ \xs -> guard (not $ null xs) >> return (splitAt n xs)
