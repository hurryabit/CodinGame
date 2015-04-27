import Control.Monad
import Data.Char
import qualified Data.Map.Strict as Map
import System.FilePath

main :: IO ()
main = do
  n <- readLn
  q <- readLn
  let parseMime str =
        let [ext,typ] = words str
        in  (map toLower ext,typ)
  table <- fmap Map.fromList . replicateM n $ fmap parseMime getLine
  exts  <- replicateM q $ fmap (map toLower . drop 1 . takeExtension) getLine
  mapM_ (\ext -> putStrLn $ Map.findWithDefault "UNKNOWN" ext table) exts
