import Control.Monad
import Data.Foldable
import qualified Data.Map as Map
import Data.Monoid
import Prelude hiding (foldr')

main :: IO ()
main = do
  n <- readLn
  numbers <- replicateM n getLine
  let dict = foldr' insert empty numbers
  print $ size dict

data Tree = Node { children :: Forest }
  deriving Show

type Forest = Map.Map Char Tree

empty :: Forest
empty = Map.empty

insert :: String -> Forest -> Forest
insert str nodes =
  case str of
    []    -> nodes
    hd:tl -> Map.alter (Just . Node . insert tl . maybe Map.empty children) hd nodes

size :: Forest -> Int
size = getSum . foldMap (Sum . succ . size . children)
