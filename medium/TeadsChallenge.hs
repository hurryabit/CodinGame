import Control.Monad
import Data.Array
import Data.Array.IO
import Data.Array.Unsafe
import Data.List
import Data.Tree

main :: IO ()
main = do
  table <- buildAdjacency
  let tree = buildTree table
  print $ bcDepth $ annotateDepth tree

buildAdjacency :: IO (Array Int [Int])
buildAdjacency = do
  n <- readLn
  table <- newArray (0,199999) [] :: IO (IOArray Int [Int])
  replicateM_ n $ do
    [x,y] <- fmap (map read . words) getLine
    mapM_ (\(a,b) -> modifyArray table a (b:)) [(x,y),(y,x)]
  unsafeFreeze table

modifyArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray arr idx fun = readArray arr idx >>= writeArray arr idx . fun

buildTree :: Array Int [Int] -> Tree Int
buildTree table =
  let builder :: (Int,Int) -> (Int,[(Int,Int)])
      builder (node,parent) =
        let children = delete parent (table ! node)
        in  (node,map (flip (,) node) children)
      Just (root,_) = find (not . null . snd) (assocs table)
  in  unfoldTree builder (root,root)

type DepthTree a = Tree (a,Int)

annotateDepth :: Tree a -> DepthTree a
annotateDepth (Node x []) = Node (x,0) []
annotateDepth (Node x ts) =
  let ts' = map annotateDepth ts
      d   = 1 + maximum (map (snd . rootLabel) ts')
  in  Node (x,d) ts'

bcDepth :: DepthTree a -> Int
bcDepth = bcDepth' 0

bcDepth' :: Int -> DepthTree a -> Int
bcDepth' n (Node _     []) = n
bcDepth' n (Node (_,d) ts) =
  minimum $ max n d : [ bcDepth' (maximum $ (1+n) : map ((2+) . snd . rootLabel) ts') t | (t,ts') <- select ts ]

select :: [a] -> [(a,[a])]
select []     = []
select (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- select xs ]
