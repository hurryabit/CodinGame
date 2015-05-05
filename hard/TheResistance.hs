import Control.Applicative
import Control.Arrow
import Data.Array
import Data.Bool
import Data.Monoid

main :: IO ()
main = do
  ms:n:ws <- fmap lines getContents
  let dict = makeDict $ take (read n) ws
      bs   = listArray (1,length ms) (unraw ms)
  print $ queryCount dict bs

type Morse = [Bool]

data MorseTrie a = Node (MorseTrie a) [a] (MorseTrie a) | Leaf
  deriving Show

insert :: MorseTrie a -> Morse -> a -> MorseTrie a
insert Leaf bs val =
  foldr (\b sub -> bool (Node sub [] Leaf) (Node Leaf [] sub) b) (Node Leaf [val] Leaf) bs
insert (Node l vals r) bs val =
  case bs of
    []        -> Node l (val:vals) r
    False:bs' -> Node (insert l bs' val) vals r
    True :bs' -> Node l vals (insert r bs' val)

query :: MorseTrie a -> Array Int Bool -> Int -> [(Int,[a])]
query Leaf _ _ = []
query (Node l vals r) bs k
  | k > snd (bounds bs) = local []
  | otherwise           = local $ query (bool l r $ bs ! k) bs (k+1)
  where
    local = if null vals then id else ((k,vals):)

type MorseDict = MorseTrie String

insertDict :: MorseDict -> String -> MorseDict
insertDict dict word = insert dict (morse word) word

makeDict :: [String] -> MorseDict
makeDict = foldl insertDict Leaf

queryDict :: MorseDict -> Array Int Bool -> [[String]]
queryDict dict bs = recurse 0
  where
    recurse k
      | k > snd (bounds bs) = [[]]
      | otherwise           = do
          (k',words) <- query dict bs k
          (:) <$> words <*> recurse k'

queryCount :: MorseDict -> Array Int Bool -> Int
queryCount dict bs =
  let (l,h) = bounds bs
      bnds  = (l,h+1)
      table = listArray bnds $ map look (range bnds)
      look k
        | k > h     = 1
        | otherwise = sum $ do
            (k',ws) <- query dict bs k
            return $ length ws * table ! k'
  in  table ! l


sampleDict = makeDict [ "HELL", "HELLO", "OWORLD", "WORLD", "TEST" ]

sampleMorse =
  let ms = unraw "......-...-..---.-----.-..-..-.."
  in  listArray (1,length ms) ms

unraw :: String -> Morse
unraw = map ('-'==)

morse :: String -> Morse
morse = concatMap (morseAlph !)

morseAlph :: Array Char Morse
morseAlph = array ('A','Z') $ map (second unraw)
  [ ('A', ".-"  )
  , ('B', "-...")
  , ('C', "-.-.")
  , ('D', "-.." )
  , ('E', "."   )
  , ('F', "..-.")
  , ('G',"--."  )
  , ('H', "....")
  , ('I', ".."  )
  , ('J', ".---")
  , ('K', "-.-" )
  , ('L', ".-..")
  , ('M', "--"  )
  , ('N', "-."  )
  , ('O', "---" )
  , ('P', ".--.")
  , ('Q', "--.-")
  , ('R', ".-." )
  , ('S', "..." )
  , ('T', "-"   )
  , ('U', "..-" )
  , ('V', "...-")
  , ('W', ".--" )
  , ('X', "-..-")
  , ('Y', "-.--")
  , ('Z', "--..")
  ]
