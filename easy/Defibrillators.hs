import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.Ord
import Text.ParserCombinators.ReadP

data Point =
  Point
    { longitude :: Double
    , latitude  :: Double
    }
    deriving (Show)

data Defibrilator =
  Defibrilator
    { name     :: String
    , position :: Point
    }
    deriving (Show)

degrees :: ReadP Double
degrees =
  (\pre post -> read $ pre ++ '.':post)
    <$> satisfy isDigit `manyTill` char ','
    <*> munch1 isDigit

point :: ReadP sep -> ReadP Point
point sep =
  Point <$> (radians <* sep) <*> radians
  where
    radians = ((/ 180.0) . (* pi)) <$> degrees

semi :: ReadP ()
semi = void $ char ';'

field :: ReadP String
field = get `manyTill` semi

eol :: ReadP ()
eol = void $ munch1 (`elem` "\r\n")

defibrilator :: ReadP Defibrilator
defibrilator =
  Defibrilator <$> (field *> field <* field <* field) <*> point semi

list :: ReadP a -> ReadP [a]
list item = do
  n <- read <$> munch1 isDigit <* eol
  replicateM n (item <* eol)

request :: ReadP (Point,[Defibrilator])
request =
    (,) <$> (point eol <* eol) <*> list defibrilator

dist :: Point -> Point -> Double
dist (Point lon1 lat1) (Point lon2 lat2) =
  let x = (lon2 - lon1) * cos ((lat1 + lat2) / 2)
      y = lat2 - lat1
  in  6371.0 * sqrt (x*x + y*y)

main :: IO ()
main =
  do  input <- getContents
      let [((me,defs),"")] = readP_to_S request input
          def = minimumBy (comparing $ dist me . position) defs
      putStrLn $ name def
