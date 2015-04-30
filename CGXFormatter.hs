import Control.Applicative hiding (empty)
import Control.Monad
import Data.Bool
import Data.Char
import Text.ParserCombinators.ReadP
import Text.PrettyPrint hiding (char)

data Element =
    Block [NamedElement]
  | Number Int
  | Boolean Bool
  | Null
  | String String
  deriving (Show)

isBlock :: Element -> Bool
isBlock elt =
  case elt of
    Block _ -> True
    _       -> False

data NamedElement = NamedElement (Maybe String) Element
  deriving (Show)

spaced :: ReadP a -> ReadP a
spaced p = p <* skipSpaces

delim :: Char -> ReadP ()
delim c = spaced (void $ char c)


elementP :: ReadP Element
elementP =
      Block <$> blockP
  <|> Number <$> numberP
  <|> Boolean <$> booleanP
  <|> Null <$ nullP
  <|> String <$> stringP

blockP :: ReadP [NamedElement]
blockP =
  between (delim '(') (delim ')') $ sepBy namedElementP (delim ';')

numberP :: ReadP Int
numberP = spaced $ read <$> munch1 isDigit

booleanP :: ReadP Bool
booleanP = spaced $ string "true" *> pure True <|> string "false" *> pure False

nullP :: ReadP ()
nullP = spaced (void $ string "null")

stringP :: ReadP String
stringP = char '\'' *> manyTill get (delim '\'')

namedElementP :: ReadP NamedElement
namedElementP =
  NamedElement <$> option Nothing (Just <$> stringP <* delim '=') <*> elementP


parseCGX :: String -> Either String NamedElement
parseCGX input =
  case readP_to_S (skipSpaces *> namedElementP <* eof) input of
    []         -> Left "no parse"
    [(elt,"")] -> Right elt
    _          -> Left "ambiguous parse"

main :: IO ()
main =
  do  n:ls <- liftM lines getContents
      let input = unlines $ take (read n) ls
      case parseCGX input of
        Left msg  -> error msg
        Right elt -> putStrLn $ render (namedElementD elt)

elementD :: Element -> Doc
elementD elt =
  case elt of
    Block elts  -> blockD elts
    Number num  -> numberD num
    Boolean bol -> booleanD bol
    Null        -> nullD
    String str  -> stringD str

blockD :: [NamedElement] -> Doc
blockD elts =
      lparen
  $+$ nest 4 (foldl ($+$) empty $ punctuate semi $ map namedElementD elts)
  $+$ rparen

numberD :: Int -> Doc
numberD = int

booleanD :: Bool -> Doc
booleanD = text . bool "false" "true"

nullD :: Doc
nullD = text "null"

stringD :: String -> Doc
stringD = quotes . text

namedElementD :: NamedElement -> Doc
namedElementD (NamedElement oname elt) =
  let cat = bool (<>) ($+$) (isBlock elt)
  in  maybe empty (\name -> stringD name <> equals) oname `cat` elementD elt
