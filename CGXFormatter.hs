import Control.Applicative hiding (empty)
import Data.Char
import Data.Foldable
import Text.ParserCombinators.ReadP
import Text.PrettyPrint hiding (char)

element :: ReadP Doc
element =
  option empty ((<> equals) <$> quotstr <* delim '=') <**>
    (flip ($+$) <$> block <|> flip (<>) <$> primitive)
  where
    delim c = char c *> skipSpaces
    quotstr =
      quotes <$> text <$> (char '\'' *> manyTill get (delim '\''))
    block =
      (\elts -> lparen $+$ nest 4 (vcat $ punctuate semi elts) $+$ rparen) <$>
        between (delim '(') (delim ')') (sepBy element (delim ';'))
    primitive =
          text <$> (asum $ munch1 isDigit:map string ["true","false","null"]) <* skipSpaces
      <|> quotstr

main :: IO ()
main =
  do  n:ls <- fmap lines getContents
      let input = unlines $ take (read n) ls
      case readP_to_S (skipSpaces *> element <* eof) input of
        [(doc,"")] -> putStrLn $ render doc
        _          -> error "Parse error."
