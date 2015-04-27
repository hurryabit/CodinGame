import Control.Applicative
import Data.Bits
import Data.Bool
import Data.Char
import Data.List

main :: IO ()
main = getLine >>= putStrLn . chuck . rle . bitify

bitify :: String -> [Bool]
bitify = concatMap (\c -> map (testBit $ ord c) [6, 5 .. 0])

rle :: Eq a => [a] -> [(a,Int)]
rle = map ((,) <$> head <*> length) . group

chuck :: [(Bool,Int)] -> String
chuck = unwords . map (\(b,l) -> bool "00" "0" b ++ " " ++ replicate l '0')
