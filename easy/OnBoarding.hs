import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    loop

loop :: IO ()
loop = do
  enemy1 <- getLine
  dist1  <- readLn :: IO Int
  enemy2 <- getLine
  dist2  <- readLn :: IO Int
  putStrLn $ if dist1 < dist2 then enemy1 else enemy2
  loop
