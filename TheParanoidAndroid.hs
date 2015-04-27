{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
import Control.Monad
import Data.Bool
import System.IO

type Floor = Int

type Position = Int

type Coordinate = (Floor, Position)

data Direction = LEFT | RIGHT
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

type Leader = (Coordinate,Direction)

data Action = WAIT | BLOCK
  deriving (Show, Eq, Ord, Enum, Bounded)

data Level = Level
  { _floors     :: Floor
  , _positions  :: Position
  , _rounds     :: Int
  , _exit       :: Coordinate
  , _clones     :: Int
  , _elevators  :: [Coordinate]
  }
  deriving (Show)

getLevel :: IO Level
getLevel = do
  [fls, pos, rnd, exf, exp, cln, _, elv] <- fmap words getLine
  elvs <-
    replicateM (read elv) $ do
      [elf, elp] <- fmap words getLine
      return (read elf, read elp)
  return $ Level (read fls) (read pos) (read rnd) (read exf,read exp) (read cln) elvs

getLeader :: IO (Maybe Leader)
getLeader = do
  [ldf,ldp,ldd] <- fmap words getLine
  return $ if ldd == "NONE" then Nothing else Just ((read ldf,read ldp),read ldd)

facing :: Leader -> Coordinate -> Bool
facing ((mef,mep),dir) (tgf,tgp) =
  mef == tgf && ((mep <= tgp && dir == RIGHT) || (mep >= tgp && dir == LEFT))

block :: Level -> [Coordinate] -> Leader -> Bool
block level blocks leader = not . any (facing leader) $ _exit level:_elevators level ++ blocks

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  level <- getLevel
  debug level
  loop level [] 0

loop :: Level -> [Coordinate] -> Int -> IO ()
loop level blocks age = do
  mleader <- getLeader
  debug mleader
  case mleader of
    Just leader
      | age > 0 && block level blocks leader -> do
          print BLOCK
          loop level (fst leader:blocks) (age - 3)
    _ -> do
          print WAIT
          loop level blocks (age+1)

debug :: Show a => a -> IO ()
debug = hPrint stderr
