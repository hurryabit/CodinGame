{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bool
import System.IO

type Floor = Int

type Position = Int

type Coordinate = (Floor, Position)

data Direction = LEFT | RIGHT
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

type Leader = (Coordinate,Direction)

data Action = WAIT | BLOCK | ELEVATOR
  deriving (Show, Eq, Ord, Enum, Bounded)

data Level = Level
  { _floors     :: Floor
  , _positions  :: Position
  , _rounds     :: Int
  , _exit       :: Coordinate
  , _clones     :: Int
  , _maxLadders :: Int
  , _elevators  :: [Coordinate]
  }
  deriving (Show)

data Helpers = Helpers
  { _leaderAge :: Int
  , _blockers  :: [Coordinate]
  , _ladders   :: [Coordinate]
  }

getLevel :: IO Level
getLevel = do
  [fls, pos, rnd, exf, exp, cln, mld, elv] <- fmap words getLine
  elvs <-
    replicateM (read elv) $ do
      [elf, elp] <- fmap words getLine
      return (read elf, read elp)
  return $ Level (read fls) (read pos) (read rnd) (read exf,read exp) (read cln) (read mld) elvs

getLeader :: MonadIO m => m (Maybe Leader)
getLeader = do
  [ldf,ldp,ldd] <- liftM words (liftIO getLine)
  return $ if ldd == "NONE" then Nothing else Just ((read ldf,read ldp),read ldd)

type Android = StateT Helpers (ReaderT Level IO)

incLeaderAge :: Int -> Android ()
incLeaderAge units = modify $ \helpers -> helpers { _leaderAge = _leaderAge helpers + units }

pushBlocker :: Coordinate -> Android ()
pushBlocker blocker = modify $ \helpers -> helpers { _blockers = blocker : _blockers helpers }

pushLadder :: Coordinate -> Android ()
pushLadder ladder = modify $ \helpers -> helpers { _ladders = ladder : _ladders helpers }

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  level <- getLevel
  debug level
  let helpers = Helpers 0 [] []
  void $ runReaderT (execStateT (forever step) helpers) level

step :: Android ()
step = do
  incLeaderAge 1
  mleader <- getLeader
  debug mleader
  case mleader of
    Nothing -> liftIO $ print WAIT
    Just leader -> do
      act <- action leader
      liftIO $ print act
      unless (act == WAIT) $ incLeaderAge (-3)
      when (act == BLOCK) $ pushBlocker (fst leader)
      when (act == ELEVATOR) $ pushLadder (fst leader)


facing :: Leader -> Coordinate -> Bool
facing ((mef,mep),dir) (tgf,tgp) =
  mef == tgf && ((mep <= tgp && dir == RIGHT) || (mep >= tgp && dir == LEFT))

levelled :: Leader -> Coordinate -> Bool
levelled ((mef,_),_) (tgf,_) = mef == tgf

action :: Leader -> Android Action
action leader = do
  objects <- liftM concat $ sequence [liftM (:[]) (asks _exit), asks _elevators, gets _blockers, gets _ladders]
  leaderAge <- gets _leaderAge
  let result
        | leaderAge <= 0 || any (leader `facing`) objects =
            WAIT
        | any (leader `levelled`) objects =
            BLOCK
        | otherwise =
            ELEVATOR
  return result

debug :: (MonadIO m, Show a) => a -> m ()
debug = liftIO . hPrint stderr
