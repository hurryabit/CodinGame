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

data Helpers = Helpers
  { _leaderAge :: Int
  , _blockers  :: [Coordinate]
  }

getLevel :: IO Level
getLevel = do
  [fls, pos, rnd, exf, exp, cln, _, elv] <- fmap words getLine
  elvs <-
    replicateM (read elv) $ do
      [elf, elp] <- fmap words getLine
      return (read elf, read elp)
  return $ Level (read fls) (read pos) (read rnd) (read exf,read exp) (read cln) elvs

getLeader :: MonadIO m => m (Maybe Leader)
getLeader = do
  [ldf,ldp,ldd] <- liftM words (liftIO getLine)
  return $ if ldd == "NONE" then Nothing else Just ((read ldf,read ldp),read ldd)

type Android = StateT Helpers (ReaderT Level IO)

incLeaderAge :: Int -> Android ()
incLeaderAge units = modify $ \helpers -> helpers { _leaderAge = _leaderAge helpers + units }

pushBlocker :: Coordinate -> Android ()
pushBlocker blocker = modify $ \helpers -> helpers { _blockers = blocker : _blockers helpers }

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  level <- getLevel
  debug level
  let helpers = Helpers 0 []
  void $ runReaderT (execStateT (forever step) helpers) level

step :: Android ()
step = do
  mleader <- getLeader
  debug mleader
  act <- maybe (return WAIT) action mleader
  liftIO $ print act
  case act of
    BLOCK -> do
      pushBlocker (maybe undefined fst mleader)
      incLeaderAge (-3)
    WAIT -> do
      incLeaderAge 1

facing :: Leader -> Coordinate -> Bool
facing ((mef,mep),dir) (tgf,tgp) =
  mef == tgf && ((mep <= tgp && dir == RIGHT) || (mep >= tgp && dir == LEFT))

action :: Leader -> Android Action
action leader = do
  exit <- asks _exit
  elevators <- asks _elevators
  blockers <- gets _blockers
  leaderAge <- gets _leaderAge
  let doWait = leaderAge <= 0 || any (leader `facing`) (exit:elevators ++ blockers)
  return $ bool BLOCK WAIT doWait

debug :: (MonadIO m, Show a) => a -> m ()
debug = liftIO . hPrint stderr
