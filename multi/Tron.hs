{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -XArrows #-}
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.Trans.State.Lazy
import Data.Array
import Data.Bool
import Data.List (unfoldr, (\\))
import Data.Maybe
import Data.Traversable
import System.Random
import System.IO

import Prelude hiding (id, (.), iterate)

data Automaton i o = forall s. Automaton s (i -> s -> (o,s))

instance Category Automaton where
  id = Automaton () (,)
  Automaton init2 step2 . Automaton init1 step1 =
    Automaton (init1,init2) $
      \a (s1,s2) ->
        let (b,s1') = step1 a s1
            (c,s2') = step2 b s2
        in  (c,(s1',s2'))

instance Arrow Automaton where
  arr f = Automaton () $ \i s -> (f i,s)
  first (Automaton init step) =
    Automaton init $
      \(i,x) s ->
        let (o,s') = step i s
        in  ((o,x),s')

instance ArrowChoice Automaton where
  left (Automaton init step) =
    Automaton init $
      \e s -> case e of
        Left  i -> first Left (step i s)
        Right x -> (Right x,s)

instance ArrowLoop Automaton where
  loop (Automaton init step) =
    Automaton init $
      \i s ->
        let ((o,x),s') = step (i,x) s
        in  (o,s')

instance Functor (Automaton i) where
  fmap = (^<<)

instance Applicative (Automaton i) where
  pure x = arr (const x)
  aut1 <*> aut2 = aut1 &&& aut2 >>> arr (uncurry ($))

runAutomaton :: Automaton i o -> [i] -> [o]
runAutomaton (Automaton init step) inputs = evalState (traverse (state . step) inputs) init

mkAutomaton :: s -> (i -> s -> (o,s)) -> Automaton i o
mkAutomaton = Automaton
  -- proc input -> do
  --   rec let (output,state) = step input stateD
  --       stateD <- delay init -< state
  --   returnA -< output

copy :: Automaton o o
copy = id

delay :: o -> Automaton o o
delay init = Automaton init (\i s -> (s,i))

output :: [o] -> Automaton i o
output os =
  mkAutomaton os (\_ (o:os') -> (o,os'))

accumulate :: o -> (i -> o -> o) -> Automaton i o
accumulate acc0 fun =
  proc i -> do
    rec let o = fun i oD
        oD <- delay acc0 -< o
    returnA -< o

accumulateD :: o -> (i -> o -> o) -> Automaton i o
accumulateD acc0 fun = accumulate acc0 fun >>> delay acc0

iterate :: o -> (o -> o) -> Automaton i o
iterate x0 fun = accumulateD x0 (const fun)

--mapA :: Automaton i [x] -> Automaton x y -> Automaton i [y]

clock :: Automaton i Int
clock = accumulateD 0 (const succ)

type Location = (Int,Int)

type Player = Int

data Event = Event
  { _me   :: Player
  , _locs :: Array Player (Location,Location)
  }
  deriving (Show)

data Direction = UP | DOWN | LEFT | RIGHT
  deriving (Show, Eq, Ord, Enum, Bounded)

type TronAI = Automaton Event Direction

execTronAI :: TronAI -> IO ()
execTronAI ai = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  events <- parseEvents <$> getContents
  mapM_ print $ runAutomaton ai events

main :: IO ()
main = execTronAI firstAI

parseEvents :: String -> [Event]
parseEvents = unfoldr (Just . splitEvent) . lines
  where
    splitEvent :: [String] -> (Event,[String])
    splitEvent (ln:rest) =
      let ints        = map read . words
          [n,me]      = ints ln
          (lns,rest') = splitAt n rest
          locs        = listArray (0,n-1) $ map ((\[x0,y0,x1,y1] -> ((x0,y0),(x1,y1))) . ints) lns
      in  (Event { _me = me, _locs = locs },rest')

-- Real AI starts here.

type PlayerSet = [Int]

type LocationTable = [(Player,Location)]

me :: Automaton Event Player
me = arr _me

myLocation :: Automaton Event Location
myLocation = snd <$> ((!) <$> arr _locs <*> me)

locations, tails :: Automaton Event LocationTable
locations = arr $ \event -> [ (pl,(x,y))| (pl,(_,(x,y))) <- assocs (_locs event), x >= 0 && y >= 0 ]
tails     = arr $ \event -> [ (pl,(x,y))| (pl,((x,y),_)) <- assocs (_locs event), x >= 0 && y >= 0 ]

living :: Automaton LocationTable PlayerSet
living = arr $ map fst

died :: Automaton PlayerSet PlayerSet
died = (\\) <$> delay [] <*> copy

type World = Array Location (Maybe Player)

world :: Automaton LocationTable World
world = copy &&& (living >>> died) >>> accumulate empty update
  where
    empty = listArray ((0,0),(29,19)) $ repeat Nothing
    update (locs,dths) wld =
      let updLocs = [ (loc,Just pl) | (pl,loc) <- locs ]
          updDths = [ (loc,Nothing) | (loc,Just pl) <- assocs wld, pl `elem` dths ]
      in  wld // (updLocs ++ updDths)

isFree :: Arrow a => a (Location,World) Bool
isFree = arr $ \(loc,brd) -> inRange (bounds brd) loc && isNothing (brd ! loc)

move :: Location -> Direction -> Location
move(x,y) dir = case dir of
  UP    -> (x  ,y-1)
  DOWN  -> (x  ,y+1)
  LEFT  -> (x-1,y  )
  RIGHT -> (x+1,y  )

moveA :: Automaton (Location,Direction) Location
moveA = arr $ uncurry move

blocked :: Automaton ((Location,Direction),World) Bool
blocked = first moveA >>> isFree

checkMove :: Automaton (Direction,(Location,World)) (Maybe Direction)
checkMove =
  proc (dir,(loc,wld)) -> do
    isf <- isFree -< (loc `move` dir,wld)
    returnA -< if isf then Just dir else Nothing

bestMove :: Automaton (Location,World) Direction
bestMove =
  proc loc_wld -> do
    rec moveD <- delay DOWN -< move
        dirs <- for [0 .. 3] (\n -> checkMove <<< first (rotate n)) -< (moveD,loc_wld)
        let move = head (catMaybes dirs)
    returnA -< move

firstAI :: TronAI
firstAI = myLocation &&& (((++) <$> locations <*> tails) >>> world) >>> bestMove

rotate :: (Enum o, Bounded o) => Int -> Automaton o o
rotate n = arr $ \o -> toEnum $ (fromEnum o + n) `mod` (fromEnum (maxBound `as` o) + 1)
  where
    as :: a -> a -> a
    as = const
