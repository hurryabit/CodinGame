{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -XArrows #-}
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.Trans.State.Lazy
import Data.Array
import Data.List
import Data.Maybe
import Data.Traversable
import System.Random
import System.IO

import Prelude hiding (id, (.))

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

copy :: Automaton o o
copy = id

delay :: o -> Automaton o o
delay init =
    mkAutomaton init (\i s -> (s,i))

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

clock :: Automaton i Int
clock = accumulateD 0 (const succ)

type Location = (Int,Int)

type Player = Int

data RawInput = RawInput
  { rawMe   :: Player
  , rawLocs :: Array Player Location
  }
  deriving (Show)

data Direction = UP | DOWN | LEFT | RIGHT
  deriving (Show, Eq, Ord, Enum, Bounded)

type TronAI = Player -> Automaton RawInput Direction

execTronAI :: TronAI -> IO ()
execTronAI ai = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  inputs <- rawInputs <$> getContents
  mapM_ print $ runAutomaton (ai $ rawMe $ head inputs) inputs

main :: IO ()
main = execTronAI firstAI

rawInputs :: String -> [RawInput]
rawInputs = unfoldr (Just . splitRawInput) . lines
  where
    splitRawInput :: [String] -> (RawInput,[String])
    splitRawInput (ln:rest) =
      let ints        = map read . words
          [n,me]      = ints ln
          (lns,rest') = splitAt n rest
          locs        = listArray (0,n-1) $ map ((\[_,_,x,y] -> (x,y)) . ints) lns
      in  (RawInput { rawMe = me, rawLocs = locs },rest')

-- Real AI starts here.

data Input = Input
  { moves  :: [(Player,Location)]
  , deaths :: [Player]
  }
  deriving (Show)

cookInput :: Automaton RawInput Input
cookInput = diff <$> (Just ^>> delay Nothing) <*> copy
  where
    diff :: Maybe RawInput -> RawInput -> Input
    diff mold new =
      let newLocs           = assocs (rawLocs new)
          alive (x,y)       = x >= 0 && y >= 0
          died old (pl,loc) = not (alive loc) && alive (rawLocs old ! pl)
      in  Input
            { moves  = filter (alive . snd) newLocs
            , deaths = maybe [] (\old -> map fst $ filter (died old) newLocs) mold
            }

type Board = Array Location Player

emptyBoard :: Board
emptyBoard = listArray ((0,0),(29,19)) $ repeat (-1)

updateBoard :: Input -> Board -> Board
updateBoard input board =
  let updateMoves  = [ (loc,pl) | (pl,loc) <- moves input ]
      updateDeaths = [ (loc,-1) | (loc,pl) <- assocs board, pl `elem` deaths input ]
  in  board // (updateMoves ++ updateDeaths)

isFree :: Board -> Location -> Bool
isFree board loc = inRange (bounds board) loc && board ! loc < 0

move :: Location -> Direction -> Location
move (x,y) dir = case dir of
  UP    -> (x  ,y-1)
  DOWN  -> (x  ,y+1)
  LEFT  -> (x-1,y  )
  RIGHT -> (x+1,y  )

firstAI :: TronAI
firstAI me =
  proc rawInput -> do
    input <- cookInput -< rawInput
    board <- accumulateD emptyBoard updateBoard -< input
    let Just pos = lookup me (moves input)
        Just dir = find (isFree board . move pos) [minBound .. maxBound]
    returnA -< dir
