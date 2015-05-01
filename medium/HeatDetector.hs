{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -XArrows #-}

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.Trans.State.Lazy
import Data.Binary
import Data.Bool
import Data.Maybe
import Data.Traversable
import Debug.Trace
import System.IO

import Prelude hiding (id, (.), iterate)

data Automaton i o = forall s. Binary s => Automaton s (i -> s -> (o,s))

instance Category Automaton where
  id = Automaton () (,)
  Automaton init2 step2 . Automaton init1 step1 =
    Automaton (init1,init2) $ \a (s1,s2) ->
      let (b,s1') = step1 a s1
          (c,s2') = step2 b s2
      in  (c,(s1',s2'))

instance Arrow Automaton where
  arr f = Automaton () $ \i s -> (f i,s)
  first (Automaton init step) =
    Automaton init $ \(i,x) s ->
      let (o,s') = step i s
      in  ((o,x),s')

instance ArrowChoice Automaton where
  left (Automaton init step) =
    Automaton init $ \e s ->
      case e of
        Left  i -> first Left (step i s)
        Right x -> (Right x,s)

instance ArrowLoop Automaton where
  loop (Automaton init step) =
    Automaton init $ \i s ->
      let ((o,x),s') = step (i,x) s
      in  (o,s')

instance Functor (Automaton i) where
  fmap = (^<<)

instance Applicative (Automaton i) where
  pure x = arr (const x)
  aut1 <*> aut2 = aut1 &&& aut2 >>> arr (uncurry ($))

runAutomaton :: Automaton i o -> [i] -> [o]
runAutomaton (Automaton init step) inputs = evalState (traverse (state . step) inputs) init

type Parser s i = String -> (s,[i])
type Printer o = o -> String

execAutomaton :: Show d => Parser s i -> Printer o -> Automaton (s,i) (o,d) -> IO ()
execAutomaton parser printer aut = do
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout LineBuffering
  (s,is) <- parser <$> getContents
  forM_ (runAutomaton aut $ map ((,) s) is) $ \(o,d) ->
    do  putStrLn (printer o)
        traceIO (show d)


mkAutomaton :: Binary s => s -> (i -> s -> (o,s)) -> Automaton i o
mkAutomaton = Automaton
  -- proc input -> do
  --   rec let (output,state) = step input stateD
  --       stateD <- delay init -< state
  --   returnA -< output

copy :: Automaton o o
copy = id

delay :: Binary o => o -> Automaton o o
delay init =
  Automaton init (\i s -> (s,i))

delayFrom :: Binary o => Automaton (o,o) o
delayFrom =
  Automaton Nothing $ \(i1,i2) s ->
    (fromMaybe i1 s,Just i2)

accum0 :: Binary o => o -> (i -> o -> o) -> Automaton i o
accum0 acc0 fun = proc i ->
  do  rec let o = fun i oD
          oD <- delay acc0 -< o
      returnA -< o

accum1 :: Binary o => o -> (i -> o -> o) -> Automaton i o
accum1 acc0 fun = accum0 acc0 fun >>> delay acc0

iterate :: Binary o => o -> (o -> o) -> Automaton i o
iterate x0 fun =
  accum1 x0 (const fun)

clock :: Automaton i Int
clock =
  accum1 0 (const succ)

data Notify a = Blip a | Mute
  deriving (Eq, Show)

isBlip :: Notify a -> Bool
isBlip rad =
  case rad of
    Blip _ -> True
    Mute   -> False

type Interval i o = Automaton i (Maybe o)

checked :: Bool -> a -> Maybe a
checked = bool (const Nothing) Just

onFor :: Int -> Interval o o
onFor n =
  onFor' n id

onFor' :: Int -> Automaton i o -> Interval i o
--onFor n aut = iterate n pred &&& aut >>^ (\(k,o) -> if k > 0 then On o else Off)
onFor' n (Automaton init1 step1)
  | n <= 0    =
      pure Nothing
  | otherwise =
      Automaton (Just (init1,n)) $ \i s ->
        case s of
          Just (s1,k) ->
            let (o,s1') = step1 i s1
            in  (Just o,checked (k>1) (s1',pred k))
          Nothing ->
            (Nothing,Nothing)

mergeL :: Interval i o -> Interval i o -> Interval i o
mergeL (Automaton init1 step1) (Automaton init2 step2) =
  Automaton (init1,init2) $ \i (s1,s2) ->
    let (o1,s1') = step1 i s1
        (o2,s2') = step2 i s2
    in  (o1 <|> o2,(s1',s2'))

cnt :: Automaton Int Int
cnt = proc input ->
  do  rec now <- delayFrom -< (input,now+1)
      returnA -< now

switch0 :: Automaton i o -> Automaton i o -> Automaton (i,Notify b) o
switch0 (Automaton init1 step1) (Automaton init2 step2) =
  Automaton (init1,init2,True) $ \(i,r) (s1,s2,t) ->
    let t' = isBlip r /= t -- /= is XOR
    in  case t' of
          True ->
            let (o,s1') = step1 i s1
            in (o,(s1',s2,t'))
          False ->
            let (o,s2') = step2 i s2
            in (o,(s1,s2',t'))

reset0 :: Automaton i o -> Automaton (i,Notify b) o
reset0 (Automaton init step) =
  Automaton init $ \(i,n) s ->
    step i (if isBlip n then init else s)

reset1 :: Automaton i o -> Automaton (i,Notify b) o
reset1 (Automaton init step) =
    Automaton init $ \(i,n) s ->
      let (o,s') = step i s
      in  (o,if isBlip n then init else s')

every :: Int -> o -> Automaton i (Notify o)
every n o =
  Automaton (n-1) $ \_ k ->
    if k == 0
      then (Blip o,n-1)
      else (Mute  ,k-1)

--------------------------------------------------------------------------------
--                                                                            --
--     P R O B L E M   S P E C I F I C   C O D E   S T A R T S   H E R E      --
--                                                                            --
--------------------------------------------------------------------------------

data Direction = U | UR | R | DR | D | DL | L | UL
  deriving (Show, Read, Eq, Ord, Enum)

isUp, isRight, isDown, isLeft :: Direction -> Bool
isUp    d = d <= UR || d == UL
isRight d = UR <= d && d <= DR
isDown  d = DR <= d && d <= DL
isLeft  d = DL <= d

type Position = (Int,Int)

heatDetector :: Automaton ((Position,Position),Direction) (Position,())
heatDetector = proc (((w,h),pos0),dir) ->
  do  rec (x0,y0) <- delayFrom   -< (pos0,(x0',y0'))
          (x1,y1) <- delay (0,0) -< (x1',y1')
          (x2,y2) <- delayFrom   -< ((w-1,h-1),(x2',y2'))
          let (x1',x2')
                | isLeft  dir = (x1,x0-1)
                | isRight dir = (x0+1,x2)
                | otherwise   = (x0,x0)
              (y1',y2')
                | isUp    dir = (y1,y0-1)
                | isDown  dir = (y0+1,y2)
                | otherwise   = (y0,y0)
              x0' = (x1'+x2') `div` 2
              y0' = (y1'+y2') `div` 2
      returnA -< ((x0',y0'),())

main :: IO ()
main = execAutomaton parser printer heatDetector
  where
    parser :: Parser (Position,Position) Direction
    parser input =
      let wh:_:x0y0:ds = lines input
          [w,h,x0,y0] = map read $ words wh ++ words x0y0
      in  (((w,h),(x0,y0)),map read ds)
    printer :: Printer Position
    printer (x,y) = unwords $ map show [x,y]
