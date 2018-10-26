{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- containers
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Set ( Set )
import qualified Data.Set as Set

-- text
import Data.Text ( Text )

-- safe
import Safe ( headDef, tailSafe )


type State = Text
type Symbol = Char
data Movement = L | S | R
-- data Transition state symbol = Transition
--   { tCurState :: state
--   , tCurSymbol :: symbol
--   , tMovement :: Movement
--   , tNextState :: state
--   , tNextSymbol :: symbol
--   }

data TM state symbol = TM
  { tmInitialState :: state
  , tmFinalState :: state
  , tmTransitions :: Map (state, symbol) (state, symbol, Movement)
  , tmBlankSymbol :: symbol
  }

data Configuration state symbol = Configuration
  { cfLeftTape :: [symbol]   -- ^ what's to the left of the reading head (first symbol of cfLeftTape is the one next to the reading head, i.e., the left part is stored in reverse order)
  , cfState :: state         -- ^ current state
  , cfRightTape :: [symbol]  -- ^ what's to the right of the reading head (reading head points to first symbol of cfRightTape)
  }
  deriving (Eq, Ord)

tmBJ :: TM Text Char
tmBJ = TM{..}
  where
    tmInitialState = "q1"
    tmFinalState = "q6"
    tmBlankSymbol = 'B'
    tmTransitions = Map.fromList
      [ ("q1",'a') `to` ("q2",'B',R)
      , ("q2",'a') `to` ("q3",'X',R)
      , ("q3",'a') `to` ("q4",'a',R)
      , ("q4",'a') `to` ("q3",'X',R)
      , ("q5",'a') `to` ("q5",'a',L)
      , ("q2",'X') `to` ("q2",'X',R)
      , ("q3",'X') `to` ("q3",'X',R)
      , ("q4",'X') `to` ("q4",'X',R)
      , ("q5",'X') `to` ("q5",'X',L)
      , ("q2",'B') `to` ("q6",'B',R)
      , ("q3",'B') `to` ("q5",'B',L)
      , ("q5",'B') `to` ("q2",'B',R)
      ]
    -- t :: Text -> Char -> Text -> Char -> Movement -> ((Text, Char), (Text, Char, Movement))
    -- t st sy st' sy' mv = ((st,sy),(st',sy',mv))
    to = (,)

-- | Simulate one step of the given Turing Machine.
-- Returns `Nothing` if no transition exists for the current configuration.
step :: (Ord state, Ord symbol)
     => TM state symbol
     -> Configuration state symbol
     -> Maybe (Configuration state symbol)
step TM{..} Configuration{..} = do
  let curSymbol = headDef tmBlankSymbol cfRightTape
  (nextState, writeSymbol, movement) <- Map.lookup (cfState, curSymbol) tmTransitions
  return $ case movement of
    L -> Configuration { cfLeftTape = tailSafe cfLeftTape
                       , cfState = nextState
                       , cfRightTape = trimBlanks3 tmBlankSymbol $ headDef tmBlankSymbol cfLeftTape : writeSymbol : tailSafe cfRightTape
                       }
    S -> Configuration { cfLeftTape = cfLeftTape
                       , cfState = nextState
                       , cfRightTape = trimBlanks3 tmBlankSymbol $ writeSymbol : tailSafe cfRightTape
                       }
    R -> Configuration { cfLeftTape = trimBlanks3 tmBlankSymbol $ writeSymbol : cfLeftTape
                       , cfState = nextState
                       , cfRightTape = tailSafe cfRightTape
                       }

-- | removes unneeded blank symbols at the border of the tape (only checks three symbols, which should be enough when it's done after every step)
trimBlanks3 :: Eq symbol => symbol -> [symbol] -> [symbol]
trimBlanks3 b [x]
  | x == b = []
trimBlanks3 b [x, y]
  | y == b = trimBlanks3 b [x]
trimBlanks3 b [x, y, z]
  | z == b = trimBlanks3 b [x, y]
trimBlanks3 _ xs = xs

data SimulationResult -- state symbol
  = Accept
  | Reject
  | Loop
  | Timeout -- (Configuration state symbol)
  deriving (Show)

simulate :: (Ord state, Ord symbol) => Int -> TM state symbol -> [symbol] -> SimulationResult
simulate maxSteps tm@TM{..} inputWord =
  let initialCf = Configuration { cfLeftTape = []
                                , cfState = tmInitialState
                                , cfRightTape = inputWord
                                }
  in case simulate' maxSteps mempty tm initialCf of
    Left _ -> Timeout
    Right result -> result

-- Left means timeout, Right means termination (or loop detected)
simulate' :: (Ord state, Ord symbol)
          => Int
          -> Set (Configuration state symbol)
          -> TM state symbol
          -> Configuration state symbol
          -> Either (Configuration state symbol) SimulationResult
simulate' 0 _ _ cf = Left cf
simulate' maxSteps seenCfs tm@TM{..} cf@Configuration{..} =
  case step tm cf of
    Nothing -> return $ if cfState == tmFinalState then Accept else Reject
    Just newCf ->
      if newCf `Set.member` seenCfs
      then return Loop
      else simulate' (maxSteps - 1) (newCf `Set.insert` seenCfs) tm newCf


main :: IO ()
main = do
  print $ simulate 10000 tmBJ "aabaaa"
