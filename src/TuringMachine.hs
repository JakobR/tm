{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module TuringMachine
  ( Movement(..)
  , TM(..)
  , Configuration(..)
  , step
  , simulate
  , SimulationResult(..)
  , simulate'
  , SimulationResult'(..)
  ) where

-- ansi-terminal
import System.Console.ANSI (
  setSGR, SGR(..), ConsoleLayer(..), Color(..), ColorIntensity(..), ConsoleIntensity(..), Underlining(..)
  )

-- containers
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Set ( Set )
import qualified Data.Set as Set

-- mtl
import Control.Monad.Writer ( MonadWriter(..), runWriter )
import Control.Monad.Except ( MonadError(..), runExceptT )

-- safe
import Safe ( headDef, tailSafe )

-- tm
import TuringMachine.Class ( SimulationResult(..), PrintableTMState(..), PrintableTMSymbol(..) )
import qualified TuringMachine.Class
import TuringMachine.Tape ( Movement(..) )


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
  deriving (Eq, Ord, Show)

instance TuringMachine.Class.TuringMachine TM where
  type Configuration TM state symbol = Configuration state symbol
  simulate = simulate
  printConfiguration = printConfiguration


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
                       , cfRightTape = trimBlanks $ headDef tmBlankSymbol cfLeftTape : writeSymbol : tailSafe cfRightTape
                       }
    S -> Configuration { cfLeftTape = cfLeftTape
                       , cfState = nextState
                       , cfRightTape = trimBlanks $ writeSymbol : tailSafe cfRightTape
                       }
    R -> Configuration { cfLeftTape = trimBlanks $ writeSymbol : cfLeftTape
                       , cfState = nextState
                       , cfRightTape = tailSafe cfRightTape
                       }
  where
    -- NOTE: Loop detection works better if we normalize the configurations
    doTrimBlanks = True
    trimBlanks = if doTrimBlanks then trimBlanks3 tmBlankSymbol else id

-- | removes unneeded blank symbols at the border of the tape (only checks three symbols, which should be enough when it's done after every step)
trimBlanks3 :: Eq symbol => symbol -> [symbol] -> [symbol]
trimBlanks3 b [x]
  | x == b = []
trimBlanks3 b [x, y]
  | y == b = trimBlanks3 b [x]
trimBlanks3 b [x, y, z]
  | z == b = trimBlanks3 b [x, y]
trimBlanks3 _ xs = xs

simulate :: (Ord state, Ord symbol) => Int -> TM state symbol -> [symbol] -> (SimulationResult, [Configuration state symbol])
simulate maxSteps tm@TM{..} inputWord =
  let initialCf = Configuration { cfLeftTape = []
                                , cfState = tmInitialState
                                , cfRightTape = inputWord
                                }
      (result', trace) = runWriter $ runExceptT $ simulate' maxSteps (Set.singleton initialCf) tm initialCf
      result = case result' of
        Left _ -> Timeout
        Right Accept' -> Accept
        Right Reject' -> Reject
        Right Loop' -> Loop
  in (result, trace)

data SimulationResult'
  = Accept'
  | Reject'
  | Loop'
  deriving Show

-- Error means timeout here.
-- The writer accumulates a trace of configurations.
simulate' :: (Ord state, Ord symbol, MonadWriter [Configuration state symbol] m, MonadError (Configuration state symbol) m)
          => Int
          -> Set (Configuration state symbol)
          -> TM state symbol
          -> Configuration state symbol
          -> m SimulationResult'
simulate' 0 _ _ cf = throwError cf
simulate' maxSteps seenCfs tm@TM{..} cf@Configuration{..} = do
  tell [cf]
  case step tm cf of
    Nothing -> return $ if cfState == tmFinalState then Accept' else Reject'
    Just newCf ->
      if newCf `Set.member` seenCfs
      then return Loop'
      else simulate' (maxSteps - 1) (newCf `Set.insert` seenCfs) tm newCf
      -- TODO: Detect loop where machine just runs off the end of the tape (only encountering blanks)


printConfiguration
    :: (PrintableTMState state, PrintableTMSymbol symbol)
    => TM state symbol
    -> Configuration state symbol
    -> IO ()
printConfiguration TM{..} Configuration{..} = do
  putState cfState
  putStr ":\t"
  putSymbols $ reverse cfLeftTape
  setSGR [ SetUnderlining SingleUnderline
         , SetConsoleIntensity BoldIntensity
         , SetColor Foreground Dull Yellow ]
  putSymbol (headDef tmBlankSymbol cfRightTape)
  setSGR []
  putSymbols (tailSafe cfRightTape)
  putChar '\n'
