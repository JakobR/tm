{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module TuringMachine.TwoTapes
  ( Movement(..)
  , TM(..)
  , Configuration(..)
  , initialConfiguration
  , step
  , simulate'
  , SimulationResult'(..)
  , simulate
  , SimulationResult(..)
  ) where

-- containers
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Set ( Set )
import qualified Data.Set as Set

-- mtl
import Control.Monad.Writer ( MonadWriter(..), runWriter )
import Control.Monad.Except ( MonadError(..), runExceptT )

-- tm
import TuringMachine.Class ( SimulationResult(..), PrintableTMState(..), PrintableTMSymbol(..) )
import qualified TuringMachine.Class
import TuringMachine.Tape



-- | Turing machine with two tapes:
-- an immutable input tape, and a mutable work tape.
data TM state symbol = TM
  { tmInitialState :: state
  , tmFinalStates :: Set state
  , tmTransitions :: Map (state, symbol, symbol) (state, symbol, Movement, Movement)
  , tmBlankSymbol :: symbol
  , tmWorkTapeLeftBoundary :: symbol  -- Z0
  , tmInputTapeLeftBoundary :: symbol  -- Z1
  , tmInputTapeRightBoundary :: symbol  -- Z2
  }

data Configuration state symbol = Configuration
  { cfState :: state              -- ^ current state
  , cfInputTape :: Tape symbol    -- ^ immutable input tape
  , cfWorkTape :: Tape symbol     -- ^ mutable work tape
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
  (nextState, symbolToWrite, inputMovement, workMovement) <- Map.lookup (cfState, readTape cfInputTape, readTape cfWorkTape) tmTransitions
  return Configuration { cfState = nextState
                       , cfInputTape = moveTape inputMovement cfInputTape
                       , cfWorkTape = writeAndMoveTape symbolToWrite workMovement cfWorkTape
                       }

initialConfiguration
  :: Eq symbol
  => TM state symbol
  -> [symbol]           -- ^ the input word
  -> Configuration state symbol
initialConfiguration TM{..} inputWord =
  -- Note that we use `moveTape R` because the read/write head should start on
  -- the first symbol of the actual content, not on the boundary symbols
  Configuration { cfState = tmInitialState
                , cfInputTape = moveTape R $ mkTape tmBlankSymbol ([tmInputTapeLeftBoundary]
                                                                   <> inputWord
                                                                   <> [tmInputTapeRightBoundary])
                , cfWorkTape = moveTape R $ mkTape tmBlankSymbol [tmWorkTapeLeftBoundary]
                }


data SimulationResult'
  = Accept'
  | Reject'
  | Loop'
  deriving Show

-- | Low-level resumable TM simulation.
-- Error means timeout here.
-- The writer accumulates a trace of configurations.
simulate'
  :: ( Ord state
     , Ord symbol
     , MonadWriter [Configuration state symbol] m
     , MonadError (Configuration state symbol) m
     )
  => Int   -- ^ the maximum number of steps to simulate
  -> Set (Configuration state symbol)   -- ^ already seen configuration; if we encounter such a configuration again we report an infinite loop
  -> TM state symbol
  -> Configuration state symbol
  -> m SimulationResult'
simulate' 0 _ _ cf = throwError cf
simulate' maxSteps seenCfs tm@TM{..} cf@Configuration{..} = do
  tell [cf]
  case step tm cf of
    Nothing -> return $ if cfState `Set.member` tmFinalStates then Accept' else Reject'
    Just newCf ->
      if newCf `Set.member` seenCfs
      then return Loop'
      else simulate' (maxSteps - 1) (newCf `Set.insert` seenCfs) tm newCf
      -- TODO: Detect loop where machine just runs off the end of the tape (only encountering blanks)


-- | High-level TM simulation
simulate
  :: (Ord state, Ord symbol)
  => Int  -- ^ the maximum number of steps to simulate
  -> TM state symbol
  -> [symbol]
  -> (SimulationResult, [Configuration state symbol])
simulate maxSteps tm@TM{..} inputWord =
  let initialCf = initialConfiguration tm inputWord
      (result', trace) = runWriter $ runExceptT $ simulate' maxSteps (Set.singleton initialCf) tm initialCf
      result = case result' of
        Left _ -> Timeout
        Right Accept' -> Accept
        Right Reject' -> Reject
        Right Loop' -> Loop
  in (result, trace)


printConfiguration
    :: (PrintableTMState state, PrintableTMSymbol symbol)
    => TM state symbol
    -> Configuration state symbol
    -> IO ()
printConfiguration TM{..} Configuration{..} = do
  putState cfState
  putStr ":\t"
  putTape cfInputTape
  putChar '\t'
  putTape cfWorkTape
  putChar '\n'
