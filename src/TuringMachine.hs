{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module TuringMachine
  ( Movement(..)
  , TM(..)
  , Configuration(..)
  , initialConfiguration
  , step
  , simulate
  , SimulationResult(..)
  , simulate'
  , SimulationResult'(..)
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



-- | A standard Turing machine with one tape.
data TM state symbol = TM
  { tmInitialState :: state
  , tmFinalState :: state
  , tmTransitions :: Map (state, symbol) (state, symbol, Movement)
  , tmBlankSymbol :: symbol
  }

data Configuration state symbol = Configuration
  { cfState :: state         -- ^ current state
  , cfTape :: Tape symbol    -- ^ the tape
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
  (nextState, symbolToWrite, movement) <- Map.lookup (cfState, readTape cfTape) tmTransitions
  return Configuration { cfState = nextState
                       , cfTape = writeAndMoveTape symbolToWrite movement cfTape
                       }

initialConfiguration
  :: TM state symbol
  -> [symbol]           -- ^ the input word
  -> Configuration state symbol
initialConfiguration TM{..} inputWord =
  Configuration { cfState = tmInitialState
                , cfTape = mkTape tmBlankSymbol inputWord
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
  => Int  -- ^ the maximum number of steps to simulate
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
  putTape cfTape
  putChar '\n'
