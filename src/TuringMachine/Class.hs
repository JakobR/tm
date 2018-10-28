{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module TuringMachine.Class
  ( TuringMachine(..)
  , SimulationResult(..)
  , SomeTuringMachine(..)
  , PrintableTMState(..)
  , PrintableTMSymbol(..)
  ) where


data SimulationResult
  = Accept
  | Reject
  | Loop
  | Timeout
  deriving (Show)

class TuringMachine (tm :: * -> * -> *) where
  type Configuration tm state symbol :: *

  -- | High-level TM simulation.
  -- In addition to the simulation result, returns a trace of configurations.
  simulate
    :: (Ord state, Ord symbol)
    => Int               -- ^ maximum number of steps to simulate
    -> tm state symbol   -- ^ the Turing Machine to simulate
    -> [symbol]          -- ^ the input word
    -> (SimulationResult, [Configuration tm state symbol])

  -- | Print the given configuration to stdout
  printConfiguration
    :: (PrintableTMState state, PrintableTMSymbol symbol)
    => tm state symbol
    -> Configuration tm state symbol
    -> IO ()

class PrintableTMState state where
  putState :: state -> IO ()

class PrintableTMSymbol symbol where
  putSymbol :: symbol -> IO ()
  putSymbols :: [symbol] -> IO ()

data SomeTuringMachine state symbol = forall tm . TuringMachine tm => SomeTuringMachine (tm state symbol)
