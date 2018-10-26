{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- base
import Control.Monad ( forM_, mapM_ )

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

-- text
import Data.Text ( Text )
import qualified Data.Text as Text

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
  deriving (Eq, Ord, Show)

tmBJ :: TM State Symbol
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
    -- trimBlanks = trimBlanks3 tmBlankSymbol
    trimBlanks = id  -- NOTE: uncomment this to disable blank trimming

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

simulate :: (Ord state, Ord symbol) => Int -> TM state symbol -> [symbol] -> (SimulationResult, [Configuration state symbol])
simulate maxSteps tm@TM{..} inputWord =
  let initialCf = Configuration { cfLeftTape = []
                                , cfState = tmInitialState
                                , cfRightTape = inputWord
                                }
      (result', trace) = runWriter $ runExceptT $ simulate' maxSteps mempty tm initialCf
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


printConfiguration :: TM State Symbol -> Configuration State Symbol -> IO ()
printConfiguration TM{..} Configuration{..} = do
  putStr $ Text.unpack cfState
  putStr ":\t"
  putStr cfLeftTape
  setSGR [ SetUnderlining SingleUnderline
         , SetConsoleIntensity BoldIntensity
         , SetColor Foreground Dull Yellow ]
  putChar (headDef tmBlankSymbol cfRightTape)
  setSGR []
  putStr (tailSafe cfRightTape)
  putChar '\n'


runSimulation :: TM State Symbol -> [Symbol] -> Bool -> IO ()
runSimulation tm input shouldAccept = do
  let (result, trace) = simulate 10000 tm input
  case result of
    Accept -> if shouldAccept
              then putStrLn $ "Success: \t" <> input
              else do putStrLn $ "Error (accepted but should not accept): " <> input
                      mapM_ (printConfiguration tm) trace
                      putChar '\n'
    Timeout -> putStrLn $ "Timeout: " <> input
    _ -> if shouldAccept
         then do putStrLn $ "Error (" <> show result <> " but should accept): " <> input
                 mapM_ (printConfiguration tm) trace
                 putChar '\n'
         else putStrLn $ "Success: \t" <> input

testTM :: TM State Symbol -> IO ()
testTM tm =
  forM_ [1..16] $ \(i :: Int) -> do
    let word = replicate i 'a'
    runSimulation tm word (i `elem` [ 2^n | (n :: Int) <- [0..10] ])

main :: IO ()
main = do
  -- runSimulation tmBJ "a" True
  testTM tmBJ
  -- let initialCf = Configuration { cfLeftTape = []
  --                               , cfState = tmInitialState tmBJ
  --                               , cfRightTape = "a"
  --                               }
  -- let (result, trace) = runWriter $ runExceptT $ simulate' 10000 mempty tmBJ initialCf
  -- forM_ trace $ \cf ->
  --   printConfiguration tmBJ cf
  -- print result
