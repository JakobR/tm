{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module TuringMachine.Examples where

-- containers
import qualified Data.Map as Map
import qualified Data.Set as Set

-- text
import Data.Text ( Text )
import qualified Data.Text as Text

-- tm
import TuringMachine
import TuringMachine.Class
import qualified TuringMachine.TwoTapes as TwoTapes


type State = Text
type Symbol = Char

instance PrintableTMState State where
  putState = putStr . Text.unpack

instance PrintableTMSymbol Symbol where
  putSymbol = putChar
  putSymbols = putStr


tmAL :: TM State Symbol
tmAL = TM{..}
  where
    tmInitialState = "q0"
    tmFinalState = "qe"
    tmBlankSymbol = 'B'
    tmTransitions = Map.fromList
      [ ("q0",'a') `to` ("q1",'C',R)  -- correct if we replace C by B
      , ("q1",'a') `to` ("q2",'X',R)
      , ("q2",'a') `to` ("q3",'a',R)
      , ("q3",'a') `to` ("q2",'X',R)
      , ("q4",'a') `to` ("q4",'a',L)
      , ("q1",'B') `to` ("qe",'B',R)
      , ("q2",'B') `to` ("q4",'B',L)
      , ("q4",'B') `to` ("q1",'B',R)
      , ("q1",'X') `to` ("q1",'X',R)
      , ("q2",'X') `to` ("q2",'X',R)
      , ("q3",'X') `to` ("q3",'X',R)
      , ("q4",'X') `to` ("q4",'X',L)
      ]

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

tmBD :: TM State Symbol
tmBD = TM{..}
  where
    tmInitialState = "q0"
    tmFinalState = "q5"
    tmBlankSymbol = 'B'
    tmTransitions = Map.fromList
      [ ("q0",'a') `to` ("q1",'B',R)
      , ("q1",'a') `to` ("q2",'X',R)
      , ("q2",'a') `to` ("q3",'a',R)
      , ("q3",'a') `to` ("q2",'X',R)
      , ("q4",'a') `to` ("q4",'a',L)
      , ("q1",'B') `to` ("q5",'B',R)
      , ("q2",'B') `to` ("q4",'B',L)
      , ("q4",'B') `to` ("q1",'B',R)
      , ("q1",'X') `to` ("q1",'X',R)
      , ("q2",'X') `to` ("q2",'X',R)
      , ("q3",'X') `to` ("q3",'X',R)
      , ("q4",'X') `to` ("q4",'X',L)
      ]

tmBM :: TM State Symbol
tmBM = TM{..}
  where
    tmInitialState = "start"
    tmFinalState = "accept"
    tmBlankSymbol = 'B'
    tmTransitions = Map.fromList
      [ ("revert",'a') `to` ("revert",'a',L)
      , ("start", 'a') `to` ("init",  '0',R)
      , ("init",  'a') `to` ("revert",'a',L)
      , ("wrd1cc",'a') `to` ("wrd1cb",'1',L)
      , ("wrd1ch",'a') `to` ("revert",'a',L)
      , ("wrd0cc",'a') `to` ("wrd0cb",'0',L)
      , ("wrd0ch",'a') `to` ("revert",'a',L)

      , ("revert",'0') `to` ("revert",'0',L)
      , ("start", '0') `to` ("wrd1cc",'1',R)
      , ("wrd1cc",'0') `to` ("wrd1cc",'0',R)
      , ("wrd1cb",'0') `to` ("wrd1bg",'0',L)
      , ("wrd1bg",'0') `to` ("wrd1bg",'0',L)
      , ("wrd1st",'0') `to` ("wrd1cc",'1',R)
      , ("wrd0cc",'0') `to` ("wrd0cc",'0',R)
      , ("wrd0cb",'0') `to` ("wrd0cb",'0',L)
      , ("wrd0bg",'0') `to` ("wrd0st",'0',R)
      , ("wrd0ch",'0') `to` ("wrd0ch",'0',R)

      , ("revert",'1') `to` ("revert",'1',L)
      , ("start", '1') `to` ("wrd0cc",'0',R)
      , ("wrd1cc",'1') `to` ("wrd1cc",'1',R)
      , ("wrd1cb",'1') `to` ("wrd1cb",'1',L)
      , ("wrd1bg",'1') `to` ("wrd1st",'1',R)
      , ("wrd1ch",'1') `to` ("wrd1ch",'1',R)
      , ("wrd0cc",'1') `to` ("wrd0cc",'1',R)
      , ("wrd0cb",'1') `to` ("wrd0bg",'1',L)
      , ("wrd0bg",'1') `to` ("wrd0bg",'1',L)
      , ("wrd0st",'1') `to` ("wrd0cc",'0',R)

      , ("revert",'B') `to` ("start" ,'B',R)
      , ("init",  'B') `to` ("accept",'B',L)
      , ("wrd1cb",'B') `to` ("wrd1ch",'B',R)
      , ("wrd1ch",'B') `to` ("accept",'B',L)
      , ("wrd0cb",'B') `to` ("wrd0ch",'B',R)
      , ("wrd0ch",'B') `to` ("accept",'B',L)
      ]

tmLF :: TM State Symbol
tmLF = TM{..}
  where
    tmInitialState = "q0"
    tmFinalState = "qE"
    tmBlankSymbol = 'l'
    tmTransitions = Map.fromList
      [ ("q0",'a') `to` ("q1",'B',R)
      , ("q1",'a') `to` ("q2",'B',R)
      , ("q2",'a') `to` ("q3",'a',S)
      , ("q3",'a') `to` ("q3",'a',L)
      , ("q5",'a') `to` ("q6",'a',S)
      , ("q6",'a') `to` ("q3",'C',L)
      , ("q8",'a') `to` ("q3",'a',L)

      , ("q3",'B') `to` ("q3",'B',L)
      , ("q4",'B') `to` ("q5",'X',R)
      , ("q5",'B') `to` ("q5",'B',R)

      , ("q3",'X') `to` ("q4",'X',R)
      , ("q7",'X') `to` ("q7",'X',L)
      , ("q8",'X') `to` ("q8",'B',R)

      , ("q3",'C') `to` ("q3",'C',L)
      , ("q4",'C') `to` ("q7",'C',L)
      , ("q5",'C') `to` ("q5",'C',R)
      , ("q7",'C') `to` ("q7",'X',L)
      , ("q8",'C') `to` ("q8",'B',R)

      , ("q1",'l') `to` ("qE",'l',S)
      , ("q2",'l') `to` ("qE",'l',S)
      , ("q3",'l') `to` ("q4",'l',R)
      , ("q7",'l') `to` ("q8",'l',R)
      , ("q8",'l') `to` ("qE",'l',S)
      ]

tmGD :: TM State Symbol
tmGD = TM{..}
  where
    tmInitialState = "q0"
    tmFinalState = "q7"
    tmBlankSymbol = 'B'
    tmTransitions = Map.fromList
      [ ("q0",'a') `to` ("q1",'a',R)
      , ("q1",'a') `to` ("q2",'X',R)
      , ("q2",'a') `to` ("q3",'a',R)
      , ("q3",'a') `to` ("q2",'X',R)
      , ("q4",'a') `to` ("q5",'a',L)
      , ("q6",'a') `to` ("q6",'a',L)

      , ("q0",'X') `to` ("q0",'X',R)
      , ("q2",'X') `to` ("q2",'X',R)
      , ("q3",'X') `to` ("q3",'X',R)
      , ("q4",'X') `to` ("q4",'X',L)
      , ("q5",'X') `to` ("q6",'X',L)
      , ("q6",'X') `to` ("q6",'X',L)

      , ("q0",'B') `to` ("q0",'B',R)
      , ("q1",'B') `to` ("q7",'B',R)
      , ("q2",'B') `to` ("q4",'B',L)
      , ("q5",'B') `to` ("q7",'B',R)
      , ("q6",'B') `to` ("q2",'B',R)
      ]

tmHP :: TM State Symbol
tmHP = TM{..}
  where
    tmInitialState = "q0"
    tmFinalState = "qe"
    tmBlankSymbol = 'B'
    tmTransitions = Map.fromList
      [ ("q0",'a') `to` ("q1",'X',R)
      , ("q1",'a') `to` ("q2",'a',R)
      , ("q2",'a') `to` ("q3",'a',R)
      , ("q3",'a') `to` ("q2",'a',R)
      , ("q4",'a') `to` ("q5",'X',L)
      , ("q5",'a') `to` ("q6",'a',L)
      , ("q6",'a') `to` ("q3",'X',R)

      , ("q2",'X') `to` ("q4",'X',L)
      , ("q3",'X') `to` ("qe",'X',S)
      , ("q5",'X') `to` ("q6",'X',R)
      , ("q6",'X') `to` ("q7",'B',L)
      , ("q7",'X') `to` ("q7",'a',L)

      , ("q1",'B') `to` ("qe",'B',S)
      , ("q2",'B') `to` ("q4",'B',L)
      , ("q5",'B') `to` ("q6",'B',R)
      , ("q7",'B') `to` ("q2",'B',R)
      ]

tmJD :: TM State Symbol
tmJD = TM{..}
  where
    tmInitialState = "init0"
    tmFinalState = "r1"
    tmBlankSymbol = 'B'
    tmTransitions = Map.fromList
      [ ("init0",     'a') `to` ("init1",     'a',R)  -- init1 instead of init0
      , ("init1",     'a') `to` ("initEven",  'X',R)
      , ("initEven",  'a') `to` ("initUneven",'a',R)
      , ("initUneven",'a') `to` ("initEven",  'X',R)
      , ("l",         'a') `to` ("l",         'a',L)
      , ("r0",        'a') `to` ("r0",        'a',R)
      , ("r1",        'a') `to` ("r1a",       'a',R)
      , ("r1a",       'a') `to` ("r1a",       'a',R)
      , ("rUneven",   'a') `to` ("rUneven",   'a',R)
      , ("rEven",     'a') `to` ("rEven",     'a',R)

      , ("initEven",  'B') `to` ("l",         'B',L)
      , ("l",         'B') `to` ("r0",        'B',R)
      , ("rEven",     'B') `to` ("l",         'B',L)  -- l statt l0

      , ("l",         'X') `to` ("l",         'X',L)
      , ("r0",        'X') `to` ("r1",        'a',R)
      , ("r1a",       'X') `to` ("rEven",     'X',R)
      , ("rUneven",   'X') `to` ("rEven",     'X',R)
      , ("rEven",     'X') `to` ("rUneven",   'a',R)

      -- init1 should also be accepting
      , ("init1",'B') `to` ("r1",'B',S)
      , ("init1",'X') `to` ("r1",'X',S)
      ]

tmKP :: TM State Symbol
tmKP = TM{..}
  where
    tmInitialState = "q0"
    tmFinalState = "q4"
    tmBlankSymbol = 'B'
    tmTransitions = Map.fromList
      [ ("q0",'a') `to` ("q1",'X',R)
      , ("q1",'a') `to` ("q2",'X',R)
      , ("q2",'a') `to` ("q3",'X',R)
      , ("q3",'a') `to` ("q2",'X',R)

      , ("q1",'B') `to` ("q4",'B',R)
      , ("q2",'B') `to` ("q4",'B',R)
      ]

tmKN :: TM State Symbol
tmKN = TM{..}
  where
    tmInitialState = "q0"
    tmFinalState = "q8"
    tmBlankSymbol = 'B'
    tmTransitions = Map.fromList
      [ ("q0",'a') `to` ("q1",'a',R)
      , ("q1",'a') `to` ("q3",'a',L)
      , ("q3",'a') `to` ("q4",'A',R)
      , ("q4",'a') `to` ("q4",'a',R)
      , ("q5",'a') `to` ("q6",'B',L)
      , ("q6",'a') `to` ("q7",'a',L)
      , ("q7",'a') `to` ("q7",'a',L)

      , ("q1",'A') `to` ("q2",'a',L)
      , ("q2",'A') `to` ("q2",'a',L)
      , ("q6",'A') `to` ("q1",'a',L)
      , ("q7",'A') `to` ("q3",'A',R)

      , ("q1",'B') `to` ("q8",'B',S)
      , ("q2",'B') `to` ("q3",'B',R)
      , ("q4",'B') `to` ("q5",'B',L)
      , ("q7",'B') `to` ("q3",'B',R)
      ]

tmMAL :: TM State Symbol
tmMAL = TM{..}
  where
    tmInitialState = "q0"
    tmFinalState = "q4"
    tmBlankSymbol = 'B'
    tmTransitions = Map.fromList
      [ ("q0",'a') `to` ("q1",'B',R)
      , ("q1",'a') `to` ("q1",'a',R)
      , ("q2",'a') `to` ("q3",'B',L)
      , ("q3",'a') `to` ("q3",'a',L)

      , ("q0",'B') `to` ("q4",'B',S)
      , ("q1",'B') `to` ("q2",'B',L)
      , ("q3",'B') `to` ("q0",'B',R)
      ]

tmSD :: TM State Symbol
tmSD = TM{..}
  where
    tmInitialState = "q0"
    tmFinalState = "q6"
    tmBlankSymbol = 'B'
    tmTransitions = Map.fromList
      [ ("q0",'a') `to` ("q1",'B',R)
      , ("q1",'a') `to` ("q2",'X',R)
      , ("q2",'a') `to` ("q3",'a',R)
      , ("q3",'a') `to` ("q2",'X',R)
      , ("q4",'a') `to` ("q4",'a',L)

      , ("q1",'X') `to` ("q1",'X',R)
      , ("q2",'X') `to` ("q2",'X',R)
      , ("q3",'X') `to` ("q3",'X',R)
      , ("q4",'X') `to` ("q4",'X',L)

      , ("q0",'B') `to` ("q5",'B',R)
      , ("q1",'B') `to` ("q6",'B',R)
      , ("q2",'B') `to` ("q4",'B',L)
      , ("q3",'B') `to` ("q5",'B',R)
      , ("q4",'B') `to` ("q1",'B',R)
      ]

tmSS :: TM State Symbol
tmSS = TM{..}
  where
    tmInitialState = "q0"
    tmFinalState = "q5"
    tmBlankSymbol = 'B'
    tmTransitions = Map.fromList
      [ ("q0",'a') `to` ("q1",'x',R)
      , ("q1",'a') `to` ("q2",'a',R)
      , ("q2",'a') `to` ("q3",'x',R)
      , ("q3",'a') `to` ("q2",'a',R)
      , ("q4",'a') `to` ("q4",'a',L)

      , ("q0",'x') `to` ("q0",'x',R)
      -- , ("q1",'x') `to` ("q1",'x',R)  -- works if we add this
      , ("q2",'x') `to` ("q2",'x',R)
      , ("q3",'x') `to` ("q3",'x',R)
      , ("q4",'x') `to` ("q4",'x',L)

      , ("q1",'B') `to` ("q5",'B',R)
      , ("q2",'B') `to` ("q4",'B',L)
      , ("q4",'B') `to` ("q0",'B',R)
      ]

tmSLJ :: TM State Symbol
tmSLJ = TM{..}
  where
    tmInitialState = "q0"
    tmFinalState = "qP"
    tmBlankSymbol = 'B'
    tmTransitions = Map.fromList
      [ ("q0",'a') `to` ("q1",'B',R)
      , ("q1",'a') `to` ("q2",'X',R)
      , ("q2",'a') `to` ("q3",'a',R)
      , ("q3",'a') `to` ("q2",'X',R)
      , ("q4",'a') `to` ("q4",'a',L)

      , ("q0",'X') `to` ("qN",'B',R)
      , ("q1",'X') `to` ("q1",'X',R)
      , ("q2",'X') `to` ("q2",'X',R)
      , ("q3",'X') `to` ("q3",'X',R)
      , ("q4",'X') `to` ("q4",'X',L)

      , ("q0",'B') `to` ("qN",'B',R)
      , ("q1",'B') `to` ("qP",'B',R)
      , ("q2",'B') `to` ("q4",'B',L)
      , ("q3",'B') `to` ("qN",'B',R)
      , ("q4",'B') `to` ("q1",'B',R)
      ]

tmTC :: TM State Symbol
tmTC = TM{..}
  where
    tmInitialState = "q0"
    tmFinalState = "q5"
    tmBlankSymbol = 'B'
    tmTransitions = Map.fromList
      [ ("q0",'a') `to` ("q1",'Z',R)
      , ("q1",'a') `to` ("q2",'a',L)
      , ("q3",'a') `to` ("q4",'Z',S)

      , ("q1",'X') `to` ("q3",'Y',R)
      , ("q3",'X') `to` ("q3",'X',R)
      , ("q4",'X') `to` ("q4",'X',L)

      , ("q2",'Y') `to` ("q2",'X',L)
      , ("q4",'Y') `to` ("q1",'Y',R)

      , ("q1",'Z') `to` ("q1",'Z',R)
      , ("q2",'Z') `to` ("q2",'X',L)
      , ("q3",'Z') `to` ("q3",'Z',R)
      , ("q4",'Z') `to` ("q4",'Z',L)

      , ("q1",'B') `to` ("q5",'B',R)
      , ("q2",'B') `to` ("q1",'B',R)

      , ("q0",'X') `to` ("qN",'B',R)
      , ("q1",'X') `to` ("q1",'X',R)
      , ("q2",'X') `to` ("q2",'X',R)
      , ("q3",'X') `to` ("q3",'X',R)
      , ("q4",'X') `to` ("q4",'X',L)

      , ("q0",'B') `to` ("qN",'B',R)
      , ("q1",'B') `to` ("qP",'B',R)
      , ("q2",'B') `to` ("q4",'B',L)
      , ("q3",'B') `to` ("qN",'B',R)
      , ("q4",'B') `to` ("q1",'B',R)
      ]

tmWJ :: TM State Symbol
tmWJ = TM{..}
  where
    tmInitialState = "q0"
    tmFinalState = "q5"
    tmBlankSymbol = 'B'
    tmTransitions = Map.fromList
      [ ("q0",'a') `to` ("q1",'a',R)
      , ("q1",'a') `to` ("q2",'X',R)
      , ("q2",'a') `to` ("q3",'a',R)
      , ("q3",'a') `to` ("q2",'X',R)
      , ("q4",'a') `to` ("q4",'a',L)

      , ("q1",'B') `to` ("q5",'B',S)
      , ("q2",'B') `to` ("q4",'B',L)
      , ("q4",'B') `to` ("q0",'B',R)

      , ("q1",'X') `to` ("q1",'X',R)
      , ("q2",'X') `to` ("q2",'X',R)
      , ("q3",'X') `to` ("q3",'X',R)
      , ("q4",'X') `to` ("q4",'X',L)
      ]

tmSS2 :: TwoTapes.TM State Symbol
tmSS2 = TwoTapes.TM{..}
  where
    tmInitialState = "p"
    tmFinalStates = Set.fromList ["v"]
    tmBlankSymbol = 'B'
    tmWorkTapeLeftBoundary = '0'
    tmInputTapeLeftBoundary = '1'
    tmInputTapeRightBoundary = '2'
    tmTransitions = Map.fromList
      [ ("p",'a','B') `to` ("q",'A',L,L)

      , ("q",'1','0') `to` ("q",'0',R,R)
      , ("q",'a','A') `to` ("q",'A',R,R)
      , ("q",'a','B') `to` ("r",'B',L,L)

      , ("r",'a','A') `to` ("r",'A',R,L)
      , ("r",'a','0') `to` ("s",'0',L,R)

      , ("s",'a','A') `to` ("s",'A',L,R)
      , ("s",'a','B') `to` ("s",'A',L,R)
      , ("s",'1','B') `to` ("t",'A',S,L)

      , ("t",'1','A') `to` ("t",'A',S,L)
      , ("t",'1','0') `to` ("q",'0',S,S)

      , ("q",'2','B') `to` ("u",'B',L,L)

      , ("u",'a','A') `to` ("v",'A',S,S)
      ]

tmForTesting :: SomeTuringMachine State Symbol
tmForTesting = SomeTuringMachine tmSS2

to :: a -> b -> (a, b)
to = (,)
