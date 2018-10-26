{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module TuringMachine.Examples where

-- containers
import qualified Data.Map as Map

-- tm
import TuringMachine

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

tmForTesting :: TM State Symbol
tmForTesting = tmLF

-- t :: Text -> Char -> Text -> Char -> Movement -> ((Text, Char), (Text, Char, Movement))
-- t st sy st' sy' mv = ((st,sy),(st',sy',mv))

to :: (a,b) -> (a,b,c) -> ((a,b),(a,b,c))
to = (,)
