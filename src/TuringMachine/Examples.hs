{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module TuringMachine.Examples where

-- containers
import qualified Data.Map as Map

-- tm
import TuringMachine

tmForTesting :: TM State Symbol
tmForTesting = tmBJ

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

to :: (a,b) -> (a,b,c) -> ((a,b),(a,b,c))
to = (,)
