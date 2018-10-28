{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- base
import Control.Monad ( forM_, mapM_ )

-- tm
import TuringMachine.Class
import TuringMachine.Examples ( State, Symbol, tmForTesting )


runSimulation :: TuringMachine tm => tm State Symbol -> [Symbol] -> Bool -> IO ()
runSimulation tm input shouldAccept = do
  let (result, trace) = simulate 10000 tm input
      showInput = if null input then "ε" else input
      putTrace = do mapM_ (printConfiguration tm) trace
                    print result
                    putChar '\n'
      success = putStrLn $ "Success: " <> showInput
      failure = do let showNot = if shouldAccept then "" else "not "
                   putStrLn $ "Error (should " <> showNot <> "accept): " <> showInput
                   putTrace
  case result of
    Accept -> if shouldAccept then success {- >> putTrace -} else failure
    Reject -> if shouldAccept then failure else success
    Loop ->   if shouldAccept then failure else success
    Timeout -> putStrLn $ "Timeout: " <> showInput

testTM :: TuringMachine tm => tm State Symbol -> IO ()
testTM tm =
  forM_ (reverse [0..32]) $ \(i :: Int) -> do
    let word = replicate i 'a'
    runSimulation tm word (i `elem` [ 2^n | (n :: Int) <- [0..10] ])

main :: IO ()
main =
  case tmForTesting of
    SomeTuringMachine tm -> testTM tm
