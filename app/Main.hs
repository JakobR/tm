{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- base
import Control.Monad ( forM_, mapM_ )

-- ansi-terminal
import System.Console.ANSI (
  setSGR, SGR(..), ConsoleLayer(..), Color(..), ColorIntensity(..), ConsoleIntensity(..), Underlining(..)
  )

-- text
import qualified Data.Text as Text

-- safe
import Safe ( headDef, tailSafe )

-- tm
import TuringMachine
import qualified TuringMachine.Examples


printConfiguration :: TM State Symbol -> Configuration State Symbol -> IO ()
printConfiguration TM{..} Configuration{..} = do
  putStr $ Text.unpack cfState
  putStr ":\t"
  putStr $ reverse cfLeftTape
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
      showInput = if null input then "ε" else input
      success = putStrLn $ "Success: " <> showInput
      failure = do let showNot = if shouldAccept then "" else "not "
                   putStrLn $ "Error (should " <> showNot <> "accept): " <> showInput
                   mapM_ (printConfiguration tm) trace
                   print result
                   putChar '\n'
  case result of
    Accept -> if shouldAccept then success else failure
    Reject -> if shouldAccept then failure else success
    Loop ->   if shouldAccept then failure else success
    Timeout -> putStrLn $ "Timeout: " <> showInput

testTM :: TM State Symbol -> IO ()
testTM tm =
  forM_ (reverse [0..32]) $ \(i :: Int) -> do
    let word = replicate i 'a'
    runSimulation tm word (i `elem` [ 2^n | (n :: Int) <- [0..10] ])

main :: IO ()
main = do
  testTM TuringMachine.Examples.tmForTesting
