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
              then putStrLn $ "Success: " <> input
              else do putStrLn $ "Error (accepted but should not accept): " <> input
                      mapM_ (printConfiguration tm) trace
                      putChar '\n'
    Timeout -> putStrLn $ "Timeout: " <> input
    _ -> if shouldAccept
         then do putStrLn $ "Error (" <> show result <> " but should accept): " <> input
                 mapM_ (printConfiguration tm) trace
                 putChar '\n'
         else putStrLn $ "Success: " <> input

testTM :: TM State Symbol -> IO ()
testTM tm =
  forM_ [1..16] $ \(i :: Int) -> do
    let word = replicate i 'a'
    runSimulation tm word (i `elem` [ 2^n | (n :: Int) <- [0..10] ])

main :: IO ()
main = do
  testTM TuringMachine.Examples.tmForTesting
