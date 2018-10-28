{-# LANGUAGE RecordWildCards #-}

module TuringMachine.Tape
  ( Tape  -- don't export constructors
  , Movement(..)
  , mkTape
  , readTape
  , writeTape
  , moveTape
  , writeAndMoveTape
  , putTape
  ) where

-- ansi-terminal
import System.Console.ANSI (
  setSGR, SGR(..), ConsoleLayer(..), Color(..), ColorIntensity(..), ConsoleIntensity(..), Underlining(..)
  )

-- safe
import Safe ( headDef, tailSafe )

-- tm
import TuringMachine.Class ( PrintableTMSymbol(..) )


data Movement = L | S | R

-- | A tape, unbounded in both directions
data Tape symbol = Tape
  { tapeLeft :: [symbol]   -- ^ what's to the left of the current position (head tapeLeft == symbol next to current position, i.e., the left part is stored in reverse order)
  , tapeRight :: [symbol]  -- ^ the current position and what's to the right of the current position (head tapeRight == symbol at current position)
  , tapeBlank :: symbol    -- ^ the blank symbol (when the read/write head exceeds the current bounds)
  }
  deriving (Eq, Ord, Show)


-- | Initializes a new tape. The read/write head will point to the first symbol of the given word.
mkTape
  :: symbol    -- ^ the blank symbol
  -> [symbol]  -- ^ the initial word on the tape
  -> Tape symbol
mkTape b w = Tape { tapeLeft = []
                  , tapeRight = w
                  , tapeBlank = b
                  }

-- | Read the symbol that is currently under the read/write head
readTape :: Tape symbol -> symbol
readTape Tape{..} = headDef tapeBlank tapeRight

-- | Writes a symbol to the current position of the read/write head on the tape
writeTape
  :: Eq symbol
  => symbol
  -> Tape symbol
  -> Tape symbol
writeTape x Tape{..} = Tape { tapeLeft = tapeLeft
                            , tapeRight = trimBlanks3 tapeBlank $ x : tailSafe tapeRight
                            , tapeBlank = tapeBlank
                            }

-- | Moves the read/write head on the tape
moveTape
  :: Eq symbol
  => Movement  -- ^ the direction to move
  -> Tape symbol
  -> Tape symbol
moveTape L Tape{..} = Tape { tapeLeft = tailSafe tapeLeft
                           , tapeRight = trimBlanks3 tapeBlank $ headDef tapeBlank tapeLeft : tapeRight
                           , tapeBlank = tapeBlank
                           }
moveTape S t = t
moveTape R Tape{..} = Tape { tapeLeft = trimBlanks3 tapeBlank $ headDef tapeBlank tapeRight : tapeLeft
                           , tapeRight = tailSafe tapeRight
                           , tapeBlank = tapeBlank
                           }
-- | Writes a symbol and moves the read/write head on the tape
writeAndMoveTape
  :: Eq symbol
  => Maybe symbol  -- ^ the symbol to write, or Nothing to leave the tape contents unchanged
  -> Movement  -- ^ in what direction to move
  -> Tape symbol
  -> Tape symbol
writeAndMoveTape Nothing m = moveTape m
writeAndMoveTape (Just x) m = moveTape m . writeTape x

-- | Removes unneeded blank symbols at the border of the tape (only checks three symbols, which should be enough when it's done after every step)
-- Loop detection works better if we normalize the configurations
-- TODO: Removing *one* blank should actually be enough
trimBlanks3 :: Eq symbol => symbol -> [symbol] -> [symbol]
trimBlanks3 b [x]
  | x == b = []
trimBlanks3 b [x, y]
  | y == b = trimBlanks3 b [x]
trimBlanks3 b [x, y, z]
  | z == b = trimBlanks3 b [x, y]
trimBlanks3 _ xs = xs

putTape
  :: PrintableTMSymbol symbol
  => Tape symbol
  -> IO ()
putTape t@Tape{..} = do
  putSymbols $ reverse tapeLeft
  setSGR [ SetUnderlining SingleUnderline
         , SetConsoleIntensity BoldIntensity
         , SetColor Foreground Dull Yellow ]
  putSymbol (readTape t)
  setSGR []
  putSymbols (tailSafe tapeRight)
