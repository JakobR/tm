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
                            , tapeRight = trimBlank tapeBlank $ x : tailSafe tapeRight
                            , tapeBlank = tapeBlank
                            }

-- | Moves the read/write head on the tape
moveTape
  :: Eq symbol
  => Movement  -- ^ the direction to move
  -> Tape symbol
  -> Tape symbol
moveTape L Tape{..} = Tape { tapeLeft = tailSafe tapeLeft
                           , tapeRight = trimBlank tapeBlank $ headDef tapeBlank tapeLeft : tapeRight
                           , tapeBlank = tapeBlank
                           }
moveTape S t = t
moveTape R Tape{..} = Tape { tapeLeft = trimBlank tapeBlank $ headDef tapeBlank tapeRight : tapeLeft
                           , tapeRight = tailSafe tapeRight
                           , tapeBlank = tapeBlank
                           }

-- | Writes a symbol and moves the read/write head on the tape
writeAndMoveTape
  :: Eq symbol
  => symbol    -- ^ the symbol to write
  -> Movement  -- ^ in what direction to move after writing
  -> Tape symbol
  -> Tape symbol
writeAndMoveTape x m = moveTape m . writeTape x

-- | Helper function to remove unneeded blank symbols at the ends of the tape.
-- Loop detection works better if we normalize the configurations.
trimBlank :: Eq symbol => symbol -> [symbol] -> [symbol]
trimBlank b [x]
  | x == b = []
trimBlank _ xs = xs

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
