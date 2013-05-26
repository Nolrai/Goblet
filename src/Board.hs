{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Board where
import System.Exit

import Data.Array.IO

import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class

data PieceSize = S | MS | ML | L
    deriving (Eq, Ord, Read, Show, Ix, Enum)

data Player = Black | White

data Column = C0 | C1 | C2 | C3
    deriving (Eq, Ord, Enum, Read, Show, Ix)
data Row = R0 | R1 | R2 | R3
    deriving (Eq, Ord, Enum, Read, Show, Ix)

type Pos = (Column, Row)

type BoardProper = IOArray (Pos, PieceSize) (Maybe Player)

data ReservesAmount = RA1 | RA2 | RA3
    deriving (Eq, Ord, Read, Show)

type ReserveArray = IOArray (Player, Pile) (Maybe PieceSize)

data Board = Board
    {boardProper :: BoardProper, reserveArray :: ReserveArray}

newtype BoardM a = BoardM (ReaderT Board IO a)
    deriving (Monad, Functor)

newBoard :: IO Board
newBoard = Board `liftM2` (makeMaxArray Nothing) (makeMaxArray (Just L)) 
   where
   makeMaxArray :: (Ix x, Bounded x) => e -> IOArray x e
   makeMaxArray = newArray (minBound, maxBound)

runBoardM (BoardM m) board = runReaderT board m

readProperBoard x = 
    do
    arr <- BoardM (asks boardProper)
    BoardM (liftIO (readArray arr x))

writeProperBoard x p =
    do
    arr <- asks boardProper
    BoardM (liftIO (writeArray arr x p))

saveArray arr =
   do


saveProperBoard =
    do
    arr <- asks boardProper
    b <- getBounds arr
    forM (range b) (toChar `liftM' readArray arr)
    where
    toChar :: Maybe Player -> Char
    toChar Nothing = '-'
    toChar (Just White) = 'O'
    toChar (Just Black = 'X'
    
mainBoard :: IO ExitCode
mainBoard = return ExitSuccess
