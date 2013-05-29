{-# LANGUAGE GeneralizedNewtypeDeriving, Rank2Types #-}
module Board
  ( PieceSize (..), Player (..), Column (..), Pos, BoardProper, ReserveArray, Board (..)
  , BoardM (), newBoard, readBoardProper, writeBoardProper, readReserveArray, writeReserveArray
  , fullBounds, fullRange, saveBoard, mainBoard
  , runBoardMOnNew
  ) where
import System.Exit

import Data.Array.IO
import Data.Array

import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO
import Control.Monad.Maybe

data PieceSize = S | MS | ML | L
    deriving (Eq, Ord, Read, Show, Ix, Enum, Bounded)

data Player = Black | White
    deriving (Eq, Ord, Read, Show, Ix, Enum, Bounded)

data Column = C0 | C1 | C2 | C3
    deriving (Eq, Ord, Enum, Read, Show, Ix, Bounded)
data Row = R0 | R1 | R2 | R3
    deriving (Eq, Ord, Enum, Read, Show, Ix, Bounded)

type Pos = (Column, Row)

type BoardProper = IOArray (Pos, PieceSize) (Maybe Player)

data PileID = P0 | P1 | P2
    deriving (Eq, Ord, Enum, Read, Show, Ix, Bounded)

type ReserveArray = IOArray (Player, PileID) (Maybe PieceSize)

type Board
    {boardProper :: BoardProper, reserveArray :: ReserveArray}

newtype BoardM a = BoardM (ReaderT (Board) (IO) a)
    deriving (Monad, Functor)

mkBoard :: IO (BoardProper) -> IO (ReserveArray) -> IO (Board)
mkBoard = liftM2 Board

newBoard :: IO (Board)
newBoard = mkBoard (makeMaxArray Nothing) (makeMaxArray (Just L)) 
   where
   makeMaxArray :: (Ix x, Bounded x) => e -> IO (IOArray x e)
   makeMaxArray fillValue = newArray fullBounds fillValue

runBoardMOnNew :: BoardM a -> 

runBoardM :: BoardM a -> Board -> IO a
runBoardM (BoardM m) arr = runReaderT m arr

cloneArray = thaw . unsafeFreeze

runBoardMOnCopy :: BoardM a -> Board -> IO a
runBoardMOnCopy m arr =
  do
  boardProperClone <- clone =<< boardProper arr
  reservesClone <- clone =<< reserveArray arr
  runBoardM m (Board boardProperClone reservesClone)

readArrayBM :: (Ix ix) => (Board -> IOArray ix e) -> ix -> BoardM e
readArrayBM getArray x = 
    do
    arr <- BoardM (asks getArray)
    BoardM (lift (readArray arr x))

writeArrayBM :: (Ix ix) => (Board -> IOArray ix e) -> ix -> e -> BoardM ()
writeArrayBM getArray x p =
    do
    arr <- BoardM (asks getArray)
    BoardM (lift (writeArray arr x p))

readBoardProper :: (Pos, PieceSize) -> BoardM (Maybe Player)
readBoardProper = readArrayBM boardProper
writeBoardProper :: (Pos, PieceSize) -> Maybe Player -> BoardM ()
writeBoardProper = writeArrayBM boardProper
readReserveArray :: (Player, PileID) -> BoardM (Maybe PieceSize)
readReserveArray = readArrayBM reserveArray
writeReserveArray :: (Player, PileID) -> Maybe PieceSize -> BoardM ()
writeReserveArray = writeArrayBM reserveArray

fullBounds :: (Ix ix, Bounded ix) => (ix, ix)
fullBounds = (minBound, maxBound)

fullRange :: (Ix ix, Bounded ix) => [ix]
fullRange = range fullBounds

saveArray :: (Ix ix, Bounded ix) => (Board -> IOArray ix e) -> (e -> Char) -> BoardM String
saveArray getArray toChar =
    do
    arr <- BoardM (asks getArray)
    BoardM . lift $
      forM fullRange 
      (\ ix -> toChar `liftM` readArray arr ix)

--a zip that fails if the two lists are differnt in length
exactZip :: [a] -> [b] -> Maybe [(a,b)]
exactZip [] [] = return []
exactZip (x:xs) (y:ys) = ((x,y) :) `fmap` exactZip xs ys

saveProperBoard :: BoardM String
saveProperBoard = saveArray boardProper toChar
    where
    toChar :: Maybe Player -> Char
    toChar Nothing = '-'
    toChar (Just White) = 'W'
    toChar (Just Black) = 'B'

saveReserveArray :: BoardM String
saveReserveArray = saveArray reserveArray toChar
  where
  toChar :: Maybe PieceSize -> Char
  toChar Nothing = '-'
  toChar (Just S) = 'S'
  toChar (Just MS) = 'm'
  toChar (Just ML) = 'M'
  toChar (Just L) = 'L'
    
saveBoard :: BoardM String
saveBoard = liftM2 (++) saveProperBoard saveReserveArray 

mainBoard :: IO ExitCode
mainBoard = return ExitSuccess
