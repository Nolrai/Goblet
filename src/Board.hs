{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Board
  ( PieceSize (..), Player (..), Column (..), Pos
  , BoardProper, ReserveArray, Board (..)
  , FrozenBoardProper, FrozenReserveArray, FrozenBoard (..)
  , BoardM (), newBoard, readBoardProper, writeBoardProper, readReserveArray, writeReserveArray
  , fullBounds, fullRange, saveBoard, mainBoard
  , runBoardMOnNew, runBoardMOnCopy, freezeBoard, thawBoard, freezeBoardM
  ) where
import System.Exit

import Data.Array.IO hiding (unsafeFreeze)
import Data.Array
import Data.Array.Unsafe (unsafeFreeze)

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Maybe ()
import Test.QuickCheck
import Test.Agata
import Prelude.SafeEnum

data PieceSize = S | MS | ML | L ; $( agatath $ derive ''PieceSize)
    deriving (Eq, Ord, Read, Show, Ix, Enum, Bounded)

data Player = Black | White; $( agatath $ derive ''Player)
    deriving (Eq, Ord, Read, Show, Ix, Enum, Bounded)

data Column = C0 | C1 | C2 | C3; $( agatath $ derive ''Player
    deriving (Eq, Ord, Enum, Read, Show, Ix, Bounded)
data Row = R0 | R1 | R2 | R3; $( agatath $ derive ''Player
    deriving (Eq, Ord, Enum, Read, Show, Ix, Bounded)

type Pos = (Column, Row)

type BoardProper = IOArray (Pos, PieceSize) (Maybe Player)
type FrozenBoardProper = Array (Pos, PieceSize) (Maybe Player)

data PileID = P0 | P1 | P2; $( agatath $ derive ''Player
    deriving (Eq, Ord, Enum, Read, Show, Ix, Bounded)

type ReserveArray = IOArray (Player, PileID) (Maybe PieceSize)
type FrozenReserveArray = Array (Player, PileID) (Maybe PieceSize)

data Board = Board
    { boardProper :: BoardProper
    , reserveArray :: ReserveArray
    }
data FrozenBoard = FrozenBoard
    { frozenBoardProper :: FrozenBoardProper
    , frozenReserveArray :: FrozenReserveArray
    }

newtype BoardM a = BoardM (ReaderT (Board) IO a)
    deriving (Monad, Functor)

mkBoard :: IO (BoardProper) -> IO (ReserveArray) -> IO (Board)
mkBoard = liftM2 Board

newBoard :: IO (Board)
newBoard = mkBoard (makeMaxArray Nothing) (makeMaxArray (Just L)) 
   where
   makeMaxArray :: (Ix x, Bounded x) => e -> IO (IOArray x e)
   makeMaxArray fillValue = newArray fullBounds fillValue

runBoardMOnNew :: BoardM a -> IO a
runBoardMOnNew m = runBoardM m =<< newBoard 

runBoardM :: BoardM a -> Board -> IO a
runBoardM (BoardM m) arr = runReaderT m arr

clone :: forall ix e. (Ix ix) => IOArray ix e -> IO (IOArray ix e)
clone a = thaw =<< (unsafeFreeze a :: IO (Array ix e))

runBoardMOnCopy :: BoardM a -> Board -> IO a
runBoardMOnCopy m arr =
  do
  boardProperClone <- clone (boardProper arr)
  reservesClone <- clone (reserveArray arr)
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

freezeBoardM :: BoardM FrozenBoard
freezeBoardM = BoardM $
  do
  b <- ask
  lift (freezeBoard b)  

freezeBoard :: Board -> IO FrozenBoard
freezeBoard (Board a b) = liftM2 FrozenBoard (freeze a) (freeze b)

thawBoard :: FrozenBoard -> IO Board
thawBoard (FrozenBoard a b) = liftM2 Board (thaw a) (thaw b)

mainBoard :: IO ExitCode
mainBoard = return ExitSuccess

data Move = Move {source :: Either PileID Pos, destination :: Pos} 

doMove :: Player -> Move -> EitherT MoveError BoardM ()
doMove player move@(Move source destination) = 
  do
  movingPiece <- fmapLT (ReadSorceError move) (readSource player source)
  targetTop <- lift $ topPiece destination
  (maxSize, isFromReserves) <-
    case source of
      Left _ -> Nothing
      Right _ -> pred movingPiece
  if targetTop > maxSize)
    then throwError $
      if isFromReserves then MovesFromReservesCantGoble move targetTop else TargetToBigToGoble move maxSize targetTop
    else return ()
  writeProperBoard (Pos, movingPiece) (Just player)
  case source of
    Left pile -> writeReservesArray (Player, pile) (pred movingPiece)
    Right sourcePos -> lift $ removeTop sourcePos
  return ()

data MoveError = ReadSourceError Move ReadSorceError 
  | MovesFromReservesCantGoble Move (Maybe PieceSize) 
  | TargetTooBigToGoble (Maybe PieceSize) (Maybe PieceSize)
  deriving (Show)

data ReadSourceError = EmptySquare | EmptyPile | WrongColor

readSource :: Player -> Either PileID Pos -> EitherT ReadSourceError BoardM ()
readSource player source =
  case source of
    Left pile -> noteAndHoist EmptyPile =<< (lift readReserveArray (player, pile))))
    Right source -> do
                    (player', size) <- hoistEither EmptySquare =<< lift (readTopSquare pos)
                    if player' != player
                      then throwT WrongColor
                      else return $ size

  where
  noteAndHoist :: Monad m => e -> Maybe a -> EitherT e m a
  noteAndHoist err = hoistEither . note err               

readTopSquare = undefined


