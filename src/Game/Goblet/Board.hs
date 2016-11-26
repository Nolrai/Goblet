{-# LANGUAGE
   GeneralizedNewtypeDeriving
 , ScopedTypeVariables
 , TemplateHaskell
 , TupleSections
   #-}
module Game.Goblet.Board
  ( PieceSize (..), Player (..), Column (..), Pos
  , BoardProper, ReserveArray, Board (..)
  , FrozenBoardProper, FrozenReserveArray, FrozenBoard (..)
  , BoardM (), newBoard, readBoardProper, writeBoardProper, readReserveArray, writeReserveArray
  , fullBounds, fullRange, saveBoard, mainBoard
  , runBoardMOnNew, runBoardMOnCopy, freezeBoard, thawBoard, freezeBoardM
  ) where
import System.Exit

import Prelude hiding (Enum(..))
import Data.Array.IO hiding (unsafeFreeze)
import Data.Array
import Data.Array.Unsafe (unsafeFreeze)

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Test.QuickCheck

data PieceSize = S | MS | ML | L
    deriving (Eq, Ord, Read, Show, Ix, Bounded)

nextSmaller S  = Nothing
nextSmaller MS = Just S
nextSmaller ML = Just MS
nextSmaller L  = Just ML

data Player = Black | White
    deriving (Eq, Ord, Read, Show, Ix, Bounded)

data Column = C0 | C1 | C2 | C3
    deriving (Eq, Ord, Read, Show, Ix, Bounded)
data Row = R0 | R1 | R2 | R3
    deriving (Eq, Ord, Read, Show, Ix, Bounded)

type Pos = (Column, Row)

type BoardProper = IOArray (Pos, PieceSize) (Maybe Player)
type FrozenBoardProper = Array (Pos, PieceSize) (Maybe Player)

data PileID = P0 | P1 | P2
    deriving (Eq, Ord, Read, Show, Ix, Bounded)

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
    deriving (Monad, Functor, Applicative, MonadReader Board, MonadIO)

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
    toChar Nothing =  maybe playerToChar '-'

saveReserveArray :: BoardM String
saveReserveArray = saveArray reserveArray toChar
  where
  toChar :: Maybe PieceSize -> Char
  toChar = maybe pieceSizeToChar '-'

playerToChar White = 'W'
playerToChar Black = 'B'

pieceSizeToChar S = 's'
pieceSizeToChar MS = 'm'
pieceSizeToChar ML = 'M'
pieceSizeToChar L = 'L'

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

instance Show Move where
  show Move {source = s, destination = d}
    = "(" : either show show s ++ ")->" ++ show d

doMove :: Player -> Move -> ExceptT MoveError BoardM ()
doMove player move@(Move source destination) =
  do
  movingPiece <- fmapLT (ReadSorceError move) (readSource player source)
  targetTop <- lift $ topPiece destination
  checkTarget source movingPiece targetPiece
  writeProperBoard (destination, movingPiece) (Just player)
  case source of
    Left pile -> writeReservesArray (Player, pile) (nextSmaller movingPiece)
    Right sourcePos -> removeTop sourcePos
  return ()

checkTarget :: MonadError MoveError m => Either PileID Pos -> PieceSize -> m ()
checkTarget _         _   Nothing     _ = return ()
checkTarget (Left _)  _   (Just tPS)  move = throwError MovesFromReserveCantGoble move tPS
checkTarget (Right _) mPS (Just tPS)  move =
  if tPS >= mPS
    then throwError TargetTooBigToGoble move mPS tPS
    else return ()

data MoveError
  = ReadSourceError Move ReadSourceError
  | MovesFromReservesCantGoble Move PieceSize
  | TargetTooBigToGoble Move PieceSize PieceSize
  deriving (Show)

data ReadSourceError = EmptySquare | EmptyPile | WrongPlayer
  deriving (Show)

readSource :: Player -> Either PileID Pos -> ExceptT ReadSourceError BoardM PieceSize
readSource player source =
  case source of
    Left pile -> note EmptyPile =<< (lift readReserveArray (player, pile))
    Right pos ->  do
                  (size, player') <- note EmptySquare =<< lift (readTopSquare pos)
                  if player' /= player
                    then throwError WrongPlayer
                    else return size
  where
  note :: MonadError e m => e -> Maybe a -> m a
  note err = maybe (throwError error) return


readTopSquare :: Pos -> BoardM (Maybe (PieceSize, Player))
readTopSquare (col,row) = takeFirst `liftM` (action `mapM` fullRange)
  where
  takeFirst :: [Maybe a] -> Maybe a
  takeFirst = foldr takeLeft Nothing
  takeLeft mx@(Just _) _ = mx
  takeLeft Nothing my = my

  action :: PieceSize -> BoardM (Maybe (PieceSize, Player))
  action ps = fmap (ps,) <$> readBoardProper ((col, row), ps)

