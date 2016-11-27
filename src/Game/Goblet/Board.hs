{-# LANGUAGE
   GeneralizedNewtypeDeriving
 , ScopedTypeVariables
 , TemplateHaskell
 , TupleSections
 , FlexibleContexts
 #-}
module Game.Goblet.Board
  ( PieceSize (..), Player (..), Column (..), Pos
  , BoardProper, ReserveArray, Board (..)
  , FrozenBoardProper, FrozenReserveArray, FrozenBoard (..)
  , BoardM (), newBoard, readBoardProper, writeBoardProper, readReserveArray, writeReserveArray
  , fullBounds, fullRange, saveBoard
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

-- The Four sizes of piece, Small, MediumSmall, MediumLarge, Large
data PieceSize = S | MS | ML | L
    deriving (Eq, Ord, Read, Show, Ix, Bounded)

-- Used for the piles. Could use a list instead.
nextSmaller :: PieceSize -> Maybe PieceSize
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
-- Together the piece and column give a square on the board.

-- The BoardProper is just the played pieces, not counting the reserves
-- In the X FrozenX pairs the frozen form is read only, the plain X is mutable.
-- The distinction allows optimizations for the (eventual) AI.
type BoardProper = IOArray (Pos, PieceSize) (Maybe Player)
type FrozenBoardProper = Array (Pos, PieceSize) (Maybe Player)

-- Each player starts the game with three stacks of pieces, that they can only use in largest to smallest order.
data PileID = P0 | P1 | P2
    deriving (Eq, Ord, Read, Show, Ix, Bounded)

-- Arguably this should be an unordered triplet, but that would be anoying visually, and to implement.
type ReserveArray = IOArray (Player, PileID) (Maybe PieceSize)
type FrozenReserveArray = Array (Player, PileID) (Maybe PieceSize)

-- The entire game state. (-Not- including a undo stack)
data Board = Board
    { boardProper :: BoardProper
    , reserveArray :: ReserveArray
    }
data FrozenBoard = FrozenBoard
    { frozenBoardProper :: FrozenBoardProper
    , frozenReserveArray :: FrozenReserveArray
    }

-- Just the IO monad with a implicit mutable board.
newtype BoardM a = BoardM (ReaderT (Board) IO a)
    deriving (Monad, Functor, Applicative, MonadReader Board, MonadIO)

newBoard :: IO (Board)
newBoard = mkBoard (makeMaxArray Nothing) (makeMaxArray (Just L))
   where
   makeMaxArray :: (Ix x, Bounded x) => e -> IO (IOArray x e)
   makeMaxArray fillValue = newArray fullBounds fillValue
   mkBoard :: IO (BoardProper) -> IO (ReserveArray) -> IO (Board)
   mkBoard = liftM2 Board

-- run a BoardM action on a game start board.
runBoardMOnNew :: BoardM a -> IO a
runBoardMOnNew m = runBoardM m =<< newBoard

-- run a BoardM action on the passed in (mutable) board
runBoardM :: BoardM a -> Board -> IO a
runBoardM (BoardM m) arr = runReaderT m arr


-- run a BoardM action on a mutable copy of the array passed in
runBoardMOnCopy :: BoardM a -> Board -> IO a
runBoardMOnCopy m arr =
  do
  boardProperClone <- clone (boardProper arr)
  reservesClone <- clone (reserveArray arr)
  runBoardM m (Board boardProperClone reservesClone)
  where
  clone :: forall ix e. (Ix ix) => IOArray ix e -> IO (IOArray ix e)
  clone a = thaw =<< (freeze a :: IO (Array ix e))

-- read from either of the implicit Board arrays
readArrayBM :: (MonadReader Board m, MonadIO m, Ix ix) => (Board -> IOArray ix e) -> ix -> m e
readArrayBM getArray x =
    do
    arr <- reader getArray
    liftIO (readArray arr x)

-- write from either of the implicit Board arrays
writeArrayBM :: (MonadReader Board m, MonadIO m, Ix ix) => (Board -> IOArray ix e) -> ix -> e -> m ()
writeArrayBM getArray x p =
    do
    arr <- reader getArray
    liftIO (writeArray arr x p)

-- Read and write to the individual arrays
readBoardProper   :: (MonadReader Board m, MonadIO m) => (Pos, PieceSize)                    -> m (Maybe Player)
readBoardProper = readArrayBM boardProper
writeBoardProper  :: (MonadReader Board m, MonadIO m) => (Pos, PieceSize) -> Maybe Player    -> m ()
writeBoardProper = writeArrayBM boardProper
readReserveArray  :: (MonadReader Board m, MonadIO m) => (Player, PileID)                    -> m (Maybe PieceSize)
readReserveArray = readArrayBM reserveArray
writeReserveArray :: (MonadReader Board m, MonadIO m) => (Player, PileID) -> Maybe PieceSize -> m ()
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
  toChar = maybe '-' playerToChar
  playerToChar White = 'W'
  playerToChar Black = 'B'


saveReserveArray :: BoardM String
saveReserveArray = saveArray reserveArray toChar
  where
  toChar :: Maybe PieceSize -> Char
  toChar = maybe '-' pieceSizeToChar
  pieceSizeToChar S = 's'
  pieceSizeToChar MS = 'm'
  pieceSizeToChar ML = 'M'
  pieceSizeToChar L = 'L'

saveBoard :: BoardM String
saveBoard = (\ x y -> x ++ '\n' : y) <$> saveProperBoard <*> saveReserveArray

freezeBoardM :: (MonadReader Board m, MonadIO m) => m FrozenBoard
freezeBoardM = freezeBoard =<< ask

freezeBoard :: MonadIO m => Board -> m FrozenBoard
freezeBoard (Board a b) = liftIO (FrozenBoard <$> (freeze a) <*> (freeze b))

thawBoard :: MonadIO m => FrozenBoard -> m Board
thawBoard (FrozenBoard a b) = liftIO (Board <$> (thaw a) <*> (thaw b))

--Need to split move into pick up and put down.

data Move = Move {source :: Either PileID Pos, destination :: Pos}

instance Show Move where
  show Move {source = s, destination = d}
    = '(' : either show show s ++ ")->" ++ show d

doMove :: Player -> Move -> ExceptT MoveError BoardM ()
doMove player move@(Move source destination) =
  do
  movingPiece <- withExceptT (ReadSourceError move) (readSource player source)
  targetTop   <- lift (readTopSquare destination)
  _           <- withExceptT ($ move) (checkTarget source movingPiece (fst <$> targetTop))
  writeBoardProper (destination, movingPiece) (Just player)
  writeToSource source player movingPiece

writeToSource :: (MonadReader Board m, MonadIO m) => Either PileID Pos -> Player -> PieceSize -> m ()
writeToSource (Left pile)       player  ps = writeReserveArray (player, pile)  (nextSmaller ps)
writeToSource (Right sourcePos) _       ps = writeBoardProper  (sourcePos, ps) Nothing

checkTarget :: MonadError (Move -> MoveError) m => Either PileID Pos -> PieceSize -> Maybe PieceSize -> m ()
checkTarget _         _   Nothing     = return ()
checkTarget (Left _)  _   (Just tPS)  = throwError (\ move -> MovesFromReserveCantGoble move tPS)
checkTarget (Right _) mPS (Just tPS)  =
  when
    (tPS >= mPS)
    (throwError $ \move -> TargetTooBigToGoble move mPS tPS)

data MoveError
  = ReadSourceError Move ReadSourceError
  | MovesFromReserveCantGoble Move PieceSize
  | TargetTooBigToGoble Move PieceSize PieceSize
  deriving (Show)

data ReadSourceError = EmptySquare | EmptyPile | WrongPlayer
  deriving (Show)

readSource :: Player -> Either PileID Pos -> ExceptT ReadSourceError BoardM PieceSize
readSource player source =
  case source of
    Left pile -> note EmptyPile =<< readReserveArray (player, pile)
    Right pos ->  do
                  (size, player') <- note EmptySquare =<< lift (readTopSquare pos)
                  if player' /= player
                    then throwError WrongPlayer
                    else return size
  where
  note :: MonadError e m => e -> Maybe a -> m a
  note err = maybe (throwError err) return


readTopSquare :: Pos -> BoardM (Maybe (PieceSize, Player))
readTopSquare (col,row) = takeFirst `liftM` (action `mapM` fullRange)
  where
  takeFirst :: [Maybe a] -> Maybe a
  takeFirst = foldr takeLeft Nothing
  takeLeft mx@(Just _) _ = mx
  takeLeft Nothing my = my

  action :: PieceSize -> BoardM (Maybe (PieceSize, Player))
  action ps = fmap (ps,) <$> readBoardProper ((col, row), ps)

