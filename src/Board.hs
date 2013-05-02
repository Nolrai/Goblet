module Board where
import System.Exit

import Data.Array.IO

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

data Reserves =
  Reserves (Maybe PieceSize) (Maybe PieceSize) (Maybe PieceSize)

normalizeReserve :: Reserves -> Reserves
normalizeReserve (Reserves a b c) =
    if a >= b
        then if b >= c
            then Reserves a b c
            else if c >= a
                then Reserves c a b
                else Reserves a c b
        else if a >= c
            then Reserves b a c
            else if b >= c
                then Reserves b c a
                else Reserves c b a

data Board = Board
    {boardProper :: BoardProper, reserves :: (Reserves, Reserves) }

(.>.) :: Player -> (Reserves, Reserves) -> Reserves
Black .>. x = fst x
White .>. x = snd x

mainBoard :: IO ExitCode
mainBoard = return ExitSuccess


