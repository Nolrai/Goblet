module Board where

import Data.Array.IO

data PieceSize = S | MS | ML | L deriving (Eq, Ord, Read, Show, Ix)

data Player = Black | White

data Column = C0 | C1 | C2 | C3 deriving (Eq, Ord, Enum, Read, Show, Ix)
data Row = R0 | R1 | R2 | R3 deriving (Eq, Ord, Enum, Read, Show, Ix)

type Pos = (Column, Row)

type Board = IOArray (Pos, PieceSize) (Maybe Player)


