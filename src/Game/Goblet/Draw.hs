{-# LANGUAGE NoMonomorphismRestriction #-}

module Game.Goblet.Draw where
import Game.Goblet.Board
import Data.Text

drawBoard : Board -> IO Text
drawBoard (Board bp wr br) =
  do
  
