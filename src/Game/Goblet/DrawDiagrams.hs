{-# LANGUAGE NoMonomorphismRestriction #-}

module Game.Goblet.DrawDiagrams where
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Board

mainDrawDiagrams = multiMain ["test", circle 2]
