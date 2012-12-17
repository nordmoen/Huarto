module Board where

import Piece

type QuartoBoard = [[Piece]]

newBoard :: QuartoBoard
newBoard = replicate 4 row --Create a 4 x 4 playing grid
	 where row = replicate 4 NoPiece

setPiece :: QuartoBoard -> (Int, Int) -> Piece -> Either String QuartoBoard
setPiece _ _ NoPiece 		= Left "Piece cannot be a NoPiece"
setPiece board pos@(x, y) piece = if getPiece board pos /= NoPiece
				  then Left "Possition already taken!"
				  else Right board

getPiece :: QuartoBoard -> (Int, Int) -> Piece
getPiece board (x, y) = (board !! y) !! x

pprint :: QuartoBoard -> IO ()
pprint board    = mapM_ putStrLn b
	where b = map show board
