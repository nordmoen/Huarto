module Board where

import Piece
import Data.List(transpose)

type QuartoBoard = [[Piece]]

newBoard :: QuartoBoard
newBoard = replicate 4 row --Create a 4 x 4 playing grid
	 where row = replicate 4 NoPiece

setPiece :: QuartoBoard -> (Int, Int) -> Piece -> Either String QuartoBoard
setPiece _ _ NoPiece 		= Left "Piece cannot be a NoPiece"
setPiece board pos@(x, y) piece = if getPiece board pos /= NoPiece
				  then Left "Possition already taken!"
				  else undefined

getPiece :: QuartoBoard -> (Int, Int) -> Piece
getPiece board (x, y) = (board !! y) !! x

win :: QuartoBoard -> Bool
win board = horizontal || vertical || cross1 || cross2
	where   horizontal = any mapEq board
		vertical   = any mapEq $ transpose board
		cross1     = mapEq $ map fst cross
		cross2     = mapEq $ map snd cross
		cross 	   = extractCross board (length $ head board) 0

extractCross :: QuartoBoard -> Int -> Int -> [(Piece, Piece)]
extractCross board len curr =   if len == curr
				then []
				else (reg, neg): extractCross board len (curr + 1)
				where   reg = getPiece board (curr, curr)
					neg = getPiece board (3 - curr, curr)

draw :: QuartoBoard -> Bool
draw = all (notElem NoPiece)

pprint :: QuartoBoard -> IO ()
pprint board    = mapM_ putStrLn b
	where b = map show board
