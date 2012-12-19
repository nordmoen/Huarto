module Game where

import Piece
import Board

type Pos = (Int, Int)

playRound :: (Piece -> QuartoBoard -> (Piece, Pos)) -> Piece -> QuartoBoard -> (Piece, QuartoBoard)
playRound player piece board = (next, nBoard)
		where	(next, pos) = player piece board
			res = setPiece board pos piece
			nBoard = case res of
				(Left problem) -> error problem
				(Right nb) -> nb

playGame player1 player2 piece board = if win board
					then Just player1
					else if draw board
					then Nothing
					else playGame player2 player1 nPiece nBoard
					where (nPiece, nBoard) = playRound player2 piece board
						
