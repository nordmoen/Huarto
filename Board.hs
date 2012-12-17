module Board where

import Piece
import Data.List

type QuartoBoard = [[Piece]]

newBoard :: QuartoBoard
newBoard = take 4 $ repeat row
	 where row = take 4 $ repeat NoPiece

setPiece :: QuartoBoard -> (Int, Int) -> Bool
setPiece board (x, y) = undefined

pprint :: QuartoBoard -> IO ()
pprint board    = mapM_ putStrLn b
	where b = map show board
