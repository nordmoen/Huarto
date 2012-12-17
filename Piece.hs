module Piece where

import Data.Word
import Data.Bits

data Piece = Piece Word8 | NoPiece deriving Show

newPiece :: Word8 -> Piece
newPiece a = 	if a >= 0 && a < 16
		then Piece a
		else NoPiece

fourEq :: Piece -> Piece -> Piece -> Piece -> Bool
fourEq _ _ _ NoPiece = False
fourEq _ _ NoPiece _ = False
fourEq _ NoPiece _ _ = False
fourEq NoPiece _ _ _ = False
fourEq (Piece a) (Piece b) (Piece c) (Piece d) = ands > 0 || xors > 0
	where   ands = a .&. b .&. c .&. d
		xors = aa .&. bb .&. cc .&. dd
		aa   = a `xor` 15
		bb   = b `xor` 15
		cc   = c `xor` 15
		dd   = d `xor` 15

instance Eq Piece where
	(==) NoPiece _ 		 = False
	(==) _ NoPiece 		 = False
	(==) (Piece a) (Piece b) = (a .&. b > 0) || (c .&. d > 0)
		where 	c = xor a 15
			d = xor b 15
