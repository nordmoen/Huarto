module Piece where

import Data.Word
import Data.Bits

data Piece = Piece Word8 | NoPiece deriving (Show, Eq)

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

mapEq :: [Piece] -> Bool
mapEq list = ands > 0 || xors > 0
	where   ands    = foldr (.&.) 15 numList
		xors    = foldr (.&.) 15 $ map (xor 15) numList
		numList = extractNums list

extractNums :: [Piece] -> [Word8]
extractNums [] 		     = []
extractNums (NoPiece:rest)   = extractNums rest
extractNums (Piece a:rest) = a : extractNums rest
