module Piece where

import Data.Word
import Data.Bits
import Data.Char(toUpper)

data Piece = Piece Word8 | NoPiece deriving (Show, Eq)
--A piece is represented as a bitmap
--where each bit represent a Quarto trait
--If the possition is 1 then the first below else the second
--Pos 0 = Square or Round - Least significant bit
--Pos 1 = Light or Dark color
--Pos 2 = Hollow or Not Hollow
--Pos 3 = Small or Large - Most significant bit

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
mapEq list = filled && (ands > 0 || xors > 0)
	where   ands    = foldr1 (.&.) numList
		xors    = foldr1 (.&.) $ map (xor 15) numList
		numList = extractNums list
		filled 	= NoPiece `notElem` list

extractNums :: [Piece] -> [Word8]
extractNums [] 		     = []
extractNums (NoPiece:rest)   = extractNums rest
extractNums (Piece a:rest)   = a : extractNums rest

showPiece :: Piece -> String
showPiece NoPiece   = replicate 4 '_'
showPiece (Piece a) = bracket : (mkLarge color) : hollow : revBrack : []
	where 	bracket  = if a .&. 1 > 0 then '[' else '('
		color 	 = if a .&. 2 > 0 then 'l' else 'd'
		hollow   = if a .&. 4 > 0 then '*' else ' '
		mkLarge  = if a .&. 8 > 0 then toUpper else id
		revBrack = if a .&. 1 > 0 then ']' else ')'
