module Main where

import Game
import System.Environment(getArgs)

main :: IO ()
main = do 
	[rounds, p1Type, p2Type, simulate] <- getArgs
	let r = read rounds :: Int
	let sim = read simulate :: Bool
	return () 
