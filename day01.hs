module Day01 where

import System.IO (isEOF)
import Data.Maybe
import Data.Char

task01a :: [Int] -> Int -> Int
task01a ls cnt =
	if null (tail ls)
	then cnt
	else
		if head ls < head (tail ls)
		then task01a (tail ls) (cnt + 1)
		else task01a (tail ls) (cnt    )

getStrings :: IO [String]
getStrings = do
	done <- isEOF
	if done then return []
	else do
		inp <- getLine
		moreVals <- getStrings
		let x = (inp : moreVals)
		putStrLn (show x)
		return x
--		return (inp : moreVals)
