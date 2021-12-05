module Day01 where

import System.IO (isEOF)
import Data.Maybe
import Data.Char
import Text.Read

import Common

task01a :: [Int] -> Int -> Int
task01a ls cnt =
	if null (tail ls)
	then cnt
	else
		if head ls < head (tail ls)
		then task01a (tail ls) (cnt + 1)
		else task01a (tail ls) (cnt    )


parseStrings :: [String] -> [Int]
parseStrings ls =
	catMaybes (map readMaybe ls)