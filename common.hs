module Common where

import System.IO (isEOF)
import Data.Maybe
import Data.Char
import Text.Read

getStrings :: IO [String]
getStrings = do
	done <- isEOF
	if done then return []
	else do
		inp <- getLine
		moreVals <- getStrings
		return (inp : moreVals)

parseStrings :: [String] -> [Int]
parseStrings ls =
	catMaybes (map readMaybe ls)

