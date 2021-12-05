module Common where

import System.IO (isEOF)

getStrings :: IO [String]
getStrings = do
	done <- isEOF
	if done then return []
	else do
		inp <- getLine
		moreVals <- getStrings
		return (inp : moreVals)
