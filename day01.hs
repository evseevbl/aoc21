module Day01 where

import System.IO (isEOF)
import Data.Maybe
import Data.Char
import Text.Read

import Common


parseStrings :: [String] -> [Int]
parseStrings ls =
	catMaybes (map readMaybe ls)