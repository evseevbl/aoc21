import Common
import Day02

import Data.Maybe

_task02a :: [Move] -> Int -> Int -> (Int, Int)
_task02a [] x y = (x, y)
_task02a moves x y =
	let
		(dx, dy) = getCoords (head moves)
	in _task02a (tail moves) (x+dx) (y+dy)

task02a :: [Move] -> Int
task02a moves =
	let (dx, dy) = _task02a moves 0 0
	in dx * dy

main = do
	vals <- getStrings
	(putStrLn . show . abs . task02a . mapMaybe parseMove ) vals


