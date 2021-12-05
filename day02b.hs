import Common
import Day02

import Data.Maybe


-- x, y, aim
_task02b :: [Move] -> Int -> Int -> Int -> (Int, Int, Int)
_task02b [] x y a = (x, y, a)
_task02b moves x y a =
	let
		m = head moves
		(newx, newy, newa) = posDiff m x y a
	in	_task02b (tail moves) newx newy newa

posDiff :: Move -> Int -> Int -> Int -> (Int, Int, Int)
posDiff m x y a = case m of
	(Up, t) -> (x, y, a - t)
	(Down, t) -> (x, y, a + t)
	(Forward, t) -> (x+t, y+a*t, a)

task02b :: [Move] -> Int
task02b moves =
	let (dx, dy, _) = _task02b moves 0 0 0
	in dx * dy

main = do
	vals <- getStrings
	(putStrLn . show . task02b . mapMaybe parseMove ) vals


