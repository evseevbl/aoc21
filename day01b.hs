import Day01
import Common

_task01b :: [Int] -> Int -> Int
_task01b ls cnt =
	let
		w1 = take 3 ls
		w2 = take 3 (tail ls)
		diff = fromEnum (sum w2 > sum w1) -- bool to int
	in
		if length w2 < 3
		then cnt
		else
			_task01b (tail ls) (cnt + diff)


task01b ls = _task01b ls 0

main = do
	vals <- getStrings

	(putStrLn . show . task01b . parseStrings) vals
