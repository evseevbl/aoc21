import Common

_task01a :: [Int] -> Int -> Int
_task01a ls cnt =
	if null (tail ls)
	then cnt
	else
		if head ls < head (tail ls)
		then _task01a (tail ls) (cnt + 1)
		else _task01a (tail ls) (cnt    )

task01a ls = _task01a ls 0

main = do
	vals <- getStrings

	(putStrLn . show . task01a . parseStrings) vals
