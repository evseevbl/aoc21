import Day01
import Common

t01a :: [Int] -> Int -> Int
t01a ls cnt =
	if null (tail ls)
	then cnt
	else
		if head ls < head (tail ls)
		then t01a (tail ls) (cnt + 1)
		else t01a (tail ls) (cnt    )

task01a ls = t01a ls 0

main = do
	vals <- getStrings

	(putStrLn . show . task01a . parseStrings) vals
