module Day02 where

import Data.Maybe
import Data.Tuple
import Text.Read

data Direction = Up | Down | Forward deriving (Enum)

parseDirection :: String -> Maybe Direction
parseDirection s = case s of
	"up" -> Just Up
	"down" -> Just Down
	"forward" -> Just Forward
	otherwise -> Nothing

instance Show Direction where
	show Up = "↑"
	show Down = "↓"
	show Forward = "→"

type Move = (Direction, Int)

parseMove :: String -> Maybe Move
parseMove s = case splitMove s of
	Nothing -> Nothing
	Just (dir, step) -> both
		(parseDirection dir)
		(readMaybe step)

-- expect two words exactly
splitMove :: String -> Maybe (String, String)
splitMove s | length (words s) /= 2 = Nothing
splitMove s =
	let
		(w1:w2:_) = words s
	in Just (w1, w2)

-- move to coords
getCoords :: Move -> (Int, Int)
getCoords (Up, y) = (0, y)
getCoords (Down, y) = (0, -y)
getCoords (Forward, x) = (x, 0)

-- assert both values present
both :: Maybe a -> Maybe b -> Maybe (a, b)
both a b = case (a, b) of
	(Nothing, _) -> Nothing
	(_, Nothing) -> Nothing
	(Just a, Just b) -> Just (a, b)
