import Data.Char (isDigit)
import Data.List (sort, group)
import Foreign (fromBool)
import Data.Maybe (mapMaybe)

int :: String -> Integer
int = read . filter (\x -> isDigit x || x == '-')

tuple :: [b] -> (b, b)
tuple (a : b : _) = (a, b)

split :: (a -> Bool) -> [a] -> [[a]]
split p xs = let (a, b) = break p xs in if null b then [a] else a : split p (tail b)

readRobot :: String -> ((Integer, Integer), (Integer, Integer))
readRobot = tuple . map (tuple . map int . split (==',')) . words

moveFor :: Integer -> ((Integer, Integer), (Integer, Integer)) -> (Integer, Integer)
moveFor t ((x, y), (vx, vy)) = ((x + vx * t) `mod` width, (y + vy * t) `mod` height)

quadrant :: (Integer, Integer) -> Maybe Integer
quadrant (x, y) =
    let xAxis = width `div` 2
        yAxis = height `div` 2
    in if x == xAxis || y == yAxis then Nothing
    else Just (fromBool (xAxis < x) + fromBool (y < yAxis) * 2)

width :: Integer
width = 101

height :: Integer
height = 103

main :: IO ()
main = do
    input <- readFile "2024/14/input.txt"
    print $ product . map length . group . sort . mapMaybe (quadrant . moveFor 100 . readRobot) . lines $ input