import Data.Char (isDigit)
import GHC.Arr ((!), Array, accumArray)
import GHC.Num (integerToInt)

int :: String -> Integer
int = read . filter (\x -> isDigit x || x == '-')

tuple :: [b] -> (b, b)
tuple (a : b : _) = (a, b)

split :: (a -> Bool) -> [a] -> [[a]]
split p xs = let (a, b) = break p xs in if null b then [a] else a : split p (tail b)

readRobot :: String -> ((Integer, Integer), (Integer, Integer))
readRobot = tuple . map (tuple . map int . split (==',')) . words

move :: ((Integer, Integer), (Integer, Integer)) -> ((Integer, Integer), (Integer, Integer))
move ((x, y), (vx, vy)) = (((x + vx) `mod` width, (y + vy) `mod` height), (vx, vy))

countRobots :: [((Integer, Integer), (Integer, Integer))] -> Array Integer Integer
countRobots = accumArray (+) 0 (0, width * height - 1) . map (\((x, y), _) -> (x + width * y, 1))

render :: [((Integer, Integer), (Integer, Integer))] -> String
render screenshot =
    let counts = countRobots screenshot
    in unlines . map (map (char . (counts!)) . row) $ [0..(height - 1)]
    where
        row y = [(y * width)..((y + 1) * width - 1)]
        char c = if c == 0 then ' ' else 'X'

loop :: Integer -> [[((Integer, Integer), (Integer, Integer))]] -> IO ()
loop i (screenshot : xs) = do
    putStrLn (render screenshot)
    putStr ("(" ++ show i ++ ") ")
    continue <- getLine
    if null continue then loop (i + 1) xs
    else if continue == " " then print i
    else let skip = int continue in loop skip (drop (integerToInt (int continue - i - 1)) xs)

width :: Integer
width = 101

height :: Integer
height = 103

main :: IO ()
main = do
    input <- readFile "2024/14/input.txt"
    let robots = map readRobot . lines $ input
    let screenshots = iterate (map move) robots
    loop 0 screenshots

{-
There appear vertical and horizontal rectangles periodically.
The rectangles look a bit like skewed images of a Cristmas tree.

Messured appearance after seconds:
    Vertical: t = {28, 129, 230, ...}
    Horizontal: t = {84, 187, 290, ...}

Derived linear equations:
    Vertical: t = 28 + 101n
    Horizontal: t = 84 + 103m
    (with n, m being natural numbers)

Equate both `t`s to find appearance of both rectangles:
    28 + 101n = t = t = 84 + 103m
    <=> 101n - 103m = 56

This is a diophantine linear equation which is solveable because:
    56 | gcd(101, -103)
    as gcd(101, -103) = 1 (as they are both prime)

Using the euclidean algorithm one can find:
    n = 103k + 75
    m = 101k + 73
    for k being any integer

The lowest positive solution would be:
    k = 0
    <=> n = 75
    <=> t = 28 + 101 * 75 = 7603
-}