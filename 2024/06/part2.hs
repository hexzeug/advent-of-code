import Data.List (elemIndex, elemIndices, group, sort)
import Data.Maybe (fromMaybe)
import Control.Parallel.Strategies (using, parListChunk, rdeepseq)

transform :: String -> String
transform = filter (/='\n')

start :: String -> Int
start = fromMaybe 0 . elemIndex '^' . transform

walls :: String -> [Int]
walls = elemIndices '#' . transform

size :: String -> Int
size = length . transform

width :: String -> Int
width = fromMaybe (-1) . elemIndex '\n'


wall :: [Int] -> Int -> Bool
wall = flip elem

outOfBounds :: Int -> Int -> Int -> Int -> Bool
outOfBounds size width x d
    | abs d == 1 = (x `div` width) /= ((x + d) `div` width)
    | otherwise = x + d < 0 || size <= x + d

turn :: Int -> Int -> Int
turn width d
  | d == -width = 1
  | d == 1 = width
  | d == width = -1
  | d == -1 = -width


emptys :: String -> [Int]
emptys input =
    let occupieds = start input : walls input
    in filter (not . wall occupieds) [0..(size input - 1)]


walk :: (Int -> Int) -> (Int -> Bool) -> (Int -> Int -> Bool) -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
walk trn wll oob d x path
    | oob x d = (x, d) : path
    | wll (x + d) =
        if (x, d) `elem` path then (-1, 0) : path
        else walk trn wll oob (trn d) x ((x, d) : path)
    | otherwise = walk trn wll oob d (x + d) path


walkWithObstacle :: String -> Int -> [(Int, Int)]
walkWithObstacle input x =
    let trn = turn $ width input
        wll = wall (x : walls input)
        oob = outOfBounds (size input) (width input)
        in walk trn wll oob (trn (-1)) (start input) []

main :: IO ()
main = do
    input <- readFile "2024/06/input.txt"
    let obstaclePositions = filter (((==(-1, 0)) . head) . walkWithObstacle input) (emptys input)
    print obstaclePositions
    print $ length obstaclePositions
    -- let test = obstaclePositions `using` parListChunk 64 rdeepseq
    -- print $ length test
    -- print test