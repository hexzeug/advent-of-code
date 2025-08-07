import Data.List (elemIndex, elemIndices, group, sort)
import Data.Maybe (fromMaybe)

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

outOfBounds :: Int -> Int -> Bool
outOfBounds size x = x < 0 || size <= x

turn :: Int -> Int -> Int
turn width d
  | d == -width = 1
  | d == 1 = width
  | d == width = -1
  | d == -1 = -width


walk :: (Int -> Int) -> (Int -> Bool) -> (Int -> Bool) -> Int -> Int -> [Int]
walk trn wll oob d x
    | oob (x + d) = [x]
    | wll (x + d) =
        let d' = trn d
        in x : walk trn wll oob d' (x + d')
    | otherwise = x : walk trn wll oob d (x + d)

main :: IO ()
main = do
    input <- readFile "2024/06/input.txt"
    let trn = turn $ width input
    let wll = wall $ walls input
    let oob = outOfBounds $ size input
    print $ length . group . sort $ walk trn wll oob (trn (-1)) (start input)