import Data.List (transpose)
import Text.Regex.TDFA ((=~))

sublists3 :: [a] -> [[a]]
sublists3 [] = []
sublists3 [_] = []
sublists3 [_, _] = []
sublists3 xs = take 3 xs : sublists3 (tail xs)

subsquares3x3 :: [[a]] -> [[[a]]]
subsquares3x3 = concatMap (transpose . map sublists3) . sublists3

diagonals :: [[a]] -> [[a]]
diagonals [[tl, _, tr], [_, c, _], [bl, _, br]] = [[tl, c, br], [bl, c, tr]]

isXmas :: [[Char]] -> Bool
isXmas = all (=~ "MAS|SAM") . diagonals

main :: IO ()
main = do
    input <- readFile "2024/04/input.txt"
    print $ length . filter isXmas . subsquares3x3 . lines $ input