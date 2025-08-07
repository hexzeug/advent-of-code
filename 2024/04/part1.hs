import Data.List (transpose)
import Text.Regex.TDFA ((=~))

rows :: [[a]] -> [[a]]
rows = id

rows' :: [[a]] -> [[a]]
rows' = map reverse . rows

columns :: [[a]] -> [[a]]
columns = transpose

columns' :: [[a]] -> [[a]]
columns' = map reverse . columns

diagonalsUL :: [[a]] -> [[a]]
diagonalsUL [] = []
diagonalsUL [row] = map (:[]) row
diagonalsUL (row : rows) =
    let diags = diagonalsUL rows
    in zipWith (:) row ([] : diags) ++ drop (length row - 1) diags

diagonalsUL' :: [[a]] -> [[a]]
diagonalsUL' = map reverse . diagonalsUL

diagonalsUR :: [[a]] -> [[a]]
diagonalsUR = diagonalsUL . rows'

diagonalsUR' :: [[a]] -> [[a]]
diagonalsUR' = map reverse . diagonalsUR

sequences :: [[a]] -> [[a]]
sequences xs = rows xs ++ rows' xs ++ columns xs ++ columns' xs ++ diagonalsUL xs ++ diagonalsUL' xs ++ diagonalsUR xs ++ diagonalsUR' xs

countXmas :: String -> Int
countXmas = (=~ "XMAS")

main :: IO ()
main = do
    input <- readFile "2024/04/input.txt"
    print $ sum . map countXmas . sequences . lines $ input