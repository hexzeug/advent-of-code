import GHC.Arr (array, Array, (//), (!), listArray, assocs)
import Data.List (find)
import Data.Maybe (fromJust, fromMaybe)

readWarehous :: [String] -> (Int, Array Int Char)
readWarehous xs =
    let assocList = zip (concat . zipWith (\x -> map (+x)) [0,100..] . map (\x -> [0..(length x - 1)]) $ xs) (concat xs)
        robot = fst . fromJust . find ((=='@') . snd) $ assocList
    in (robot, listArray (0, length xs * 100 - 1) (repeat '\NUL') // assocList)

readMoves :: [String] -> [Int]
readMoves = map f . concat
    where
        f x
            | x == '^' = -100
            | x == '>' = 1
            | x == 'v' = 100
            | x == '<' = -1


(?:) :: a -> Maybe [a] -> Maybe [a]
(?:) x xs' = case xs' of
    Just xs -> Just (x : xs)
    Nothing -> Nothing

warehouseChanges :: Array Int Char -> Int -> Int -> Maybe [(Int, Char)]
warehouseChanges warehouse r d
    | warehouse ! (r + d) == '.' = Just [(r + d, warehouse ! r)]
    | warehouse ! (r + d) == 'O' = (r + d, warehouse ! r) ?: warehouseChanges warehouse (r + d) d
    | warehouse ! (r + d) == '#' = Nothing

move :: (Int, Array Int Char) -> Int -> (Int, Array Int Char)
move (robot, warehouse) direction = case warehouseChanges warehouse robot direction of
    Just changes -> (robot + direction, warehouse // ((robot, '.') : changes))
    Nothing -> (robot, warehouse)

boxes :: Array Int Char -> [Int]
boxes = map fst . filter ((=='O') . snd) . assocs

prettyPrint :: Array Int Char -> [Char]
prettyPrint = filter (/='\NUL') . map f . assocs
    where f (i, c) = if i `mod` 100 == 99 then '\n' else c

main :: IO ()
main = do
    input <- readFile "2024/15/input.txt"
    let (linesWarehouse, linesMoves) = break null . lines $ input
    print $ sum . boxes . snd . foldl move (readWarehous linesWarehouse) . readMoves $ linesMoves