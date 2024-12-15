import GHC.Arr (array, Array, (//), (!), listArray, assocs, bounds)
import Data.List (find)
import Data.Maybe (fromJust, fromMaybe)

readWarehous :: [String] -> (Int, Array Int Char)
readWarehous xs =
    let xs' = map (concatMap f) xs
        assocList = zip (concat . zipWith (\x -> map (+x)) [0,100..] . map (\x -> [0..(length x - 1)]) $ xs') (concat xs')
        robot = fst . fromJust . find ((=='@') . snd) $ assocList
    in (robot, listArray (0, length xs' * 100 - 1) (repeat '\NUL') // assocList)
    where
        f c
            | c == '#' = "##"
            | c == 'O' = "[]"
            | c == '.' = ".."
            | c == '@' = "@."

readMoves :: [String] -> [Int]
readMoves = map f . concat
    where
        f x
            | x == '^' = -100
            | x == '>' = 1
            | x == 'v' = 100
            | x == '<' = -1

infixr 8 ?:
(?:) :: a -> Maybe [a] -> Maybe [a]
(?:) x xs' = case xs' of
    Just xs -> Just (x : xs)
    Nothing -> Nothing

(~~) :: [a] -> [a] -> [a]
(~~) [] [] = []
(~~) xs [] = xs
(~~) [] ys = ys
(~~) (x : xs) (y : ys) = x : y : (xs ~~ ys)

(?~~) :: Maybe [a] -> Maybe [a] -> Maybe [a]
(?~~) xs' ys' = case xs' of
    Nothing -> Nothing
    Just xs -> case ys' of
        Nothing -> Nothing
        Just ys -> Just (xs ~~ ys)

warehouseChanges :: Array Int Char -> Int -> Int -> Maybe [(Int, Char)]
warehouseChanges warehouse r d
    | warehouse ! (r + d) == '.' = Just [(r, '.'), (r + d, warehouse ! r)]
    | warehouse ! (r + d) == '#' = Nothing
    | abs d == 1 || warehouse ! r == warehouse ! (r + d) = (r, '.') ?: (r + d, warehouse ! r) ?: warehouseChanges warehouse (r + d) d
    | otherwise =
        let s = if warehouse ! (r + d) == '[' then 1 else -1 in
        (r, '.')
        ?: (r + d, warehouse ! r)
        ?: warehouseChanges warehouse (r + d) d
        ?~~ warehouseChanges warehouse (r + d + s) d

move :: (Int, Array Int Char) -> Int -> (Int, Array Int Char)
move (robot, warehouse) direction = case warehouseChanges warehouse robot direction of
    Just changes -> (robot + direction, warehouse // reverse changes)
    Nothing -> (robot, warehouse)

boxes :: Array Int Char -> [Int]
boxes = map fst . filter ((=='[') . snd) . assocs

prettyPrint :: Array Int Char -> String
prettyPrint = filter (/='\NUL') . map f . assocs
    where f (i, c) = if i `mod` 100 == 99 then '\n' else c

printList :: [String] -> IO ()
printList [] = putStrLn ""
printList (x : xs) = do
    putStrLn x
    printList xs

brokenBox :: Array Int Char -> Bool
brokenBox warehouse = any f [1..(snd (bounds warehouse))]
    where
        f i =
            warehouse ! (i - 1) == '[' && warehouse ! i /= ']'
            || warehouse ! i == ']' && warehouse ! (i - 1) /= '['

main :: IO ()
main = do
    input <- readFile "2024/15/input.txt"
    let (linesWarehouse, linesMoves) = break null . lines $ input
    print $ sum . boxes . snd . foldl move (readWarehous linesWarehouse) . readMoves $ linesMoves