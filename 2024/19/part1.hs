import Data.Char (isSpace)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.List (stripPrefix)

split :: Char -> String -> [String]
split p str =
    let (x, xs) = break (==p) str
    in if null xs then [x] else x : split p (tail xs)

strip :: String -> String
strip = filter (not . isSpace)

readInput :: String -> ([String], [String])
readInput str =
    let (towels : _ : designs) = lines str
    in (map strip (split ',' towels), filter (not . null) designs)

arrange :: Eq a => [[a]] -> [a] -> Maybe [[a]]
arrange _ [] = Just []
arrange towels design = listToMaybe . mapMaybe f $ towels
    where
        f towel = case stripPrefix towel design of
            Nothing -> Nothing
            Just design' -> case arrange towels design' of
                Nothing -> Nothing
                Just arrangement -> Just (towel : arrangement)

main :: IO ()
main = do
    input <- readFile "2024/19/input.txt"
    let (towels, desings) = readInput input
    print $ length . mapMaybe (arrange towels) $ desings