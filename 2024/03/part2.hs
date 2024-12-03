import Text.Regex.TDFA ((=~), AllMatches(getAllMatches), AllTextMatches (getAllTextMatches))

int :: String -> Int
int = read

doOperation :: String
doOperation = "do\\(\\)"

don'tOperation :: String
don'tOperation = "don't\\(\\)"

mulOperation :: String
mulOperation = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"

opIndecies :: String -> String -> [Int]
opIndecies op x = map fst (getAllMatches (x =~ op) :: [(Int, Int)])

doIndecies :: String -> [Int]
doIndecies = opIndecies doOperation

don'tIndecies :: String -> [Int]
don'tIndecies = opIndecies don'tOperation

mulIndecies :: String -> [Int]
mulIndecies = opIndecies mulOperation

extractOperations :: String -> [String]
extractOperations x = getAllTextMatches (x =~ mulOperation)

maskEnableds :: Bool -> [Int] -> [Int] -> [Int] -> [Bool]
maskEnableds state eabl dabl xs
  | state && null dabl = replicate (length xs) True
  | not state && null eabl = replicate (length xs) False
  | otherwise = let i = if state then head dabl else head eabl
                    s = length $ takeWhile (<i) xs
                    rest = maskEnableds (not state) (dropWhile (<=i) eabl) (dropWhile (<=i) dabl) (drop s xs)
                    in
                        if state
                            then replicate s True ++ rest
                            else replicate s False ++ rest

filterByMask :: [Bool] -> [b] -> [b]
filterByMask mask xs = map snd . filter fst $ zip mask xs

fth :: (a, b, c, d) -> d
fth (_, _, _, x) = x

getOperands :: String -> [Int]
getOperands x = map int $ fth (x =~ mulOperation :: (String, String, String, [String]))

main :: IO ()
main = do
    input <- readFile "2024/03/input.txt"
    let mask = maskEnableds True (doIndecies input) (don'tIndecies input) (mulIndecies input)
    print $ sum . map (product . getOperands) . filterByMask mask . extractOperations $ input