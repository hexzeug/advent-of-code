import Text.Regex.TDFA ( (=~), AllTextMatches(getAllTextMatches) )

int :: String -> Int
int = read

mulOperation :: String
mulOperation = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"

extractOperations :: String -> [String]
extractOperations x = getAllTextMatches (x =~ mulOperation)

fth :: (a, b, c, d) -> d
fth (_, _, _, x) = x

getOperands :: String -> [Int]
getOperands x = map int $ fth (x =~ mulOperation :: (String, String, String, [String]))

main :: IO ()
main = do
    input <- readFile "2024/03/input.txt"
    print $ sum . map (product . getOperands) . extractOperations $ input