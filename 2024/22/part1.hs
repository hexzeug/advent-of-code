import Data.Bits (Bits(shift, xor, (.&.)))

int :: String -> Word
int = read

xorshift :: Int -> Word -> Word
xorshift n x = x `xor` x `shift` n .&. (2 ^ 24 - 1)

rand :: Word -> Word
rand = xorshift 11 . xorshift (-5) . xorshift 6

main :: IO ()
main = do
    input <- readFile "2024/22/input.txt"
    print $ sum . map ((!! 2000) . iterate rand . int) . lines $ input