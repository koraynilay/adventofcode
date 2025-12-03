-- this is just a cleaner and more compact version

import Data.List.Split

isInvalid :: Integer -> Integer
isInvalid x | (nl `rem` 2 == 0) && l == r = x
            | otherwise = 0
  where
        nl = numLength x
        d = 10^(nl `div` 2)
        l = x `div` d
        r = x `rem` d

checkRange :: String -> [Integer]
checkRange s = map isInvalid [start..end]
  where
        range = splitOn "-" s
        start = read (range!!0)::Integer
        end = read (range!!1)::Integer

numLength :: Integer -> Integer
numLength x | x < 10 = 1
            | x < 100 = 2
            | x < 1000 = 3
            | x < 10000 = 4
            | x < 100000 = 5
            | x < 1000000 = 6
            | otherwise = 1 + numLength (x `div` 10)

main :: IO ()
main = do
    file <- readFile "input.txt"
    print $ sum $ concat $ map checkRange $ splitOn "," file
