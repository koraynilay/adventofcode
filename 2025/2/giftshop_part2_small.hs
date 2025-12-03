-- little bit cleaner and more compact version, but not much

import Data.List.Split

isInvalid :: Integer -> Integer
isInvalid x = isInvalidAux x (numLength x `div` 2)

isInvalidAux :: Integer -> Integer -> Integer
isInvalidAux _ 0 = 0
isInvalidAux x n | res && rel = x
                 | otherwise = isInvalidAux x (n-1)
  where
        baseRem = x `rem` (10^n)
        res = all (== baseRem) ll
        rel = all ((== n).numLength) ll
        ll = makelist n
        makelist k | d == 0 = []
                   | otherwise = d `rem` (10^n) : makelist (k+n)
          where d = x `div` (10^k)

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
