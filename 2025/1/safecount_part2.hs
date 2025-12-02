conv :: String -> Int
conv ('L':xs) = negate (read xs::Int)
conv ('R':xs) = (read xs::Int)

pp :: Int -> [Int] -> Int
pp _ [] = 0
pp y (x:xs) = (if xp == 0 then 1 else 0) + kk + ka + pp xp xs
  where
        rm = (x `rem` 100) + y
        xp = (rm + 100) `rem` 100
        kk = (abs x `div` 100)
        ka | y == 0 = 0
           | rm < 0 || rm > 100 = 1
           | otherwise = 0

main :: IO ()
main = do
    file <- readFile "input.txt"
    print $ pp 50 $ map conv $ lines file
