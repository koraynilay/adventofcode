conv :: String -> Int
conv ('L':xs) = negate (read xs::Int)
conv ('R':xs) = (read xs::Int)

pp :: Int -> [Int] -> Int
pp _ [] = 0
pp y (x:xs) = (if xp == 0 then 1 else 0) + pp xp xs
  where xp = (x + y + 100) `rem` 100

main :: IO ()
main = do
    file <- readFile "input.txt"
    print $ pp 50 $ map conv $ lines file
