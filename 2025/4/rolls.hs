-- iterates through the matrix and for each position occupied by a roll ('@') counts how many rolls
-- are adjacient, if there are less than 4 then the roll is accessible to the forklift
findRolls :: [[Char]] -> Int
findRolls m = findRollsMatrix $ (length m)-1
  where
        findRollsMatrix :: Int -> Int
        findRollsMatrix (-1) = 0
        findRollsMatrix i    = findRollsRow ((length (m!!i))-1) (reverse (m!!i)) + findRollsMatrix (i-1)
          where
                findRollsRow :: Int -> [Char] -> Int
                findRollsRow (-1) _ = 0
                findRollsRow j    (x:xs) | x == '@' = cc + findRollsRow (j-1) xs
                                         | otherwise = findRollsRow (j-1) xs
                  where
                        cc | countAdj '@' i j m < 5 = 1 -- 5 cuz it counts the current one too
                           | otherwise = 0

countAdj :: Char -> Int -> Int -> [[Char]] -> Int
countAdj c i j m = countElem c $ concat $ adjs i j m

countElem :: Eq a => a -> [a] -> Int
countElem e xs = length $ filter (== e) xs

adjs :: Int -> Int -> [[a]] -> [[a]]
adjs i j m = adjCols adjRows
  where adjRows = adjSingle i 1 m
        adjCols [] = []
        adjCols (x:xs) = (adjSingle j 1 x) : adjCols xs

adjSingle :: Int -> Int -> [a] -> [a]
adjSingle n s xs = drop (n-s) $ take (n+s+1) xs

main :: IO ()
main = do
    file <- readFile "input.txt"
    print $ findRolls $ lines file
