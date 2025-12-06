import Control.Lens

findRolls :: [[Char]] -> [(Int,Int)]
findRolls m = findRollsMatrix $ (length m)-1
  where
        findRollsMatrix :: Int -> [(Int,Int)]
        findRollsMatrix (-1) = []
        findRollsMatrix i    = findRollsRow ((length (m!!i))-1) (reverse (m!!i)) ++ findRollsMatrix (i-1)
          where
                findRollsRow :: Int -> [Char] -> [(Int,Int)]
                findRollsRow (-1) _ = []
                findRollsRow j    (x:xs) | x == '@' = cc : findRollsRow (j-1) xs
                                         | otherwise = findRollsRow (j-1) xs
                  where
                        cc | countAdj '@' i j m < 5 = (i,j) -- 5 cuz it counts the current one too
                           | otherwise = (-1,-1)

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

-- finds accessible rolls and removes them until none of those are left
findRollsUpd :: [[Char]] -> Int
findRollsUpd m | res == 0 = 0
               | otherwise = res + findRollsUpd updatedMatrix
  where
        toRm = filter (/= (-1,-1)) $ findRolls m
        res = length toRm
        updatedMatrix = updMatrix '.' toRm m

-- takes a list of positions (i,j) and a matrix and replaces all those elements with c
updMatrix :: a -> [(Int,Int)] -> [[a]] -> [[a]]
updMatrix c [] m = m
updMatrix c (pos:toUpd) m = updMatrix c toUpd $ updMatrixSingle c pos m

-- thx https://stackoverflow.com/a/16075801/12206923
-- takes a matrix and replaces the element at pos = (i,j) with c
updMatrixSingle :: a -> (Int,Int) -> [[a]] -> [[a]]
updMatrixSingle c pos m = set (element i) newRow m
  where
        i = fst pos
        j = snd pos
        newRow = set (element j) c (m!!i)

main :: IO ()
main = do
    file <- readFile "input.txt"
    print $ findRollsUpd $ lines file
